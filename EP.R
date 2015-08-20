source('D:/working/R/MyFunction.R')
library("dplyr", lib.loc="~/R/win-library/3.1")
library("RMySQL", lib.loc="~/R/win-library/3.1")
library("TTR", lib.loc="~/R/win-library/3.1")
library("reshape2", lib.loc="~/R/win-library/3.1")
library("xlsx", lib.loc="~/R/win-library/3.1")
library("RSQLServer", lib.loc="~/R/win-library/3.1")
library("rCharts", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("lubridate", lib.loc="~/R/win-library/3.1")

##################################################################################################
# 下载需要的数据


channel <- src_sqlserver(server="SQL", database="XY", user="libo.jin", password="123456")
data <- list()
data$ReturnDaily <- tbl(channel, "ReturnDaily") %>%
  filter(IfTradingDay == 1) %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr, TradingDay, DailyReturn,
         MarketCap, FloatMarketCap, IndustryCodeNew, IndustryNameNew, IfSuspended) %>%
  collect %>%
  mutate(TradingDay = as.Date(TradingDay))


nIndexCode <- 4982
data$SecuMainIndex <- tbl(channel, "SecuMain") %>%
  filter(InnerCode == nIndexCode) %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr) %>%
  collect 

data$IndexComponent <- tbl(channel, "LC_IndexComponent") %>%
  select(IndexInnerCode, SecuInnerCode, InDate, OutDate) %>%
  filter(IndexInnerCode == nIndexCode) %>%
  collect %>%
  mutate(InDate = as.Date(InDate), OutDate = as.Date(OutDate)) 
data$IndexComponent$OutDate[is.na(data$IndexComponent$OutDate)] <- as.Date("2999-12-31")

data$TradingDay <- tbl(channel, "QT_TradingDayNew") %>%
  filter(SecuMarket == 83) %>%
  select(TradingDate, IfTradingDay, IfWeekEnd, IfMonthEnd, IfYearEnd) %>%
  collect %>%
  mutate(TradingDate = as.Date(TradingDate))

data$earning <- tbl(channel, "TTM_LC_IncomeStatementAll") %>%
  select(SecuCode, DataDate, NetProfit, NPParentCompanyOwners) %>%
  collect %>%
  mutate(DataDate = as.Date(ymd(DataDate))) %>% 
  mutate(Earning = ifelse(is.na(NPParentCompanyOwners), NetProfit, NPParentCompanyOwners)) %>% 
  select(SecuCode, DataDate, Earning) %>% 
  filter(!is.na(Earning))

##################################################################################################
#  确定交易时间和周期, 先尝试周度

startdate <- as.Date("2007-01-15")
enddate <- as.Date("2015-8-14")

trading_date <- data$TradingDay %>%
  filter(IfWeekEnd == 1, TradingDate >= startdate & TradingDate <= enddate) %>%
  select(TradingDate) %>%
  mutate(Start = lag(TradingDate)) %>%
  rename(End =  TradingDate) %>%
  select(Start, End) %>%
  na.omit()

###################################################################################################
#行业周度收益率
industry_weekly_return <- data.frame()
for(i in c(1:nrow(trading_date))){
  start <- trading_date[[i, 1]]
  end <- trading_date[[i, 2]]
  
  industry_weekly_return_temp <- data$ReturnDaily %>%
    filter(TradingDay > start & TradingDay <= end) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, start >= InDate & start < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) %>% 
    filter(!is.na(IndustryNameNew)) %>%
    group_by(InnerCode) %>%
    summarise(StockWeeklyReturn = expm1(sum(log1p(DailyReturn)))) %>%
    ungroup() %>%
    inner_join(data$ReturnDaily %>% filter(TradingDay == start), by = "InnerCode") %>% 
    group_by(IndustryNameNew) %>% 
    summarise(WeeklyReturn = weighted.mean(StockWeeklyReturn, FloatMarketCap),
              WeeklyReturn_SQRT = weighted.mean(StockWeeklyReturn, sqrt(FloatMarketCap)),
              FloatMarketCap = sum(FloatMarketCap)) %>% 
    mutate(Start = start, End = end)
  
  industry_weekly_return <- rbind(industry_weekly_return, industry_weekly_return_temp)
}

#################################################################################################

industry_ep_data <- data.frame()
for(i in c(1:nrow(trading_date))){
  start <- trading_date[[i, 1]]
  end <- trading_date[[i, 2]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == start) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, start >= InDate & start < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) 
  
  industry_ep_temp <- temp %>%
    inner_join(data$earning %>% filter(DataDate == start),
               by = c("SecuCode" = "SecuCode")) %>%
    mutate(EPS = Earning/MarketCap) %>%
    group_by(TradingDay) %>%
    mutate(WinsoriseEPS = ifelse(EPS > quantile(EPS, 0.99), quantile(EPS, 0.99),
                                 ifelse(EPS < quantile(EPS, 0.01), quantile(EPS, 0.01), EPS))) %>%
    group_by(TradingDay, IndustryNameNew) %>%
    summarise(IndustryEP = weighted.mean(EPS, FloatMarketCap)) %>%
    ungroup() 
  
  stock_return_temp <- temp %>%
    inner_join(data$ReturnDaily %>% 
                filter(TradingDay > start,  TradingDay <= end) %>%
                 select(InnerCode, DailyReturn), by = "InnerCode") %>% 
    group_by(InnerCode, FloatMarketCap, IndustryNameNew) %>% 
    summarise(StockReturn = expm1(sum(log1p(DailyReturn.y)))) %>%
    ungroup()
  
  industry_return_temp <- stock_return_temp %>% 
    group_by(IndustryNameNew) %>%
    summarise(UnSespendedFloatMarketCap = sum(FloatMarketCap)) %>% 
    inner_join(stock_return_temp %>% semi_join(temp %>% filter(IfSuspended == 0), by = "InnerCode"),
               by = "IndustryNameNew") %>%
    group_by(IndustryNameNew, UnSespendedFloatMarketCap) %>% 
    summarise(IndustryReturn = weighted.mean(StockReturn, FloatMarketCap),
              SespendedFloatMarketCap = sum(FloatMarketCap)) %>%
    ungroup()
  
  industry_ep_data_temp <- industry_ep_temp %>% 
    inner_join(industry_return_temp, by = "IndustryNameNew")
  
  industry_ep_data <- rbind(industry_ep_data, industry_ep_data_temp)
}

ggplot(industry_ep_data, aes(x = IndustryNameNew, y = IndustryEP)) +
  geom_boxplot() + 
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(("Industry EP"))

##################################################################################################

half_life <- 104
industry_number <- 5
portfolio <- industry_value %>%
  group_by(IndustryNameNew) %>%
  arrange(Start) %>%
  mutate(IndustryValueScore =  Score(IndustryValue, half_life)) %>%
  filter(!is.na(IndustryValueScore)) %>%
  group_by(Start) %>%
  arrange(desc(IndustryValueScore)) %>% 
  slice(c(1:industry_number)) %>%
  group_by(Start) %>%
  summarise(PortfolioReturn_equal = mean(IndustryReturn),
            PortfolioReturn_Sespended = weighted.mean(IndustryReturn, SespendedFloatMarketCap),
            PortfolioReturn_UnSespended = weighted.mean(IndustryReturn, UnSespendedFloatMarketCap)) %>%
  arrange(Start) %>%
  mutate(Equal = expm1(cumsum(log1p(PortfolioReturn_equal))),
         Sespended = expm1(cumsum(log1p(PortfolioReturn_Sespended))),
         UnSespended = expm1(cumsum(log1p(PortfolioReturn_UnSespended)))) %>%
  ungroup() %>% 
  melt(id = c("Start"), measure = c("Equal", "Sespended", "UnSespended"))

ggplot(portfolio, aes(x = Start, y =  value, color = variable)) + geom_line() +
        ggtitle(paste(valuename, half_life)) +
        xlab(NULL) + ylab(NULL) 


###########################################################################################
IndustryShow(industry_ep_data, half_life = 104, "ep")
