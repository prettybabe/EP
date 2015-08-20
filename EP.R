source('D:/working/R/MyFunction.R')
source('D:/working/IndustryValue/IndustrySummary.R')
library("dplyr", lib.loc="~/R/win-library/3.1")
library("RMySQL", lib.loc="~/R/win-library/3.1")
library("TTR", lib.loc="~/R/win-library/3.1")
library("reshape2", lib.loc="~/R/win-library/3.1")
library("xlsx", lib.loc="~/R/win-library/3.1")
library("RSQLServer", lib.loc="~/R/win-library/3.1")
library("rCharts", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("lubridate", lib.loc="~/R/win-library/3.1")
load("D:/working/Data/data.RData")
load("D:/working/IndustryValue/IndustyWeeklyReturn.RData")

##################################################################################################
# 下载需要的数据
channel <- src_sqlserver(server="SQL", database="XY", user="libo.jin", password="123456")
earning <- tbl(channel, "TTM_LC_IncomeStatementAll") %>%
  select(SecuCode, DataDate, NetProfit, NPParentCompanyOwners) %>%
  collect %>%
  mutate(DataDate = as.Date(ymd(DataDate))) %>% 
  mutate(Earning = ifelse(is.na(NPParentCompanyOwners), NetProfit, NPParentCompanyOwners)) %>% 
  select(SecuCode, DataDate, Earning) %>% 
  filter(!is.na(Earning))

##################################################################################################
# 确认市场指数， 全流通 4088，中证500 4978， 中证800 4982， 沪深300 3145

nIndexCode <- 4982  

##################################################################################################
#  确定交易时间和周期, 先尝试周度

startdate <- as.Date("2007-01-15")
enddate <- as.Date("2015-6-29")

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
# EP
industry_ep <- data.frame()
for(i in c(1:nrow(trading_date))){
  date <- trading_date[[i, 1]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == date) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, date >= InDate & date < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) 
  
  industry_ep_temp <- temp %>%
    inner_join(earning %>% filter(DataDate == date),
               by = c("SecuCode" = "SecuCode")) %>%
    mutate(EPS = Earning/MarketCap) %>%
    group_by(TradingDay) %>%
    mutate(WinsoriseEPS = ifelse(EPS > quantile(EPS, 0.99), quantile(EPS, 0.99),
                                 ifelse(EPS < quantile(EPS, 0.01), quantile(EPS, 0.01), EPS))) %>%
    group_by(TradingDay, IndustryNameNew) %>%
    summarise(IndustryEP = weighted.mean(EPS, FloatMarketCap)) %>%
    ungroup() 
  
  industry_ep <- rbind(industry_ep, industry_ep_temp)
}

ggplot(industry_ep, aes(x = IndustryNameNew, y = IndustryEP)) +
  geom_boxplot() + 
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(("Industry EP"))

##################################################################################################

IndustryShow(industry_ep, industry_weekly_return,  half_life = 104, "ep")
