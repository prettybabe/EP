source('D:/working/R/MyFunction.R')
library("dplyr", lib.loc="~/R/win-library/3.1")
library("reshape2", lib.loc="~/R/win-library/3.1")
library("RSQLServer", lib.loc="~/R/win-library/3.1")
library("rCharts", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("lubridate", lib.loc="~/R/win-library/3.1")
load("D:/working/GetData/IndustryWeeklyReturn.RData")
load("D:/working/EP/Data.RData")
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
data$SecuMainIndex <- tbl(channel, "QT_IndexQuote") %>%
  filter(InnerCode %in% c(3145, 4978, 4982)) %>%
  select(InnerCode, TradingDay, PrevClosePrice, ClosePrice) %>%
  collect %>%
  mutate(TradingDay = as.Date(TradingDay))

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

#################################################################################################

industry_ep <- data.frame()
for(i in c(1:nrow(trading_date))){
  start <- trading_date[[i, 1]]
  end <- trading_date[[i, 2]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == start) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, start >= InDate & start < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) %>%
    mutate(SecuCode = ifelse(SecuCode == '600849', '601607', SecuCode)) %>% 
    arrange(InnerCode)
  
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
  
  industry_ep <- rbind(industry_ep, industry_ep_temp)
}


ggplot(industry_ep, aes(x = IndustryNameNew, y = IndustryEP)) +
  geom_boxplot() + 
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(("Industry EP"))

##################################################################################################
#
index_weekly_return <- data.frame()
for(i in c(1:nrow(trading_date))){
  start <- trading_date[[i, 1]]
  end <- trading_date[[i, 2]]
  index_weekly_return_temp <- data$SecuMainIndex %>% 
    filter(TradingDay > start & TradingDay <= end) %>% 
    group_by(InnerCode) %>% 
    summarise(IndexWeeklyReturn = expm1(sum(log(ClosePrice/PrevClosePrice)))) %>% 
    mutate(TradingDay = start)
  
  index_weekly_return <- rbind(index_weekly_return, index_weekly_return_temp)
}
index_weekly_return <- index_weekly_return %>% 
  dcast(TradingDay ~ InnerCode, value.var = 'IndexWeeklyReturn') 
names(index_weekly_return) <- c("TradingDay", "CSI300", "CSI500", "CSI800")

############################################################################################

industry_ep_data <- industry_ep %>% 
  inner_join(industry_weekly_return, by = c("TradingDay", "IndustryNameNew"))


half_life <- 100
industry_number <- 6

portfolio <- industry_ep_data %>%
  group_by(IndustryNameNew) %>%
  arrange(TradingDay) %>%
  mutate(IndustryValueScore =  Score(IndustryEP, half_life)) %>%
  filter(!is.na(IndustryValueScore)) %>%
  group_by(TradingDay) %>%
  arrange(desc(IndustryValueScore)) %>% 
  slice(c(1:industry_number)) %>%
  group_by(TradingDay) %>%
  summarise(Sespended = weighted.mean(IndustryReturn, SespendedFloatMarketCap)) %>%
  inner_join(index_weekly_return, by = "TradingDay") %>%
  arrange(TradingDay) %>%
  mutate(Sespended_CSI300 = expm1(cumsum(log1p(Sespended - CSI300))),
         Sespended_CSI500 = expm1(cumsum(log1p(Sespended - CSI800)))) %>%
  ungroup() %>% 
  melt(id = c("TradingDay"), measure = c("Sespended_CSI300", "Sespended_CSI500"))

ggplot(portfolio, aes(x = TradingDay, y =  value, color = variable)) + geom_line() +
  ggtitle(paste(valuename, half_life, "industry_number ", industry_number)) +
  xlab(NULL) + ylab(NULL) 

###########################################################################################
IndustryShow(industry_ep_data, half_life = 100, "ep")
