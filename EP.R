source('D:/working/R/MyFunction.R')
library("dplyr", lib.loc="~/R/win-library/3.1")
library("reshape2", lib.loc="~/R/win-library/3.1")
library("RSQLServer", lib.loc="~/R/win-library/3.1")
library("rCharts", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("lubridate", lib.loc="~/R/win-library/3.1")

source('D:/working/Function/MyFunction.R', encoding = 'UTF-8')


# load("D:/working/GetData/IndustryWeeklyReturn.RData")
# load("D:/working/EP/Data.RData")
##################################################################################################
# 下载需要的数据
source('D:/working/GetData/GetBaseData.R')
data$earning <- tbl(channel, "TTM_LC_IncomeStatementAll") %>%
  select(SecuCode, DataDate, NetProfit, NPParentCompanyOwners) %>%
  collect %>%
  mutate(DataDate = as.Date(ymd(DataDate))) %>% 
  mutate(Earning = ifelse(is.na(NPParentCompanyOwners), NetProfit, NPParentCompanyOwners)) %>% 
  select(SecuCode, DataDate, Earning) %>% 
  filter(!is.na(Earning))

##################################################################################################
#  确定交易时间和周期, 先尝试周度
nIndexCode <- 4982
startdate <- as.Date("2007-01-15")
enddate <- as.Date("2015-8-14")
nFrequency <- "Weekly"

trading_date <- TradingDay(data, startdate, enddate, nFrequency)

industry_return <- IndustryReturn(data, nIndexCode, startdate, enddate, nFrequency)

index_return <- IndexReturn(data, startdate, enddate, nFrequency)

index_return <- index_return %>% 
  dcast(TradingDay ~ InnerCode, value.var = 'IndexReturn') 
names(index_return) <- c("TradingDay", "CSI300", "CSI500", "CSI800", "ZZLT")

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
    summarise(IndustryEP = weighted.mean(EPS, FloatMarketCap),
              IndustryEP_SQRT = weighted.mean(EPS, sqrt(FloatMarketCap))) %>%
    ungroup() 
  
  industry_ep <- rbind(industry_ep, industry_ep_temp)
}

ggplot(industry_ep, aes(x = IndustryNameNew, y = IndustryEP)) +
  geom_boxplot() + 
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(("Industry EP"))

############################################################################################
industry_value <- industry_ep %>% 
  select(TradingDay, IndustryNameNew, IndustryEP_SQRT) %>%
  inner_join(industry_return, by = c("TradingDay", "IndustryNameNew"))
names(industry_value) <- c("TradingDay", "IndustryNameNew","IndustryValue",
                           "IndustryReturn", "FloatMarketCap")

half_life <- 24
industry_number <- 6

portfolio <- industry_value %>%
  group_by(IndustryNameNew) %>%
  arrange(TradingDay) %>%
  mutate(IndustryValueScore =  Score(IndustryValue, half_life)) %>%
  filter(!is.na(IndustryValueScore)) %>%
  group_by(TradingDay) %>%
  arrange(desc(IndustryValueScore)) %>% 
  slice(c(1:industry_number)) %>%
  group_by(TradingDay) %>%
  summarise(Return = weighted.mean(IndustryReturn, sqrt(FloatMarketCap)))

YearlyIR(portfolio$Return, 12)

portfolio_cumulate <- portfolio %>%
  inner_join(index_return, by = "TradingDay") %>%
  arrange(TradingDay) %>%
  mutate(Return = expm1(cumsum(log1p(Return))),
         CSI300 = expm1(cumsum(log1p(CSI300))),
         CSI500 = expm1(cumsum(log1p(CSI800)))) %>%
  ungroup() %>% 
  melt(id = c("TradingDay"), measure = c("Return", "CSI300", "CSI500"))

ggplot(portfolio_cumulate, aes(x = TradingDay, y =  value, color = variable)) + geom_line() +
  ggtitle(paste("ep", half_life, "industry_number ", industry_number)) +
  xlab(NULL) + ylab(NULL) 

###########################################################################################

portfolio_demean <- industry_value %>% 
  inner_join(index_return %>% select(TradingDay,  ZZLT), by = "TradingDay") %>% 
  mutate(IndustryReturn = IndustryReturn - ZZLT) %>% 
  select(-ZZLT)

IndustryShow(portfolio_demean, half_life = 24, "ep") 

