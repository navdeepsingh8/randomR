a <- unzip('~/Downloads/sp.zip',exdir='~/Downloads/SP')
b <- read.table(a[1],header=F,sep=',')
colnames(b) <- c('Date','Open','High','Low','Close','Vol','OIC')
as.Date(paste0(c(1,2,2)[match(substr(b$Date,1,1),c(9,0,1))],b$Date),format='%Y%m%d')

install.packages('forecast')
library(dplyr)
a <- tbl_df(read.csv('~/Downloads/YAHOO-INDEX_GSPC.csv'))

a %>% 
  mutate(Date=as.POSIXct(Date),
         Range=log(High)-log(Low)) %>%
  arrange(Date) %>%
  select(Date:Volume,Range) -> b

b %>%
  filter(!(Range>1e-8)) %>% 
  last %>% 
  select(Date) -> date.thresh

b %>%
  filter(Date>date.thresh$Date) -> b

b %>% mutate(Jump=log(Open)-lag(log(Close)),
             True_Range=pmax(Range,abs(Jump))) -> b

library(xts)

library(forecast)
as.xts(select(b,-Date),b$Date) -> c
head(c,10)
plot(c$True_Range)
ss <- ses(as.vector(c$True_Range[-1]),initial='simple',alpha=0.0)
plot(ss$fitted)
str(c)
