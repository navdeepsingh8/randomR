data.path <- "~/Documents/Market Data/TradingBlox/Forex/"


library(xts)
library(lubridate)
library(TTR)

fx.files <- dir(data.path)
fx.prices <- read.zoo(file=file.path(data.path,fx.files[1]),
         header=FALSE,sep=",",FUN="ymd")[,1:4]
colnames(fx.prices) <- c("Open","High","Low","Close")
plot(fx.prices$Close)

hlr <- with(fx.prices,log(High/Low))
hlr <- hlr[!(hlr==0)]
fx.prices <- fx.prices[!(hlr==0)]

qqnorm(log(hlr))
qqline(log(hlr))
plot(log(hlr))

vol.forecast <- exp(HoltWinters(log(hlr),0.1,FALSE,FALSE))
vol.forecast <- lag(exp(EMA(log(hlr),25)),1)
plot(vol.forecast)

ema.crossover <- function(z,ema1,ema2) {
  return(lag(EMA(z,ema1)-EMA(z,ema2),1))
}

ret <- with(fx.prices,log(Close/lag(Close,1)))

strat.pnl <- function(SIG_FUN,stdise,...) {
  signal <- sign(get(SIG_FUN)(na.trim(fx.prices$Close/ifelse(stdise,vol.forecast,1)),...))
  strat.ret <- na.trim(signal*ret*(1/vol.forecast))
  return(mean(strat.ret)*sqrt(260)/sd(strat.ret))
}

mas <- seq(20,200,20)
combos <- lapply(mas,function(m) rbind(mas[mas<m],rep(m,sum(mas<m))))
combos <- do.call("cbind",combos)
short.mas <- unique(combos[1,])
long.mas <- unique(combos[2,])

strat.rets <- sapply(1:ncol(combos),function(c) 
  strat.pnl("ema.crossover",FALSE,combos[1,c],combos[2,c]))

ret.table <- matrix(NA,nrow=length(short.mas),
                    ncol=length(long.mas))
rownames(ret.table) <- short.mas
colnames(ret.table) <- long.mas

for (c in 1:ncol(combos)) {
  ret.table[which(combos[1,c]==short.mas),
            which(combos[2,c]==long.mas)] <- 
    strat.rets[c]
}

signif(ret.table,digits=1)


