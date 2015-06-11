
freq <- 60*24*10
quants <- seq(0.9,0.7,-0.01)

quant.estimates <- function() {
log.prices <- cumsum(rnorm(freq,0,1/sqrt(freq)))
diff <- quantile(log.prices,quants)-quantile(log.prices,1-quants)
return(list(diff/(-1.418+2.717*quants),abs(tail(log.prices,1))*sqrt(pi/2)))
}

sim.estimates <- lapply(1:1000,function(z) quant.estimates())
mean.estim <- apply(sapply(sim.estimates,"[[",1),1,mean)
abs.estim <- mean(sapply(sim.estimates,"[[",2))
plot(quants,mean.estim,type="b",ylim=c(0,2))
lm.estim <- lm(mean.estim~quants)

plot(quants,var(sapply(sim.estimates,"[[",2))/apply(sapply(sim.estimates,"[[",1),1,var),
     ylab="")