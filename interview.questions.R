compute.N <- function(input) {
x <- runif(1000,-1,1)
y <- runif(1000,-1,1)
z <- sqrt(x^2 + y^2)
N <- diff(c(0,which(z > 1)))
mean(N)
}
#N.series <- sapply(1:length(N), function(n) mean(N[1:n]))
#plot(N.series,type='l')
#plot(N.series[-(1:4999)],type="l")
#abline(h=1/(1-pi/4))

dist.N <- sapply(1:10000,compute.N)
hist(dist.N,50)
abline(v=1/(1-pi/4))

gam <- seq(0,1,length.out=100)
plot(gam,-1*gam^2+1.62*gam-0.38,type="l")
abline(h=0)

x <- seq(-10,10,length.out=100)
plot(x,(x)^2,type="l")
lines(x,(x+5)^2,col="red")

