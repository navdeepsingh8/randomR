#Simulate the hypothesis testing method
first.rejection <- function(sig.lev,n,k) {
  x <- rnorm(n,0,1)
  z <- cumsum(x)/(1:n)
  p.vals <- 1-pnorm(z[k:n],0,1/sqrt(k:n))
  reject.at <- head(which((p.vals > (1-sig.lev/2)) | (p.vals < (sig.lev/2))),1)
  if (length(reject.at)==0) {
    return(0)
  } else{
    return(1)
  }
}

#Set up simulations
k.vec <- seq(10,200,10) #initial sample sizes
n.mult <- 10 #maximum sample size
sig.lev <- 0.05 #significance level
sims <- 2000 #simulation size

#Run through simulations and store results
sim.results <- sapply(k.vec,function(k) 
  mean(sapply(1:sims,function(x) 
    first.rejection(sig.lev,k*n.mult,k))
  )
)

#Plot true size against k
plot(k.vec,sim.results,type="l",
     xlab="k",ylab="True size",
     ylim=c(0,1),
     main=paste("n=",n.mult,"*k,sig=",sig.lev,sep=""))
abline(h=sig.lev,lty=2)
grid()

