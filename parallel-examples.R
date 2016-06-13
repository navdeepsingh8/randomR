library(parallel)
library(doParallel)
library(foreach)

#mcparallel / mccollect - run code in separate processes to the current process

#mclapply / mcmapply - parallel versions of lapply and mapply -- one core per element of the list
#pvec operates on chunks of the list at a time... it assumes that c(f(x[1]),f(x[2]))==f(x[1:2])

#parLapply, parSapply, parApply
cl <- makeCluster(detectCores()-1)
parSapply(cl,1:20,`+`,1)
stopCluster(cl)

#foreach

Filter

test.fun <- function(x,y,op=`+`) op(x,y)
mapply(test.fun,rep(1,4),1:4,MoreArgs=list(op=`*`))
Map(test.fun,rep(1,4),1:4)
#Map is a way of applying a k-ary function to k vectors of parameters

Reduce(test.fun,c(1,2,3,4))
#Reduce is a way of applying a binary function recursively to a vector of data

Filter(Negate(is.na),c(1,2,NA))

Position(Negate(is.na),c(1,2,NA))

Find(Negate(is.na),c(1,2,NA))

