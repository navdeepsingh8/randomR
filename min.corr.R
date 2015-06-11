
min.minor <- function(rho,dim) {
  P <- matrix(rho,dim,dim)
  diag(P) <- 1
  minors <- lapply(2:dim,function(k) determinant(P[1:k,1:k],F))
  min(sapply(minors,"[[","modulus"))
}

rho.seq <- seq(-1,1,0.001)
dim.seq <- 2:31
val <- matrix(NA,length(rho.seq),length(dim.seq))

for (j in 1:length(dim.seq)) {
  for (i in 1:length(rho.seq)) {  
    val[i,j] <- min.minor(rho.seq[i],dim.seq[j])
  }
}

filled.contour(x=rho.seq,y=dim.seq,z=val,
               xlab="pairwise correlation",ylab="assets",
               main="Minimum principal minor")

