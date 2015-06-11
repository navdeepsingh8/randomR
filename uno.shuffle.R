#Function defs
fresh.deck <- function() {
  deck <- data.frame(number=c(rep(0:9,2*4),rep(0,8)),
                     colour=c(rep(c("R","G","B","Y"),rep(20,4)),
                              rep("W",8)))
  deck$index <- 1:nrow(deck)
  return(deck)
}

played.deck <- function() {
  d <- fresh.deck()
  p <- d[sample(1:nrow(d),1,FALSE),]
  l <- d[!(p$index==d$index),]
  while(nrow(l)>0) {
    if (p[nrow(p),"colour"]=="W") {
      s <- l
    } else {
      s <- l[l$colour==p[nrow(p),"colour"]|
               l$number==p[nrow(p),"number"]|
               l$colour=="W",]
    }
    if (nrow(s)>0) {
      next.card <- s[sample(1:nrow(s),1,FALSE),]
    }      else {
      next.card <- l[sample(1:nrow(l),1,FALSE),]
    }
    p[nrow(p)+1,] <- next.card
    l <- l[!(next.card$index==l$index),]
  }
  return(p)
}

random.deck <- function() {
  d <- fresh.deck()
  return(d[sample(1:nrow(d)),])
}

deck.randomness <- function(d) {
  
  return(mean(d[1:(nrow(d)-1),"colour"]==d[2:nrow(d),"colour"] |
                d[1:(nrow(d)-1),"number"]==d[2:nrow(d),"number"]))
  
}

deck.randomness.2 <- function(d) {
  
  total.distance <- 0
  for (c in 1:(nrow(d)-1)) {
    this.card <- d[c,]
    local.distance <- 1
    next.card <- d[c+local.distance,]    
    while(this.card$number != next.card$number &&
            this.card$colour != next.card$colour &&
            this.card$colour != "W" &&
            next.card$colour != "W") {
      local.distance <- local.distance + 1
      if (c+local.distance <= nrow(d))
        next.card <- d[c+local.distance,]
      else
        break
    }
    total.distance <- total.distance + local.distance
  }
  return(total.distance/(nrow(d)-1))
}

shuffle.deck <- function(d,max.skim=10) {
  #for (t in 1:times) {
    skim.top <- sample(1:max.skim,1)
    skim.bottom <- sample(1:max.skim,1)
    d <- rbind(d[1:skim.top,],
               d[(nrow(d)-skim.bottom+1):nrow(d),],
               d[(skim.top+1):(nrow(d)-skim.bottom),])
  #}
  return(d)
}

riffle.deck <- function(d,max.flutter=5) {
  
  #for (t in 1:times) {
    cut <- sample(floor(nrow(d)*0.4):ceiling(nrow(d)*0.6),1)
    left <- d[1:cut,]
    right <- d[(cut+1):nrow(d),]
    i <- 0
    j <- 0
    s <- d[-(1:nrow(d)),]
    while((i<nrow(left))&&j<nrow(right)) {
      left.flutter <- sample(1:max.flutter,1)
      s <- rbind(s,left[(i+1):min(nrow(left),i+left.flutter),])
      i <- i+left.flutter
      right.flutter <- sample(1:max.flutter,1)
      s <- rbind(s,right[(j+1):min(nrow(right),j+right.flutter),])
      j <- j+right.flutter
    }
    if (i < nrow(left)) {
      s <- rbind(s,left[(i+1):nrow(left),])
    } else if (j < nrow(right)) {
      s <- rbind(s,right[(j+1):nrow(right),])
    }
    #d <- s
  #}
  return(s)
}

multiple.shuffle <- function(shuffle.fun,times,d,...) {
  r <- vector("numeric",length(times))
  for (t in 1:tail(times,1)) {
    s <- get(shuffle.fun)(d,...)
    if (t %in% times) {
      r[which(t==times)] <- deck.randomness.2(s)
    }
    d <- s
  }
  return(r)
}

col2alpha <- function(color,alpha) {
  rgb.values <- col2rgb(color)
  color <- rgb(rgb.values[1], rgb.values[2], rgb.values[3], alpha=alpha, maxColorValue=255)
  return(color)
}

# Compute stats

total.randomness <- sapply(1:100,function(z) deck.randomness.2(random.deck()))
played.randomness <- sapply(1:100,function(z) deck.randomness.2(played.deck()))

shuffle.times <- seq(5,100,5)
shuffled.randomness <- sapply(1:100,function(z) 
  multiple.shuffle("shuffle.deck",shuffle.times,played.deck()))
  
riffle.times <- 1:20
riffled.randomness <- sapply(1:100,function(z)
  multiple.shuffle("riffle.deck",riffle.times,played.deck()))


#Build plots
setwd("~/Desktop/")
png(filename="harpreet.shuffles.png",
    width=800,height=450)
plot(c(0,shuffle.times),
     c(mean(played.randomness),apply(shuffled.randomness,1,mean)),
     type="b",ylim=c(1,3),ylab="Order",xlab="Times shuffled",
     main="Harpreet style")
lines(c(0,shuffle.times),
      c(quantile(played.randomness,0.975),
        apply(shuffled.randomness,1,quantile,0.975)),
      type="l",lty=3)
lines(c(0,shuffle.times),
      c(quantile(played.randomness,0.025),
        apply(shuffled.randomness,1,quantile,0.025)),
      type="l",lty=3)

usr <- par('usr')
rect(usr[1], quantile(total.randomness,0.025), 
     usr[2], quantile(total.randomness,0.975), 
     col=col2alpha("darkgreen",50),border=NA) 
rect(usr[1], quantile(played.randomness,0.025), 
     usr[2], quantile(played.randomness,0.975), 
     col=col2alpha("red",50),border=NA) 
dev.off()

# Build riffle plot
png(filename="navdeep.shuffles.png",
    width=800,height=450)
plot(c(0,riffle.times),
     c(mean(played.randomness),apply(riffled.randomness,1,mean)),
     type="b",ylim=c(1,3),ylab="Order",xlab="Times shuffled",
     main="Navdeep style")
lines(c(0,riffle.times),
      c(quantile(played.randomness,0.975),
        apply(riffled.randomness,1,quantile,0.975)),
      type="l",lty=3)
lines(c(0,riffle.times),
      c(quantile(played.randomness,0.025),
        apply(riffled.randomness,1,quantile,0.025)),
      type="l",lty=3)

usr <- par('usr')
rect(usr[1], quantile(total.randomness,0.025), 
     usr[2], quantile(total.randomness,0.975), 
     col=col2alpha("darkgreen",50),border=NA) 
rect(usr[1], quantile(played.randomness,0.025), 
     usr[2], quantile(played.randomness,0.975), 
     col=col2alpha("red",50),border=NA) 
dev.off()
