overfit <- read.csv("~/Downloads/overfitting.csv")
overfit$train <- as.logical(overfit$train)

save(overfit,file="~/Downloads/overfitting.RData")
head(overfit)
names(overfit)
with(overfit,Target_Practice[train])
#binary response variable

#predictors
pred.names <- names(overfit)[grep("^var_",names(overfit))]
a <- sapply(overfit,function(x) c(mean(x[overfit$train]),
                                  mean(x[!overfit$train])))
plot(a[1,-(1:5)])
plot(sapply(overfit,sd)[-(1:5)])


decomp <- svd(overfit[overfit$train,-(1:5)])
plot(decomp$d/sum(decomp$d))
barplot(decomp$v[,1])

cor.matrix <- cor(overfit[overfit$train,-(1:5)])
plot(sort(cor.matrix[upper.tri(cor.matrix,diag=F)],decreasing=T),
     ylim=c(-1,1),type="l")
abline(h=0,lty=3)
#could run a test to see whether these are significant
#but by snooping on the full data set we can see the variables are uncorrelated!

#single regressions
pred.aic <- vector("numeric",length(pred.names))
pred.pval <- pred.aic
for (n in 1:length(pred.names)) {
  glm.fit <- glm(Target_Practice~get(pred.names[n]),
                 data=overfit,family="binomial",subset=train)
  pred.aic[n] <- glm.fit$aic
  pred.pval[n] <- summary(glm.fit)$coefficients[2,4]
}

aic.rank  <- order(pred.aic,decreasing=F)
plot(pred.aic[aic.rank],type="b")
plot(pred.pval[aic.rank],type="b")


#actually, need a familywise critical value!
#Bonferonni adjustment is to divide the desired p-value by N!

crit.prob <- 0.50
pred.names[pred.pval < crit.prob/length(pred.names)]

#Holm step down
holm.crit <- crit.prob / (length(pred.names)+1-(1:length(pred.names)))
best.vars <- pred.names[aic.rank[1:which(pred.pval[aic.rank] > holm.crit)[1]-1]]

#Fit this model
glm.best <- glm(as.formula(paste("Target_Practice ~",paste(best.vars,collapse=" + "))),
    family="binomial",data=overfit,subset=train)
summary(glm.best)
plot(glm.best)
plot(glm.best$fitted.values,glm.best$model$Target_Practice)

misclass.error <- function(model,thresh=0.5) {
  fitted <- vector("numeric",length(model$fitted.values))
  fitted[model$fitted.values>thresh] <- 1
  fitted[model$fitted.values<=thresh] <- 0
  confusion.table <- table(fitted,model$model$Target_Practice)
  err <- (confusion.table[1,2])/sum(confusion.table)
  if (nrow(confusion.table)>1)
    err <- err+confusion.table[2,1]/sum(confusion.table)
  return(err)
}

auc <- function(outcome, proba){
  N = length(proba)
  N_pos = sum(outcome)
  df = data.frame(out = outcome, prob = proba)
  df = df[order(-df$prob),]
  df$above = (1:N) - cumsum(df$out)
  return( 1- sum( df$above * df$out ) / (N_pos * (N-N_pos) ) )
}

#Mislassification error by threshold
errors <- sapply(seq(0.01,0.99,0.01),function(t) misclass.error(glm.best,t))
plot(errors,type="b")

#AUC in and out of sample
auc(glm.best$model$Target_Practice,glm.best$fitted.values)
predictions <- predict.glm(glm.best,newdata=overfit[!overfit$train,])
auc(overfit$Target_Practice[!overfit$train],predictions)

#Fit a stepwise model
library(MASS)
glm.step <- stepAIC(glm(Target_Practice~1,
                        family="binomial",data=overfit,subset=train),
                    scope=as.formula(paste("Target_Practice ~",paste(pred.names,collapse=" + "))),
                    k=log(sum(overfit$train)))
plot(glm.step$anova$AIC)
plot(glm.step)
#this is almost certainly overfitted
  
  plot(glm.best$model[,2],glm.best$model[,1])
lims <- quantile(glm.best$model[,2],c(0,1))
new.data <- data.frame(seq(lims[1],lims[2],length.out=100))
colnames(new.data) <- pred.names[n]
new.data$fitted.val <- predict.glm(glm.best,newdata=new.data,type="response")
lines(new.data)

