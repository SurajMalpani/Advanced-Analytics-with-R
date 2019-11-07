
#Loading required packages
require(tidyverse)
require(data.table)
options(scipen=10)

#Setting the seed
seed <- 436604030

#Data loading and pre-processing
raw <- airquality
raw$Day <- NULL
raw$Month <- as.factor(raw$Month) 
raw <- raw[complete.cases(raw),]
rownames(raw) <- NULL

#Regression Models
base.model <- lm(Ozone~., data=raw)

set.seed(seed)
n <- 12       # size of sample
t <- 1:nrow(raw)
tst <- sample(t, n)
i <- tst

#tst <- as.integer(rownames(sample_frac(raw, 0.1)))
cv.model <- lm(Ozone~., data=raw[-tst,])
cv.fitted <- predict(cv.model, newdata =  raw[tst,])  


sub_df <- function(raw) {
  m <- lm(Ozone~., data=raw[-i,])
  p <- predict(m, raw[i,])
  df <- data.table(loc = as.integer(rownames(raw[i,])),
                   diff = raw[i,]$Ozone-p)
  return(df)
  }

cv.resid <- sub_df(raw = raw) #actual-predicted

# cv.resid <- data.table(
#   loc = as.integer(rownames(raw[tst,])),
#   diff = test$Ozone - unname(cv.fitted) #actual-predicted
# )
# all.equal(cv.resid, cv.resid2)


# Jackknife [ LOOCV ] -----
set.seed(seed)
i  <- 1:nrow(raw)

sub_jk <- function(i) {
  m <- lm(Ozone~., data=raw[-i,])
  p <- predict(m, raw[i,])
  return(raw[i,]$Ozone-p)
}

tmp <- data.table(loc=t, loocv=t)
jk.resid <- tmp[,.(diff=sub_jk(loocv)), by=.(loc) ]

#K-Fold  ---- 
# All obs 
set.seed(seed)
i <- 1:nrow(raw)
n  <- nrow(raw)
k <- 10
t <- rep(1:k, each=ceiling(n/k) ) #generates 12 entries rather than 10
t <- t[1:n]      
s <- sample(i, n)   #Shuffling


tmp <- data.table(loc=t, loocv=s)
kf.resid <- tmp[,.(diff=sub_jk(loocv)), by=.(loc)]
kf.resid$k <- t
kf.resid$loc <- s
kf.resid <- kf.resid[,c("k","loc","diff")]


### 4b Questions -----
mean(base.model$residuals)
mean(cv.resid$diff)
mean(jk.resid$diff)
mean(kf.resid$diff)

mean(base.model$residuals) - 1.96*(sd(base.model$residuals))
mean(cv.resid$diff)- 1.96*(sd(cv.resid$diff))
mean(jk.resid$diff)- 1.96*(sd(jk.resid$diff))
mean(kf.resid$diff)- 1.96*(sd(kf.resid$diff))

quantile(base.model$residuals, probs = seq(0,1,0.025))["97.5%"]
quantile(cv.resid$diff, probs=seq(0,1,0.025))["97.5%"]
quantile(jk.resid$diff, probs=seq(0,1,0.025))["97.5%"]
quantile(kf.resid$diff, probs=seq(0,1,0.025))["97.5%"]



