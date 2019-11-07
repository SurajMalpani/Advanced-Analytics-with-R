
#Loading required packages
require(tidyverse)
require(data.table)
require(broom)

#Setting the seed
seed <- 436604030
set.seed(seed)

#Data loading and pre-processing
raw <- airquality
raw$Day <- NULL
raw$Month <- as.factor(raw$Month) 
raw <- raw[complete.cases(raw),]  
rownames(raw) <- NULL

n <- nrow(raw)
b <- 500

#Classic ------
set.seed(seed)
t <- NULL
g <- NULL
k <- NULL
cl.loop <- NULL
t <- as.vector(replicate(b, sample(n, replace=T), simplify=T))
g <- rep(1:b, each=n)
for (i in 1:b ){
  k <- t[g==i]
  cl.loop <- as.data.table(rbind(cl.loop, tidy(lm(Ozone~ ., data = raw[k,]))))
}

#classic - dt
set.seed(seed)
t <- NULL
g <- NULL
dt <- NULL
cl.dt <- NULL
t <- as.vector(replicate(b, sample(n, replace=T), simplify=T))
g <- rep(1:b, each=n)
dt <- data.table(t=t, g=g)
cl.dt <- dt[,tidy(lm(Ozone~ ., data = raw[t,])), by=g]

#Balanced ------
set.seed(seed)
t <- NULL
g <- NULL
k <- NULL
bal.loop <- NULL
t <- sample(rep(1:n, b))
g <- rep(1:b, each=n) 
for (j in 1:b ){
  k <- t[g==j] # index values
  bal.loop <- as.data.table(rbind(bal.loop, tidy(lm(Ozone~ ., data = raw[k,]))))
}

#balanced: dt
set.seed(seed)
t <- NULL
g <- NULL
dt <- NULL
bal.dt <- NULL
t <- sample(rep(1:n, b))
g <- rep(1:b, each=n)
dt <- data.table(t=t, g=g)
bal.dt <- dt[,tidy(lm(Ozone~ ., data = raw[t,])), by=g]

### 5B Ques -----
library(dplyr)
cl.dt %>%
  filter(term == "Wind") %>%
  summarize(mean(estimate))

cl_wind <-  cl.dt %>%
  filter(term == "Wind")
quantile(cl_wind$estimate, probs=seq(0,1,0.025))["2.5%"]
mean(cl_wind$estimate)+ 1.96*(sd(cl_wind$estimate))

cl_temp <-  cl.dt %>%
  filter(term == "Temp")
sd(cl_temp$estimate)
quantile(cl_temp$estimate, probs=seq(0,1,0.025))["97.5%"]
mean(cl_temp$estimate)- 1.96*(sd(cl_temp$estimate))   #---*

cl_wind %>%
  filter(p.value <= 0.05) %>%
  summarize(count = n())/nrow(cl_wind)

bal_wind <- bal.dt %>%
  filter(term == "Wind")
sd(bal_wind$estimate)
quantile(bal_wind$estimate, probs=seq(0,1,0.025))["97.5%"]
mean(bal_wind$estimate)- 1.96*(sd(bal_wind$estimate))

bal_temp <- bal.dt %>%
  filter(term == "Temp")
mean(bal_temp$estimate)
quantile(bal_temp$estimate, probs=seq(0,1,0.025))["2.5%"]
mean(bal_temp$estimate)+ 1.96*(sd(bal_temp$estimate))

bal_temp %>%
  filter(p.value < 0.05) %>%
  summarize(count = n())/nrow(bal_temp)

