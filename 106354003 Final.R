library(ggplot2)
library(dplyr)
set.seed(2018)

x_old <- 100; x_new <- NULL; accept <- 0

for( i in 1:5000){
# Define proposal function  
y <- ifelse(x_old == 0, sample(c(0,1),1), sample(c(x_old-1,x_old+1),1))
# Define alpha 
alpha <- min((factorial(x_old)/factorial(y))*5^(y - x_old),1)
# Accept y accroding the probability alpha
temp <- sample(c(x_old,y),1,prob = c(1-alpha,alpha))
# if we accept the y, then acceptance + 1
if(x_old != temp){accept <- accept + 1}
x_new <- c(x_new, temp)
x_old <- temp
}

# time series plot
plot(x_new,type="l", xlab="time", ylab="x")
# ggplot
temp <- data.frame(index = 1:(5000), x_new = x_new)
ggplot(data = temp)+
  geom_line(mapping = aes(x = index, y = x_new))+
  ylab("x")+
  xlab("time")

acf(x)

# acceptance rate
accept.rate <- accept/5000
accept.rate

# Discard the first 2000 of xt
x.3000 <- x_new[2001:5000]

# mean of x.3000
mean(x.3000)

# variance of x.3000
var(x.3000)

# # of the x = 0,1,2...,10,11...
table(x.3000)

# rate of  the x = 0,1,2...,10,11... in percentage 
rate <- (table(x.3000)/3000*100) %>% round(., digits = 3)
rate
