## (1) Neyman-based inference; binary treatment and response

## Data

x <- c(rep(0,20), rep(1,20))
y <- c(0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,
       1,1,1,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,1,1)
n <- length(x)
k <- sum(x)  # size of treatment group

mean.ctrl <- mean(y[x==0])
mean.trt <- mean(y[x==1])
hat.ace <-  mean.trt - mean.ctrl


## Confidence interval:
var.ctrl <- var(y[x==0])
var.trt <- var(y[x==1])

hat.var.hat.ace <- var.trt/k + var.ctrl/(n-k)

##95% asymptotic confidence interval:

hat.ace - 1.96*sqrt(hat.var.hat.ace)
hat.ace + 1.96*sqrt(hat.var.hat.ace)

#######################################################

# (2) Neyman-based inference; binary treatment and 
#     continuous response

x <- c(rep(0,20), rep(1,20))
y <- c(9.87, 12.14,9.62,8.63,11.40,7.40,6.88,10.00,7.39,11.31,
       11.56, 8.78, 3.70, 10.36, 10.68, 6.60, 11.11, 7.94, 10.97, 10.43,
       14.90, 17.06, 20.62, 18.18, 15.29, 12.38, 18.27, 16.00, 13.45, 16.29,
       14.93, 11.35, 17.02, 16.60, 13.99, 16.59, 10.76, 16.79, 13.42, 15.30)
n <- length(x)
k <- sum(x)

mean.ctrl <- mean(y[x==0])
mean.trt <- mean(y[x==1])
hat.ace <-  mean.trt - mean.ctrl


## Confidence interval:
var.ctrl <- var(y[x==0])
var.trt <- var(y[x==1])

hat.var.hat.ace <- var.trt/k + var.ctrl/(n-k)

##95% asymptotic confidence interval:

hat.ace - 1.96*sqrt(hat.var.hat.ace)
hat.ace + 1.96*sqrt(hat.var.hat.ace)

#######################################################

# (3) Neyman-based inference; binary treatment and 
#     continuous response
#     Investigating coverage properties and additivity


n <- 40  #finite population size
k <- 20  #treatment group size


## (3a) Simulating potential outcomes under additivity
ace.true <- 3
yctrl <- rnorm(n,10,2) # simulating Y(0) [from N(10,2)]
ytrt <- yctrl + ace.true  # additivity
mean(ytrt) - mean(yctrl) # true ACE
nsims <- 5000
covered <- rep(NA,nsims) # vector for coverage indicators
hat.ace.vec <- rep(NA,nsims)
hat.var.hat.ace.vec <- rep(NA,nsims)

for(i in 1:nsims){
  trt.grp <- sample((1:n),k)  #choose our treatment group
  x <- rep(0,n)
  x[trt.grp] <- 1  #construct treatment vector
  y <- yctrl*(x==0) + ytrt*(x==1)  #build observed data vector
  ### Neyman procedure:
  mean.ctrl <- mean(y[x==0])
  mean.trt <- mean(y[x==1])
  hat.ace <-  mean.trt - mean.ctrl
  hat.ace.vec[i] <- hat.ace	# store the ACE estimate
  var.ctrl <- var(y[x==0])  # variance of control grp
  var.trt <- var(y[x==1])   # varianc
  hat.var.hat.ace <- var.trt/k + var.ctrl/(n-k)
  covered[i] <- (((hat.ace - 1.96*sqrt(hat.var.hat.ace)) < ace.true) &
                   ((hat.ace + 1.96*sqrt(hat.var.hat.ace)) > ace.true))
  hat.var.hat.ace.vec[i] <- hat.var.hat.ace # store var(hat ACE) estimate
}
sum(covered)/nsims  ## about 95%
mean(hat.ace.vec)  # are we unbiased ?
var(hat.ace.vec)   # true variability
mean(hat.var.hat.ace.vec) # are we unbiased?

plot(hat.ace.vec,hat.var.hat.ace.vec, main="hat(ACE) vs. hat(var(hat(ACE)))",
     ylab="hat(var(hat(ACE)))", xlab="hat(ACE)" )


## (3b) Simulating potential outcomes under non-additivity

ace.true <- 3
yctrl <- rnorm(n,10,2) # simulating from N(10,2)
heterog <- rnorm(n,0,3) # building disturbance vector
heterog <- heterog - mean(heterog) # ensuring sums to 0
ytrt <- yctrl + ace.true + heterog # building ytrt
mean(ytrt) - mean(yctrl) # True ACE
nsims <- 5000
covered <- rep(NA,nsims) # vector for coverage indicators
hat.ace.vec <- rep(NA,nsims)
hat.var.hat.ace.vec <- rep(NA,nsims)

for(i in 1:nsims){
  trt.grp <- sample((1:n),k)  #choose our treatment group
  x <- rep(0,n)
  x[trt.grp] <- 1  #construct treatment vector
  y <- yctrl*(x==0) + ytrt*(x==1)  #build observed data vector
  ### Neyman procedure:
  mean.ctrl <- mean(y[x==0])
  mean.trt <- mean(y[x==1])
  hat.ace <-  mean.trt - mean.ctrl
  hat.ace.vec[i] <- hat.ace	# store the ACE estimate
  var.ctrl <- var(y[x==0])  # variance of control grp
  var.trt <- var(y[x==1])   # varianc
  hat.var.hat.ace <- var.trt/k + var.ctrl/(n-k)
  covered[i] <- (((hat.ace - 1.96*sqrt(hat.var.hat.ace)) < ace.true) &
                   ((hat.ace + 1.96*sqrt(hat.var.hat.ace)) > ace.true))
  hat.var.hat.ace.vec[i] <- hat.var.hat.ace # store var(hat ACE) estimate
}
sum(covered)/nsims  ## 
mean(hat.ace.vec)  # are we unbiased ?
var(hat.ace.vec)   # true variability
mean(hat.var.hat.ace.vec) # are we unbiased?

plot(hat.ace.vec,hat.var.hat.ace.vec, main="hat(ACE) vs. hat(var(hat(ACE)))",
     ylab="hat(var(hat(ACE)))", xlab="hat(ACE)" )

## (3c) Simulating potential outcomes: Extreme scenario

ace.true <- 3
yctrl <- rnorm(n,0,2) # simulating from N(0,2)
yctrl <- yctrl - mean(yctrl)
ytrt <- ace.true - yctrl  # building ytrt
mean(ytrt) - mean(yctrl) # True ACE
nsims <- 5000
covered <- rep(NA,nsims) # vector for coverage indicators
hat.ace.vec <- rep(NA,nsims)
hat.var.hat.ace.vec <- rep(NA,nsims)

for(i in 1:nsims){
  trt.grp <- sample((1:n),k)  #choose our treatment group
  x <- rep(0,n)
  x[trt.grp] <- 1  #construct treatment vector
  y <- yctrl*(x==0) + ytrt*(x==1)  #build observed data vector
  ### Neyman procedure:
  mean.ctrl <- mean(y[x==0])
  mean.trt <- mean(y[x==1])
  hat.ace <-  mean.trt - mean.ctrl
  hat.ace.vec[i] <- hat.ace	# store the ACE estimate
  var.ctrl <- var(y[x==0])  # variance of control grp
  var.trt <- var(y[x==1])   # varianc
  hat.var.hat.ace <- var.trt/k + var.ctrl/(n-k)
  covered[i] <- (((hat.ace - 1.96*sqrt(hat.var.hat.ace)) < ace.true) &
                   ((hat.ace + 1.96*sqrt(hat.var.hat.ace)) > ace.true))
  hat.var.hat.ace.vec[i] <- hat.var.hat.ace # store var(hat ACE) estimate
}
sum(covered)/nsims  ##  100% coverage!
mean(hat.ace.vec)  # are we unbiased ?
var(hat.ace.vec)   # true variability = 0 (!)
mean(hat.var.hat.ace.vec) # are we unbiased?

plot(hat.ace.vec,hat.var.hat.ace.vec, main="hat(ACE) vs. hat(var(hat(ACE)))",
     ylab="hat(var(hat(ACE)))", xlab="hat(ACE)" )


#######################################################
## (4) Fisher's test of the null hypothesis of no effect

## Vesikari data (from lecture)

x <- c(rep(1,100),rep(0,100))
y   <- c(rep(1,63),rep(0,37),rep(1,48),rep(0,52))
table(y,x)

obs.test.stat <- sum(x*(1-y))

n <- length(x)
k <- sum(x)

nsims <- 5000
test.stat.vec <- rep(NA,nsims)

for(i in 1:nsims){
  trt.grp <- sample((1:n),k)  #choose our treatment group
  xsim <- rep(0,n)
  xsim[trt.grp] <- 1  #construct treatment vector
  test.stat <- sum(xsim*(1-y))
  test.stat.vec[i] <- test.stat
}


sum(test.stat.vec < (obs.test.stat+0.5))/nsims

## Compare to the true value
phyper(37,100,100,89)





#######################################################
##  (5) Fisher's randomization test continuous response
##
#data 
trt<-c("A","A","B","B","A","B","B","B","A","A","B","A")
y<-c(26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6)

ya<-y[trt=="A"]
yb<-y[trt=="B"]


#descriptive analysis
par(mfrow=c(1,2))
hist(ya) ; hist(yb)

par(mfrow=c(1,1))
boxplot(y~as.factor(trt))

mean(ya)   # same as mean(y[trt=="A"])
mean(yb)   # same as mean(y[trt=="B"])

sd(ya)
sd(yb)                                  


#now lets do a randomization test, 
#using the difference in means as our test 
#statistic. 


diff.obs<- (mean(yb)-mean(ya)) 




nsim<-10000
diff.null<-rep(0,nsim)
p.value<-rep(0,nsim)      


for(i in 1:nsim) {
  
  trt.sim<-sample(trt) 
  ya.sim<-y[trt.sim=="A"]
  yb.sim<-y[trt.sim=="B"]
  
  
  diff.sim<- (mean(yb.sim)-mean(ya.sim))
  
  diff.null[i]<-diff.sim
  
  p.value[i]<-mean(abs(diff.null[1:i])>=abs(diff.obs))
  #can you figure out what the above line does?
} 

#has the *estimated* p-value converged?
plot(p.value)
#note that p.value[nsim] will converge to the 
#correct p-value as nsim -> infinity. To decide
#if your nsim is big enough, use the above plot 
#and decide if it has converged

#ok, I think it has converged
hist(diff.null,prob=T)    # note prob=T makes the height equal to a proportion
abline(v= c( -abs(diff.obs), abs(diff.obs)), lty=2,col="blue")
p.value.rdist<-mean( abs(diff.null)>=abs(diff.obs) )
p.value.rdist

# compare to a normal distribution

sd.diff <- sqrt(var(ya)/(length(ya)) + var(yb)/(length(yb)))

x<-seq(-3*sd.diff,3*sd.diff,length=100)
lines(x, dnorm(x,0,sd.diff),col="green")


