rm(list=ls())
### D-separation in the context of linear models

# Simulate a dataset from a DAG
# b --> x <-- c
# b --> y <-- c
# c --> z <-- d
# z --> r
#
## Model 1
n <- 1000

# Error terms
eb <- rnorm(n,0,1)
ec <- rnorm(n,0,1)
ed <- rnorm(n,0,1)
ex <- rnorm(n,0,1)
ey <- rnorm(n,0,1)
ez <- rnorm(n,0,1)
er <- rnorm(n,0,1)

b <- 2 + eb
c <- -1 + ec
d <- 2 + ed
x <- 1 + 2*b - 1*c + ex     # b --> x <-- c
y <- -1 -b + 2*c + ey       # b --> y <-- c
z <- -2 + 3*c - 2*d + ez    # c --> z <-- d
r <- 3 + 2*z + er           # z --> r

######################################


## Marginal Independence 
## Using d-separation work out which coefficients should be non-zero
## before doing the regressions:

## Is X ind Y?
lm(y~x)
tmp <- lm(y ~ x)
summary(tmp)

## Is B ind C?
lm(b~c)
tmp <- lm(b ~ c)
summary(tmp)

### Conditional Independence

## Is Y ind X given B and C?
tmp <- lm(y ~ b + c + x)
summary(tmp)
## Note: check coeff of x in regression of y on b,c,x


## Is B _||_ C given X and Y?
tmp <- lm(b ~ c + x + y)
summary(tmp)

## Is C _||_ D given R?
tmp <- lm(c ~ d + r)
summary(tmp)

## Is X _||_ D given R?
tmp <- lm(x ~ d + r)
summary(tmp)

## Is X _||_ D given C,R?
tmp <- lm(x ~ d + c+ r)
summary(tmp)

## Is D _||_ B given R?
tmp <- lm(d ~ b + r)
summary(tmp)




