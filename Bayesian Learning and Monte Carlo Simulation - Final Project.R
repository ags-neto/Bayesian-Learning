#Loading packages
library(readxl)
library(ggplot2)
library(rstanarm)
library(bayestestR)
library(insight)
library(runjags)
library(tidyverse)
library(Amelia)
library(kableExtra)
library(flextable)

#Loading the dataset
forestfire <- read.table("D:/Ann Wanjiru/Bayesian Analysis/forestfire.txt")
attach(forestfire)

#Grouping data
forest <- forestfire %>% group_by(month)


#Model 1
modelString <-"
model {
## sampling
for (i in 1:N){
   y[i] ~ dnorm(beta0 + beta1*FFMC[i] +
              beta2*DMC[i]+beta3*DC[i]+beta4*ISI[i]+beta5*temp[i]+beta6*RH[i]+beta7*wind[i]+beta8*rain[i]+beta9*month[i], invsigma2)
}
## priors
beta0 ~ dnorm(mu0, sigma_0)
beta1 ~ dnorm(mu1, sigma_1)
beta2 ~ dnorm(mu2, sigma_2)
beta3 ~ dnorm(mu3, sigma_3)
beta4 ~ dnorm(mu4, sigma_4)
beta5 ~ dnorm(mu5, sigma_5)
beta6 ~ dnorm(mu6, sigma_6)
beta7 ~ dnorm(mu7, sigma_7)
beta8 ~ dnorm(mu8, sigma_8)
beta9 ~ dnorm(mu9, sigma_9)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}
"
y <- forest$area
FFMC <- forest$FFMC
DMC <- forest$DMC
DC <- forest$DC
ISI <- forest$ISI
temp <- forest$temp
RH <- forest$RH
wind <- forest$wind
rain <- forest$rain
month <- as.factor(forest$month)
N <- length(y)
s0 <- 0.01
s1 <- 1/(var(forest$FFMC))
s2 <- 1/(var(forest$DMC))
s3 <- 1/(var(forest$DC))
s4 <-1/(var(forest$ISI))
s5 <- 1/(var(forest$temp))
s6 <- 1/(var(forest$RH))
s7 <- 1/(var(forest$wind))
s8 <- 1/(var(forest$rain))
  
the_data <- list("y" = y, "FFMC" = FFMC,
                 "DMC" = DMC,"DC"=DC,"ISI"=ISI,"temp"=temp,"RH"=RH,"wind"=wind,"rain"=rain,"month"=month,"N" = N,
                 "mu0" = 0, "sigma_0" = s0,
                 "mu1" = 0, "sigma_1" = s1,
                 "mu2" = 0, "sigma_2" = s2,
                 "mu3" = 0, "sigma_3" = s3,
                 "mu4" = 0, "sigma_4" = s4,
                 "mu5" = 0, "sigma_5" = s5,
                 "mu6" = 0, "sigma_6" = s6,
                 "mu7" = 0, "sigma_7" = s7,
                 "mu8" = 0, "sigma_8" = s8,
                 "mu9" = 0, "sigma_9" = 0.001,
                 "a" = 0.001, "b" = 0.001)
posterior <- run.jags(modelString,
                      n.chains = 2,
                      data = the_data,
                      monitor = c("beta0", "beta1",
                                  "beta2","beta3","beta4","beta5","beta6","beta7","beta8","beta9", "sigma"),
                      burnin = 2000,
                      sample = 5000)
#Posterior summary
print(posterior, digits = 3)

#Model 1
modelString1 <-"
model {
## sampling
for (i in 1:N){
   y[i] ~ dnorm(beta0 +beta5*temp[i]+beta6*RH[i]+beta7*wind[i]+beta8*rain[i], invsigma2)
}
## priors
beta0 ~ dnorm(mu0, sigma_0)
beta5 ~ dnorm(mu5, sigma_5)
beta6 ~ dnorm(mu6, sigma_6)
beta7 ~ dnorm(mu7, sigma_7)
beta8 ~ dnorm(mu8, sigma_8)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}
"
y <- forest$area
temp <- forest$temp
RH <- forest$RH
wind <- forest$wind
rain <- forest$rain
N <- length(y)
s0 <- 0.01
s5 <- 1/(var(forest$temp))
s6 <- 1/(var(forest$RH))
s7 <- 1/(var(forest$wind))
s8 <- 1/(var(forest$rain))
  
the_data1 <- list("y" = y,"temp"=temp,"RH"=RH,"wind"=wind,"rain"=rain,"N" = N,
                 "mu0" = 0, "sigma_0" = s0,
                 "mu5" = 0, "sigma_5" = s5,
                 "mu6" = 0, "sigma_6" = s6,
                 "mu7" = 0, "sigma_7" = s7,
                 "mu8" = 0, "sigma_8" = s8,
                 "a" = 0.001, "b" = 0.001)

posterior1 <- run.jags(modelString1,
                      n.chains = 2,
                      data = the_data1,
                      monitor = c("beta0","beta5","beta6","beta7","beta8", "sigma"),
                      burnin = 2000,
                      sample = 5000)
#Posterior summary
print(posterior1, digits = 3)

#model 2
modelString2 <-"
model {
## sampling
for (i in 1:N){
   y[i] ~ dnorm(beta0 +beta5, invsigma2)
}
## priors
beta0 ~ dnorm(mu0, sigma_0)
beta5 ~ dnorm(mu5, sigma_5)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}
"
y <- forest$area
temp <- forest$temp
N <- length(y)
s0 <- 0.01
s5 <- 1/(var(forest$temp))
  
the_data2 <- list("y" = y,"temp"=temp,"N" = N,
                 "mu0" = 0, "sigma_0" = s0,
                 "mu5" = 0, "sigma_5" = s5,
                 "a" = 0.001, "b" = 0.001)

posterior2 <- run.jags(modelString2,
                      n.chains = 2,
                      data = the_data2,
                      monitor = c("beta0","beta5", "sigma"),
                      burnin = 2000,
                      sample = 5000)
#Posterior summary
print(posterior2, digits = 3)

#Computing DIC of the model
extract.runjags(posterior, "dic")
#Computing DIC of the model
extract.runjags(posterior1, "dic")
#Computing DIC of the model
extract.runjags(posterior2, "dic")

plot(posterior)



