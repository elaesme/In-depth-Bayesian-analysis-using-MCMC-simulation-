
# REQUIRED PACKAGES

require(R2jags)
require(mcmcse)
require(bayesplot)
require(TeachingDemos)
require(ggmcmc)

# READ DATA

datt <- read.csv("JordanVsBird.csv")
datt

datjordan<- datt[datt$Player=='Jordan',]
datbird<- datt[datt$Player=='Bird',]

## CHECK THEY ARE DIFFERENT PLAYERS WITH DIFFERENT PROPORTIONS

phatjordan<-sum(datjordan$FG)/sum(datjordan$FGA)
phatbird<-sum(datbird$FG)/sum(datbird$FGA)
phatjordan
phatbird

# MARKOV CHAIN MONTE CARLO

# MODEL CONSTRUCTION FOR JAGS
model<- function() {
  for(i in 1:N)
  {
    #LIKELIHOOD
    FG[i] ~ dbinom(p[i], FGA[i])
    
    logit(p[i]) <- beta0j[players[i]]  # LOGIT AS LINK FUNCTION
  }
  
  # PRIORS
  for (i in 1:playertypes)
  {
    beta0j[i] ~ dnorm(0, 1.0E-6)
  }
}


# DATA NEEDED TO USE JAGS: FG: FIELD GOALS(SUCCESS), FGA: FIELD GOAL ATTEMPTS (TRIALS)
FG <- datt$FG
FGA <- datt$FGA
N <- nrow(datt)

players <- as.factor(datt$Player)
playertypes <- length(levels(players))

dat.jags <- list("FG", "FGA", "N", "players", "playertypes")


# PARAMETERS OF INTEREST FOR JAGS 
mod.params <- c("beta0j")

# INITIAL VALUES FOR THE MODEL TO START WITH
mod.inits <-  function(){
  list("beta0j" = rnorm(2, 0, 10))
}

# RUNNING THE MODEL
set.seed(123)
mod.fit <- jags(data = dat.jags,                                    
                model.file = model, inits = mod.inits,          
                parameters.to.save = mod.params,                  
                n.chains = 3, n.iter = 9000, n.burnin = 1000, n.thin=10)       


# RESULTS AND DIAGNOSTICS WITH PLOTS, INTERVAL AND POINT ESTIMATES

mod.fit

chainArray <- mod.fit$BUGSoutput$sims.array

# BAYESPLOT 
bayesplot::mcmc_combo(chainArray)
bayesplot::mcmc_acf(chainArray)

# DIAGNOSTICS WITH CODA
coda.fit <- as.mcmc(mod.fit)

coda::geweke.plot(coda.fit)
coda::gelman.plot(coda.fit)

#CHAIN MANIPULATION

chainMat <- mod.fit$BUGSoutput$sims.matrix

# POINT ESTIMATES
beta.hat <- colMeans(chainMat)
beta.hat

# EQUAL TAIL INTERVAL WITH 0.95 CREDIBILITY
cred <- 0.95
beta.EqualT <- apply(chainMat, 2, quantile, prob=c((1-cred)/2, 1-(1-cred)/2))
beta.EqualT

# HIGHEST POSTERIOR DENSITY INTERVAL
beta.HPD <- coda::HPDinterval(as.mcmc(chainMat))
beta.HPD

# COLLECTION OF DIAGNOSTIC GRAPHS AS A PDF (ALREADY CREATED)
#S <- ggs(as.mcmc(mod.fit))
#ggmcmc(S)



##########MODEL COMPARISON WITH RANDOM EFFECT MODEL##########

modelRand <- function() {
  # Likelihood
  for(i in 1:N)
  {
    FG[i] ~ dbinom(p[i], FGA[i])
    
    logit(p[i]) <- beta0j[players[i]]  # Link
  }
  
  
  # Pooling
  for (i in 1:playertypes)
  {
    beta0j[i] ~ dnorm(mu, tau)
  }
  mu ~ dnorm(0, 1.0E-6)
  tau ~ dgamma(1.0E-3, 1.0E-3)
  
  # PARAMETER TRANS
  sigma <- 1/sqrt(tau)
}



players <- as.factor(datt$Player)
playertypes <- length(levels(players))

# Data in list
datrand.jags <- list("FG", "FGA", "N", "players", "playertypes")


# Parameters --------------------------------------------------------------

modRand.params <- c("beta0j", "mu", "sigma")

# Starting values

modRand.inits <- function(){
  list("beta0j" = rnorm(2, 0, 10),
       "mu" = rnorm(1, 0, 10),
       "tau" = rgamma(1, 1, 1))
}


# Run JAGS ----------------------------------------------------------------

set.seed(1234)
modRandom.fit <- jags(data = datrand.jags,                                    # DATA
                      model.file = modelRand, inits = modRand.inits,          # MODEL
                      parameters.to.save = modRand.params,                  
                      n.chains = 3, n.iter = 9000, n.burnin = 1000, n.thin=10)       # MCMC

mod.fit
modRandom.fit


#######SIMULATING THE MODEL#######


# SAMPLE SIZE

N <- 100

# SIMULATE FIELD GOAL ATTEMPT SIZE    #(sum(datjordan$FGA)+ sum(datbird$FGA))/25==21.24
FGA <- rpois(N,22)

# SIMULATING COVARIATE
Player <- sample(c("jordan", "bird"), N, replace=T)

# FIXED PARAMETERS FOR THE MODEL
beta01 <- 0.044
beta02 <- -0.015


# SIMULATE RESPONSE WITH PREVIOUSLY DECLARED MODEL
linpred <- beta01*(Player=="bird") + beta02*(Player=="jordan")
pis <- exp(linpred)/(1+exp(linpred))
FG <- rbinom(N, FGA, pis)

datt <- data.frame(FG=FG, FGA=FGA, Player=Player)

# MARKOV CHAIN MONTE CARLO INFERENCE

# DATA NEEDED TO USE JAGS: FG: FIELD GOALS(SUCCESS), FGA: FIELD GOAL ATTEMPTS (TRIALS)
FG <- datt$FG
FGA <- datt$FGA
N <- nrow(datt)


players <- as.factor(datt$Player)
playertypes <- length(levels(players))

dat.jags <- list("FG", "FGA", "N", "players", "playertypes")


# RUNNING THE MODEL
set.seed(123)
mod.fit <- jags(data = dat.jags,                                    
                model.file = model, inits = mod.inits,          
                parameters.to.save = mod.params,                  
                n.chains = 3, n.iter = 9000, n.burnin = 1000, n.thin=10)       


# RESULTS AND DIAGNOSTICS WITH PLOTS, INTERVAL AND POINT ESTIMATES

mod.fit
mod.fit$BUGSoutput$summary


#FREQINTIST APPROACH TRIAL

mod1 <- lm(FG/FGA ~ Player, data = datt)

plot(mod1)
summary(mod1)



