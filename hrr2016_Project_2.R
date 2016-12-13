## setting working directory ##

setwd("G:/Fall 2016/POL 667_Human Rights and Repression/Projects/Datasets")

## Loading necessary packages ##

require(foreign)
require(haven)
require(SDMTools)
require(MASS)
require(car)
require(DataCombine)
require(data.table)
require(plyr)
require(gdata)
require(brglm)
require(tidyr)
require(sampleSelection)
require(stargazer)
require(ggplot2)
require(Hmisc)
require(reshape2)
require(effects)
require(aod)
require(pscl)
require(ordinal)

## loading datasets##

hrr2016 <- read.csv("hrr2016.csv")
ucdp2014 <- read.csv("ucdp2014.csv")
pts2015 <- read.csv("pts2015b.csv")
gleditsch2011<- read.csv("gleditsch2012.csv")
countrydata <- read.csv("countrydata.csv")

## for loop adding intrastate conflict variable to hrr2016 dataset ##

hrr2016$intrastateconflict<-NA

for(i in 1:nrow(hrr2016)) ## searching through nrow of hrr2016 dataset ##
  
{
  
  test<-ucdp2014$incidencev414[which(ucdp2014$gwno==hrr2016$COW[i]
                                     & ucdp2014$year==hrr2016$YEAR[i] ) ] # looking through UCDP dataset
  
  if(length(test)==1) # since pre populated with "NA's" we need to only pull data with a value
    
  {
    
    hrr2016$intrastateconflict[i] <- test # attach/assign test to hrr2016 #
    
  } #if
  
  if(length(test) > 1)
    
  {
    
    hrr2016$intrastateconflict[i] <- max(test)
    
  } #if
  
}

## for loop adding lagged GDP variable to hrr2016 dataset ##

hrr2016$rgdppc2005lag<-NA

for(i in 1:nrow(hrr2016)) ## searching through nrow of hrr2016 dataset ##
  
{
  
  test<-countrydata$rgdppc2005lag[which(countrydata$lccode==hrr2016$COW[i]
                                        & countrydata$year==hrr2016$YEAR[i] ) ] # looking through countrydata dataset
  
  if(length(test)==1) # since pre populated with "NA's" we need to only pull data with a value
    
  {
    
    hrr2016$rgdppc2005lag[i] <- test # attach/assign test to hrr2016 #
    
  } #if
  
  if(length(test) > 1)
    
  {
    
    hrr2016$rgdppc2005lag[i] <- max(test)
    
  } #if
  
}

## for loop adding militarized interstate dispute variable to hrr2016 dataset ##

hrr2016$mid<-NA

for(i in 1:nrow(hrr2016)) ## searching through nrow of hrr2016 dataset ##
  
{
  
  test<-countrydata$mid[which(countrydata$lccode==hrr2016$COW[i]
                              & countrydata$year==hrr2016$YEAR[i] ) ] # looking through countrydata dataset
  
  if(length(test)==1) # since pre populated with "NA's" we need to only pull data with a value
    
  {
    
    hrr2016$mid[i] <- test # attach/assign test to hrr2016 #
    
  } #if
  
  if(length(test) > 1)
    
  {
    
    hrr2016$mid[i] <- max(test)
    
  } #if
  
}

## for loop adding civil resistance dummy variable to hrr2016 dataset ##

hrr2016$civilres<-NA

for(i in 1:nrow(hrr2016)) ## searching through nrow of hrr2016 dataset ##
  
{
  
  test<-countrydata$civilres[which(countrydata$lccode==hrr2016$COW[i]
                                   & countrydata$year==hrr2016$YEAR[i] ) ] # looking through countrydata dataset
  
  if(length(test)==1) # since pre populated with "NA's" we need to only pull data with a value
    
  {
    
    hrr2016$civilres[i] <- test # attach/assign test to hrr2016 #
    
  } #if
  
  if(length(test) > 1)
    
  {
    
    hrr2016$civilres[i] <- max(test)
    
  } #if
  
}

## for loop adding lagged regime type variable to hrr2016 dataset ##

hrr2016$xpolity2lag<-NA

for(i in 1:nrow(hrr2016)) ## searching through nrow of hrr2016 dataset ##
  
{
  
  test<-countrydata$xpolity2lag[which(countrydata$lccode==hrr2016$COW[i]
                                      & countrydata$year==hrr2016$YEAR[i] ) ] # looking through countrydata dataset
  
  if(length(test)==1) # since pre populated with "NA's" we need to only pull data with a value
    
  {
    
    hrr2016$xpolity2lag[i] <- test # attach/assign test to hrr2016 #
    
  } #if
  
  if(length(test) > 1)
    
  {
    
    hrr2016$xpolity2lag[i] <- max(test)
    
  } #if
  
}

## for loop adding average Political Terror Scale score variable to hrr2016 dataset ##

hrr2016$ptsavg<-NA

for(i in 1:nrow(hrr2016)) ## searching through nrow of hrr2016 dataset ##
  
{
  
  test<-pts2015$ptsavg[which(pts2015$COW_Code_N==hrr2016$COW[i]
                             & pts2015$Year==hrr2016$YEAR[i] ) ] # looking through pts2015b dataset
  
  if(length(test)==1) # since pre populated with "NA's" we need to only pull data with a value
    
  {
    
    hrr2016$ptsavg[i] <- test # attach/assign test to hrr2016 #
    
  } #if
  
  if(length(test) > 1)
    
  {
    
    hrr2016$ptsavg[i] <- max(test)
    
  } #if
  
}

## Adding in lagged intrastateconflict variable ##

intrastateconflictlag <- slide(hrr2016, Var = 'intrastateconflict', slideBy = -1)

hrr2016 <- intrastateconflictlag

hrr2016$intrastateconflictlag <- hrr2016[ , 47]

hrr2016[ , 47]<- NULL

## Adding in lagged mid variable ##

midlag <- slide(hrr2016, Var = 'mid', slideBy = -1)

hrr2016 <- midlag

hrr2016$midlag <- hrr2016[ , 48]

hrr2016[ , 48]<- NULL

## Adding in lagged civilres variable ##

civilreslag <- slide(hrr2016, Var = 'civilres', slideBy = -1)

hrr2016 <- civilreslag

hrr2016$civilreslag <- hrr2016[ , 49]

hrr2016[ , 49]<- NULL

## Adding in lagged neighborhood variable for state failure (Iqbal & Starr method) ##

is_nblag <- slide(hrr2016, Var = 'is_nb', slideBy = -1)

hrr2016 <- is_nblag

hrr2016$is_nblag <- hrr2016[ , 50]

hrr2016[ , 50]<- NULL

## Adding in lagged neighborhood variable for state failure (Lambach et al method) ##

ljb_nblag <- slide(hrr2016, Var = 'ljb_nb', slideBy = -1)

hrr2016 <- ljb_nblag

hrr2016$ljb_nblag <- hrr2016[ , 51]

hrr2016[ , 51]<- NULL

## Adding in lagged neighborhood variable for state failure (PITF mag 3 and 4 method) ##

pitf_nblag <- slide(hrr2016, Var = 'pitf_nb', slideBy = -1)

hrr2016 <- pitf_nblag

hrr2016$pitf_nblag <- hrr2016[ , 52]

hrr2016[ , 52]<- NULL

## Adding in lagged neighborhood variable for political instability (POLITY scores -66, -77, and -88) ##

instab_nblag <- slide(hrr2016, Var = 'instab_nb', slideBy = -1)

hrr2016 <- instab_nblag

hrr2016$instab_nblag <- hrr2016[ , 53]

hrr2016[ , 53]<- NULL

## Adding in lagged neighborhood variable for political crises (POLITY score shift of + or - 3) ##

crisis_nblag <- slide(hrr2016, Var = 'crisis_polity_nb', slideBy = -1)

hrr2016 <- crisis_nblag

hrr2016$crisis_nblag <- hrr2016[ , 54]

hrr2016[ , 54]<- NULL

## Adding in lagged neighborhood variable for any fragility event (any failure, instability, or crisis event) ##

fragility_nblag <- slide(hrr2016, Var = 'fragility_any_nb', slideBy = -1)

hrr2016 <- fragility_nblag

hrr2016$fragility_nblag <- hrr2016[ , 55]

hrr2016[ , 55]<- NULL

## for loop adding lagged population size variable to hrr2016 dataset ##

hrr2016$pop<-NA

for(i in 1:nrow(hrr2016)) ## searching through nrow of hrr2016 dataset ##
  
{
  
  test<-countrydata$pop[which(countrydata$lccode==hrr2016$COW[i]
                              & countrydata$year==hrr2016$YEAR[i] ) ] # looking through countrydata dataset
  
  if(length(test)==1) # since pre populated with "NA's" we need to only pull data with a value
    
  {
    
    hrr2016$pop[i] <- test # attach/assign test to hrr2016 #
    
  } #if
  
  if(length(test) > 1)
    
  {
    
    hrr2016$pop[i] <- max(test)
    
  } #if
  
}

## Adding in lagged neighborhood variable for population size ##

poplag <- slide(hrr2016, Var = 'pop', slideBy = -1)

hrr2016 <- poplag

hrr2016$poplag <- hrr2016[ , 57]

hrr2016[ , 57]<- NULL

## Adding in logged varaibles for GDP and population ##

hrr2016$loggdp <- log(hrr2016$rgdppc2005lag)

hrr2016$logpop <- log(hrr2016$poplag)

## Adding in lagged PHYSINT variable ##

PHYSINTlag <- slide(hrr2016, Var = 'PHYSINT', slideBy = -1)

hrr2016 <- PHYSINTlag

hrr2016$PHYSINTlag <- hrr2016[ , 60]

hrr2016[ , 60]<- NULL

## Adding in lagged DISAP variable ##

DISAPlag <- slide(hrr2016, Var = 'DISAP', slideBy = -1)

hrr2016 <- DISAPlag

hrr2016$DISAPlag <- hrr2016[ , 61]

hrr2016[ , 61]<- NULL

## Adding in lagged KILL variable ##

KILLlag <- slide(hrr2016, Var = 'KILL', slideBy = -1)

hrr2016 <- KILLlag

hrr2016$KILLlag <- hrr2016[ , 62]

hrr2016[ , 62]<- NULL

## Adding in lagged POLPRIS variable ##

POLPRISlag <- slide(hrr2016, Var = 'POLPRIS', slideBy = -1)

hrr2016 <- POLPRISlag

hrr2016$POLPRISlag <- hrr2016[ , 63]

hrr2016[ , 63]<- NULL

## Adding in lagged TORT variable ##

TORTlag <- slide(hrr2016, Var = 'TORT', slideBy = -1)

hrr2016 <- TORTlag

hrr2016$TORTlag <- hrr2016[ , 64]

hrr2016[ , 64]<- NULL

## Generating correlation plot for IVs ##

correlations <- cor(hrr2016[,c(29,33,47,48,49,50,52,57,59,60)], use="pairwise", method="spearman")

## Estimating most fully restricted models ##

model0a <- polr(as.factor(PHYSINT) ~ is_nblag, data = hrr2016, Hess = TRUE) 
exp((-coef(model0a)))

model0b <- polr(as.factor(PHYSINT) ~ pitf_nblag, data = hrr2016, Hess = TRUE)

## Results for fully restricted models ##

stargazer(model0a, model0b, type="text", title = "Table 1. Regression Results--Fully Restricted Models", dep.var.labels=c("Human Rights Respect (PHYSINT Scores)"),covariate.labels=c("-77 Polity", "Adv. Reg. Chg."))

stargazer()

################# Predicted Probabilities for Bivariate Models ##################

## Plotting predicted probabilities for model0a ##

x <- seq(0:99)/100
plot(x, plogis(model0a$zeta[1]-model0a$coefficients*x), xlab = "Failed Neighbor (-77 Polity Method)",
      ylab = "Probability of being in Category", ylim=c(0,0.25), type="l", lwd=5, pch=2, col=2, main = "Figure 1: Human Rights & Failed Neighbors")
      points(x, 1-plogis(model0a$zeta[8]-model0a$coefficients*x), type="l", lwd=5, pch=2, col=3)
      legend(x=0.8, y=0.25, cex=0.7, inset=c(-2.0,0), legend=c("8", "0"), pch = c(19), title = "PHYSINT Score", col=c("limegreen", "red"))

################ Plotting predicted probabilities for model0b ################
      
x <- seq(0:99)/100
plot(x, plogis(model0b$zeta[1]-model0b$coefficients*x), xlab = "Failed Neighbor (Adverse Regime Change Method)",
      ylab = "Probability of being in Category", ylim=c(0,0.25), type="l", lwd=5, pch=2, col=2, main = "Figure 2: Human Rights & Failed Neighbors")
      points(x, 1-plogis(model0b$zeta[8]-model0b$coefficients*x), type="l", lwd=5, pch=2, col=3)
      legend(x=0.8, y=0.25, cex=0.7, inset=c(-2.0,0), legend=c("8", "0"), pch = c(19), title = "PHYSINT Score", col=c("limegreen", "red"))

## Estimating "General Models" ordered logit models for dependent variable PHYSINT ##

model1 <- polr(as.factor(PHYSINT) ~ is_nblag + is_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + PHYSINTlag, data = hrr2016, Hess = TRUE)

model2 <- polr(as.factor(PHYSINT) ~ pitf_nblag + pitf_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + PHYSINTlag, data = hrr2016, Hess = TRUE)

model3 <- polr(as.factor(PHYSINT) ~ crisis_nblag + crisis_polity + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + PHYSINTlag, data = hrr2016, Hess = TRUE)

model4 <- polr(as.factor(PHYSINT) ~ instab_nblag + instab_polity + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + PHYSINTlag, data = hrr2016, Hess = TRUE)

model5 <- polr(as.factor(PHYSINT) ~ ljb_nblag + ljb_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + PHYSINTlag, data = hrr2016, Hess = TRUE)

model6 <- polr(as.factor(PHYSINT) ~ fragility_nblag + fragility_any + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + PHYSINTlag, data = hrr2016, Hess = TRUE)

################Plotting predicted Probabilities for Failed States, Model 2####################

x <- seq(0:99)/100
plot(x, 1-plogis(model2$zeta[1]-model2$coef[1]*0+model2$coef[2]*x+model2$coef[3]*0+model2$coef[4]*0+model2$coef[5]*0 +
    model2$coef[6]*3+model2$coef[7]*8.526067+model2$coef[8]*15.75209+model2$coef[9]*5), 
    xlab = "Failed State (Adverse Regime Change Method)",
    ylab = "Probability of being in Category", ylim=c(0,0.3), type="l", lwd=5, pch=2, col=2, main = "Figure 3: Human Rights in Failed States")
    points(x, 1-plogis(model2$zeta[8]-model2$coef[1]*0+model2$coef[2]*x+model2$coef[3]*0+model2$coef[4]*0+model2$coef[5]*0 +
    model2$coef[6]*3+model2$coef[7]*8.526067+model2$coef[8]*15.75209+model2$coef[9]*5), 
    type="l", lwd=5, pch=2, col=3)
    legend(x=0.0, y=0.3, cex=0.7, inset=c(-2.0,0), legend=c("8", "0"), pch = c(19), title = "PHYSINT Score", col=c("limegreen", "red"))
      
## Generating Results for model1 variants with disaggregated physical integrity rights violation scores ##

model1a <- polr(as.factor(DISAP) ~ is_nblag + is_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + DISAPlag, data = hrr2016, Hess = TRUE)

model1b <- polr(as.factor(KILL) ~ is_nblag + is_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + KILLlag, data = hrr2016, Hess = TRUE)

model1c <- polr(as.factor(POLPRIS) ~ is_nblag + is_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + POLPRISlag, data = hrr2016, Hess = TRUE)

model1d <- polr(as.factor(TORT) ~ is_nblag + is_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + TORTlag, data = hrr2016, Hess = TRUE)

################Plotting predicted Probabilities for Torture, Model 1D####################

x <- seq(0:99)/100
plot(x, plogis(model1d$zeta[1]-model1d$coef[1]*x+model1d$coef[2]*0+model1d$coef[3]*0+model1d$coef[4]*0+model1d$coef[5]*0 +
    model1d$coef[6]*3+model1d$coef[7]*8.526067+model1d$coef[8]*15.75209+model1d$coef[9]*1), 
    xlab = "Failed Neighbor (-77 Polity)",
    ylab = "Probability of being in Category", ylim=c(0,0.99), type="l", lwd=5, pch=2, col=2, main = "Figure 4: Torture & Failed Neighbors")
    points(x, 1-plogis(model1d$zeta[2]-model1d$coef[1]*x+model1d$coef[2]*0+model1d$coef[3]*0+model1d$coef[4]*0+model1d$coef[5]*0 +
    model1d$coef[6]*3+model1d$coef[7]*8.526067+model1d$coef[8]*15.75209+model1d$coef[9]*1),  
    type="l", lwd=5, pch=2, col=3)
    legend(x=0.8, y=1.0, cex=0.7, inset=c(-2.0,0), legend=c("2", "0"), pch = c(19), title = "TORT Score", col=c("limegreen", "red"))
     
## Generating Results for model2 variants with disaggregated physical integrity rights violation scores ##

model2a <- polr(as.factor(DISAP) ~ pitf_nblag + pitf_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + DISAPlag, data = hrr2016, Hess = TRUE)

model2b <- polr(as.factor(KILL) ~ pitf_nblag + pitf_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + KILLlag, data = hrr2016, Hess = TRUE)

model2c <- polr(as.factor(POLPRIS) ~ pitf_nblag + pitf_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + POLPRISlag, data = hrr2016, Hess = TRUE)

model2d <- polr(as.factor(TORT) ~ pitf_nblag + pitf_fail + midlag + intrastateconflictlag + civilreslag + xpolity2lag + loggdp + logpop + TORTlag, data = hrr2016, Hess = TRUE)

################Plotting predicted Probabilities for Torture, Model 2D####################

x <- seq(0:99)/100
plot(x, plogis(model2d$zeta[1]-model2d$coef[1]*x+model2d$coef[2]*0+model2d$coef[3]*0+model2d$coef[4]*0+model2d$coef[5]*0 +
      model2d$coef[6]*3+model2d$coef[7]*8.526067+model2d$coef[8]*15.75209+model2d$coef[9]*1), 
      xlab = "Failed Neighbor (Adverse Regime Change Method)",
      ylab = "Probability of being in Category", ylim=c(0,0.99), type="l", lwd=5, pch=2, col=2, main = "Figure 5: Torture & Failed Neighbors")
      points(x, 1-plogis(model2d$zeta[2]-model2d$coef[1]*x+model2d$coef[2]*0+model2d$coef[3]*0+model2d$coef[4]*0+model2d$coef[5]*0 +
      model2d$coef[6]*3+model2d$coef[7]*8.526067+model2d$coef[8]*15.75209+model2d$coef[9]*1),  
      type="l", lwd=5, pch=2, col=3)
      legend(x=0.8, y=1.0, cex=0.7, inset=c(-2.0,0), legend=c("2", "0"), pch = c(19), title = "TORT Score", col=c("limegreen", "red"))

## Getting predicted probabilities charts for model1d results ##
      
(plogis(model1d$zeta[1] - model1d$coefficients[1]*0 - model1d$coefficients[2]*0 - model1d$coefficients[3]*median(hrr2016$midlag, na.rm = TRUE) - model1d$coefficients[4]*0 - model1d$coefficients[5]*0 - model1d$coefficients[6]*median(hrr2016$xpolity2lag, na.rm = TRUE) - model1d$coefficients[7]*median(hrr2016$rgdppc2005lag, na.rm = TRUE)- model1d$coefficients[8]* median(hrr2016$poplag - model1d$coefficients[9]*median(hrr2016$PHYSINTlag, na.rm = TRUE), na.rm = TRUE)))

phat_model1d <- predict(model1d, type="probs")
      
j1d <- c(0, 1)
predprobmodel1d <- predict(model1d, list(is_nblag = j1d, is_fail=c(0, 0), midlag=rep(median(hrr2016$midlag, na.rm=T), 2), intrastateconflictlag=c(0,0), civilreslag=c(0,0), xpolity2lag=rep(median(hrr2016$xpolity2lag, na.rm=T), 2), loggdp=rep(median(hrr2016$loggdp, na.rm=T), 2), logpop=rep(median(hrr2016$logpop, na.rm=T), 2), TORTlag=rep(median(hrr2016$TORTlag, na.rm=T), 2)), type="probs")
predprobmodel1d
      
t_predprobmodel1d <- t(predprobmodel1d)

stargazer(predprobmodel1d, type="text", title = "Model 1D: Predicted Probability of Torture", dep.var.labels=c("Failed Neighbor -77 Polity Method"),covariate.labels=c("Failed Neighbor","0","1","2","3","4", "5", "6","7","8"))

stargazer()

## Getting predicted probabilities charts for model2d results ##

(plogis(model2d$zeta[1] - model2d$coefficients[1]*0 - model2d$coefficients[2]*0 - model2d$coefficients[3]*median(hrr2016$midlag, na.rm = TRUE) - model2d$coefficients[4]*0 - model2d$coefficients[5]*0 - model2d$coefficients[6]*median(hrr2016$xpolity2lag, na.rm = TRUE) - model1d$coefficients[7]*median(hrr2016$rgdppc2005lag, na.rm = TRUE)- model1d$coefficients[8]* median(hrr2016$poplag - model1d$coefficients[9]*median(hrr2016$PHYSINTlag, na.rm = TRUE), na.rm = TRUE)))

phat_model2d <- predict(model2d, type="probs")

j2d <- c(0, 1)
predprobmodel2d <- predict(model2d, list(pitf_nblag = j2d, pitf_fail=c(0, 0), midlag=rep(median(hrr2016$midlag, na.rm=T), 2), intrastateconflictlag=c(0,0), civilreslag=c(0,0), xpolity2lag=rep(median(hrr2016$xpolity2lag, na.rm=T), 2), loggdp=rep(median(hrr2016$loggdp, na.rm=T), 2), logpop=rep(median(hrr2016$logpop, na.rm=T), 2), TORTlag=rep(median(hrr2016$TORTlag, na.rm=T), 2)), type="probs")
predprobmodel2d

t_predprobmodel2d <- t(predprobmodel2d)

stargazer(predprobmodel2d, type="text", title = "Model 2D: Predicted Probability of Torture", dep.var.labels=c("Failed Neighbor -77 Polity Method"),covariate.labels=c("Failed Neighbor","0","1","2","3","4", "5", "6","7","8"))

stargazer()

##Results table for predprobmodel2 ##

stargazer(predprobmodel2, type="text", title = "Model 2: Predicted Probability of Human Rights Respect", dep.var.labels=c("Failed Neighbor"),covariate.labels=c("Failed Neighbor","0","1","2","3","4", "5", "6","7","8"))

stargazer()

## Generating results tables for disaggregated versions of model1 ##

stargazer(model1a, type="text", title = "Model 1A Regression Results", dep.var.labels=c("Disappearance"),covariate.labels=c("Constant","Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

stargazer(model1b, type="text", title = "Model 1B Regression Results", dep.var.labels=c("Killing"),covariate.labels=c("Constant","Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

stargazer(model1c, type="text", title = "Model 1C Regression Results", dep.var.labels=c("Political Imprisonment"),covariate.labels=c("Constant","Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

stargazer(model1d, type="text", title = "Model 1D Regression Results", dep.var.labels=c("Torture"),covariate.labels=c("Constant","Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

## Generating consolidated results table for models 1a-1b##

stargazer(model1a, model1b, model1c, model1d, type="text", ord.intercepts = TRUE, title = "Table 2. Regression Results--Disaggregated DVs, Model 1 Variants", dep.var.labels=c(),covariate.labels=c("Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

## Generating results tables for disaggregated versions of model2 ##

stargazer(model2a, type="text", ord.intercepts = TRUE, title = "Model 2A Regression Results", dep.var.labels=c("Disappearance"),covariate.labels=c("Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

stargazer(model2b, type="text", ord.intercepts = TRUE, title = "Model 2B Regression Results", dep.var.labels=c("Killing"),covariate.labels=c("Constant","Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

stargazer(model2c, type="text", ord.intercepts = TRUE, title = "Model 2C Regression Results", dep.var.labels=c("Political Imprisonment"),covariate.labels=c("Constant","Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

stargazer(model2d, type="text", ord.intercepts = TRUE, title = "Model 2D Regression Results", dep.var.labels=c("Torture"),covariate.labels=c("Constant","Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","Lagged DV"))

stargazer()

## Generating consolidated results table for models 2a-2b##

stargazer(model2a, model2b, model2c, model2d, type="text", ord.intercepts = TRUE, title = "Table 3. Regression Results--Disaggregated DVs, Model 2 Variants", dep.var.labels=c("Disappearance", "Killing", "Pol. Imprison.", "Torture"),covariate.labels=c("Failed Neighbor","State Failure","MIDs","Civil War","Civil Resistance", "Regime Type", "GDP","Population Size","DISAP Lag", "KILL Lag", "POLPRIS Lag", "TORT Lag"))

stargazer()

