##################################################################
## Kelly Gordell - POL683 - Midterm Election Prediction - R Code - 

## CLEAR & LOAD PACKAGES - 

rm(list=ls())
setwd("C:/Users/Kelly/Desktop/683 Midterm Files")

library(foreign)
library(plyr)
library(ggplot2)
library(plotly)


## Load data - 

POL <- read.dta("votes12-2.dta")


##################################################################
## Shares out of dem & rep parties, does not include percent that
## went to third parties/other - Example: 2012 - Obama had 51.06%

## of the vote according to FEC (rounded is 51.1%), however that
## is calculated based on total votes of Reps and the 1.73% that
## went to third parties - these calculations only focus on dem/
## rep share so they are the following for checks below:

## 2012 - Dem share: 51.96% Rep: 48.03% 
## 2008 - Dem share: 53.68% Rep: 46.31%
## 2004 - Dem share: 48.75% Rep: 51.24%

#Notes 
## 2012 - Others: 1.73%   ## 2012 - w/others factored: Dem: 51.06% Rep: 47.20%
## 2008 - Others: 1.42%   ## 2008 - w/others factored: Dem: 52.93% Rep: 45.65%
## 2004 - Others: 1.00%   ## 2004 - w/others factored: Dem: 48.27% Rep: 50.73%



##################################################################
## Calculating ACTUAL historical democratic share of 2-party vote
## in each election - years 1964-2012 - *******STATE-LEVEL SHARES


POL$ave2012 <- POL$d2012/(POL$d2012+POL$r2012)
POL$ave2008 <- POL$d2008/(POL$d2008+POL$r2008)
POL$ave2004 <- POL$d2004/(POL$d2004+POL$r2004) 
POL$ave2000 <- POL$d2000/(POL$d2000+POL$r2000) 
POL$ave1996 <- POL$d1996/(POL$d1996+POL$r1996)
POL$ave1992 <- POL$d1992/(POL$d1992+POL$r1992)
POL$ave1988 <- POL$d1988/(POL$d1988+POL$r1988)
POL$ave1984 <- POL$d1984/(POL$d1984+POL$r1984)
POL$ave1980 <- POL$d1980/(POL$d1980+POL$r1980)
POL$ave1976 <- POL$d1976/(POL$d1976+POL$r1976)
POL$ave1972 <- POL$d1972/(POL$d1972+POL$r1972)


##################################################################
## Calculating normal vote predictions for elections - Rolling 
## check for election years that have passed as a way to determine
## confidence in model - /12 to reflect all years inbetween
## election years up to the prediction year
## Calculating normal vote predictions using weighted normal vote


## CHECK 01 - If I wanted to predict the 2004 election: 

c1.2000 <- (POL$ave1996*5 + POL$ave1992*4 + POL$ave1988*2 + POL$ave1984*1)/12 ## equivalent to 2004
c1.1996 <- (POL$ave1992*5 + POL$ave1988*4 + POL$ave1984*2 + POL$ave1980*1)/12
c1.1992 <- (POL$ave1988*5 + POL$ave1984*4 + POL$ave1980*2 + POL$ave1976*1)/12
c1.1988 <- (POL$ave1984*5 + POL$ave1980*4 + POL$ave1976*2 + POL$ave1972*1)/12


## CHECK 02 - If I wanted to predict the 2008 election:

c2.2004 <- (POL$ave2000*5 + POL$ave1996*4 + POL$ave1992*2 + POL$ave1988*1)/12 ## equivalent to 2008
c2.2000 <- (POL$ave1996*5 + POL$ave1992*4 + POL$ave1988*2 + POL$ave1984*1)/12 
c2.1996 <- (POL$ave1992*5 + POL$ave1988*4 + POL$ave1984*2 + POL$ave1980*1)/12
c2.1992 <- (POL$ave1988*5 + POL$ave1984*4 + POL$ave1980*2 + POL$ave1976*1)/12


## CHECK 03 - If I wanted to predict the 2012 election:

c3.2008 <- (POL$ave2004*5 + POL$ave2000*4 + POL$ave1996*2 + POL$ave1992*1)/12  ## equivalent to 2012
c3.2004 <- (POL$ave2000*5 + POL$ave1996*4 + POL$ave1992*2 + POL$ave1988*1)/12  
c3.2000 <- (POL$ave1996*5 + POL$ave1992*4 + POL$ave1988*2 + POL$ave1984*1)/12 
c3.1996 <- (POL$ave1992*5 + POL$ave1988*4 + POL$ave1984*2 + POL$ave1980*1)/12


## AIM PRED - If I wanted to predict the 2016 election:

p.2012 <- (POL$ave2008*5 + POL$ave2004*4 + POL$ave2000*2 + POL$ave1996*1)/12 ## equivalent to 2016
p.2008 <- (POL$ave2004*5 + POL$ave2000*4 + POL$ave1996*2 + POL$ave1992*1)/12 
p.2004 <- (POL$ave2000*5 + POL$ave1996*4 + POL$ave1992*2 + POL$ave1988*1)/12  
p.2000 <- (POL$ave1996*5 + POL$ave1992*4 + POL$ave1988*2 + POL$ave1984*1)/12 




##################################################################
## Check model against last elections - matrices of actual and 
## predicted election results for 2004/2008/2012/*2016


## Actual election results

actuals.1 <- cbind(POL$ave2000, POL$ave1996, POL$ave1992, POL$ave1988)
actuals.2 <- cbind(POL$ave2004, POL$ave2000, POL$ave1996, POL$ave1992)
actuals.3 <- cbind(POL$ave2008, POL$ave2004, POL$ave2000, POL$ave1996)
actuals.p <- cbind(POL$ave2012, POL$ave2008, POL$ave2004, POL$ave2000)


## Predicted election results for 2004 Election

predicts.c1.1 <- cbind(c1.2000, c1.1996, c1.1992, c1.1988)


## Predicted election results for 2008 Election

predicts.c2.1 <- cbind(c2.2004, c2.2000, c2.1996, c2.1992)


## Predicted election results for 2012 Election

predicts.c3.1 <- cbind(c3.2008, c3.2004, c3.2000, c3.1996)


## Predicted election results for 2016 Election

predicts.p.1 <- cbind(p.2012, p.2008, p.2004, p.2000)



##################################################################
## Check model against last elections - standard deviations
## To establish an estimate of error among predictions


## Put simply, the standard error of the sample mean is an estimate of how far the sample 
## mean is likely to be from the population mean, whereas the standard deviation of the sample 
## is the degree to which individuals within the sample differ from the sample mean.



test.wrong <- matrix(NA, nrow=nrow(actuals.1), ncol=4)

for(i in 1:nrow(test.wrong)){
  test.wrong[i,1] <- sqrt(mean((actuals.1[i,] - predicts.c1.1[i,])^2, na.rm=T))
  test.wrong[i,2] <- sqrt(mean((actuals.2[i,] - predicts.c2.1[i,])^2, na.rm=T))
  test.wrong[i,3] <- sqrt(mean((actuals.3[i,] - predicts.c3.1[i,])^2, na.rm=T))
  test.wrong[i,4] <- sqrt(mean((actuals.p[i,] - predicts.p.1[i,])^2, na.rm=T))
  
}


mean.diffs.weighted <- c(mean(test.wrong[,1]), mean(test.wrong[,2]), mean(test.wrong[,3]), mean(test.wrong[,4]))

mean.diffs.weighted


#           [,1]

#[1,] 0.06688524 #[2,] 0.06500830 #[3,] 0.05352314 #[4,] 0.04088441* *expect this much average error for 2016 prediction model



##################################################################
## GRAPH OF ACTUALS VS PREDICTED FOR 2012 ELECTION AS EXAMPLE:
## GRAPH OF PREDICTED VALUES FOR 2016 ELECTION
## Nicer versions made directly with plotly - but the rcode still 
## gives non-function error when transferred from there - these are simplified versions


predicted2012 <- (c3.2008+c3.2004+c3.2000+c3.1996)/4
predicted2012


actual2012 <- (POL$ave2012+POL$ave2008+POL$ave2004+POL$ave2000)/4
actual2012


predicted2016 <- (p.2012+p.2008+p.2004+p.2000)/4
predicted2016


data <- data.frame(POL$state, predicted2012, predicted2016, actual2012)

plot2012 <- plot_ly(x=POL$state, y=predicted2012, name='Predicted Dem Vote', type = 'scatter', mode='markers')%>%
  add_trace(x=POL$state, y=actual2012, name='Actual Dem Vote', mode='markers')

plot2012


plot2016 <- plot_ly(data=data, x=POL$state, y=predicted2016, name='Predicted Dem Vote', type = 'scatter', mode='markers')
  
plot2016





##################################################################
## Graphing Weighted Normal Vote with 2012 CI's
## In reference to 2016 election prediction - predicts.p.1

statediff.stor <- c()
sds <- seq(1:51)

for(j in 1:nrow(actuals.1)){	
  acts <- actuals.p[j,]		
  preds <- predicts.p.1[j,]	
  
  for(i in 1:1000){
    draw <- sample(1:11, 11, replace=T)
    subseta <- acts[draw]
    subsetb <- preds[draw]
    test <- sqrt(mean((subseta - subsetb)^2, na.rm=T))
    statediff.stor[i] <- test
  }
  sds[j] <- mean(statediff.stor, na.rm=T)
}

sds


NV.p <- (POL$ave2012*5 + POL$ave2008*4 + POL$ave2004*2 + POL$ave2000*1)/12


NORMgraph <- function(sds, NV, mid, name=POL$abb){
  dems <- which(NV>=mid)
  reps <- which(NV<mid)
  NV.dems <- NV[dems]
  NV.reps <- NV[reps]
  ub.dem <- NV.dems + (2.36*sds[dems])
  lb.dem <- NV.dems - (2.36*sds[dems])
  ub.rep <- NV.reps + (2.36*sds[reps])
  lb.rep <- NV.reps - (2.36*sds[reps])
  colors.dem <- c()
  colors.rep <- c()
  for(i in 1:length(lb.dem)){
    if(lb.dem[i]<=mid){colors.dem[i]="black"}
    else{colors.dem[i]="blue"}
  }
  for(i in 1:length(lb.rep)){
    if(ub.rep[i]>mid){colors.rep[i]="black"}
    else{colors.rep[i]="red"}
  }
  plot(x=NV.dems, y=dems, pch=20, col="blue", ylab="States", xlim=c(mid-.5,mid+.5), ylim=c(0,52), xlab="Democratic Vote Share", yaxt="n")
  text(lb.dem-.02, dems, labels=name[dems], cex=.7, col=colors.dem)
  points(x=NV.reps, y=reps, pch=20, col="red")
  text(lb.rep-.02, reps, labels=name[reps], cex=.7, col=colors.rep)
  segments(x0=lb.dem, x1=ub.dem, y0=dems, y1=dems, col="blue")
  segments(x0=lb.rep, x1=ub.rep, y0=reps, y1=reps, col="red")
  abline(v=mid, lty="dotted")
}


NORMgraph(sds, NV.p, .5)


##################################################################
## Calculating ACTUAL historical democratic share of 2-party vote
## in each election - years 1964-2012 - *****NATIONAL-LEVEL SHARES


us2012 <- sum(POL$d2012)/(sum(POL$d2012+POL$r2012))
us2008 <- sum(POL$d2008)/(sum(POL$d2008+POL$r2008))
us2004 <- sum(POL$d2004)/(sum(POL$d2004+POL$r2004))
us2000 <- sum(POL$d2000)/(sum(POL$d2000+POL$r2000))
us1996 <- sum(POL$d1996)/(sum(POL$d1996+POL$r1996))
us1992 <- sum(POL$d1992)/(sum(POL$d1992+POL$r1992))
us1988 <- sum(POL$d1988)/(sum(POL$d1988+POL$r1988))
us1984 <- sum(POL$d1984)/(sum(POL$d1984+POL$r1984))
us1980 <- sum(POL$d1980)/(sum(POL$d1980+POL$r1980))
us1976 <- sum(POL$d1976)/(sum(POL$d1976+POL$r1976))
us1972 <- sum(POL$d1972)/(sum(POL$d1972+POL$r1972))


##################################################################


## CHECK 01 - 2004 Election - 

nc1.2000 <- (us1996*5 + us1992*4 + us1988*2 + us1984*1)/12
nc1.1996 <- (us1992*5 + us1988*4 + us1984*2 + us1980*1)/12
nc1.1992 <- (us1988*5 + us1984*4 + us1980*2 + us1976*1)/12
nc1.1988 <- (us1984*5 + us1980*4 + us1976*2 + us1972*1)/12


## CHECK 02 - 2008 Election - 

nc2.2004 <- (us2000*5 + us1996*4 + us1992*2 + us1988*1)/12
nc2.2000 <- (us1996*5 + us1992*4 + us1988*2 + us1984*1)/12
nc2.1996 <- (us1992*5 + us1988*4 + us1984*2 + us1980*1)/12
nc2.1992 <- (us1988*5 + us1984*4 + us1980*2 + us1976*1)/12


## CHECK 03 - 2012 Election - 

nc3.2008 <- (us2004*5 + us2000*4 + us1996*2 + us1992*1)/12
nc3.2004 <- (us2000*5 + us1996*4 + us1992*2 + us1988*1)/12
nc3.2000 <- (us1996*5 + us1992*4 + us1988*2 + us1984*1)/12
nc3.1996 <- (us1992*5 + us1988*4 + us1984*2 + us1980*1)/12


## AIM OF PREDICTION - 2016 Election - 

aim1.2012 <- (us2008*5 + us2004*4 + us2000*2 + us1996*1)/12
aim1.2008 <- (us2004*5 + us2000*4 + us1996*2 + us1992*1)/12
aim1.2004 <- (us2000*5 + us1996*4 + us1992*2 + us1988*1)/12
aim1.2000 <- (us1996*5 + us1992*4 + us1988*2 + us1984*1)/12


##################################################################



actuals.n1 <- cbind(us2000, us1996, us1992, us1988)
actuals.n2 <- cbind(us2004, us2000, us1996, us1992)
actuals.n3 <- cbind(us2008, us2004, us2000, us1996)
actuals.np <- cbind(us2012, us2008, us2004, us2000)

predicts.nc1 <- cbind((nc1.2000+0), (nc1.1996+.02), (nc1.1992+.02), (nc1.1988+0))
predicts.nc2 <- cbind((nc2.2004+.02) ,(nc2.2000+0), (nc2.1996+.02), (nc2.1992+.02))
predicts.nc3 <- cbind((nc3.2008+0), (nc3.2004+.02), (nc3.2000+0), (nc3.1996+.02))
predicts.aim1 <- cbind((aim1.2012+.02), (aim1.2008+.0), (aim1.2004+.02), (aim1.2000+0))



test.wrongN <- matrix(NA, nrow=nrow(actuals.n1), ncol=4)

for(i in 1: nrow(test.wrongN)){
  test.wrongN[i,1] <- sqrt(mean((actuals.n1[i,] - predicts.nc1[i,])^2, na.rm=T))
  test.wrongN[i,2] <- sqrt(mean((actuals.n2[i,] - predicts.nc2[i,])^2, na.rm=T))
  test.wrongN[i,3] <- sqrt(mean((actuals.n3[i,] - predicts.nc3[i,])^2, na.rm=T))
  test.wrongN[i,4] <- sqrt(mean((actuals.np[i,] - predicts.aim1[i,])^2, na.rm=T))
}

mean.diffs.weightedN <- c(mean(test.wrongN[,1]), mean(test.wrongN[,2]), mean(test.wrongN[,3]), mean(test.wrongN[,4]))

mean.diffs.weightedN



##################################################################

#Previous National Prediction checks - 


check2004p <- mean(predicts.nc1)

us2004
check2004p


check2008p <- mean(predicts.nc2)

us2008
check2008p

check2012p <- mean(predicts.nc3)

us2012
check2012p



##National Prediction

predicts.aim1

DemVoteShare <- mean(predicts.aim1)

DemVoteShare

# 0.524655 + error 


