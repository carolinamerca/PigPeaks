#SERVICES

library(ISOweek)
library(lubridate)
library(abind)
library(stringr)
library(plotly)


source("Definitions.r")
source("Functions.r")
source("Functions-plotting.r")
source("Dashboard.R")
source("TS_VSS_Carolina.R")
load("all_indicators.RData")

library(abind)
library(qcc)



# Reservices per week ----
## VS

#take off the first 200 weeks because there were almost only 0's (for every weekly indicator)
reservices.week.total.ind <- reservices.week.total[200:471]

#create a table
baseline.reservices.per.week <- c(rep(NA, length(reservices.week.total.ind)))
UCL.reservices.per.week <- c(rep(NA, length(reservices.week.total.ind)))
LCL.reservices.per.week <- c(rep(NA, length(reservices.week.total.ind)))
alarm.reservices.per.week <- c(rep(NA,length(reservices.week.total.ind)))

reservices.per.week <- data.frame(index.dates.week$start[200:471], reservices.week.total.ind, 
                                  baseline.reservices.per.week, UCL.reservices.per.week,
                                  LCL.reservices.per.week, alarm.reservices.per.week)

  colnames(reservices.per.week) <- c("date", "observed", "baseline", "UCL", "LCL", "alarms")

  
## EWMA Code
          
ewma_indicator <-   function(
                    indicator=indicator.chosen,
                    evaluate.window=72,
                    baseline.window=104,
                    lambda=0.2,
                    limit.sd=c(2.5,3,3.5),
                    guard.band=2,
                    correct.baseline=FALSE,
                    #alarm.dim=2,
                    UCL=1,
                    LCL=1,
                    #pre.process=FALSE,
                    #family="poisson",
                    #formula=NULL,
                    frequency=52
          )
          {
            
            if(guard.band<1)(guard.band<-1)
            
            #require(abind)
            
            #number of time points to iterate           
            range <- (dim(indicator)[1]-evaluate.window+1):dim(indicator)[1]  

                                                        #indicator=reservices.per.week
            
              #if baseline for the syndrome in question is not available
              #(filled with NA just to reach dimensions necessary), then
              #for the syndrome in use replace with data from observed
              if (sum(indicator[,"baseline"], na.rm=TRUE)==0){
                indicator[,"baseline"] <- indicator[,"observed"]
              }
              
              
              for (tpoint in range){
                                            #tpoint=615
                

                    start = tpoint-baseline.window-guard.band
                    end   = tpoint-1
                    
                    to.cc <- c(indicator[start:end,"baseline"],indicator[tpoint,"observed"])
                    correct <- 0
                
                
                for (l in 1:length(limit.sd)){
                                                    #l=1
                  #require(qcc)
                  ewma1 = ewma(to.cc,
                               center=mean(to.cc[1:(length(to.cc)-guard.band)],na.rm=TRUE),
                               std.dev=sd(to.cc[1:(length(to.cc)-guard.band)],na.rm=TRUE),
                               lambda=lambda,nsigmas=limit.sd[l],plot=FALSE)
                  
                  
                  last <- length(to.cc) 
                  upr.alarm.detected <- 0
                  lwr.alarm.detected <- 0
                  
                  if(length(ewma1$violations)>0){
                    if(ewma1$violations[length(ewma1$violations)]==last&
                       ewma1$y[last]>ewma1$limits[last,2]){
                      upr.alarm.detected <- 1
                    }}
                  
                  if(length(ewma1$violations)>0){
                    if(ewma1$violations[length(ewma1$violations)]==last&
                       ewma1$y[last]<ewma1$limits[last,1]){
                      lwr.alarm.detected <- 1
                    }}
                  
                  UCL.value= ceiling(correct  +  ewma1$limits[[length(ewma1$limits[,2]),2]])
                  LCL.value= floor(correct    +  ewma1$limits[[length(ewma1$limits[,1]),1]])
                  #before deciding if an alarm exists, a zero is automatically added to the
                  #time point if this is the first loop for two reasons:
                  #1-because if the data were never analysed, the slot had a NA before,
                  #and adding 0 will signal that it has now been processed
                  #2-because if the data HAS been analyzed before, we want the results of these
                  #analyses to OVERRIDE, not to SUM to the previous analyses.
                  if(l==1){
                    indicator[tpoint,"alarms"]<-0
                  }
                  
                  if (l==UCL){
                    indicator[tpoint,"UCL"]<-UCL.value
                  }
                  
                  if (l==LCL){
                    indicator[tpoint,"LCL"]<-LCL.value
                  }
                  
                  #ADD a one if the result of this loop was a detection
                  if (upr.alarm.detected){
                    indicator[tpoint,"alarms"]<-indicator[tpoint,"alarms"]+1
                  }
                  
                  if (lwr.alarm.detected){
                    indicator[tpoint,"alarms"]<-indicator[tpoint,"alarms"]-1
                  }
                  
                  
                  #Correct baseline IF the user indicated so
                  if (correct.baseline==l){
                    indicator[tpoint,"baseline"] <- indicator[tpoint,"observed"]
                    if (indicator[tpoint,"observed"] > max(0,UCL.value)){
                      indicator[tpoint,"baseline"] <- max(0,round(UCL.value))
                    }
                    if (indicator[tpoint,"observed"] < max(0,LCL.value)){
                      indicator[tpoint,"baseline"] <- max(0,round(LCL.value))
                    }
                  }
                  
                }
              }
            return(indicator)
            }


## Applying EWMA

ewma_indicator(indicator=reservices.per.week)


## Results

reservices.per.week <- ewma_indicator(indicator=reservices.per.week)

View(reservices.per.week)


## Dashboard

reservices.barplot <- TS.barplot (series = reservices.per.week$observed,
                                  indicator.label="Reservices per week",
                                  show.window = weeks.to.show,
                                  index.dates = index.dates.week[200:471,],
                                  ylabel = 'Number of sows',
                                  xlabel = 'Week',
                                  target.vector.UCL = reservices.per.week$UCL,
                                  target.vector.LCL = NULL,
                                  shading.matrix = NULL,
                                  limits = NULL)

ggplotly(reservices.barplot)


#save(reservices.barplot,
#file="Carolina.dashboard.plots.RData")



# Time to reservice, last 100 ----
## VS

#create a table
baseline.time.to.reservice <- c(rep(NA, dim(time.to.reservice)[1]))
UCL.time.to.reservice <- c(rep(NA, dim(time.to.reservice)[1]))
LCL.time.to.reservice <- c(rep(NA, dim(time.to.reservice)[1]))
alarm.time.to.reservice <- c(rep(0, dim(time.to.reservice)[1])) #pus 0's

#time.to.reservice[640,"indicator"] <-  90
#time.to.reservice[630,"indicator"] <-  2

time.between.service.reservice <- data.frame(time.to.reservice[,"date"],time.to.reservice[,"sowINDEX"],
                                             time.to.reservice[,"indicator"], baseline.time.to.reservice,
                                             UCL.time.to.reservice, LCL.time.to.reservice,
                                             alarm.time.to.reservice)

  colnames(time.between.service.reservice) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")
  
  
## Applying EWMA
  
ewma_indicator(indicator=time.between.service.reservice)
                                    
                                    
## Results
                                    
time.between.service.reservice <- ewma_indicator(indicator=time.between.service.reservice)
                                    
View(time.between.service.reservice)                                  
                                    

## Dashboards
                                    
time.between.service.reservice.barplot <- nonTS.barplot.timeless(series = time.between.service.reservice,
                                                                 indicator.label="Time to reservice",
                                                                 show.window = nonTS.to.show,
                                                                 vertical.line = NULL,
                                                                 vertical.line.label=NULL,
                                                                 index.dates = index.dates.days,
                                                                 ylabel = 'Time to reservice (in days)',
                                                                 xlabel = 'Last 100 events',
                                                                 target.UCL = tail(time.between.service.reservice$UCL,100), #last 100
                                                                 target.LCL = tail(time.between.service.reservice$LCL,100), #last 100
                                                                 alarms = tail(time.between.service.reservice$alarms,100),
                                                                 target.unit="vector", #c("value","vector")
                                                                 target.label="target",
                                                                 series.label="sows")



ggplotly(time.between.service.reservice.barplot)



# % Reservices after 4 weeks ----
## VS

#sum the lines because we want no parity
numerator.perc.reservice.week <- rowSums(perc.reservice[,,1])
denominator.perc.reservice.week <- rowSums(perc.reservice[,,2])
observed.perc.reservice.week <- round(((rowSums(perc.reservice[,,1])/rowSums(perc.reservice[,,2]))*100),2)

#take off the first 200 weeks because there were almost only 0's (for every weekly indicator)
indicator.perc.reservice.week <- data.frame(numerator.perc.reservice.week[200:471], 
                                            denominator.perc.reservice.week[200:471],
                                            observed.perc.reservice.week[200:471])

#create a table  
baseline.perc.reservice.week <- c(rep(NA, dim(indicator.perc.reservice.week)[1]))
UCL.perc.reservice.week <- c(rep(NA, dim(indicator.perc.reservice.week)[1]))
LCL.perc.reservice.week <- c(rep(NA, dim(indicator.perc.reservice.week)[1]))
alarm.perc.reservice.week <- c(rep(NA, dim(indicator.perc.reservice.week)[1]))

perc.reservice.week <- data.frame(index.dates.week$start[200:471],indicator.perc.reservice.week,
                                  baseline.perc.reservice.week, UCL.perc.reservice.week,
                                  LCL.perc.reservice.week, alarm.perc.reservice.week)

  colnames(perc.reservice.week) <- c("date", "numerator", "denominator", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=perc.reservice.week)


## Results

perc.reservice.week <- ewma_indicator(indicator=perc.reservice.week)

View(perc.reservice.week) 


## Dashboard

perc.reservice.week.barplot <- TS.barplot (series = perc.reservice.week$observed,
                                  indicator.label="% Reservices after 4 weeks",
                                  show.window = weeks.to.show,
                                  index.dates = index.dates.week[200:471,],
                                  ylabel = 'Number of sows',
                                  xlabel = 'Week',
                                  target.vector.UCL = perc.reservice.week$UCL,
                                  target.vector.LCL = NULL,
                                  shading.matrix = NULL,
                                  limits = NULL)

ggplotly(perc.reservice.week.barplot)



# % Failures after 4 weeks ----
## VS

#sum the lines because we want no parity
numerator.perc.failure.week <- rowSums(perc.failure[,,1])
denominator.perc.failure.week <- rowSums(perc.failure[,,2])
observed.perc.failure.week <- round(((rowSums(perc.failure[,,1])/rowSums(perc.failure[,,2]))*100),2)


#take off the first 200 weeks because there were almost only 0's (for every weekly indicator)
indicator.perc.failure.week <- data.frame(numerator.perc.failure.week[200:471], 
                                            denominator.perc.failure.week[200:471],
                                            observed.perc.failure.week[200:471])

#create a table  
baseline.perc.failure.week <- c(rep(NA, dim(indicator.perc.failure.week)[1]))
UCL.perc.failure.week <- c(rep(NA, dim(indicator.perc.failure.week)[1]))
LCL.perc.failure.week <- c(rep(NA, dim(indicator.perc.failure.week)[1]))
alarm.perc.failure.week <- c(rep(NA, dim(indicator.perc.failure.week)[1]))

perc.failure.week <- data.frame(index.dates.week$start[200:471],indicator.perc.failure.week,
                                  baseline.perc.failure.week, UCL.perc.failure.week,
                                  LCL.perc.failure.week, alarm.perc.failure.week)

  colnames(perc.failure.week) <- c("date", "numerator", "denominator", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=perc.failure.week)


## Results

perc.failure.week <- ewma_indicator(indicator=perc.failure.week)

View(perc.failure.week) 


## Dashboard

perc.failure.week.barplot <- TS.barplot (series = perc.failure.week$observed,
                                           indicator.label="% Failures after 4 weeks",
                                           show.window = weeks.to.show,
                                           index.dates = index.dates.week[200:471,],
                                           ylabel = 'Number of sows',
                                           xlabel = 'Week',
                                           target.vector.UCL = perc.failure.week$UCL,
                                           target.vector.LCL = NULL,
                                           shading.matrix = NULL,
                                           limits = NULL)

ggplotly(perc.failure.week.barplot)



# Time to re-reservice, last 100 ---- 
##(Only 6, without VS)

#create a table
baseline.rereservices <- c(rep(NA, dim(rereservices)[1]))
UCL.rereservices <- c(rep(NA, dim(rereservices)[1]))
LCL.rereservices <- c(rep(NA, dim(rereservices)[1]))
alarm.rereservices <- c(rep(NA,dim(rereservices)[1]))

time.between.reservice.reservice <- data.frame(rereservices[,"date"],rereservices[,"sowINDEX"],
                                             rereservices[,"indicator"], baseline.rereservices,
                                             UCL.rereservices, LCL.rereservices,
                                             alarm.rereservices)

  colnames(time.between.reservice.reservice) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")



## Dashboards

time.between.reservice.reservice.barplot <- nonTS.barplot.timeless(series = time.between.reservice.reservice,
                                                                 indicator.label="Time to re-reservice",
                                                                 show.window = dim(time.between.reservice.reservice)[1],
                                                                 vertical.line = NULL,
                                                                 vertical.line.label=NULL,
                                                                 index.dates = index.dates.days,
                                                                 ylabel = 'Time to re-reservice (in days)',
                                                                 xlabel = 'Last 100 events',
                                                                 target.UCL = NULL,
                                                                 target.LCL = NULL,
                                                                 target.unit="vector", #c("value","vector)
                                                                 target.label="target",
                                                                 series.label="sows")


ggplotly(time.between.reservice.reservice.barplot)



# Time to abortion, last 100 ---- 
##(Only 34, without VS)

#create a table
baseline.time.to.abortion <- c(rep(NA, dim(time.to.abortion)[1]))
UCL.time.to.abortion <- c(rep(NA, dim(time.to.abortion)[1]))
LCL.time.to.abortion <- c(rep(NA, dim(time.to.abortion)[1]))
alarm.time.to.abortion <- c(rep(NA,dim(time.to.abortion)[1]))

time.to.abortion.df <- data.frame(time.to.abortion[,"date"],time.to.abortion[,"sowINDEX"],
                                               time.to.abortion[,"indicator"], baseline.time.to.abortion,
                                               UCL.time.to.abortion, LCL.time.to.abortion,
                                               alarm.time.to.abortion)

  colnames(time.to.abortion.df) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Dashboards

time.to.abortion.df.barplot <- nonTS.barplot.timeless(series = time.to.abortion.df,
                                                      indicator.label="Time to abortion",
                                                      show.window = dim(time.to.abortion.df)[1],
                                                      vertical.line = NULL,
                                                      vertical.line.label=NULL,
                                                      index.dates = index.dates.days,
                                                      ylabel = 'Time to abortion (in days)',
                                                      xlabel = 'Last 100 events',
                                                      target.UCL = NULL,
                                                      target.LCL = NULL,
                                                      target.unit="vector", #c("value","vector)
                                                      target.label="target",
                                                      series.label="sows")


ggplotly(time.to.abortion.df.barplot)
