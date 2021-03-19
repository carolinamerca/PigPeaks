#EMPTY SOWS

library(ISOweek)
library(lubridate)
library(abind)
library(stringr)
library(plotly)


source("Definitions.r")
source("Functions.r")
source("Functions-plotting.r")
source("Services_Project_Thesis.R") # dá erro porquê?
load("all_indicators.RData")



# Sows empty longer than 4 days ----

#sum the lines because we want no parity
observed.empty.long.week <- rowSums(empty.long.week)

#take off the first 200 weeks because there were almost only 0's (for every weekly indicator)
indicator.empty.long.week <- observed.empty.long.week[200:471]

#create a table  
baseline.empty.long.week  <- c(rep(NA, length(indicator.empty.long.week)))
UCL.empty.long.week  <- c(rep(NA, length(indicator.empty.long.week)))
LCL.empty.long.week  <- c(rep(NA, length(indicator.empty.long.week)))
alarm.empty.long.week  <- c(rep(0, length(indicator.empty.long.week))) #Pus 0's

sows.empty.longer.four.days <- data.frame(index.dates.week$start[200:471],indicator.empty.long.week ,
                                  baseline.empty.long.week , UCL.empty.long.week ,
                                  LCL.empty.long.week , alarm.empty.long.week )

  colnames(sows.empty.longer.four.days) <- c("date", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=sows.empty.longer.four.days)


## Results

sows.empty.longer.four.days <- ewma_indicator(indicator=sows.empty.longer.four.days)

View(sows.empty.longer.four.days) 


## Dashboard

sows.empty.longer.four.days.barplot <- TS.barplot (series = sows.empty.longer.four.days$observed,
                                           indicator.label="Sows Empty Longer than 4 Days",
                                           show.window = weeks.to.show,
                                           index.dates = index.dates.week[200:471,],
                                           ylabel = 'Number of sows',
                                           xlabel = 'Week',
                                           target.vector.UCL = sows.empty.longer.four.days$UCL,
                                           target.vector.LCL = NULL,
                                           alarms = sows.empty.longer.four.days$alarms,
                                           shading.matrix = NULL,
                                           limits = NULL)


ggplotly(sows.empty.longer.four.days.barplot)

