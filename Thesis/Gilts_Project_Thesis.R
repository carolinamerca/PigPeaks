#GILTS

library(ISOweek)
library(lubridate)
library(abind)
library(stringr)
library(plotly)


source("Definitions.r")
source("Functions.r")
source("Functions-plotting.r")
source("Dashboard.R")
source("Services_Project_Thesis.R") # dá erro porquê?
load("all_indicators.RData")



# Age at first service, last 200 ----
##(Managment, without VS)

#create a table
baseline.time.to.first.service <- c(rep(NA, dim(time.to.first.service)[1]))
UCL.time.to.first.service <- c(rep(NA, dim(time.to.first.service)[1]))
LCL.time.to.first.service <- c(rep(NA, dim(time.to.first.service)[1]))
alarm.time.to.first.service <- c(rep(NA, dim(time.to.first.service)[1]))

age.at.first.service <- data.frame(time.to.first.service[,"date"],time.to.first.service[,"sowINDEX"],
                                  time.to.first.service[,"indicator"], baseline.time.to.first.service,
                                  UCL.time.to.first.service, LCL.time.to.first.service,
                                  alarm.time.to.first.service)

  colnames(age.at.first.service) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Dashboards

age.at.first.service.barplot <- nonTS.barplot.timeless(series = age.at.first.service,
                                                      indicator.label="Age at First Service",
                                                      show.window = nonTS.to.show*2, #last 200
                                                      vertical.line = NULL,
                                                      vertical.line.label=NULL,
                                                      index.dates = index.dates.days,
                                                      ylabel = 'Age at first service (in days)',
                                                      xlabel = 'Last 200 events',
                                                      target = 248, #248 dias de vida
                                                      target.unit="value", #c("value","vector")
                                                      target.label="target",
                                                      series.label="sows")



ggplotly(age.at.first.service.barplot)



# Age at first farrowing, last 200 ----
##(Managment, without VS)

#create a table
baseline.time.to.first.farrowing <- c(rep(NA, dim(time.to.first.farrowing)[1]))
UCL.time.to.first.farrowing <- c(rep(NA, dim(time.to.first.farrowing)[1]))
LCL.time.to.first.farrowing <- c(rep(NA, dim(time.to.first.farrowing)[1]))
alarm.time.to.first.farrowing <- c(rep(NA, dim(time.to.first.farrowing)[1]))

age.at.first.farrowing <- data.frame(time.to.first.farrowing[,"date"],time.to.first.farrowing[,"sowINDEX"],
                                   time.to.first.farrowing[,"indicator"], baseline.time.to.first.farrowing,
                                   UCL.time.to.first.farrowing, LCL.time.to.first.farrowing,
                                   alarm.time.to.first.farrowing)

  colnames(age.at.first.farrowing) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Dashboards

age.at.first.farrowing.barplot <- nonTS.barplot.timeless(series = age.at.first.farrowing,
                                                       indicator.label="Age at First Farrowing",
                                                       show.window = nonTS.to.show*2, #last 200
                                                       vertical.line = NULL,
                                                       vertical.line.label=NULL,
                                                       index.dates = index.dates.days,
                                                       ylabel = 'Age at first farrowing (in days)',
                                                       xlabel = 'Last 200 events',
                                                       target = 365, # 1 year old
                                                       target.unit="value", #c("value","vector")
                                                       target.label="target",
                                                       series.label="sows")



ggplotly(age.at.first.farrowing.barplot)
