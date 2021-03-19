#WEANING/WEANED

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



# Days to weaning, 200 last ----



# Weanings per week ----



# Piglets weaned per week ----
##(Managment, without VS)

#sum the lines because we want no parity
observed.total.wean.week <- rowSums(total.wean.week)

#take off the first 200 weeks because there were almost only 0's (for every weekly indicator)
indicator.total.wean.week <- observed.total.wean.week[200:471]

#create a table  
baseline.total.wean.week  <- c(rep(NA, length(indicator.total.wean.week)))
UCL.total.wean.week  <- c(rep(NA, length(indicator.total.wean.week)))
LCL.total.wean.week  <- c(rep(NA, length(indicator.total.wean.week)))
alarm.total.wean.week  <- c(rep(NA, length(indicator.total.wean.week)))

piglets.weaned.per.week <- data.frame(index.dates.week$start[200:471],indicator.total.wean.week ,
                                          baseline.total.wean.week , UCL.total.wean.week ,
                                          LCL.total.wean.week , alarm.total.wean.week )

  colnames(piglets.weaned.per.week) <- c("date", "observed", "baseline", "UCL", "LCL", "alarms")


## Dashboard

piglets.weaned.per.week.barplot <- TS.barplot(series = piglets.weaned.per.week$observed,
                                              indicator.label="Piglets Weaned per Week",
                                              show.window = weeks.to.show,
                                              index.dates = index.dates.week[200:471,],
                                              ylabel = 'Number of piglets',
                                              xlabel = 'Week',
                                              target.vector = NULL,
                                              shading.matrix = NULL,
                                              limits = NULL)


ggplotly(piglets.weaned.per.week.barplot)

###TS.barplot.pg.perc?



# Weaned per weaning, 200 last ----
##VS

###nome??




# Weaned weight per weaning, 200 last ----
##VS

#create a table
baseline.weight.wean.litter <- c(rep(NA, dim(weight.wean.litter)[1]))
UCL.weight.wean.litter <- c(rep(NA, dim(weight.wean.litter)[1]))
LCL.weight.wean.litter <- c(rep(NA, dim(weight.wean.litter)[1]))
alarm.weight.wean.litter <- c(rep(0, dim(weight.wean.litter)[1])) #Pus 0's

weaned.weight.per.weaning <- data.frame(weight.wean.litter[,"date"],weight.wean.litter[,"sowINDEX"],
                                          weight.wean.litter[,"indicator"], baseline.weight.wean.litter,
                                          UCL.weight.wean.litter, LCL.weight.wean.litter,
                                          alarm.weight.wean.litter)

  colnames(weaned.weight.per.weaning) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=weaned.weight.per.weaning)


## Results

weaned.weight.per.weaning <- ewma_indicator(indicator=weaned.weight.per.weaning)

View(weaned.weight.per.weaning)


## Dashboards

weaned.weight.per.weaning.barplot <- nonTS.barplot.timeless(series = weaned.weight.per.weaning,
                                                              indicator.label="Weaned Weight per Weaning",
                                                              show.window = nonTS.to.show*2, #last 200
                                                              vertical.line = NULL,
                                                              vertical.line.label=NULL,
                                                              index.dates = index.dates.days,
                                                              ylabel = 'Weight (Kg)',
                                                              xlabel = 'Last 200 events',
                                                              target.UCL = NULL,
                                                              target.LCL = tail(weaned.weight.per.weaning$LCL,200), #last 200
                                                              alarms = tail(weaned.weight.per.weaning$alarms,200),
                                                              target.unit="vector", #c("value","vector")
                                                              target.label="target",
                                                              series.label="piglets")



ggplotly(weaned.weight.per.weaning.barplot)

##Dividir por numero de leitoes??



# Mortality weaned piglets per week ----
##VS

#take off the first 200 weeks because there were almost only 0's (for every weekly indicator)
piglets.deaths.week.ind <- piglets.deaths.week[200:471]

#create a table
baseline.piglets.deaths.week <- c(rep(NA, length(piglets.deaths.week.ind)))
UCL.piglets.deaths.week <- c(rep(NA, length(piglets.deaths.week.ind)))
LCL.piglets.deaths.week <- c(rep(NA, length(piglets.deaths.week.ind)))
alarm.piglets.deaths.week <- c(rep(0,length(piglets.deaths.week.ind))) #Pus 0's

mortality.weaned.piglets.per.week <- data.frame(index.dates.week$start[200:471], piglets.deaths.week.ind, 
                                                baseline.piglets.deaths.week, UCL.piglets.deaths.week,
                                                LCL.piglets.deaths.week, alarm.piglets.deaths.week)

  colnames(mortality.weaned.piglets.per.week) <- c("date", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=mortality.weaned.piglets.per.week)


## Results

mortality.weaned.piglets.per.week <- ewma_indicator(indicator=mortality.weaned.piglets.per.week)

View(mortality.weaned.piglets.per.week)


## Dashboard

mortality.weaned.piglets.per.week.barplot <- TS.barplot (series = mortality.weaned.piglets.per.week$observed,
                                                                  indicator.label="Mortality during the TILLVÄXT period",
                                                                  show.window = weeks.to.show,
                                                                  index.dates = index.dates.week[200:471,],
                                                                  ylabel = 'Piglets dead',
                                                                  xlabel = 'Week',
                                                                  target.vector.UCL = mortality.weaned.piglets.per.week$UCL,
                                                                  target.vector.LCL = NULL,
                                                                  #alarms = mortality.weaned.piglets.per.week$alarms,
                                                                  shading.matrix = NULL,
                                                                  limits = NULL)

ggplotly(mortality.weaned.piglets.per.week.barplot)


### tillvaxt.mortality ??
### Atemporal ??


