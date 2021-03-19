#FARROWING

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



# Pregnancy length, 200 last ----
##VS

#create a table
baseline.pregnancy.length <- c(rep(NA, dim(pregnancy.length)[1]))
UCL.pregnancy.length <- c(rep(NA, dim(pregnancy.length)[1]))
LCL.pregnancy.length <- c(rep(NA, dim(pregnancy.length)[1]))
alarm.pregnancy.length <- c(rep(0, dim(pregnancy.length)[1])) #pus 0's

pregnancy.length.df <- data.frame(pregnancy.length[,"date"],pregnancy.length[,"sowINDEX"],
                                             pregnancy.length[,"indicator"], baseline.pregnancy.length,
                                             UCL.pregnancy.length, LCL.pregnancy.length,
                                             alarm.pregnancy.length)

  colnames(pregnancy.length.df) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=pregnancy.length.df)


## Results

pregnancy.length.df <- ewma_indicator(indicator=pregnancy.length.df)

View(pregnancy.length.df)                                  


## Dashboards

pregnancy.length.df.barplot <- nonTS.barplot.timeless(series = pregnancy.length.df,
                                                      indicator.label="Pregnancy Length",
                                                      show.window = nonTS.to.show*2, #last 200
                                                      vertical.line = NULL,
                                                      vertical.line.label=NULL,
                                                      index.dates = index.dates.days,
                                                      ylabel = 'Pregnancy length (in days)',
                                                      xlabel = 'Last 200 events',
                                                      target.UCL = tail(pregnancy.length.df$UCL,200), #last 200
                                                      target.LCL = tail(pregnancy.length.df$LCL,200), #last 200
                                                      alarms = tail(pregnancy.length.df$alarms,200),
                                                      target.unit="vector", #c("value","vector")
                                                      target.label="target",
                                                      series.label="sows")


ggplotly(pregnancy.length.df.barplot)



# Days between farrowings, 200 last ----
##(Depends on several factors, Without SV)

#create a table
baseline.days.between.farrowings <- c(rep(NA, dim(days.between.farrowings)[1]))
UCL.days.between.farrowings <- c(rep(NA, dim(days.between.farrowings)[1]))
LCL.days.between.farrowings <- c(rep(NA, dim(days.between.farrowings)[1]))
alarm.days.between.farrowings <- c(rep(0, dim(days.between.farrowings)[1])) #Pus 0's

days.between.farrowings.df <- data.frame(days.between.farrowings[,"date"],days.between.farrowings[,"sowINDEX"],
                                  days.between.farrowings[,"indicator"], baseline.days.between.farrowings,
                                  UCL.days.between.farrowings, LCL.days.between.farrowings,
                                  alarm.days.between.farrowings)

  colnames(days.between.farrowings.df) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


# ## Applying EWMA
# 
# ewma_indicator(indicator=days.between.farrowings.df)
# 
# 
# ## Results
# 
# days.between.farrowings.df <- ewma_indicator(indicator=days.between.farrowings.df)
# 
# View(days.between.farrowings.df)


## Dashboards

days.between.farrowings.df.barplot <- nonTS.barplot.timeless(series = days.between.farrowings.df,
                                                      indicator.label="Days Between Farrowings",
                                                      show.window = nonTS.to.show*2, #last 200
                                                      vertical.line = NULL,
                                                      vertical.line.label=NULL,
                                                      index.dates = index.dates.days,
                                                      ylabel = 'Days between farrowings',
                                                      xlabel = 'Last 200 events',
                                                      target.UCL = tail(days.between.farrowings.df$UCL,200), #last 200
                                                      target.LCL = tail(days.between.farrowings.df$LCL,200), #last 200
                                                      #alarms = tail(days.between.farrowings.df$alarms,200),
                                                      target.unit="vector", #c("value","vector")
                                                      target.label="target",
                                                      series.label="sows")


ggplotly(days.between.farrowings.df.barplot)



# Total piglets per farrowing, 200 last ----
##VS

#create a table
baseline.total.born.litter <- c(rep(NA, dim(total.born.litter)[1]))
UCL.total.born.litter <- c(rep(NA, dim(total.born.litter)[1]))
LCL.total.born.litter <- c(rep(NA, dim(total.born.litter)[1]))
alarm.total.born.litter <- c(rep(NA, dim(total.born.litter)[1]))

total.piglets.per.farrowing <- data.frame(total.born.litter[,"date"],total.born.litter[,"sowINDEX"],
                                         total.born.litter[,"indicator"], baseline.total.born.litter,
                                         UCL.total.born.litter, LCL.total.born.litter,
                                         alarm.total.born.litter)

  colnames(total.piglets.per.farrowing) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=total.piglets.per.farrowing)


## Results

total.piglets.per.farrowing <- ewma_indicator(indicator=total.piglets.per.farrowing)

View(total.piglets.per.farrowing)


## Dashboards

total.piglets.per.farrowing.barplot <- nonTS.barplot.timeless(series = total.piglets.per.farrowing,
                                                             indicator.label="Total Piglets per Farrowing",
                                                             show.window = nonTS.to.show*2, #last 200
                                                             vertical.line = NULL,
                                                             vertical.line.label=NULL,
                                                             index.dates = index.dates.days,
                                                             ylabel = 'Total Piglets',
                                                             xlabel = 'Last 200 events',
                                                             target.UCL = tail(total.piglets.per.farrowing$UCL,200), #last 200
                                                             target.LCL = tail(total.piglets.per.farrowing$LCL,200), #last 200
                                                             #alarms = tail(total.piglets.per.farrowing$alarms,200),
                                                             target.unit="vector", #c("value","vector")
                                                             target.label="target",
                                                             series.label="piglets")



ggplotly(total.piglets.per.farrowing.barplot)



# Live piglets per farrowing, 200 last ----
##VS

#create a table
baseline.live.born.litter <- c(rep(NA, dim(live.born.litter)[1]))
UCL.live.born.litter <- c(rep(NA, dim(live.born.litter)[1]))
LCL.live.born.litter <- c(rep(NA, dim(live.born.litter)[1]))
alarm.live.born.litter <- c(rep(NA, dim(live.born.litter)[1]))

live.piglets.per.farrowing <- data.frame(live.born.litter[,"date"],live.born.litter[,"sowINDEX"],
                                          live.born.litter[,"indicator"], baseline.live.born.litter,
                                          UCL.live.born.litter, LCL.live.born.litter,
                                          alarm.live.born.litter)

colnames(live.piglets.per.farrowing) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=live.piglets.per.farrowing)


## Results

live.piglets.per.farrowing <- ewma_indicator(indicator=live.piglets.per.farrowing)

View(live.piglets.per.farrowing)


## Dashboards

live.piglets.per.farrowing.barplot <- nonTS.barplot.timeless(series = live.piglets.per.farrowing,
                                                              indicator.label="Live Piglets per Farrowing",
                                                              show.window = nonTS.to.show*2, #last 200
                                                              vertical.line = NULL,
                                                              vertical.line.label=NULL,
                                                              index.dates = index.dates.days,
                                                              ylabel = 'Live Piglets',
                                                              xlabel = 'Last 200 events',
                                                              target.UCL = NULL,
                                                              target.LCL = tail(live.piglets.per.farrowing$LCL,200), #last 200
                                                              #alarms = tail(live.piglets.per.farrowing$alarms,200),
                                                              target.unit="vector", #c("value","vector")
                                                              target.label="target",
                                                              series.label="piglets")



ggplotly(live.piglets.per.farrowing.barplot)



# % dead piglets per farrowing, 200 last ----
##VS

#create a table
baseline.perc.dead.born.litter <- c(rep(NA, dim(perc.dead.born.litter)[1]))
UCL.perc.dead.born.litter <- c(rep(NA, dim(perc.dead.born.litter)[1]))
LCL.perc.dead.born.litter <- c(rep(NA, dim(perc.dead.born.litter)[1]))
alarm.perc.dead.born.litter <- c(rep(NA, dim(perc.dead.born.litter)[1]))

perc.dead.piglets.per.farrowing <- data.frame(perc.dead.born.litter[,"date"],perc.dead.born.litter[,"sowINDEX"],
                                         round((perc.dead.born.litter[,"indicator"]*100),2), baseline.perc.dead.born.litter,
                                         UCL.perc.dead.born.litter, LCL.perc.dead.born.litter,
                                         alarm.perc.dead.born.litter)

  colnames(perc.dead.piglets.per.farrowing) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=perc.dead.piglets.per.farrowing)


## Results

perc.dead.piglets.per.farrowing <- ewma_indicator(indicator=perc.dead.piglets.per.farrowing)

View(perc.dead.piglets.per.farrowing)


## Dashboards

perc.dead.piglets.per.farrowing.barplot <- nonTS.barplot.timeless(series = perc.dead.piglets.per.farrowing,
                                                             indicator.label="% Dead Piglets per Farrowing",
                                                             show.window = nonTS.to.show*2, #last 200
                                                             vertical.line = NULL,
                                                             vertical.line.label=NULL,
                                                             index.dates = index.dates.days,
                                                             ylabel = '% of Dead Piglets',
                                                             xlabel = 'Last 200 events',
                                                             target.UCL = tail(perc.dead.piglets.per.farrowing$UCL,200), #last 200
                                                             target.LCL = NULL, 
                                                             #alarms = tail(perc.dead.piglets.per.farrowing$alarms,200),
                                                             target.unit="vector", #c("value","vector")
                                                             target.label="target",
                                                             series.label="piglets")



ggplotly(perc.dead.piglets.per.farrowing.barplot)



# Mummified piglets per farrowing, 200 last ----
##VS

#create a table
baseline.mummi.born.litter <- c(rep(NA, dim(mummi.born.litter)[1]))
UCL.mummi.born.litter <- c(rep(NA, dim(mummi.born.litter)[1]))
LCL.mummi.born.litter <- c(rep(NA, dim(mummi.born.litter)[1]))
alarm.mummi.born.litter <- c(rep(NA, dim(mummi.born.litter)[1]))

mummi.piglets.per.farrowing <- data.frame(mummi.born.litter[,"date"],mummi.born.litter[,"sowINDEX"],
                                          mummi.born.litter[,"indicator"], baseline.mummi.born.litter,
                                          UCL.mummi.born.litter, LCL.mummi.born.litter,
                                          alarm.mummi.born.litter)

  colnames(mummi.piglets.per.farrowing) <- c("date", "sowINDEX", "observed", "baseline", "UCL", "LCL", "alarms")


## Applying EWMA

ewma_indicator(indicator=mummi.piglets.per.farrowing)


## Results

mummi.piglets.per.farrowing <- ewma_indicator(indicator=mummi.piglets.per.farrowing)

View(mummi.piglets.per.farrowing)


## Dashboards

mummi.piglets.per.farrowing.barplot <- nonTS.barplot.timeless(series = mummi.piglets.per.farrowing,
                                                              indicator.label="Mummified Piglets per Farrowing",
                                                              show.window = nonTS.to.show*2, #last 200
                                                              vertical.line = NULL,
                                                              vertical.line.label=NULL,
                                                              index.dates = index.dates.days,
                                                              ylabel = 'Mummified Piglets',
                                                              xlabel = 'Last 200 events',
                                                              target.UCL = tail(mummi.piglets.per.farrowing$UCL,200), #last 200
                                                              target.LCL = NULL,
                                                              #alarms = tail(mummi.piglets.per.farrowing$alarms,200),
                                                              target.unit="vector", #c("value","vector")
                                                              target.label="target",
                                                              series.label="piglets")



ggplotly(mummi.piglets.per.farrowing.barplot)
