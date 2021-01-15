
library(ISOweek)
library(lubridate)
library(abind)
library(stringr)
library(plotly)


source("Definitions.r")
source("Functions.r")
source("Functions-plotting.r")
load("indicators.RData")





# services ----

services.week.pg <- per.parity.indicator.grouping(parity.group = parity.group2,
                                                  indicator.matrix = services.week,
                                                  percentage = FALSE)


services.plot <- TS.barplot.pg(series = services.week.pg,
                               indicator.label="services",
                               show.window = weeks.to.show,
                               index.dates = index.dates.week,
                               ylabel = 'Number of sows',
                               xlabel = 'Week',
                               target.vector = NULL,
                               shading.matrix = NULL,
                               limits = NULL,
                               group.labels=c('gilts','young','prime','mature'))






# reservices ----

reservices.week.pg <- per.parity.indicator.grouping(parity.group = parity.group2,
                                                    indicator.matrix = reservices.week,
                                                    percentage = FALSE)


reservices.plot <- TS.barplot.pg(series = reservices.week.pg,
                                 indicator.label="Reservices",
                                 show.window = weeks.to.show,
                                 index.dates = index.dates.week,
                                 ylabel = 'Number of sows',
                                 xlabel = 'Week',
                                 target.vector = NULL,
                                 shading.matrix = NULL,
                                 limits = NULL,
                                 group.labels=c('gilts','young','prime','mature'))



# days.between.farrowings-----


days.between.farrowings.plot <- nonTS.barplot.pg.timeless(series = days.between.farrowings,
                                                           indicator.label="Days between farrowings",
                                                           show.window = nonTS.to.show*4,
                                                           vertical.line = NULL,
                                                           vertical.line.label=NULL,
                                                           index.dates = index.dates.days,
                                                           ylabel = 'Days between farrowings',
                                                           xlabel = '',
                                                           target = NULL
                                                           #shading.matrix = NULL,
                                                           #limits = NULL
)

# mortality in tillväxt pigs ----

tillvaxt.mortality <- TS.barplot(series = piglets.deaths.week,
                                 indicator.label="Mortality in small piglets (TILLVÄXT)",
                                 show.window = weeks.to.show*2,
                                 index.dates = index.dates.week,
                                 ylabel = 'piglets dead',
                                 xlabel = 'Week',
                                 target.vector = NULL,
                                 shading.matrix = NULL,
                                 limits = NULL)


# number.deaths.week.plot ----
number.deaths.week.pg <- per.parity.indicator.grouping(parity.group = parity.group2,
                                                       indicator.matrix = number.deaths.week,
                                                       percentage = FALSE)


number.deaths.week.plot <- TS.barplot.pg(series = number.deaths.week.pg,
                                         indicator.label="Death",
                                         show.window = weeks.to.show,
                                         index.dates = index.dates.week,
                                         ylabel = 'Number of sows',
                                         xlabel = 'Week',
                                         target.vector = NULL,
                                         shading.matrix = NULL,
                                         limits = NULL,
                                         group.labels=c('gilts','young','prime','mature'))


save(services.plot,
     reservices.plot,
     tillvaxt.mortality,
     number.deaths.week.plot,
     days.between.farrowings.plot,
     file="dashboard.plots.RData")

