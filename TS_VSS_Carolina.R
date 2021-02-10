library(ISOweek)
library(lubridate)
library(abind)
library(stringr)
library(plotly)


source("Definitions.r")
source("Functions.r")
source("Functions-plotting.r")
source("Dashboard.R")
load("indicators.RData")



##Functions-plotting

TS.lineplot.pg <- function(series = series.pg,
                          indicator.label="indicator",
                          show.window = weeks.to.show,
                          index.dates = index.dates.week,
                          ylabel = 'Number of sows',
                          xlabel = 'Week',
                          target.vector = NULL,
                          shading.matrix = NULL,
                          limits = NULL,
                          group.labels=c('gilts','young','prime','mature')
                          
){
  
  plot.range <- max(1,(dim(series)[1]-show.window+1)):dim(series)[1]
  
  y =  series[plot.range]
  
  data =  as.data.frame(series[plot.range,])
  
  
  x=index.dates.week$start[plot.range]
  x.week=paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
  y1 = data$gilt
  y2 = data$young
  y3 = data$prime
  y4 = data$mature
  y = y1+y2+y3+y4
  
  #labels
  t1 = group.labels[1]
  t2 = group.labels[2]
  t3 = group.labels[3]
  t4 = group.labels[4]
  
  text1=str_c("Parity group:",t1,
              "<br>",indicator.label,":",y1,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text2=str_c("Parity group:",t2,
              "<br>",indicator.label,":",y2,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text3=str_c("Parity group:",t3,
              "<br>",indicator.label,":",y3,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  text4=str_c("Parity group:",t4,
              "<br>",indicator.label,":",y4,
              "<br>Week:",x.week,
              "<br>WeekMonday:",x)
  
  
  
  target.vector = target.vector[plot.range]
  
  if(!is.null(shading.matrix)){
    shading.matrix=shading.matrix[plot.range,]
  }
  
  if(!is.null(shading.matrix)){
    LL3 <- shading.matrix[,1]
    LL2 <- shading.matrix[,2]  
    LL1 <- shading.matrix[,3]  
    UL1 <- shading.matrix[,4]  
    UL2 <- shading.matrix[,5]  
    UL3 <- shading.matrix[,6]  
  }
  
  if(!is.null(limits)){
    if(limits=="low"){
      UL1 <- max(y)
      UL2 <- max(y)
      UL3 <- max(y)
    }
    if(limits=="high"){
      LL1 <- min(0,min(data,na.rm=T),na.rm=T)
      LL2 <- min(0,min(data,na.rm=T),na.rm=T)
      LL3 <- min(0,min(data,na.rm=T),na.rm=T)
    }
  }
  
  
  
  
  color1=color.pg[1]
  color2=color.pg[2]
  color3=color.pg[3]
  color4=color.pg[4]
  
  
  plot <-
    plot_ly()
  
  if(!is.null(shading.matrix)){
    plot <- plot %>%
      add_trace(x=x,y = rep(max(y,na.rm=T),show.window),
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL3,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL2,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL1,
                name = 'normal', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = LL1,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%   
      add_trace(x=x,y = LL2,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%    
      add_trace(x=x,y = LL3,
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE)
    
  }
  
  
  
  plot <- plot %>%
    add_lines(x=x,y = y1,name=t1,type = 'scatter',mode = 'lines',yaxis="y2",
              line = list(color = color1, width = 2),text = text1, hoverinfo = 'text') %>%
    add_lines(x=x,y = y2,name=t2,type = 'scatter',mode = 'lines',yaxis="y2",
              line = list(color = color2, width = 2),text = text2, hoverinfo = 'text') %>%
    add_lines(x=x,y = y3,name=t3,type = 'scatter',mode = 'lines',yaxis="y2",
              line = list(color = color3, width = 2),text = text3, hoverinfo = 'text') %>%
    add_lines(x=x,y = y4,name=t4,type = 'scatter',mode = 'lines',yaxis="y2",
              line = list(color = color4, width = 2),text = text4, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'right', title = "", range = c(min(data,na.rm=T), max(y1+1,y2+1,y3+1,y4+1,na.rm=T))),
           yaxis2 = list(side = 'left', title = ylabel,overlaying = "y",range = c(min(data,na.rm=T), max(y1+1,y2+1,y3+1,y4+1,na.rm=T))),
           xaxis = list(title = xlabel),
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))
  
  
  
  if(!is.null(target.vector)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='indicator target',yaxis="y2",
        line=list(color = '#fc2821'), showlegend = FALSE
      )
  }
  
  
  
  
  return(plot)    
}



TS.lineplot <- function(series = series,
                        indicator.label="indicator",
                        show.window = weeks.to.show,
                        index.dates = index.dates.week,
                        ylabel = 'Number of sows',
                        xlabel = 'Week',
                        target.vector = NULL,
                        shading.matrix = NULL,
                        limits = NULL){
  
  plot.range <- max(1,(length(series)-show.window+1)):length(series)
  
  y =  series[plot.range]
  
  
  x=index.dates.week$start[plot.range]
  x.week=paste(index.dates$ISOweekYear[plot.range],index.dates$week[plot.range],sep="-")
  
  
  #labels
  
  text=str_c(indicator.label,":",y,
             "<br>Week:",x.week,
             "<br>WeekMonday:",x)
  
  target.vector = target.vector[plot.range]
  
  if(!is.null(shading.matrix)){
    shading.matrix=shading.matrix[plot.range,]
  }
  
  if(!is.null(shading.matrix)){
    LL3 <- shading.matrix[,1]
    LL2 <- shading.matrix[,2]  
    LL1 <- shading.matrix[,3]  
    UL1 <- shading.matrix[,4]  
    UL2 <- shading.matrix[,5]  
    UL3 <- shading.matrix[,6]  
  }
  
  if(!is.null(limits)){
    if(limits=="low"){
      UL1 <- max(y)
      UL2 <- max(y)
      UL3 <- max(y)
    }
    if(limits=="high"){
      LL1 <- min(0,min(data,na.rm=T),na.rm=T)
      LL2 <- min(0,min(data,na.rm=T),na.rm=T)
      LL3 <- min(0,min(data,na.rm=T),na.rm=T)
    }
  }
  
  
  
  
  plot <-
    plot_ly()
  
  if(!is.null(shading.matrix)){
    plot <- plot %>%
      add_trace(x=x,y = rep(max(y,na.rm=T),show.window),
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL3,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL2,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = UL1,
                name = 'normal', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='lightgreen',
                fill = 'tozeroy',showlegend = FALSE) %>%
      add_trace(x=x,y = LL1,
                name = '90%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='yellow',
                fill = 'tozeroy',showlegend = FALSE) %>%   
      add_trace(x=x,y = LL2,
                name = '95%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='orange',
                fill = 'tozeroy',showlegend = FALSE) %>%    
      add_trace(x=x,y = LL3,
                name = '99%', type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                fillcolor='tomato',
                fill = 'tozeroy',showlegend = FALSE)
    
  }
  
  
  
  plot <- plot %>%
    add_lines(x=x,y = y,type = 'scatter',mode = 'lines',yaxis="y2",
              text = text, hoverinfo = 'text') %>%
    layout(yaxis = list(side = 'right', title = "", range = c(min(y,na.rm=T), max(y,na.rm=T))),
           yaxis2 = list(side = 'left', title = ylabel,overlaying = "y",range = c(min(y,na.rm=T), max(y,na.rm=T))),
           xaxis = list(title = xlabel),
           legend=list(orientation="h",
                       x=0.25,y=max(y,na.rm=T),
                       traceorder='normal'))
  
  
  
  if(!is.null(target.vector)){
    plot <- plot %>%
      add_lines(
        x=x,
        y=target.vector,
        name='indicator target',yaxis="y2",
        line=list(color = '#fc2821'), showlegend = FALSE
      )
  }
  
  
  
  
  return(plot)    
}


##Dashboard per group

#Services ----

TS.services.pg.plot <- TS.lineplot.pg (series = services.week.pg,
                                            indicator.label="services",
                                            show.window = weeks.to.show,
                                            index.dates = index.dates.week,
                                            ylabel = 'Number of sows',
                                            xlabel = 'Week',
                                            target.vector = NULL,
                                            shading.matrix = NULL,
                                            limits = NULL,
                                            group.labels=c('gilts','young','prime','mature'))

ggplotly(TS.services.pg.plot)



#Reservices ----

TS.reservices.pg.plot <- TS.lineplot.pg (series = reservices.week.pg,
                                       indicator.label="reservices",
                                       show.window = weeks.to.show,
                                       index.dates = index.dates.week,
                                       ylabel = 'Number of sows',
                                       xlabel = 'Week',
                                       target.vector = NULL,
                                       shading.matrix = NULL,
                                       limits = NULL,
                                       group.labels=c('gilts','young','prime','mature'))

ggplotly(TS.reservices.pg.plot)


#Number.deaths.week.plot ----

TS.number.deaths.week.pg.plot <- TS.lineplot.pg(series = number.deaths.week.pg,
                                                indicator.label="Death",
                                                show.window = weeks.to.show,
                                                index.dates = index.dates.week,
                                                ylabel = 'Number of sows',
                                                xlabel = 'Week',
                                                target.vector = NULL,
                                                shading.matrix = NULL,
                                                limits = NULL,
                                                group.labels=c('gilts','young','prime','mature'))

ggplotly(TS.number.deaths.week.pg.plot)



## SUM total for each week

services.week.total <- rowSums(services.week)

reservices.week.total <- rowSums(reservices.week)

number.deaths.week.total <- rowSums(number.deaths.week)



##Dashboard total

#Services ----

TS.services.total.plot <-  TS.lineplot (series = services.week.total,
                                             indicator.label="services",
                                             show.window = weeks.to.show,
                                             index.dates = index.dates.week,
                                             ylabel = 'Number of sows',
                                             xlabel = 'Week',
                                             target.vector = NULL,
                                             shading.matrix = NULL,
                                             limits = NULL)


ggplotly(TS.services.total.plot)


#Reservices ----

TS.reservices.total.plot <-  TS.lineplot (series = reservices.week.total,
                                        indicator.label="reservices",
                                        show.window = weeks.to.show,
                                        index.dates = index.dates.week,
                                        ylabel = 'Number of sows',
                                        xlabel = 'Week',
                                        target.vector = NULL,
                                        shading.matrix = NULL,
                                        limits = NULL)


ggplotly(TS.reservices.total.plot)


#Mortality in tillväxt pigs ----

TS.tillvaxt.mortality <- TS.lineplot (series = piglets.deaths.week,
                                     indicator.label="Mortality in small piglets (TILLVÄXT)",
                                     show.window = weeks.to.show*2,
                                     index.dates = index.dates.week,
                                     ylabel = 'piglets dead',
                                     xlabel = 'Week',
                                     target.vector = NULL,
                                     shading.matrix = NULL,
                                     limits = NULL)

ggplotly(TS.tillvaxt.mortality)


#Number.deaths.week.plot ----

TS.number.deaths.week.total.plot <- TS.lineplot (series = number.deaths.week.total,
                                                indicator.label="Death",
                                                show.window = weeks.to.show,
                                                index.dates = index.dates.week,
                                                ylabel = 'Number of sows',
                                                xlabel = 'Week',
                                                target.vector = NULL,
                                                shading.matrix = NULL,
                                                limits = NULL)

ggplotly(TS.number.deaths.week.total.plot)



## VetSyn

library(vetsyn)

indicators.observed <- services.week.total %>% 
  cbind(reservices.week.total, piglets.deaths.week, number.deaths.week.total)
colnames(indicators.observed) <- c("Services", "Reservices", "PigletsDeaths", "SowsDeaths")

my.syndromic <- syndromicW(indicators.observed[200:nrow(indicators.observed),], 
                           dates = index.dates.week[200:nrow(index.dates.week),]) #start on week 12 year 2013

colnames(my.syndromic@dates) <- c("year", "week", "start")


my.syndromic <- clean_baseline_perc(my.syndromic, run.window = 104, plot=FALSE)

 
# HoltWinters ----

my.syndromic <- holt_winters_synd(x=my.syndromic,
                                   evaluate.window=12,
                                   frequency=52,
                                   baseline.window=104,
                                   limit.sd=c(2.5,3,3.5), #default
                                   nahead=2,
                                   correct.baseline=2,
                                   alarm.dim=1)

# EWMA ----

my.syndromic <- ewma_synd(x=my.syndromic,
                           evaluate.window=12,
                           baseline.window=104,
                           lambda=0.2,
                           limit.sd=c(2.5,3,3.5), #default
                           guard.band=2,
                           correct.baseline=FALSE,
                           alarm.dim=2,
                           # UCL=1,
                           # LCL=FALSE,
                           pre.process=FALSE,
                           formula=NULL,
                           frequency=52)

my.syndromic <- ewma_synd(x=my.syndromic,
                          evaluate.window=12,
                          baseline.window=104,
                          lambda=0.2,
                          limit.sd=c(2.5,3,3.5), #default
                          guard.band=2,
                          correct.baseline=FALSE,
                          alarm.dim=2,
                          # UCL=1,
                          # LCL=FALSE,
                          pre.process="glm",
                          formula=list(week~sin+cos),
                          frequency=52)

# Shewhart ----

my.syndromic <- shew_synd(x=my.syndromic,
                           evaluate.window=12,
                           baseline.window=104,
                           limit.sd=c(2.5,3,3.5), #default
                           guard.band=2,
                           correct.baseline=FALSE,
                           alarm.dim=3,
                           pre.process=FALSE,
                           formula=NULL,
                           frequency=52)

# CUSUM ----

my.syndromic <- cusum_synd(x=my.syndromic,
                            evaluate.window=12,
                            baseline.window=104,
                            limit.sd=c(2.5,3,3.5), #default
                            guard.band=2,
                            correct.baseline=FALSE,
                            alarm.dim=4,
                            pre.process=FALSE,
                            formula=NULL,
                            frequency=52)
