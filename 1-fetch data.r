# set up ------

# libraries
library(ISOweek)
library(lubridate)
library(abind)

# base code
source("Definitions.r")
source("Functions.r")


#load data from the farm (saved from database) ----
    #you should manually paste the data into the data folder of this repository
    #in your own computer
    #GIT will ignore those files, and they will NOT
    #be shared through the repository
  #in a farm application, data would be fetched directly from an SQL database
    #example
    #library(RODBC)
    #conn <- odbcDriverConnect('driver={SQL Server};server=svasql16;database=WP_Test1;trusted_connection=true')
    #animal <- sqlQuery(conn, "SELECT * FROM animal;")


load("data/pigpeaks.farm01.RData")
#This will load the following data frames
#animal; service; pregnancyTest; abortion; farrowing; weaning; dead.piglet;
#dead.piglet.group; progeny.count; progeny.dead; progeny.entry;
#progeny.exit; progeny.transfer; cause

#from WinPigs databases coding:
swedish=6

# date columns ----

animal$BirthDate<-as_date(animal$BirthDate)
animal$EntryDate<-as_date(animal$EntryDate)
animal$ExitDate<-as_date(animal$ExitDate)
service$EventDate<-as_date(service$EventDate)
abortion$EventDate<-as_date(abortion$EventDate)
farrowing$EventDate<-as_date(farrowing$EventDate)
weaning$EventDate<-as_date(weaning$EventDate)
pregnancyTest$EventDate<-as_date(pregnancyTest$EventDate)
progeny.dead$EventDate<-as_date(progeny.dead$EventDate)



# create BASE arrays for the first time ----

active.sows <- which(animal$Sex==1)
active.sows.ID <- animal$ID[active.sows]
active.sows.displayID <- data.frame(codesID=as.numeric(as.character(animal$ID[active.sows])),
                                    displayID=as.numeric(as.character(animal$AnimalNumber[active.sows])))
LAST.ANIMAL <- tail(animal$ID,1)

birth <-data.frame(AnimalId=animal$ID[active.sows],
                   EventDate=animal$BirthDate[active.sows])

exit <-data.frame(AnimalId=animal$ID[active.sows],
                  EventDate=animal$ExitDate[active.sows],
                  ExitType=animal$ExitType[active.sows],
                  ExitCause=animal$ExitCause2Id[active.sows],
                  AnimalType=animal[match(animal$ID[active.sows],animal$ID),"AnimalType"] )

exit <- exit[exit$AnimalType==0|exit$AnimalType==3,]

death <- exit[exit$ExitType==3|exit$ExitType==4,]


# dates.index ----
#start.date for that farm
#is the MONDAY before the date when the first sow was born

start.date   <- lastmon(min(animal$BirthDate[active.sows],na.rm=T))
#start.date   <- lastmon(as.Date("2018-01-01"))


index.dates.week<-data.frame(ISOweek=date2ISOweek(start.date),
                             year=as.numeric(substr(as.character(date2ISOweek(start.date)),1,4)),
                             week=as.numeric(substr(as.character(date2ISOweek(start.date)),7,8)),
                             start=start.date,
                             end=start.date+6
)

index.dates.days <-dates_df(start.date, start.date,
                            date.format="%Y-%m-%d"
)
index.dates.days$ISOweek=date2ISOweek(index.dates.days$dates)


# data structure - all events for individual sows ----
# record individually all events that happen with each sow each day
# and the day when they change status, etc


individual.sows.info <- c(sow.events,sow.info, events.info)
individual.sows <- list()

empty.matrix <- matrix(NA,
                       nrow=dim(index.dates.days)[1],
                       ncol=length(active.sows))
colnames(empty.matrix)<- active.sows.ID

for (i in 1:length(individual.sows.info)){
  individual.sows[[i]]<-empty.matrix
}

names(individual.sows)<- c(sow.events,sow.info, events.info)




# # an alternative based on all individual matrices
# # (abandoned for now because it is harder to apply functions to all if not grouped)
# 
# individual.sows.info <- c(sow.events,sow.info, events.info)
# 
# empty.matrix <- matrix(NA,
#                        nrow=dim(index.dates.days)[1],
#                        ncol=length(active.sows))
# colnames(empty.matrix)<- active.sows.ID
# 
# # ".ind" in front of name because some names are already data-frames
# #from the original database 
# for (i in 1:length(individual.sows.info)){
#   assign(paste0("ind.",individual.sows.info[i]),empty.matrix)
# }
# 
# individual.sows.matrix.list <- paste0("ind.",individual.sows.info)
# 

