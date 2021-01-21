library(RColorBrewer)


#indicators construction ----

parity = 1:15

reservice.threshold <- 90
reservice.perc.window <- 4 #(weeks)

empty.days.target <- 4
#opendays.target <- 4

plot.years=2
baseline.years=3

# ploting ----

weeks.to.show <- 78
nonTS.to.show <- 100
#days.ago.nonTS <- 114
group.window <- 114


# events ----


sow.events <- c("birth","service","pregnancyTest","abortion","farrowing","weaning","exit","death")
sow.events.before.exit <- c("birth","service","reservice","abortion","farrowing","weaning")


sow.info <- c("parity","status")
events.info <- c("NrBornAlive","NrBornDead","NrSmallStillBorn","NrWeakBorn","NrMummified",
                 "NrMoved","NrWeaned","WeanedTotalWeight","ExitReason", "ExitType")

#codes from Winpig ----
exit.type <- data.frame(Slaughtered=1,
                        Sold=2,
                        Euthanized=3,
                        Dead=4,
                        Sold.Pregnant=5,
                        Missing=6,
                        Exported=7)

animal.category <- data.frame(Farm.animal=0,
                              Parent=1,
                              Insemination.donor=2,
                              Gylts.produced=3)

# coding ----
#NA always equal to NOT RELEVANT

#birth.coding   <- data.frame(code=c(1,2),
#                             meaning=c("birth-date","alive/active"))
#service.coding <- data.frame(code=c(0,1,2,3),
#                      meaning=c("no service","service-date","reservice-date","serviced")) 
service.coding <- data.frame(code=c(1,2),
                             meaning=c("service","reservice")) 
pregnancy.coding <- data.frame(code=c(0,2), #2 because that's how it was on WinPig
                               meaning=c("not pregnant","pregnant")) #NA will be the unknown
# abortion.coding   <- data.frame(code=c(1),
#                              meaning=c("abortion-date"))
# farrowing.coding   <- data.frame(code=c(1,2),
#                                 meaning=c("farrow-date","farrowed"))
# weaning.coding   <- data.frame(code=c(1),
#                                  meaning=c("wean-date"))
# exit.coding   <- data.frame(code=c(0,1,2),
#                                  meaning=c("not-exited","exit-date","exited"))
status.coding   <- data.frame(code=c(0,1,2,3,4,5),
                              meaning=c("gilt","empty","assumedPregnant","Pregnant","nursing","exited"))



# grouping parity ----
c1 <- 0:15
c2 <- c(rep("gilt",2),
        rep("2",1),
        rep("3-5",3),
        rep(">5",10)
)
c3 <- c(rep("gilt",2),
        rep("young",1),
        rep("prime",3),
        rep("mature",10)
)


#parity.group <- data.frame(parity = c1, group=c2, group.name = c3)
parity.group <- data.frame(parity = c1, group.name = ordered(c3,levels=c("gilt","young","prime","mature")))



qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_parity = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

parity.group$color1 <- col_parity[as.numeric(as.factor(parity.group$group.name))]
parity.group$color2 <- col_parity[as.numeric(as.factor(parity.group$parity))]

parity.group2 <- parity.group[-1,]

colors.custom<- c(rep("#4287f5",1),
                  rep("#28ab1f",2),
                  rep("#f5942c",3),
                  rep("#a15a4c",9)
)
parity.group2 <-cbind(parity.group2,colors.custom) 
parity.group2$colors.custom<-colors.custom

color.pg <- c("#4287f5","#28ab1f","#f5942c","#a15a4c")



