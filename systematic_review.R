##########
# [burden-eu] Systematic literature review of BoD studies ########

## Systematic Review ######################
library(readr)

data<-read_csv("Included_literature_CDs.csv")
head(data)
View(data)
dim(data)
require(dplyr)
data%>%filter(Journal=="IHME")



data_injury<-read_csv("Included_literature_injuries.csv")
head(data_injury)
View(data_injury)
data_injury%>%filter(Journal=="IHME")
dim(data_injury)


table(names(data))==table(names(data_injury))

plyr::count(data$Authors)
plyr::count(data_injury$Authors)

full_with_replications<-rbind(data,data_injury,rep)
dim(full)

?rbind

identical(data,data_injury)

identical(full, rbind(data,data_injury, make.row.names=FALSE))

full<-unique(rbind(data,data_injury))

dim(full)
View(full)





