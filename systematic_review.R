##########

# [burden-eu] Systematic literature review of BoD studies ########

## Systematic Review ######################


library(readr)
library(dplyr)


################################################
# setwd("C:/Users/Valerio/Documents/R/Projects/Systematic_literature_review_BoD")

#load data "literature CDs" and check of the dimention and structure
data<-read_csv("Included_literature_CDs.csv")

head(data)
glimpse(data)
dim(data)# 1048475       6
#get a rid of the NA
data<-data%>%filter(!is.na(data))
dim(data)
data

#check if further "NAs" are in the "data" set
table(is.na.data.frame(data))
missing_values<-data[rowSums(is.na(data)) > 0, ]

missing_values["URL"]
missing_values["Objectives"]
missing_values["Title"]

# url is not matching
# url<-"https://pubmed-ncbi-nlm-nih-gov.eur.idm.oclc.org/10465078/"
utils::browseURL(url)

# the article is found at :
# https://journals.lww.com/aidsonline/fulltext/1999/08200/mortality_in_young_adults_in_england_and_wales_.14.aspx
# and missing "Journal" is:
# Wolters Kluwer AIDS


# check for duplicates-------------------------
names(data)
data_processed<-data
dupes<-data_processed%>%
  dplyr::select(Authors,Title,URL,Objectives)%>%
  janitor::get_dupes()

dupes_url<-dupes$URL[1]
browseURL(dupes_url)

# investigate the same URL
data_processed%>%
  filter(URL==dupes_url)%>%
  glimpse()

# it looks like there is one duplicate
no_duplicates_data<-distinct(data)
dim(no_duplicates_data)

data_duplicated<-subset(data,duplicated(data))
data_duplicated["Authors"]


#check of the Author's name spelled correctly
plyr::count(data$Authors)

######################################################

#load data "literature_injuries" and check of the dimention and structure
data_injury<-read_csv("Included_literature_injuries.csv")

head(data_injury)
glimpse(data_injury)
dim(data_injury)#1048253       6
#get a rid of the NA
data_injury<-data_injury%>%filter(!is.na(data_injury))
dim(data_injury)

#check if further "NAs" are in the "data" set
table(is.na.data.frame(data_injury))#there are no NAs in this set


# it looks like there is no duplicates
no_duplicates_data_injury<-distinct(data_injury)
dim(no_duplicates_data_injury)

data_duplicated<-subset(data,duplicated(data))
data_duplicated["Authors"]


###########################################

#check of the repetitions within the two sets
s<-data%>%filter(Journal=="IHME")
d<-data_injury%>%filter(Journal=="IHME")
s==d

#build a full data frame without repetitions
full<-data.frame()
full<-rbind(data,data_injury)
dim(full)
head(full)


# it looks like there are some duplicates within the two literature sets
no_duplicates_full<-distinct(full)
dim(no_duplicates_full)

data_duplicated_full<-subset(full,duplicated(full))
data_duplicated_full
dim(data_duplicated_full)
#it looks like there are 70 replications within the two main sets of whic one is the repetition i the "data" set found above
data_duplicated_full["Authors"]

require(readr)
write.csv(data_duplicated_full,"data_duplicated_full.csv")


###################################

# open selected literatures to
data.frame(full$URL)

utils::browseURL(url)







