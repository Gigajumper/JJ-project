###############################################################################################
####################    JJ data set cleaning     #####################
###############################################################################################
library(lattice)
library(ggplot2)
library(caret)
library(dplyr)
library(stats)
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
install.packages("stringi")
library(stringi)
install.packages("corrplot")
library("RColorBrewer")
library(scales)

getwd();
JJdata=read.csv("C:\\Users\\khade\\MachineLearning\\JJproject\\JJ_tickets_data\\support_tickets_dataset_ML.csv", header = TRUE, dec = ".")

#trow <- 
#21750
nrow(JJdata)
#18
ncol(JJdata)

#10522
#sdata <- igdata[seq(1,trow,2),]
#nr <- nrow(sdata)
sdata <- JJdata

#Remove irrelevant features
sdata <- select(sdata,-incident)
sdata <- select(sdata,-cust_time)
sdata <- select(sdata,-pend_time)
sdata <- select(sdata,-call_log)
sdata <- select(sdata,-chat_log)
sdata <- select(sdata,-Closed)

#Make sure there are no NA entries.
sum(is.na(sdata))  #0

sum(sdata$resolved == "")  #455
#Remove rows with blanks
sdata <- sdata[which(sdata$resolved != "" ),]

sum(sdata$opened == "")
sum(sdata$application == "")
sum(sdata$region == "")
sum(sdata$prod_line == "")
sum(sdata$app_category == "")
sum(sdata$priority == "")
sum(sdata$sup_grp == "")

sum(sdata$grp_level == "")  #1
#Remove rows with blanks
sdata <- sdata[which(sdata$grp_level != "" ),]

sum(sdata$res_category == "") #1
#Remove rows with blanks
sdata <- sdata[which(sdata$res_category != "" ),]

sum(sdata$opened == "")
sum(sdata$urgency == "")
sum(sdata$impact == "")

#------------21293
nrow(sdata)

sum(!complete.cases(sdata)) #0

sum(duplicated(sdata))  #17

#duplicated observations are most probably an error (these can be valid data but unlikely)
sdata <- distinct(sdata)

nrow(sdata) #21276

#Convert ordinal categorical variables to numeric and save in a new feature.
sdata["impactN"] <- "NA"
sdata[sdata$impact=="Limited","impactN"] <- as.numeric(1) 
sdata[sdata$impact=="Large","impactN"] <- as.numeric(2)
sdata[sdata$impact=="Widespread","impactN"] <- as.numeric(3)

sdata["urgencyN"] <- "NA"
sdata[sdata$urgency=="Low","urgencyN"] <- as.numeric(1) 
sdata[sdata$urgency=="Medium","urgencyN"] <- as.numeric(2)
sdata[sdata$urgency=="High","urgencyN"] <- as.numeric(3)

sdata["priorityN"] <- "NA"
sdata[sdata$priority=="Priority 4","priorityN"] <- as.numeric(1) 
sdata[sdata$priority=="Priority 3","priorityN"] <- as.numeric(2)
sdata[sdata$priority=="Priority 2","priorityN"] <- as.numeric(3)

sdata$impactN <- as.numeric(sdata$impactN)
sdata$urgencyN <- as.numeric(sdata$urgencyN)
sdata$priorityN <- as.numeric(sdata$priorityN)

#Remove impact, urgency, priority (since their numeric counterparts are included.)
sdata <- select(sdata,-impact)
sdata <- select(sdata,-urgency)
sdata <- select(sdata,-priority)

#convert dates to date objects
sdata["open_date"] <- "NA"
sdata["open_date"] <- as.Date(as.character(sdata[,"opened"]),"%Y-%m-%d")
sdata["resolve_date"] <- "NA"
sdata["resolve_date"] <- as.Date(as.character(sdata[,"resolved"]),"%Y-%m-%d")

#Calculate days required to close a ticket
sdata["ndays"] <- "NA"
sdata["ndays"] <- as.numeric(sdata[,"resolve_date"] - sdata[,"open_date"])

#Apart from Level 1, Level 2, Level 3, there are levels "" and "3rd party" with 0 and 2 entries.
levels(sdata$grp_level)
#Drop these levels.
sdata <- sdata[which(sdata$grp_level != "" & sdata$grp_level != "3rd Party"),]
sdata$grp_level <- droplevels(sdata$grp_level)


#Convert ordinal level to numeric and save in new feature
sdata["levelN"] <- as.numeric(0)
sdata[sdata$grp_level=="Level 1","levelN"] <- as.numeric(1) 
sdata[sdata$grp_level=="Level 2","levelN"] <- as.numeric(2)
sdata[sdata$grp_level=="Level 3","levelN"] <- as.numeric(3)

#Remove grp_level
sdata <- select(sdata,-grp_level)

#21274
nrow(sdata)

#drop low frequency level for certain features. Threshold = 2.5%
perc <- round(nrow(sdata)*2.5/100)

tt <- table(sdata$app_category)
rare_levels <- names(tt)[tt<perc]
sdata <- subset(sdata,!app_category %in% rare_levels)
sdata$app_category <- factor(sdata$app_category)

tt <- table(sdata$res_category)
rare_levels <- names(tt)[tt<perc]
sdata <- subset(sdata,!res_category %in% rare_levels)
sdata$res_category <- factor(sdata$res_category)

tt <- table(sdata$region)
rare_levels <- names(tt)[tt<perc]
sdata <- subset(sdata,!region %in% rare_levels)
sdata$region <- factor(sdata$region)

#17410
nrow(sdata)

#Since priority and urgency are highly correlated (0.89) urgency is dropped from further analysis.
sdata <- select(sdata,-urgencyN)

## Calculating Performance
#We are rating the servcies provided by the vendor the following way:
#Excellent:  P1, P2 or P3 tickets resolved within 1 day
#Good:       For P2 tickets resolved within 5 Days
#Average:    For P2 or P1 Tickets resolved within 10 days
#Failed:     For any other ticket is considered Bad performance is rated as failed service.
#- P3 resolved in more than 1 day
#- P2 resolved in more than 5 days
#- P1 resolved in more than 10 days

sdata$performance <- ifelse(sdata$ndays < 1, "Excellent", ifelse(sdata$priorityN == 2 & sdata$ndays <= 5, "Good", ifelse(sdata$priorityN <= 2 & sdata$ndays <= 10, "Average", "Failed")))
sdata$performance <- factor(sdata$performance)

str(sdata)

