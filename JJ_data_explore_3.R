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

sdata <- sdata[which(sdata$resolved != "" ),]

#Though sdata is complete, I am not sure if some rows containing NULL or weid entries are caught by this command.
sum(!complete.cases(sdata))

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

#convert dates to date objects
sdata["open_date"] <- "NA"
sdata["open_date"] <- as.Date(as.character(sdata[,"opened"]),"%Y-%m-%d")
sdata["resolve_date"] <- "NA"
sdata["resolve_date"] <- as.Date(as.character(sdata[,"resolved"]),"%Y-%m-%d")

#Calculate days required to close a ticket
sdata["ndays"] <- "NA"
sdata["ndays"] <- as.numeric(sdata[,"resolve_date"] - sdata[,"open_date"])

#Drop levels is group level that are too low.
sdata <- sdata[which(sdata$grp_level != "" & sdata$grp_level != "3rd Party"),]
sdata$grp_level <- droplevels(sdata$grp_level)

#Convert ordinal level to numeric and save in new feature
sdata["levelN"] <- as.numeric(0)
sdata[sdata$grp_level=="Level 1","levelN"] <- as.numeric(1) 
sdata[sdata$grp_level=="Level 2","levelN"] <- as.numeric(2)
sdata[sdata$grp_level=="Level 3","levelN"] <- as.numeric(3)

#Since priority and urgency are highly correlated (0.89) urgency is dropped from further analysis.
sdata <- select(sdata,-urgencyN)
sdata <- select(sdata,-urgency)

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

