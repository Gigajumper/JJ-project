##############################################################################
####   Data cleaning code from Jairo's ML1000CourseProject_RetailGrp #########
##############################################################################

library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(fpc)
library(stats)    #   clustering algorithms
library(gower) # for using Gower to introduce categorical values with Hierarchical CLustering
library(StatMatch) # for using Gower to introduce categorical values with Hierarchical CLustering
library(NbClust)
library(stringi)
library(RColorBrewer)
library(scales)
library(rpart)
library(rpart.plot)
library(corrplot)

getwd();
sdata=read.csv("C:\\Users\\khade\\MachineLearning\\JJproject\\JJ_tickets_data\\support_tickets_dataset_ML.csv", header = TRUE, dec = ".")

#21750
nrow(sdata)

#Make sure there are no NA entries.
sum(is.na(sdata))  #0

sdata <- distinct(sdata)
nrow(sdata) #21750
#Removing observations where tickets are not closed
sdata <- sdata[which(sdata$resolved != "" ),]
sdata <- sdata[which(sdata$res_category != "" ),]

#21294
nrow(sdata)


#Removing any other groups that re not under ITSM Governance
sdata <- subset(sdata, grp_level=='Level 1' | grp_level=='Level 2' | grp_level=='Level 3')
nrow(sdata)
#21291


#Adding validation to avoid data issues
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

#21291
nrow(sdata)

#Remove irrelevant features
sdata <- select(sdata,-incident)
sdata <- select(sdata,-cust_time)
sdata <- select(sdata,-pend_time)
sdata <- select(sdata,-call_log)
sdata <- select(sdata,-chat_log)
sdata <- select(sdata,-Closed)
#Ensuring all the observations are valid
sdata <- na.omit(sdata)
#str(sdata)

#21291
nrow(sdata)

sdata["impactN"] <- "NA"
sdata[sdata$impact=="Limited","impactN"] <- as.numeric(1) 
sdata[sdata$impact=="Large","impactN"] <- as.numeric(2)
sdata[sdata$impact=="Widespread","impactN"] <- as.numeric(3)
sdata <- select(sdata,-impact)

sdata["urgencyN"] <- "NA"
sdata[sdata$urgency=="Low","urgencyN"] <- as.numeric(1) 
sdata[sdata$urgency=="Medium","urgencyN"] <- as.numeric(2)
sdata[sdata$urgency=="High","urgencyN"] <- as.numeric(3)
sdata <- select(sdata,-urgency)

sdata["priorityN"] <- "NA"
sdata[sdata$priority=="Priority 4","priorityN"] <- as.numeric(1) 
sdata[sdata$priority=="Priority 3","priorityN"] <- as.numeric(2)
sdata[sdata$priority=="Priority 2","priorityN"] <- as.numeric(3)
sdata <- select(sdata,-priority)

#Convert ordinal level to numeric and save in new feature
sdata["levelN"] <- as.numeric(0)
sdata[sdata$grp_level=="Level 1","levelN"] <- as.numeric(1) 
sdata[sdata$grp_level=="Level 2","levelN"] <- as.numeric(2)
sdata[sdata$grp_level=="Level 3","levelN"] <- as.numeric(3)
#sdata <- select(sdata,-grp_level)

str(sdata)
#21291
nrow(sdata)

sum(!complete.cases(sdata))
sum(is.na(sdata))

sdata$impactN <- as.numeric(sdata$impactN)
sdata$priorityN <- as.numeric(sdata$priorityN)
sdata$urgencyN <- as.numeric(sdata$urgencyN)


#convert dates to date objects
sdata["open_date"] <- "NA"
sdata["open_date"] <- as.Date(as.character(sdata[,"opened"]),"%Y-%m-%d")
sdata <- select(sdata,-opened)
sdata["resolve_date"] <- "NA"
sdata["resolve_date"] <- as.Date(as.character(sdata[,"resolved"]),"%Y-%m-%d")
sdata <- select(sdata,-resolved)
#Calculate days required to resolve a ticket
sdata["ndays"] <- "NA"
sdata["ndays"] <- as.numeric(sdata[,"resolve_date"] - sdata[,"open_date"])

#21291
nrow(sdata)
#str(sdata)

sdata <- select(sdata,-urgencyN)

#21291
nrow(sdata)

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

#summary(sdata)
#17423
nrow(sdata)


