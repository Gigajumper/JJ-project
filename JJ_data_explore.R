###############################################################################################
####################    Subset wine dataset     #####################
### Delete bad data, choose top 10 varieties and regions by frequency ###
### Final data set smalldata has 10000 rows.
### It has columns : country, description, points, price, province, region_1 and variety.
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
sum(!complete.cases(sdata))
nrow(unique(sdata))

#Remove irrelevant features
sdata <- select(sdata,-incident)
sdata <- select(sdata,-cust_time)
sdata <- select(sdata,-pend_time)
sdata <- select(sdata,-call_log)
sdata <- select(sdata,-chat_log)
sdata <- select(sdata,-resolved)

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
sdata["close_date"] <- "NA"
sdata["close_date"] <- as.Date(as.character(sdata[,"Closed"]),"%Y-%m-%d")

#Calculate days required to close a ticket
sdata["ndays"] <- "NA"
sdata["ndays"] <- as.numeric(sdata[,"close_date"] - sdata[,"open_date"])

#head(sdata[,c("opened","open_date")],20)
#head(sdata[,c("Closed","close_date")])

#sdata[17808,c("opened", "Closed")]
#sdata[17808,c("open_date", "close_date")]
#sdata[17808,"ndays"]

#save
tempDF <- sdata
#sdata <- tempDF

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

#ggplot(sdata, aes(x=ndays)) + geom_bar(aes(y=..count../sum(..count..)),col='red',fill='red') +
#coord_cartesian(xlim=c(0,50))
#ggplot(sdata, aes(x=levelN)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='deepskyblue') +
#  coord_flip()

#Pcorr <- cor(sdata[,c("impactN","urgencyN","priorityN","levelN","ndays")])

#head(round(Pcorr,2))
#corrplot(Pcorr, type="upper",col=brewer.pal(n=8, name="RdYlBu"))

#vec <- as.Date(as.character(sdata[,"opened"]),"%Y-%m-%d")
#freqs <- aggregate(vec, by=list(vec), FUN=length)
#freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")
#ggplot(freqs, aes(x=names, y=x)) + geom_bar(stat="identity",color="blue") + 
#  ggtitle('Date ticket opened') + xlab('Date') + coord_cartesian(ylim=c(0,500))

#vecc <- as.Date(as.character(sdata[,"Closed"]),"%Y-%m-%d")
#freqsc <- aggregate(vecc, by=list(vecc), FUN=length)
#freqsc$names <- as.Date(freqsc$Group.1, format="%Y-%m-%d")
#ggplot(freqsc, aes(x=names, y=x)) + geom_bar(stat="identity",color="dodgerblue3") + 
#  ggtitle('Date ticket closed') + xlab('Date') + coord_cartesian(ylim=c(0,500))

#str1 <- strptime( paste(sdata[,1], dat[,2]), "%Y-%m-%d %H:%M:%S")

#savedir <- 'C:\\Users\\khade\\MachineLearning\\JJproject\\figs\\explore\\'
#fnm <- paste(c(savedir,'impact_hori.pdf'),collapse='')
#pdf(fnm)
#pp <- ggplot(sdata, aes(x=urgencyN)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='goldenrod') + 
#  coord_flip()
#pp
#print(pp)
#dev.off()

pp <- ggplot(sdata, aes(x=application)) + geom_bar(aes(y=..count../sum(..count..)),col='violet',fill='violet') + 
  coord_flip()

pp <- ggplot(sdata, aes(x=sup_grp)) + geom_bar(aes(y=..count../sum(..count..)),col='coral4',fill='coral4') + 
  coord_flip()

fnm <- paste(c(savedir,'app_category_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=app_category)) + geom_bar(aes(y=..count../sum(..count..)),col='coral4',fill='coral4') + 
     coord_flip() 
print(pp)
dev.off()

fnm <- paste(c(savedir,'priority_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=priority)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='chocolate') + 
   coord_flip()
print(pp)
dev.off()

fnm <- paste(c(savedir,'priorityN_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=priorityN)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='chocolate') + 
  coord_flip()
print(pp)
dev.off()

fnm <- paste(c(savedir,'res_category_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=res_category)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='deeppink') + 
  coord_flip()
print(pp)
dev.off()

fnm <- paste(c(savedir,'urgency_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=urgency)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='deeppink') + 
  coord_flip()
print(pp)
dev.off()

fnm <- paste(c(savedir,'urgencyN_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=urgencyN)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='deeppink') + 
  coord_flip()
print(pp)
dev.off()



fnm <- paste(c(savedir,'prod_line_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=prod_line)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='firebrick1') + 
  coord_flip()
print(pp)
dev.off()

fnm <- paste(c(savedir,'impact_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=impact)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='goldenrod') + 
  coord_flip()
print(pp)
dev.off()

fnm <- paste(c(savedir,'impactN_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=impactN)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='goldenrod') + 
  coord_flip()
print(pp)
dev.off()

fnm <- paste(c(savedir,'grp_level_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=grp_level)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='deepskyblue') + 
      coord_flip() 
print(pp)
dev.off()

fnm <- paste(c(savedir,'region_hori.pdf'),collapse='')
pdf(fnm)
pp <- ggplot(sdata, aes(x=region)) + geom_bar(aes(y=..count../sum(..count..)),col='black',fill='darkorange') + 
  coord_flip() 
print(pp)
dev.off()


