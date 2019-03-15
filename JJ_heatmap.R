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
#install.packages("Rtsne")
#library(Rtsne)
library(gplots)
library(abind)

source("C:\\Users\\khade\\MachineLearning\\JJproject\\Rcode\\JJ_clean_final.R")
source("C:\\Users\\khade\\MachineLearning\\JJproject\\Rcode\\vik_R_functs.R")

#17410
nrow(sdata)

#670
sum(sdata$levelN == 3)
#13385
sum(sdata$levelN == 2)
#3355
sum(sdata$levelN == 1)

#Do subsampling
set.seed(123)
indx2 <- sample(which(sdata$levelN == 2), round(0.2*sum(sdata$levelN == 2)),replace=FALSE)
set.seed(123)
indx1 <- sample(which(sdata$levelN == 1), round(0.8*sum(sdata$levelN == 1)),replace=FALSE)
underDF <- rbind(sdata[indx1,], sdata[indx2,])
underDF <- rbind(underDF,sdata[which(sdata$levelN == 3),])

#6031
nrow(underDF)

sum(sdata$ndays<0)

#savedir <- 'C:\\Users\\khade\\MachineLearning\\JJproject\\figs\\heatmap\\'
#fnm <- paste(c(savedir,'ndays_underDF.pdf'),collapse='')
#pdf(fnm)
#pp <- ggplot(underDF, aes(x=ndays)) + geom_bar(aes(y=..count../sum(..count..)),col='red',fill='red') +
#  coord_cartesian(xlim=c(0,20))
#print(pp)
#ev.off()

#Define a new DF for convenience
nr <- nrow(underDF)
smallDF <- underDF[,c('levelN','priorityN','impactN','app_category','res_category','region','ndays')]
nrs <- nrow(smallDF)

kmax <- 10

#Read in previously saved PAM object.
pamobj <- readRDS('C:\\Users\\khade\\MachineLearning\\JJproject\\Rcode\\PAMobjects\\pamobj_kmax10_under_prodline.rds')

kmax <- 10
clusvec <- vector(length=kmax-1)
silvec <- vector(length=kmax-1)
avgdiss <- vector(length=kmax-1)
sep  <- vector(length=kmax-1)

for (i in 2:kmax){
  clusvec[i-1] <- c(i)           
  silvec[i-1] <- pamobj[[i-1]]$silinfo$avg.width
  avgdiss[i-1] <- mean(pamobj[[i-1]]$clusinfo[,3])
  sep[i-1] <- mean(pamobj[[i-1]]$clusinfo[,5])
}

plot(clusvec,avgdiss,type='b',xlab='# of clusters',ylab='Average dissimilarity',main='')  
plot(clusvec,sep,type='b')
plot(clusvec,silvec,type='b')

#choose k
optpam <- pamobj[[9]]

#----analyse clusters
rm(pdDF)
plDF <- data.frame(smallDF)
plDF[,"clusnum"] <- optpam$clustering
plDF$clusnum <- factor(plDF$clusnum)

#icl = 1
ndaysarr <- c(0,1,2,3,4,5,1000)
nd = length(ndaysarr)-1
#pctdys <- pct_share_num(plDF,plDF$ndays,icl,ndaysarr)
#make_pie(pctdys,ndaysarr[1:6],'ndays',icl)


featurs = c('levelN','priorityN','impactN','region','app_category','res_category','ndays')
nf = 0
for (f in featurs){
 print(f)  
 #print(length(levels(as.factor(smallDF[[f]])  )))
 #print(levels(as.factor(smallDF[[f]])))
 if (f != 'ndays' ) {
 nf = nf + length(levels(as.factor(smallDF[[f]])  ))
 }
 else
 {
   nf = nf + nd
 } 
 }

 print(nf)
 
kmax = 10
rm(vikmat)
vikmat = matrix(,nrow=nf,ncol=kmax)

for (icl in 1:kmax) {
  levstr <- c(1,2,3)
  vikmat[1:3,icl]   = pct_share_cat(plDF,plDF$levelN,icl,levstr)
  vikmat[4:6,icl]   = pct_share_cat(plDF,plDF$priorityN,icl,levstr)
  vikmat[7:9,icl]   = pct_share_cat(plDF,plDF$impactN,icl,levstr)
  vikmat[10:14,icl] = pct_share_cat(plDF,plDF$region,icl,levels(plDF$region))
  vikmat[15:19,icl] = pct_share_cat(plDF,plDF$app_category,icl,levels(plDF$app_category))
  vikmat[20:25,icl] = pct_share_cat(plDF,plDF$res_category,icl,levels(plDF$res_category))
  vikmat[26:31,icl] = pct_share_num(plDF,plDF$ndays,icl,ndaysarr)
}

rownames(vikmat) <- c('L1','L2','L3','P1','P2','P3','I1','I2','I3','R01','R07','R16','R24','R28','App','Help','Moni','Serv','Soft','Conf','Conn', 'Data','JobF','UserA', 'UserT','0','1','2','3','4','5')
colnames(vikmat) <- c('clus1','clus2','clus3','clus4','clus5','clus6','clus7','clus8','clus9','clus10')

breaks=c(0,10,20,30,40,50,60,70,80,90,100)
color=c("burlywood1","burlywood3","cyan","cyan4","chartreuse","chartreuse4","hotpink","hotpink4","red","darkred")

pheatmap::pheatmap(vikmat,cluster_rows=FALSE, cluster_cols=FALSE,scale='none',breaks=breaks,color=color,main='k=10')

 
#savedir <- 'C:\\Users\\khade\\MachineLearning\\JJproject\\figs\\undersample\\k10\\'
#('levelN','priorityN','impactN','app_category','res_category','region','ndays')
#levstr <- c(1,2,3)
#pdf(paste(c(savedir,'levelN_',iclstr,'.pdf'),collapse=''))
#pct <- pct_share_cat(plDF,plDF$levelN,icl,levstr)
#make_pie(pct,levstr,'levelN',icl)


  
  
  