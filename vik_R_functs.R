###############################################################################################
####################    Subset wine dataset     #####################
### Delete bad data, choose top 10 varieties and regions by frequency ###
### Final data set smalldata has 10000 rows.
### It has columns : country, description, points, price, province, region_1 and variety.
###############################################################################################
library(lattice)
library(ggplot2)
library(caret)
library(stringi)
library("RColorBrewer")
library(gplots)

#Piechart of variables in varstr for a particular cluster # myicl.
make_pie <- function(mypct,varstr,tit,myicl){
  iclstr <- sprintf("%d",myicl)
  pieDF = data.frame("mycat" = c(varstr),"share" = c(mypct))
  mypie <- ggplot(pieDF, aes(x="", y=share, fill=as.factor(mycat)))+ geom_bar(stat="identity", width=1)
  mypie <- mypie + coord_polar("y", start=0) + geom_text(aes(label = paste0(share, "%")),  size=5,position = position_stack(vjust = 0.4)) 
  mypie <- mypie + labs(x = NULL, y = NULL, fill = NULL, title = paste(tit,c(" : cluster "),iclstr,collapse=''))
  mypie <- mypie + scale_fill_manual(breaks=varstr[],values=c("gold","darkgoldenrod", "chartreuse", "chartreuse3","aquamarine","aquamarine3",
                                                              "cornflowerblue", "chocolate1","brown1","brown4"))
  mypie <- mypie + theme_classic() + theme(axis.line = element_blank(),
                                           axis.text = element_blank(),
                                           axis.ticks = element_blank(),
                                           plot.title = element_text(hjust = 0.4, color = "black"))
  return(mypie)
}

#Return share (in percent) of each of the levels for myfeature for cluster #icl.
pct_share_cat <- function(plotDF,myfeature,icl,dummyarr){
  totrow <- sum(plotDF$clusnum==icl)
  nn = length(dummyarr)  
  pctg <- vector(length=nn)
  for (i in 1:nn) {
    pctg[i] <- round(sum(as.numeric(plotDF$clusnum)==icl & myfeature==dummyarr[i])/(totrow)*100)
  }
  return(pctg)
}


#Return share (in percent) of each of the numerical levels for myfeature for cluster #icl.
pct_share_num <- function(plotDF,myfeature,icl,dummyarr){
  totrow <- sum(plotDF$clusnum==icl)
  nn = length(dummyarr)-1
  pctg <- vector(length=nn)
  
  for (j in 1:nn) {
    cc <- sum(plotDF$clusnum==icl &  (myfeature>=dummyarr[j] & myfeature<dummyarr[j+1]))
    pctg[j] <- round(cc/(totrow)*100)
  }
  return(pctg)
}

