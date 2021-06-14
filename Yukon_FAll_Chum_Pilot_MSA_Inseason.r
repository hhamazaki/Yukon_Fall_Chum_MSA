################################################################################
#   Yukon River Fall Chum Salmon Pilot Station Inseason Stratified estimate 
#
#   Author:  Toshihide "Hammachan" Hamazaki
#   Date: 
#   Description
#   This program reads Pilot Station genetic and Run data and estimates 
#   Run and stratified proportion by strata and run 
################################################################################
################################################################################
#   Model running and file Setting Instructions 
#   Step 1  Update data in data folder
#   MSA_data folder 
#   1. MSA_Strata.csv:  Include sampling strata, dates, and sample size 
#   2. MSA_prop.csv:  Include Stock prop by strata groupID 
#   3. StockID.csv:  Include groupID, stock name, display order 
#
#   Pilot_data folder 
#   4. Daily_Passage_By_Species_yyyy.csv:  Daily Pilot Passage by species 
#   5. Daily_Variance_By_Species_yyyy.csv:  Daily Pilot Passage Variance by species 
#   Notes:
#   Pilot passage and variance files are located in Pilot folder 
#   Other files are located in main director
################################################################################   
#-------------------------------------------------------------------------------
# Setup file directories 
#-------------------------------------------------------------------------------
# Clear up existing files 
rm(list = ls(all = TRUE))
#-------------------------------------------------------------------------------
# Standard 
#-------------------------------------------------------------------------------
# Main <- 'C:/Projects/Yukon_River/Fall_Chum/MSA/' 
# fdr <- paste0(Main, 'R_functions/')  # R functions
#-------------------------------------------------------------------------------
# Rprojects 
#-------------------------------------------------------------------------------
fdr <- './R_functions/'
#===============================================================================
#  1.0: Run MSA
#===============================================================================
inSeason <- TRUE 
# Set year 
this.year <- 2020
#-------------------------------------------------------------------------------
#  1.1: Set MSA data directory and file names 
#-------------------------------------------------------------------------------

source(paste0(fdr,'Yukon_Chum_MSA_RUN.R'))  
stbreak <- stb(this.year)
Pilot.total <- mlist[mlist$Strata==100,]
Pilot.summer <-mlist[mlist$Strata==101,]
Pilot.fall <- mlist[mlist$Strata==102,]
#-------------------------------------------------------------------------------
#  1.1: Set MSA data directory and file names 
#-------------------------------------------------------------------------------
#===============================================================================
#  2.0: Read summary data
#===============================================================================
#-------------------------------------------------------------------------------
#  2.1: Read Summer vs. Fall % data
#-------------------------------------------------------------------------------
# Read MSA Strata data
Pilot.d.min.max <- read.csv(paste0(wd_Sum,'Pilot_d_min_max.csv'),stringsAsFactors = FALSE)

# Read MSA Strata data
Pilot.sft <- read.csv(paste0(wd_Sum,'Pilot_sft.csv'),stringsAsFactors = FALSE)
# Find the number of years in the data 
years <- unique(Pilot.sft$Year)
years <- years[order(years)]
# number of years
ny <- length(years)
#===============================================================================
#  Graphics 
#===============================================================================
#-------------------------------------------------------------------------------
#  Pilot Run vs Sampling Strata 
#=------------------------------------------------------------------------------ 
if(fig1==TRUE){
windows(record=TRUE)
par(mfrow=c(1,1),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l',las=1)
  rstr.y <- rstr[rstr$Year==this.year,]
  xlims<-c(min(stbreak),max(stbreak))
  plot(Run~Date, type='h',lwd=4,col='gray',data=Pilot,xlim=xlims,xaxt='n')
  axis.Date(1, at = stbreak,format= "%b%d",cex=0.6)
  abline(v=c(rstr.y$Strata_Start_Date),col=4,lwd=2)
mtext(paste("Sampling Strata"), side = 3, line = 0, outer = TRUE,cex=1.5)
mtext('Pilot Station Run', side = 2, line = 1.5, las=0, outer = TRUE,cex=1.5)
mtext("Date", side = 1, line = 1, outer = TRUE,cex=1.5)
}

#-------------------------------------------------------------------------------
#  Plot mean stock proportion by sampling strata 
#-------------------------------------------------------------------------------
Pilot.stp <- Pilot.m[,c('Year','Strata','Run', ststocks)]
Pilot.stp[,ststocks] <- 100*Pilot.stp[,ststocks] /Pilot.stp$Run
Pilot.stpl <- melt(Pilot.stp[,c('Year','Strata',ststocks)], 
                   id.vars = c('Year','Strata'), variable.name = "group", value.name = "percent") 
Pilot.stpl <- Pilot.stpl[order(Pilot.stpl$Year,Pilot.stpl$Strata),]
gname <- stockID[stockID$grpID %in%ststockID,c('GroupName')]      
names(gname) <- ststockID
# Base plot 			
windows()
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 9.1),yaxs='i',bty='l',las=1) 
temp1 <- Pilot.stpl[Pilot.stpl$group==ststockID[1],]
plot(order(temp1$Strata),type='n', xlim=c(1,9),ylim=c(0,100), ylab='',xlab='')
for (i in 1:6){
  lines(percent~Strata, type ='o',lwd=2,col=i,pch=i,data=Pilot.stpl[Pilot.stpl$group==ststockID[i],])
}
title(main = this.year)
legend('topright',legend = gname, col = c(1:6),  pch=c(1:6),lty=1,lwd=2, xpd = TRUE, cex = 1, seg.len=1, bty = 'n')
mtext('Stock %', side = 2, line = 1,las=0, outer = TRUE)
mtext("Sampling Strata", side = 1, line = 1, outer = TRUE)


#-------------------------------------------------------------------------------
#  Plot mean stock proportion by Standard Strata
#-------------------------------------------------------------------------------
gname <- stockID[stockID$grpID %in%ststockID,c('GroupName')]      
names(gname) <- ststockID

par(mfrow=c(1,1),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l',las=1)
temp1 <- with(Pilot.d.min.max, Pilot.d.min.max[group ==ststockID[1],])
plot(order(temp1$stbreak),type='n', xlim=c(1,9),ylim=c(0,100), ylab='',xlab='',
     yaxt='n',xaxt='n')
axis(2, seq(0,100,20),las=2, font=2 )
axis(1, seq(1,9,1), labels = stbl,tick=1)
for(i in 1:6){
  temp <- with(Pilot.d.min.max, Pilot.d.min.max[group ==ststockID[i],])  
  temp <- temp[order(temp$stbreak),]
  with(temp, lines(stbreak,Mean, type='o',lwd=2,col=i,pch=i))
}
txt <- c(gname)
legend('topright',legend=txt,col=c(1:6),pch=c(1:6),lty=1,lwd=2,bty='n')
mtext(paste("Run stock proportion "), side = 3, line = 0, outer = TRUE)
mtext('Stock %', side = 2, line = 1,las=0, outer = TRUE)
mtext("Dates ", side = 1, line = 1, outer = TRUE)

#-------------------------------------------------------------------------------
#  Plot stock proportion by standard Strata min-max
#-------------------------------------------------------------------------------
windows()
par(mfrow=c(3,2),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l')
gname <- stockID[stockID$grpID %in%ststockID,'GroupName']      
for(j in 1:6){
  temp1 <- with(Pilot.d.min.max, Pilot.d.min.max[group ==ststockID[j],])
  temp1 <- temp1[order(temp1$stbreak),]
  plot(Mean~stbreak,data=temp1, type='n',xlim=c(1,9),ylim=c(0,100), 
       yaxt='n',xaxt='n',main=gname[j])
  axis(2, seq(0,100,20),las=2, label=FALSE)
  if(j %in% c(1,3,5)) {axis(2, seq(0,100,20),las=2, font=2)}
  axis(1,seq(1,9,1), labels = FALSE,tick=1)
  if(j %in% c(5,6)) {axis(1, seq(1,9,1), labels = stbl,tick=1)}
  with(temp1,polygon(c(stbreak,rev(stbreak)),c(Min,rev(Max)),col=tcol('gray',0),border=NA))
  lines(percent~stbreak,lwd=2,col=2, data=Pilot.sd[Pilot.sd$ group ==ststockID[j],])
  }

mtext(paste("Run stock proportion "), side = 3, line = 0, outer = TRUE)
mtext('Stock %', side = 2, line = 1, outer = TRUE)
mtext("Dates ", side = 1, line = 1, outer = TRUE)

#-------------------------------------------------------------------------------
#  3.2 Summer vs. Fall
#------------------------------------------------------------------------------- 
  # Base plot 
  par(mfrow=c(5,5),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l') 
  for(i in 1:ny){
    temp <- with(Pilot.sft, Pilot.sft[Year==years[i],])
    plot(Summer~stbreak, type ='o',col=4, xlim=c(1,9),ylim=c(0,100), 
         yaxt='n',xaxt='n',lwd = 2, data=temp,main=years[i])
    lines(Fall~stbreak,type ='o',col=2,lwd = 2, data=temp)
    axis(2, seq(0,100,20),las=2, labels=NA)
    if (years[i] %in% c(1999,2005, 2010,2015, 2020)) axis(2, seq(0,100,20),las=2, font=2)
    axis(1, seq(1,9,1),labels = NA,cex.axis = 0.9)
    if (years[i] > 2015) axis(1, seq(1,9,1), labels = stbl,cex.axis = 0.9)
  }
  mtext(paste("Summer vs. Fall "), side = 3, line = 0, outer = TRUE,cex=1.5)
  mtext('Stock %', side = 2, line = 1, outer = TRUE,cex=1.5)
  mtext("Dates ", side = 1, line = 1, outer = TRUE,cex=1.5)
  

# Just for around 7/19
  par(mfrow=c(5,5),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l') 
    for(i in 1:ny){
    temp <- with(Pilot.sft, Pilot.sft[Year==years[i],])
    plot(Summer~stbreak, type ='o',col=4, xlim=c(4,6),ylim=c(0,100), 
         yaxt='n',xaxt='n',lwd = 2, data=temp,main=years[i])
    lines(Fall~stbreak,type ='o',col=2,lwd = 2, data=temp)
    axis(2, seq(0,100,20),las=2, labels=NA)
    if (years[i] %in% c(1999,2005, 2010,2015, 2020)) axis(2, seq(0,100,20),las=2, font=2)
    axis(1, seq(1,9,1),labels = NA,cex.axis = 0.9)
    if (years[i] > 2015) axis(1, seq(1,9,1), labels = stbl,cex.axis = 0.9)
  }
  mtext(paste("Summer vs. Fall "), side = 3, line = 0, outer = TRUE,cex=1.5)
  mtext('Stock %', side = 2, line = 1, outer = TRUE,cex=1.5)
  mtext("Dates ", side = 1, line = 1, outer = TRUE,cex=1.5)
  

#-------------------------------------------------------------------------------
#  3.2 Pie Chart 
#------------------------------------------------------------------------------- 
temp <- Pilot.fall[Pilot.fall$grpID %in% c(4,7,8,10,11,13,16),]
temp$pct <- with(temp, paste0(round(100*p),'%'))
par(mfrow=c(1,1))
pie(temp$p, labels = paste(temp$GroupName,temp$pct),col=c(2:8),main="Stock Proportion")
par(new=TRUE)
pie(temp$p,density=10, labels='',angle=c(20,90,30,10,40,0))
pie(temp$p, labels = paste(temp$GroupName,temp$pct),col=gray.colors(6),main="Stock Proportion")
