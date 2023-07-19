#'##############################################################################
#   Yukon River Fall Chum Salmon: Post-Season Stratified estimate 
#
#   Author:  Toshihide "Hammachan" Hamazaki
#   Date: 
#   Description
#   This program reads Pilot Station genetic and Run data and estimates 
#   Run and stratified proportion by strata and run 
#'##############################################################################
#'##############################################################################
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
#'############################################################################## 
#'############################################################################## 
#   BIG NOTE !!!!
#   THIS PROGRAM OVERWRITE ALL YEARS' CI BOUNDS
#   SET OVERWRITE FALSE IF YOU DON'T WANT TO UPDATE PREVIOUS YEARS
#'############################################################################### 
#'------------------------------------------------------------------------------
# Set Running environment-------
#'------------------------------------------------------------------------------
# Clear up existing files 
rm(list = ls(all = TRUE))
#'------------------------------------------------------------------------------
# Standard 
#'------------------------------------------------------------------------------
# Main <- 'C:/Projects/Yukon_River/Fall_Chum/MSA/' 
# fdr <- paste0(Main, 'R_functions/')  # R functions
#'------------------------------------------------------------------------------
# Projects 
#'------------------------------------------------------------------------------
fdr <- './R_functions/'
#'==============================================================================
#  1.0 Run MSA Routine ----
#       Check Yukon_Chum_MSA_RUN.R for details 
#'==============================================================================
# Is this PostSeason? 
PostSeason <- TRUE
# Overwrite historical data 
# This is necessary when you updated historical data (e.g., Pilot numbers, MSAs)
ciOverwrite <- FALSE
# ggplot ?
gg <- FALSE
# Set year 
this.year <- 2022
#'------------------------------------------------------------------------------
##  1.0 : Set MSA and Pilot data file names ---- 
#'------------------------------------------------------------------------------
# Strata info file name 
strata_file <- 'MSA_Strata.csv'
# Stock Prop info file name 
stock_prop_file <- 'MSA_prop.csv'
# Stock ID file name 
stock_id_file <- 'StockID.csv'
# Pilot Station Run  
Pilot_Run <- 'Daily_Passage_By_Species_'
# Pilot Station Var  
Pilot_Var <- 'Daily_Variance_By_Species_'
#'------------------------------------------------------------------------------
#' Output file names 
#'------------------------------------------------------------------------------ 
# Output EXCEL file name 
sumxlsx <- paste0('Yukon_Pilot_Chum_MSA_',Sys.Date(),'.xlsx')
# Output Annual Stock proportions by summer and fall:
sf_p <- 'Pilot_sfp.csv'
# Output Annual Summer vs. Fall Stock proportions by standard break:
sf_t <- 'Pilot_sft.csv'
# Output min_max file name 
min_max <- paste0('Pilot_d_min_max_',this.year,'.csv')

#'------------------------------------------------------------------------------


#'------------------------------------------------------------------------------
##  1.1: Set MSA data directory and file names ----
#'------------------------------------------------------------------------------
# Postseason data update 
source(paste0(fdr,'Yukon_Chum_MSA_STD.R'))  

EXlist <- list()   
for(i in 1:ny){
  EXlist[[i]] <- read.csv(paste0(wd_Sum,'Pilot_MSA_Sum_',years[i],'.csv'),stringsAsFactors = FALSE)
}  
test <- as.data.frame(do.call(rbind,EXlist))
st108 <- test[test$Strata==108,]
st108.w <- dcast(st108, Year~GroupName,value.var='p')

#'==============================================================================
#  2.0 Graphics---- 
#'==============================================================================
windows(record=TRUE)
#'------------------------------------------------------------------------------
##  2.1: Pilot Run vs Sampling Strata ----
#'------------------------------------------------------------------------------ 
  if(gg==TRUE){
    ## ggplot version 
    ggplot() + theme_simple() + 
      facet_wrap( ~factor(Year),scale='free') + 
      theme(axis.text.x = element_text(size=10))+
      labs(title = "Summer vs. Fall\n")+  xlab("Season Strata") +
      geom_line(data = Pilot, aes( x=Date,y=Run) ) +
      geom_vline(xintercept = rstr$Strata_Start_Date,color=4)+
      geom_vline(xintercept =(rstr$Strata_End_Date),color=2)
  }else{
    # Base plot 
# mfrow=c(5,5) can put up to 25 figures in 5x5 format.  Change it to higher  
    par(mfrow=c(5,5),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l') 
    for(i in 1:ny){
      temp <- Pilot[Pilot$Year==years[i],]
      rstr.y <- rstr[rstr$Year==years[i],]
      plot(Run~Date, type='l',data=temp,main=years[i])
      abline(v=c(rstr.y$Strata_Start_Date,max(rstr.y$Strata_End_Date)),col=4,lwd=2)
    }
    mtext(paste("Sampling Strata "), side = 3, line = 0, outer = TRUE,cex=1.5)
    mtext('Run', side = 2, line = 1, outer = TRUE,cex=1.5)
    mtext("Dates ", side = 1, line = 1, outer = TRUE,cex=1.5)
  }

#'------------------------------------------------------------------------------
##  2.2: Plot mean stock proportion by sampling strata 
#'------------------------------------------------------------------------------
# Read Pilot Stock proportion data 
Pilot.stp <- Pilot.m[,c('Year','Strata','Run', ststocks)]
# Change to Percentage 
Pilot.stp[,ststocks] <- 100*Pilot.stp[,ststocks] /Pilot.stp$Run
# Change wide to long format 
Pilot.stpl <- melt(Pilot.stp[,c('Year','Strata',ststocks)], 
                   id.vars = c('Year','Strata'), variable.name = "group", value.name = "percent") 
# Reorder by Year and stock 
Pilot.stpl <- Pilot.stpl[order(Pilot.stpl$Year,Pilot.stpl$Strata),]
# Extract stock group name  
gname <- stockID[stockID$grpID %in%ststockID,c('GroupName')] 
# Assign the name
names(gname) <- ststockID

# Base plot only: Have not created ggplot version  			
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 9.1),yaxs='i',bty='l',las=1) 
temp1 <- Pilot.stpl[Pilot.stpl$group==ststockID[1],]
plot(order(temp1$Strata),type='n', xlim=c(1,9),ylim=c(0,100), ylab='',xlab='')
for (i in 1:6){
  lines(percent~Strata, type ='o',lwd=2,col=i,pch=i,data=Pilot.stpl[Pilot.stpl$group==ststockID[i]&Pilot.stpl$Year==this.year,])
}
title(main = this.year)
legend('topright',legend = gname, col = c(1:6),  pch=c(1:6),lty=1,lwd=2, xpd = TRUE, cex = 1, seg.len=1, bty = 'n')
mtext('Stock %', side = 2, line = 1,las=0, outer = TRUE)
mtext("Sampling Strata", side = 1, line = 1, outer = TRUE)


#'------------------------------------------------------------------------------
## 2.3  Plot mean stock proportion by Standard Strata----
#'------------------------------------------------------------------------------
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

#'------------------------------------------------------------------------------
##  2.4 Plot stock proportion by standard Strata min-max ----
#'------------------------------------------------------------------------------
windows()
par(mfrow=c(3,3),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l')
fig.stock.id <- c(4,8,11,7,10,19,16,3)
for(j in 1:8){
  temp1 <- with(Pilot.d.min.max, Pilot.d.min.max[group ==fig.stock.id[j],])
  temp1 <- temp1[order(temp1$stbreak),]
  plot(Mean~stbreak,data=temp1, type='n',xlim=c(1,9),ylim=c(0,100), 
       yaxt='n',xaxt='n',main=unique(temp1$GroupName))
  axis(2, seq(0,100,20),las=2, label=FALSE)
  if(j %in% c(1,4,7)) {axis(2, seq(0,100,20),las=2, font=2)}
  axis(1,seq(1,9,1), labels = FALSE,tick=1)
  if(j %in% c(6:8)) {axis(1, seq(1,9,1), labels = stbl,tick=1)}
  with(temp1,polygon(c(stbreak,rev(stbreak)),c(Min,rev(Max)),col=tcol('gray',0),border=NA))
  lines(percent~stbreak,lwd=2,col=2, data=Pilot.sd[Pilot.sd$group ==fig.stock.id[j],])
  }

mtext(paste("Run stock proportion "), side = 3, line = 0, outer = TRUE)
mtext('Stock %', side = 2, line = 1, outer = TRUE)
mtext("Dates ", side = 1, line = 1, outer = TRUE)

#'------------------------------------------------------------------------------
##  2.5 Summer vs. Fall --------
#'------------------------------------------------------------------------------ 
  # Base plot 
  par(mfrow=c(5,5),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l') 
  for(i in 1:ny){
    temp <- with(Pilot.hsft, Pilot.hsft[Year==years[i],])
    plot(Summer~stbreak, type ='o',col=4, xlim=c(1,9),ylim=c(0,100), 
         yaxt='n',xaxt='n',lwd = 2, data=temp,main=years[i])
    lines(Fall~stbreak,type ='o',col=2,lwd = 2, data=temp)
    axis(2, seq(0,100,20),las=2, labels=NA)
    if (years[i] %in% c(1999,2005, 2010,2015, 2020)) axis(2, seq(0,100,20),las=2, font=2)
    axis(1, seq(1,9,1),labels = NA,cex.axis = 0.9)
    if (years[i] > 2015) axis(1, seq(1,9,1), labels = stbl,cex.axis = 0.9)
  }
# This year 
 plot(Summer~stbreak, type ='o',col=4, xlim=c(1,9),ylim=c(0,100), 
     yaxt='n',xaxt='n',lwd = 2, data=Pilot.sft,main=this.year)
 lines(Fall~stbreak,type ='o',col=2,lwd = 2, data=Pilot.sft)
 axis(2, seq(0,100,20),las=2, font=2)
 axis(1, seq(1,9,1), labels = stbl,cex.axis = 0.9)
 
  mtext(paste("Summer vs. Fall "), side = 3, line = 0, outer = TRUE,cex=1.5)
  mtext('Stock %', side = 2, line = 1, outer = TRUE,cex=1.5)
  mtext("Dates ", side = 1, line = 1, outer = TRUE,cex=1.5)
  

# Just for around 7/19
  par(mfrow=c(5,5),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l') 
    for(i in 1:ny){
    temp <- with(Pilot.hsft, Pilot.hsft[Year==years[i],])
    plot(Summer~stbreak, type ='o',col=4, xlim=c(4,6),ylim=c(0,100), 
         yaxt='n',xaxt='n',lwd = 2, data=temp,main=years[i])
    lines(Fall~stbreak,type ='o',col=2,lwd = 2, data=temp)
    axis(2, seq(0,100,20),las=2, labels=NA)
    if (years[i] %in% c(1999,2005, 2010,2015, 2020)) axis(2, seq(0,100,20),las=2, font=2)
    axis(1, seq(1,9,1),labels = NA,cex.axis = 0.9)
    if (years[i] > 2015) axis(1, seq(1,9,1), labels = stbl,cex.axis = 0.9)
    }
# This year 
  plot(Summer~stbreak, type ='o',col=4, xlim=c(4,6),ylim=c(0,100), 
       yaxt='n',xaxt='n',lwd = 2, data=Pilot.sft,main=this.year)
  lines(Fall~stbreak,type ='o',col=2,lwd = 2, data=Pilot.sft)
  axis(2, seq(0,100,20),las=2, font=2)
  axis(1, seq(1,9,1), labels = stbl,cex.axis = 0.9)
  
  mtext(paste("Summer vs. Fall "), side = 3, line = 0, outer = TRUE,cex=1.5)
  mtext('Stock %', side = 2, line = 1, las=0, outer = TRUE,cex=1.5)
  mtext("Dates ", side = 1, line = 1, outer = TRUE,cex=1.5)
  

#'------------------------------------------------------------------------------
##  2.6 Pie Chart ---- 
#'------------------------------------------------------------------------------ 
temp <- Pilot.fall[Pilot.fall$grpID %in% c(4,7,8,10,11,13,16),]
temp$pct <- with(temp, paste0(round(100*p),'%'))
par(mfrow=c(1,1))
pie(temp$p, labels = paste(temp$GroupName,temp$pct),col=c(2:8),main="Stock Proportion")
par(new=TRUE)
pie(temp$p,density=10, labels='',angle=c(20,90,30,10,40,0))
pie(temp$p, labels = paste(temp$GroupName,temp$pct),col=gray.colors(6),main="Stock Proportion")
