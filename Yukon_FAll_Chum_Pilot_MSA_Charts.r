################################################################################
#   Yukon River Fall Chum Salmon Pilot Station Stratified estimate 
#
#   Author:  Toshihide "Hammachan" Hamazaki
#   Date: 
#   Description
#   This program reads Pilot Station genetic and Run data and estimates 
#   Run and stratified proportion by strata and run 
#-------------------------------------------------------------------------------
#   Group ID and description (P) indicates Primary group directly identified by 
#   GSI
# 1 Upper Canada (1999-present): 3 White + 6 Teslin
#	2 Total Summer (1999-present): 
#		(1999-2002)  4 Lower Summer + 5 Middle Summer  
#		(2004-2007)  23 Lower Summer+UpperKoy+Main + 8 Tanana Summer  
#		(2007-present)  4 Lower Summer + 5 Middle Summer  
#	3 (P) White UC (1999-present): 	 
#	4 (P) Lower Summer (1999-2002, 2008-present)
#   	(2004-2007) (P) Lower Summer+UpperKoy+Main -> Move to 23 		
#	5 (P) Total Middle(1999-2002): 
#		(2008-present) 7 Uppkoy+Main + 8 Tanana summer 
#	6 (P) Teslin UC (1999- present)   
#	7 (P) UppKoy+Main(2008- present)
#	8 (P) Tanana Summer (2004- present)
# 9 Total Fall (1999-present)
#		(1999-2002) 1 Upper Canada + 10 Tanana Fall + 22 Border US&CA
#		(2004-present):  15 Fall US + 19 Total Canada  
#	10 (P) Tanana Fall (2004-present):  
#		(1999-2002)  20 Toklat + 21 Upper Tanana
#	11 (P) Border US (2004- present):
#	12 Border CA (2004-present): 13 Porcupine + 14 Mainstem CA 
#	13 (P) Porcupine CA (2004- present)
#	14 (P) Mainstem CA (2004- present) 
#	15 Fall US (2004-present)
#		 (2004- present): 10 Tanana Fall + 11 Border US
#	16 Mainstem+Upper CA (2004-present): 1 Upper Canada + 14 Mainsten CA
#	17 Border US+Canada (1999-present): 
#		 (1999-2002) 1 Upper Canada + 22 Border US&CA 
#		 (2004-present) 11 Border US + 19 Total Canada
#	18 Total USA (2004-present): 2 Total Summer + 15 Fall US
#	19 Total Canada (2004-present): 1 Upper Canada + 12 Border CA 
# 20 (P) Toklat (1999-2002):  
#	21 (P) Upper Tanana (1999-2002):   
#	22 (P) Border US+ Border CA (1999-2002): 
# 23 (P) Lower Summer+UpperKoy+Mai (2004-2007) moved from 4 
#		 (2008+present)  4 Lower Summer +7 UpperKoy+Mai  
################################################################################
# Output Strata
# 1 - 10: Survey Sample strata
# Strata 100 both summer and fall seasons (beware of stratum date differences in early years 1999-2007)
# Strata 101 summer season only (thru July 18, data complete 2008 on)
# Strata 102 fall season only (July 19 to end, data complete 2004 on)

# Strata 103 proportions of only summer stocks in both seasons (Total summer stock passage/total passage)
# Strata 103 would be strata 100   
# 1) grpID=4 for Lower summer divided into grpID=2 Total Summer.
# 2) grpID=8 for Tanana summer divided into grpID=2 Total Summer.
# 3) grpID=5 for Middle divided into grpID=2 Total Summer.

# Strata 104 proportions of only summer in summer season  (Total summer passage/total passage)
# Strata 104 would be strata 101
# 1) grpID=4 for Lower summer divided into grpID=2 Total Summer.
# 2) grpID=8 for Tanana summer divided into grpID=2 Total Summer.
# 3) grpID=5 for Middle divided into grpID=2 Total Summer.

# Strata 105 proportions of only summer in fall season  (summer stock passage in fall season/total fall passage)
# Strata 105 would be strata 102
# 1) grpID=4 for Lower summer divided into grpID=2 Total Summer.
# 2) grpID=8 for Tanana summer divided into grpID=2 Total Summer.
# 3) grpID=5 for Middle divided into grpID=2 Total Summer.

# Strata 106 proportions of only fall stocks in both seasons
# Strata 106 would be strata 100
# 1) grpID=10 for Tanana fall divided into grpID=9 Total Fall.
# 2) grpID=11 for BorderUS divided into grpID=9 Total Fall.
# 3) grpID=19 for Canada divided into grpID=9 Total Fall.

# Strata 107 proportion of only fall in summer season
# Strata 107 would be strata 101
# 1) grpID=10 for Tanana fall divided into grpID=9 Total Fall.
# 2) grpID=11 for BorderUS divided into grpID=9 Total Fall.
# 3) grpID=19 for Canada divided into grpID=9 Total Fall.

# Strata 108 proportion of only fall in fall season
# Strata 108 would be strata 102
# 1) grpID=10 for Tanana fall divided into grpID=9 Total Fall.
# 2) grpID=11 for BorderUS divided into grpID=9 Total Fall.
# 3) grpID=16 for Mainstem+Upper CA divided into grpID=9 Total Fall.
# 4) grpID=13 for Porcupine divided into grpID=9 Total Fall.
# 5) grpID=19 for Canada divided into grpID=9 Total Fall.
################################################################################
################################################################################
#  Changes in Primary stock groups
#  1999 - 2002: 3, 4, 5, 6. 20, 21, 22
#  2004 - 2007: 3, 4, 6, 8, 10, 11, 13, 14
#  2008 - present: 3, 4, 6, 7, 8, 10, 11, 13, 14
################################################################################
#===============================================================================
#  1.0: Clear Memory and Set Working Environment 
#===============================================================================
# Clear up existing files 
rm(list=ls(all=TRUE))
# Add packages needed 
library(openxlsx)   # Used to create EXCEL output file
library(reshape2)   # Used to transpose data file 
library(ggplot2)    # Used for ggplot graphics 
library(lemon)      # Used for ggplot: better figures (facet_rep_wrap)
#palette('Okabe-Ito')  # Change color palette color blinds friendly
options(scipen=999)   # Prevent R using scientific notation xxx e-yyy
# Functions Source
#-------------------------------------------------------------------------------
# Setup directories 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Standard 
#-------------------------------------------------------------------------------
# Main <- 'C:/Projects/Yukon_River/Fall_Chum/MSA/' 
# fdr <- paste0(Main, 'R_functions/')  # R functions
# wd_MSA <- paste0(Main,'data/MSA_data/') # MSA data
# wd_Plt <- paste0(Main,'/data/Pilot_data/') #Pilot data
# wd_Sum <- paste0(Main,'/data/Summary_data/') #Summary data used for figures
# wd_Obs <- paste0(Main,'/data/Obs_data/') #data used for figures comparison 
#-------------------------------------------------------------------------------
# Rprojects 
#-------------------------------------------------------------------------------
fdr <- './R_functions/'
wd_MSA <- './data/MSA_data/'
wd_Plt <- './data/Pilot_data/'
wd_Sum <- './data/Summary_data/'
wd_Obs <- './data/Obs_data/'
----------------------------------------------------------------
# Import Source files 
#-------------------------------------------------------------------------------
source(paste0(fdr,'Yukon_Chum_MSA_functions.R'))  # Function sets used for MSA analyses
source(paste0(fdr,'ggplot_theme.R'))  # ggplot themes 
#===============================================================================
#  2.0: Read summary data
#===============================================================================
#-------------------------------------------------------------------------------
#  2.1: Read Summer vs. Fall % data
#-------------------------------------------------------------------------------
# Read MSA Strata data
Pilot.sft <- read.csv(paste0(wd_Sum,'Pilot_sft.csv'),stringsAsFactors = FALSE)
# Find the number of years in the data 
years <- unique(Pilot.sft$Year)
years <- years[order(years)]
# number of years
ny <- length(years)
#-------------------------------------------------------------------------------
# 2.2: Read Pilot summary CSV data 
#-------------------------------------------------------------------------------
mlist <- list()
for(i in 1:ny){
  mlist[[i]] <- read.csv(paste0(wd_Sum,'Pilot_MSA_Sum_',years[i],'.csv'),stringsAsFactors = FALSE)
}
# Change to data.frame 
Pilot.all <- as.data.frame(do.call(rbind, mlist)) 
# Isolate data 
Pilot.total <- Pilot.all[Pilot.all$Strata==100,]
Pilot.summer <- Pilot.all[Pilot.all$Strata==101,]
Pilot.fall <- Pilot.all[Pilot.all$Strata==102,]


#===============================================================================
# 3.0: Figures 
#===============================================================================
windows(record=TRUE)
#-------------------------------------------------------------------------------
#  3.1: Create Annual passage estimate for selected groups
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#  Annual Trends Comparison with Obs data 
#  grpID 10 = Tanana
#  grpID 11 = Border US
#  grpID 13 = Porcupine-Fishing Branch
#  grpID 16 = Eagle
#------------------------------------------------------------------------------- 
# Read Obs_data
obs <- read.csv(paste0(wd_Obs,'Fall_Chum_esc.csv'),stringsAsFactors = FALSE)

error.bar <- function(x, upper, lower, length=0,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}
# Extract only data needed 
windows(record = TRUE)
par(mfrow=c(4, 1), mar=c(4, 3, 3, 8))
grp <- c(10,11,13,16)
grp.n <- c('Tanana','Border US','Porcupine','Eagle')
for(i in 1:4){
msa <- Pilot.fall[Pilot.fall$grpID==grp[i],c('Year','mean','UCI.m','LCI.m')]
msa <- merge(obs[,c(1,i+1)],msa,by='Year')
names(msa) <- c('Year','Observed','MSA','UCI','LCI')
# transpose
temp <- t(msa)
bars <-barplot(temp[c(2,3),],names.arg=temp[1,],beside=TRUE, axis.lty=1,
        ylim =c(0,max(temp[c(2:5),],na.rm=TRUE)),       
        main=grp.n[i],legend.text=TRUE,args.legend=list(x='topright',inset=c(-0.15,0)))
temp1 <- temp[c(3,4,5),]
temp1[1,]<-NA
error.bar(bars,temp1[c(1,2),],temp1[c(1,3),])
}


if(gg==TRUE){
  for(i in 1:4){
    msa <- Pilot.fall[Pilot.fall$grpID==grp[i],c('Year','mean','UCI.m','LCI.m')]
    msa <- merge(obs[,c(1,i+1)],msa,by='Year')
    names(msa) <- c('Year','Observed','MSA','UCI','LCI')
    # transpose
    df <- melt(msa[,c(1:3)],id='Year')
    temp <- msa[,c('Year','UCI','LCI')]
    temp$variable <- 'MSA'
    df <- merge(df,temp,by=c('Year','variable'),all=TRUE)
    ggplot() + theme_simple() +
    geom_bar(data=df,aes(fill=variable,x=Year,y=value),position=position_dodge(),stat='identity')
    geom_errorbar(data=df,aes(x=Year,ymin=LCI,ymax=UCI),width=.2,na.rm=TRUE,
                  position=position_dodge(.9))
    
  }
} 
#-------------------------------------------------------------------------------
#  3.2 Pie Chart 
#=------------------------------------------------------------------------------ 
temp <- Pilot.fall[Pilot.fall$Year ==2020 & Pilot.fall$grpID %in% c(4,7,8,10,11,13,16),]
temp$pct <- with(temp, paste0(round(100*p),'%'))
temp$lab.ypos = with(temp, cumsum(p) - 0.5*p)
ggplot(temp,aes(x='',y=p,fill=as.character(o)))+ 
geom_bar(width = 1, stat = "identity", color = "white")+
coord_polar("y", start = 0)+
geom_text(aes(y = lab.ypos, label = pct), color = "white")+
theme_void()   

pie(temp$p, labels = temp$GroupName, main="Stock Proportion")


#-------------------------------------------------------------------------------
#  3.2 Summer vs. Fall
#=------------------------------------------------------------------------------ 
  if(gg==TRUE){
    # ggplot2 	
    windows(record = TRUE)
    #  Create long data 
    Pilot.sfl <- melt(Pilot.sft,id.vars=c('Year','stbreak'), 
                      measure.vars=c("Summer", "Fall"),
                      variable.name='SF', value.name='percent')
    Pilot.sfl2 <- dcast(Pilot.sfl,Year+SF ~ stbreak)
    Pilot.sfl3 <- melt(Pilot.sfl2,id.vars=c('Year','SF'), 
                       variable.name='stbreak', value.name='percent')
    # ggplot
    ggplot() + theme_simple() + 
      facet_rep_wrap( ~factor(Year)) +
      #   facet_wrap( ~factor(Year),scale='free') + 
      scale_x_continuous( breaks=c( 1:9 ),labels=stbl) + ylim(0, 100)+
      theme(axis.text.x = element_text(size=7))+
      labs(title = "Summer vs. Fall\n")+  xlab("Season Strata") +
      geom_line(data = Pilot.sfl3, aes( x=as.numeric(stbreak),y=percent,color=SF ) )+
      geom_point(data = Pilot.sfl3, aes( x=as.numeric(stbreak),y=percent,color=SF ),size=2)
  } else {  

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
  }

