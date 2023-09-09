#'##############################################################################
#   Yukon River Fall Chum Salmon Pilot Station Stratified estimate 
#   Code name:  Yukon_Chum_MSA_STD.R
#   Author:  Toshihide "Hammachan" Hamazaki
#   Date: 
#   Description
#   This program reads Pilot Station Run and variance data, genetic composition 
#   data and create stratified estimates 
#   This sets MSA conditions
#   
#'##############################################################################
#'-------------------------------------------------------------------------------
#  Group ID and description ---- 
#  (P) indicates Primary group directly identified by 
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
#'##############################################################################
# Output Strata ----
# 1 - 10: Survey Sample strata
# Strata 100 both summer and fall seasons (beware of stratum date differences in early years 1999-2007)
# Strata 101 summer season only (through July 18, data complete 2008 on)
# Strata 102 fall season only (July 19 to end, data complete 2004 on)

# Strata 103 proportions of summer stocks in both seasons (Total summer stock passage/total passage)
# Strata 103 would be strata 100   
# 1) grpID=4 for Lower summer divided by grpID=2 Total Summer.
# 2) grpID=8 for Tanana summer divided by grpID=2 Total Summer.
# 3) grpID=5 for Middle divided by grpID=2 Total Summer.

# Strata 104 proportions of only summer in summer season(Total summer passage/total passage)

# Strata 104 would be strata 101
# 1) grpID=4 for Lower summer divided by grpID=2 Total Summer.
# 2) grpID=8 for Tanana summer divided by grpID=2 Total Summer.
# 3) grpID=5 for Middle divided by grpID=2 Total Summer.

# Strata 105 proportions of summer in fall season (summer stock passage in fall season/total fall passage)
# Strata 105 would be strata 102
# 1) grpID=4 for Lower summer divided by grpID=2 Total Summer.
# 2) grpID=8 for Tanana summer divided by grpID=2 Total Summer.
# 3) grpID=5 for Middle divided by grpID=2 Total Summer.

# Strata 106 proportions of only fall stocks in both seasons
# Strata 106 would be strata 100
# 1) grpID=10 for Tanana fall divided by grpID=9 Total Fall.
# 2) grpID=11 for BorderUS divided by grpID=9 Total Fall.
# 3) grpID=19 for Canada divided by grpID=9 Total Fall.

# Strata 107 proportion of fall stock in summer season (Strata 101) 
# 1) grpID=10 for Tanana fall divided by grpID=9 Total Fall.
# 2) grpID=11 for BorderUS divided by grpID=9 Total Fall.
# 3) grpID=19 for Canada divided by grpID=9 Total Fall.

# Strata 108 proportion of only fall in fall season (Strata 102)
# 1) grpID=10 for Tanana fall divided by grpID=9 Total Fall.
# 2) grpID=11 for BorderUS divided by grpID=9 Total Fall.
# 3) grpID=16 for Mainstem+Upper CA divided by grpID=9 Total Fall.
# 4) grpID=13 for Porcupine divided by grpID=9 Total Fall.
# 5) grpID=19 for Canada divided by grpID=9 Total Fall.
#'##############################################################################
#'##############################################################################
#  Changes in Primary stock groups ----
#  1999 - 2002: 3, 4, 5, 6. 20, 21, 22
#  2004 - 2007: 3, 4, 6, 8, 10, 11, 13, 14
#  2008 - present: 3, 4, 6, 7, 8, 10, 11, 13, 14
#'#############################################################################
#'==============================================================================
#  1.0: Clear Memory and Set Working Environment ----
#'==============================================================================
# Add packages needed 
library(openxlsx)   # Used to create EXCEL output file
library(reshape2)   # Used to transpose data file 
#library(ggplot2)    # Used for ggplot graphics 
#library(lemon)      # Used for ggplot: better figures (facet_rep_wrap)
#palette('Okabe-Ito')  # Change color palette color blinds friendly
options(scipen=999)   # Prevent R using scientific notation xxx e-yyy
# Functions Source
#'-------------------------------------------------------------------------------
## 1.1 Setup file directories ----
#'------------------------------------------------------------------------------
#'------------------------------------------------------------------------------
# Standard 
#'------------------------------------------------------------------------------
# Main <- file.path('C:','Projects','Yukon_River','Fall_Chum','MSA') 
# fdr <-  file.path(Main, 'R_functions')  # R functions
# wd_MSA <- file.path(Main,'data/MSA_data') # MSA data
# wd_Plt <- file.path(Main,'data','Pilot_data') #Pilot data
# wd_Sum <- file.path(Main,'data','Summary_data') #Summary data used for figures
#'------------------------------------------------------------------------------
## 1.2  R projects folder setting ----
#'------------------------------------------------------------------------------
fdr <- './R_functions'        
wd_MSA <- './data/MSA_data'
wd_Plt <- './data/Pilot_data'
wd_Sum <- './data/Summary_data'
#wd_Ins <- './data/Inseason'
#'------------------------------------------------------------------------------
## 1.3 Import Source files ---- 
#'------------------------------------------------------------------------------
source(file.path(fdr,'Yukon_Chum_MSA_functions.R'))  # Function sets used for MSA analyses
source(file.path(fdr,'Yukon_Chum_MSA_ReadData.R'))  # Function sets used for MSA analyses
# source(file.path(fdr,'ggplot_theme.R'))  # ggplot themes 
#'------------------------------------------------------------------------------
##  1.4: Set MSA and Pilot data file names ---- 
#'------------------------------------------------------------------------------
# Strata info file name 
# Stock ID file name 
stock_id_file <- 'StockID.csv'
#'------------------------------------------------------------------------------

#'------------------------------------------------------------------------------
##  1.5: Set Standard Stock Groups ---- 
#'------------------------------------------------------------------------------
# Standard Stock groups 	
# Primary Stock group directly estimated from GSI 
#  1999 - 2002: 3, 4, 5, 6. 20, 21, 22
#  2004 - 2007: 3, 4, 6, 8, 10, 11, 13, 14
#  2008 - present: 3, 4, 6, 7, 8, 10, 11, 13, 14
stgrp99 <- c(3, 4, 5, 6, 20, 21, 22)
stgrp04 <- c(3, 4, 6, 8, 10, 11, 13, 14)
stgrp08 <- c(3, 4, 6, 7, 8, 10, 11, 13, 14)
# srgrpID:  the primary stock groups used all years
stgrpID <- unique(c(stgrp99,stgrp04,stgrp08))
# stgrpIDn:  used to select primary stock group columns 
stgrpIDn <- as.character(stgrpID)

#'------------------------------------------------------------------------------
##  1.3: Specify Simulation and outputs ----
#'------------------------------------------------------------------------------
# Set the number of simulation replicates: default 10000
nrep <- 10000
# % CI range
ci <- 90
#'==============================================================================
#  2.0: Data Read and summarize ---- 
#'==============================================================================
#'-------------------------------------------------------------------------------
## Read MSA data data ----
#       This function reads Pilot data, combine with strata data 
#'-------------------------------------------------------------------------------
#'-------------------------------------------------------------------------------
###  2.1: Read Strata Data: This creates file: rstr ----
#'-------------------------------------------------------------------------------
# Read MSA Strata data
if(exists('inSeason')){
rstr <- read.csv(file.path(wd_Ins,strata_file_Ins),stringsAsFactors = FALSE)
  } else {
rstr <- read.csv(file.path(wd_MSA,strata_file),stringsAsFactors = FALSE)
}
# Convert Date to Date format 
rstr$Strata_Start_Date <- as.Date(rstr$Strata_Start_Date,'%m/%d/%Y')
rstr$Strata_End_Date <- as.Date(rstr$Strata_End_Date,'%m/%d/%Y')
# sort data by Start date
rstr <- rstr[order(rstr$Strata_Start_Date),]
# Find the number of years in the data 
years <- unique(rstr$Year)
# number of years
ny <- length(years)
#'------------------------------------------------------------------------------
###  2.2: Read MSA Data: This creates file: MSAL ----
#       MSAL will be used for the rest of the analyses 
#'------------------------------------------------------------------------------
# Read MSA group ID table 
stockID <-  read.csv(file.path(wd_MSA,stock_id_file),stringsAsFactors = FALSE)
# Read MSA prop table 
if(exists('inSeason')){
MSA <- read.csv(file.path(wd_Ins,stock_prop_file_Ins),stringsAsFactors = FALSE)
} else {
MSA <- read.csv(file.path(wd_MSA,stock_prop_file),stringsAsFactors = FALSE)
}

# Extract primary groupID
MSAs <- MSA[MSA$grpID %in% stgrpID,]
# Change Long to Wide format
MSAL <- dcast(MSAs, Year+Strata~grpID, value.var='Mean')
# Clean data  
MSAL <-grpclean(MSAL)
# Standardize MSA proportion, so that total will be 1.0  
MSAL[,-c(1:2)] <- MSAL[,-c(1:2)]/rowSums(MSAL[,-c(1:2)],na.rm=TRUE)
# Merge the stock proportion with sample size:  needed for simulation
MSAL <- merge(MSAL,rstr[,c('Year','Strata','Sample_Size')],by=c('Year','Strata'))

#'------------------------------------------------------------------------------
###  2.3: Read Pilot Station Run, var, and stratum Info ---- 
#       This creates Daily Pilot run and var with strata info
#'------------------------------------------------------------------------------
#  If PostSeason is TRUE, then read all historical Pilot data. 
if(exists('PostSeason')){ 
  # Create list file
  Pilot.list <- list()  
  for(i in 1:ny){
    Pilot.list[[i]] <- read.Pilot.data(rstr,years[i],FALSE)  
  }
  # Convert list file to to data.frame
  Pilot <- as.data.frame(do.call(rbind,Pilot.list))
#  Otherwise, default is read this.year's data 
  }else{
   Pilot <- read.Pilot.data(rstr,this.year,TRUE) 
  }

#'==============================================================================
#  3.0: Data Preparation ----  
#'==============================================================================
# Pilot.st: Sum of Pilot Passage by strata, 
Pilot.st <- aggregate(cbind(Run, Var)~Year+Strata+stbreak+sf, FUN=sum,data=Pilot)

#'------------------------------------------------------------------------------
##  3.1 Pilot.d: Run by all strata by year:  PRIMARY GROUPS ----    
#'------------------------------------------------------------------------------
# Add daily stock proportion to Pilot data: Note this eliminate run without GSI/MSA
Pilot.d <- merge(Pilot.st, MSAL,by = c('Year','Strata'))
# Calculate daily run by stock 
Pilot.d[,stgrpIDn] <-Pilot.d[,stgrpIDn]*Pilot.d$Run

#'------------------------------------------------------------------------------
##  3.2 Pilot.t, Pilot.tp : stock run and proportion Total:  ALL GROUPS ----
#'------------------------------------------------------------------------------
temp <- Pilot.d[,c('Year','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
Pilot.t <- aggregate(.~Year, FUN=function(x) sum(x,na.rm=TRUE),data=temp) 
Pilot.t <- grpclean(Pilot.t)
# Add additional stock groups
Pilot.t  <- add.sum(Pilot.t)
#'------- Proportion -----------------------------------------------------------
Pilot.tp <- Pilot.t
Pilot.tp[,-c(1:2)] <- Pilot.tp[,-c(1:2)]/Pilot.t$Run

#'------------------------------------------------------------------------------
##  3.3 Pilot.m, Pilot.mp: stock run and proportion by Strata: ALL GROUPS ----
#'------------------------------------------------------------------------------
# Calculate sum by sample strata 	
temp <- Pilot.d[,c('Year','Strata','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
#'------ Mean Passage ----------------------------------------------------------
Pilot.m <- aggregate(.~Year+Strata, FUN=sum,data=temp) 
Pilot.m <- grpclean(Pilot.m)
# Add additional stock groups
Pilot.m  <- add.sum(Pilot.m)
#'------ Mean Proportion -------------------------------------------------------
Pilot.mp <- Pilot.m
Pilot.mp[,-c(1:3)] <- Pilot.mp[,-c(1:3)]/Pilot.m$Run

#'------------------------------------------------------------------------------
##  3.4 Pilot.sf, Pilot.sfp" stock run and proportion by Summer-Fall : ALL GROUPS  ----
#'------------------------------------------------------------------------------
# Calculate sum by summer fall  	
temp <- Pilot.d[,c('Year','sf','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
#'------ Mean Passage ----------------------------------------------------------
Pilot.sf <- aggregate(.~Year+sf, FUN=sum,data=temp) 
# Add additional stock groups
Pilot.sf <- grpclean(Pilot.sf)
Pilot.sf  <- add.sum(Pilot.sf)
#'------ Change to  Proportion -------------------------------------------------
Pilot.sfp <- Pilot.sf
Pilot.sfp[,-c(1:3)] <- Pilot.sfp[,-c(1:3)]/Pilot.sf$Run
Pilot.sfp <- Pilot.sfp[,c('Year','sf','Run',as.character(c(1:23)))]
#'------------------------------------------------------------------------------
#'
#'------------------------------------------------------------------------------
##  3.5: Pilot.sft:  Summer vs. Fall stock proportion by standard strata ----   
#'------------------------------------------------------------------------------
# Summarize stock run by standard strata
temp <- add.sum(Pilot.d)
# Change NA to 0
temp[is.na(temp)] <- 0
Pilot.sft <- aggregate(.~Year+stbreak, FUN=sum,data=temp[,c('Year','stbreak','Run','2','9')]) 
names(Pilot.sft) <- c('Year','stbreak','Run','Summer','Fall')
# Calculate stock proportion by standard strata
Pilot.sft[,c('Summer','Fall')]  <- 100*Pilot.sft[,c('Summer','Fall')] /Pilot.sft$Run 
# Reorder 
Pilot.sft <- Pilot.sft[with(Pilot.sft,order(Year,stbreak)),]

#'-------------------------------------------------------------------------------
#  3.6 Pilot.sd:Estimate mean stock proportion by standard strata ----
#'-------------------------------------------------------------------------------# Calculate sum by sample strata 	
temp <- Pilot.d[,c('Year','stbreak','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
#'------ Mean Proportion --------------------------------------------------------
Pilot.sd <- aggregate(.~Year+stbreak, FUN=sum,data=temp) 
Pilot.sd <- grpclean(Pilot.sd)
# Add additional stock groups
Pilot.sd  <- add.sum(Pilot.sd)
# Reduce data to standard stocks 
#Pilot.sd <- Pilot.sd[,c('Year','stbreak','Run',ststocks)]  
# Calculate stock proportion by standard strata
Pilot.sd[,-c(1:3)]  <- 100*Pilot.sd[,-c(1:3)] /Pilot.sd$Run 
# Transpose from wide to long 
Pilot.sd <- melt(Pilot.sd[,-3], 
                 id.vars = c('Year','stbreak'), variable.name = "group", value.name = "percent")
# Change 0 percent to NA
Pilot.sd$percent[Pilot.sd$percent==0]<-NA

#'==============================================================================
# 4.0 Calculate CI Range of passage and proportion by stock group and strata ----
#    Data used: MSAL, 
#     Pilot.st : Sum of Pilot Passage by each designated stratum
#     Pilot.m Pilot.mp:  Total number and prop of fish by stock group by strata 
#     Pilot.t Pilot.tp : Total number and prop of fish by stock: season total 
#     ilot.sf Pilot.sfp: Total number and prop of summer and fall fish: season total
#             stgrpIDn
#  Functions Used: grpclean, add.sum, ciout
#'==============================================================================
#'------------------------------------------------------------------------------
## 4.1  Summarize Pilot data by Sampling-summer-fall strata ----
#'------------------------------------------------------------------------------
# Summarize by summer vs. fall
Pilot.st.y <-  aggregate(cbind(Run,Var) ~ Year+Strata+sf, FUN=sum,data=Pilot.st)

#'===============================================================================
#  5.0: Data Output  ----
#'===============================================================================
#'-------------------------------------------------------------------------------
## 5.1 temp.m.ysg: mean run and proportion by Year and strata and group ----
#'-------------------------------------------------------------------------------
# Extract mean passage and proportion and combine with CI 
temp.m <- melt(Pilot.m[,-3],id.vars = c('Year','Strata'), variable.name = "grpID", value.name = "mean")
temp.mp <- melt(Pilot.mp[,-3],id.vars = c('Year','Strata'), variable.name = "grpID", value.name = "p")
temp.m.ysg <- merge(temp.m,temp.mp,by=c('Year','Strata','grpID'))

#'------------------------------------------------------------------------------
## 5.2  Strata 100 ----
#  temp.t.yg: mean run and proportion by Year group
#'------------------------------------------------------------------------------
# Extract total mean passage and proportion and combine with CI 
temp.t <- melt(Pilot.t[,-2],id.vars = c('Year'), variable.name = "grpID", value.name = "mean")
temp.tp <- melt(Pilot.tp[,-2],id.vars = c('Year'), variable.name = "grpID", value.name = "p")
temp.t.yg <- merge(temp.t,temp.tp,by=c('Year','grpID'))
temp.t.yg$Strata <- 100

#'-------------------------------------------------------------------------------
## 5.3  Strata 101,102 ----
# temp.sf.y: mean run and proportion by Year and summer-fall group
#'-------------------------------------------------------------------------------
temp.sf <- melt(Pilot.sf[,-3],id.vars = c('Year','sf'), variable.name = "grpID", value.name = "mean")
temp.sfp <- melt(Pilot.sfp[,-3],id.vars = c('Year','sf'), variable.name = "grpID", value.name = "p")
temp.sf.y <- merge(temp.sf,temp.sfp,by=c('Year','sf','grpID'))
temp.sf.y$sf<- temp.sf.y$sf+100
names(temp.sf.y)[2] <- 'Strata'

#'-------------------------------------------------------------------------------
## 5.4 Strata 103  ----
# temp.ts: mean proportion by Year
#'-------------------------------------------------------------------------------
# Strata 103
Pilot.ts <- summer.p(Pilot.t,1)
temp.ts <- melt(Pilot.ts,id.vars = c('Year'), variable.name = "grpID", value.name = "p")
temp.ts$mean <- NA
temp.ts$Strata <- 103

#'------------------------------------------------------------------------------
## 5.5 Strata 104,105 ----   
#  temp.sfs: mean proportion of summer-fall in Summer Season 
#'------------------------------------------------------------------------------
# Strata 104 105
Pilot.sfs <- summer.p(Pilot.sf,2)
# Strata 104 105
temp.sfs <- melt(Pilot.sfs,id.vars = c('Year','sf'), variable.name = "grpID", value.name = "p")
temp.sfs$mean <- NA
temp.sfs$sf <- temp.sfs$sf+103
names(temp.sfs)[2] <- 'Strata'

#'-------------------------------------------------------------------------------
## 5.6 Strata 106 ----
#  temp.tf: mean proportion by summer-fall in Fall Season
#'-------------------------------------------------------------------------------
# Strata 106
Pilot.tf <- fall.p(Pilot.t,1)
# Strata 106
temp.tf <- melt(Pilot.tf,id.vars = c('Year'), variable.name = "grpID", value.name = "p")
temp.tf$mean <- NA
temp.tf$Strata <- 106

#'------------------------------------------------------------------------------
## 5.7 Strata 107, 108 ----
#  temp.sff: mean proportion by Year summer-fall
#'------------------------------------------------------------------------------
# Strata 107 108
Pilot.sff <- fall.p(Pilot.sf,2)
# Strata 107 108
temp.sff <- melt(Pilot.sff,id.vars = c('Year','sf'), variable.name = "grpID", value.name = "p")
temp.sff$mean <- NA
temp.sff$sf <- temp.sff$sf+106
names(temp.sff)[2] <- 'Strata'

#'-------------------------------------------------------------------------------
##  temp.m: Combine ALL Summary DATA ---- 
#'-------------------------------------------------------------------------------
# combine all data a
temp.m <- rbind(temp.m.ysg, temp.t.yg,temp.sf.y,temp.ts,temp.sfs,temp.tf,temp.sff)
# Add group ID
temp.m$grpID <- as.numeric(as.character(temp.m$grpID))

#'-------------------------------------------------------------------------------
# Function sum data ---
#'-------------------------------------------------------------------------------
sumdata <- function(tempm,tempci){
# combine per strata and annual data per year 
tempm <- merge(tempm,tempci,by=c('Year','Strata','grpID'))
# Add name 
names(tempm)[6:9] <- c('LCI.m','UCI.m','LCI.p','UCI.p')
# Merge stock ID Name 
tempm <- merge(stockID,tempm, by=c('grpID'))
# Sort data by SortID 
tempm <- tempm[order(tempm$Strata,tempm$SortID),
             c('Year','Strata','SortID','grpID','GroupName','mean','LCI.m','UCI.m','p','LCI.p','UCI.p')]
return(tempm)
}

#'-------------------------------------------------------------------------------
## 5.8 Calculate ci and create mlist and save 
#'-------------------------------------------------------------------------------  
# Postseason and Inseason just do this year   
  MSA.y <- MSAL[MSAL$Year==this.year,]
  Temp.st <- Pilot.st.y[Pilot.st.y$Year ==this.year,]
  temp.ci <- sim.ci(MSA.y,Temp.st,sgrpIDn,nrep,ci,this.year) 
  mlist <- sumdata(temp.m,temp.ci)
  write.csv(mlist,
    file.path(ifelse(exists('inSeason'),wd_Ins,wd_Sum),
              paste0('Pilot_MSA_Sum_',this.year,'.csv')),
              na='',row.names=FALSE)
#'-------------------------------------------------------------------------------  

#'------------------------------------------------------------------------------
##  Post season Output ----
#'------------------------------------------------------------------------------
if (exists('PostSeason')){
#'------------------------------------------------------------------------------
#  CI update for overwrite 
#'------------------------------------------------------------------------------
  if(isTRUE(ciOverwrite)){
#  temp.ci <- list()
#  mlist <- list()
  for(j in 1:ny){
  # Stock prop
  MSA.y <- MSAL[MSAL$Year==years[j],]
  # Pilot st 
  Temp.st <- Pilot.st.y[Pilot.st.y$Year==years[j],]
  temp.ci <- sim.ci(MSA.y,Temp.st,sgrpIDn,nrep,ci,years[j])
  mlist <- sumdata(temp.m[temp.m$Year==years[j],],temp.ci)
  write.csv(mlist,file.path(wd_Sum,paste0('Pilot_MSA_Sum_',years[j],'.csv')),
                                 na='',row.names=FALSE)
      } # End for Year [j]
   } 
#'------------------------------------------------------------------------------
###  EXCEL table output ----
#'------------------------------------------------------------------------------
  EXlist <- list()   
  for(i in 1:ny){
      EXlist[[i]] <- read.csv(file.path(wd_Sum,paste0('Pilot_MSA_Sum_',years[i],'.csv')),stringsAsFactors =  FALSE)
      }  
  names(EXlist) <- years	  
  write.xlsx(EXlist,file.path(wd_Out,sumxlsx),rowNames=FALSE)
  
#'------------------------------------------------------------------------------
###  EXCEL Annual table output ------------
#'------------------------------------------------------------------------------
# Change Pilot Data from list to data.frame 
Pilot.df <- as.data.frame(do.call(rbind,EXlist))
# Extract necessary data from Starata 102 
st102.s <- Pilot.df[with(Pilot.d,which(Strata==102 & grpID %in% c(10,11,13,2,9,15,16,19))), ]

### JTC Table A7 (Prop) ---------------
# Change long to wide 
JTC.A7.p <-dcast(st102.s, Year~GroupName,value.var='p') 
# Extract and arrange columns 
JTC.A7.p <- JTC.A7.p[,c(1,9,8,6,2,4,7)]
names(JTC.A7.p)[-1] <- c('Summer','Fall','Tanana Fall','Border U.S.','Fall U.S.','Canada')
### JTC Table A7 (Number) ---------------
JTC.A7.n <-dcast(st102.s, Year~GroupName,value.var='mean') 
JTC.A7.n <- JTC.A7.n[,c(1,9,8,6,2,4,7)]
names(JTC.A7.n)[-1] <- c('Summer','Fall','Tanana Fall','Border U.S.','Fall U.S.','Canada')

### Table 108 ----------------
# Extract strata  108
st108 <- Pilot.df[Pilot.df$Strata==108,]
# Change long to wide
st108.w <- dcast(st108, Year~GroupName,value.var='p')
# Add CA main
st108.w$camain <- with(st102.s,st102.s[grpID==16,'mean']/st102.s[grpID==9,'mean'])
# Add Porcupine
st108.w$caporc <- with(st102.s,st102.s[grpID==13,'mean']/st102.s[grpID==9,'mean'])
# Arrange Column order
st108.w <- st108.w[,c(1,3,2,4,5,6)]
# Change name: 
names(st108.w)[-1] <- c('Tanana Fall','Border U.S.','Total Canada','Mainstem Canada','Porcupine')
st108.wm <- st108.w
st108.wm[,-1] <- st108.wm[,-1]*st102.s[st102.s$grpID==9,'mean']

out.excel <- list()
out.excel$JTC.A7.p <- JTC.A7.p
out.excel$JTC.A7.n <- JTC.A7.n
out.excel$St108.p <- st108.w
out.excel$St108.n <- st108.wm
write.xlsx(out.excel,file.path(wd_Out,'out_table.xlsx'),rowNames=FALSE)
    
#'------------------------------------------------------------------------------
### Summer-Fall Proportion total, Summer-Fall Proportion by Standard strata ----
#'------------------------------------------------------------------------------
#  Output Pilot Summer and Fall Proportion: Pilot_sfp 
  write.csv(Pilot.sfp,file.path(wd_Sum,sf_p),na='',row.names=FALSE)
#  Output Pilot Summer and Fall Standard time frame : Pilot_sft
  write.csv(Pilot.sft,file.path(wd_Sum,sf_t),na='',row.names=FALSE)
  
#'-------------------------------------------------------------------------------
###   Pilot.sd.min.max:Estimate mean stock proportion by standard strata ----  
#'-------------------------------------------------------------------------------
 Pilot.d.min.max <- aggregate(percent~group+stbreak, FUN=function(x) c(min=min(x),max=max(x),mean=mean(x)),data=Pilot.sd) 
# Change to dataframe
 Pilot.d.min.max <- do.call(data.frame,Pilot.d.min.max)
# Rename Column
 names(Pilot.d.min.max)[3:5] <- c('Min','Max','Mean')
 Pilot.d.min.max<- merge(Pilot.d.min.max,stockID, by.x = 'group', by.y = 'grpID')
#------ File output ------------------------------------------------------------
  write.csv(Pilot.d.min.max,file.path(wd_Sum,min_max),na='',row.names=FALSE)
}

