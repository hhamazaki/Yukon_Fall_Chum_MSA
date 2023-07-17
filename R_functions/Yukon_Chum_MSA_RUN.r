#'##############################################################################
#   Yukon River Fall Chum Salmon Pilot Station Stratified estimate 
#   Code name:  Yukon_Chum_MSA_RUN.R
#   Author:  Toshihide "Hammachan" Hamazaki
#   Date: 
#   Description
#   This program reads Pilot Station Run and variance data, genetic composition 
#   data and create stratified estimates 
#   Model dependencies:
#   Yukon_Chum_MSA_functions.R  # Function sets used for MSA analyses
#   Yukon_Chum_MSA_ReadData.R'  # Function sets to Read data 
#   ggplot_theme.R    # ggplot themes if using 
#   
#   Other dependency variables 
#   this.year : 
#   PostSeason :  whether analyses is postSeason
#   ciOverwrite:  whether re 
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
#'##############################################################################
#   Model running and file Setting Instructions ----
#   Input Files needed
#   1. MSA_Strata.csv:  Include sampling strata, dates, and sample size 
#   2. MSA_prop.csv:  Include Stock prop by strata groupID 
#   3. StockID.csv:  Include groupID, stock name, displqy order 
#   4. Daily_Passage_By_Species_yyyy.csv:  Daily Pilot Passage by species 
#   5. Daily_Variance_By_Species_yyyy.csv:  Daily Pilot Passage Variance by species 
#   Notes:
#   Pilot passage and variance files are located at data/Pilot_data folder 
#   MSA and StockID files are Other files are located at data/MSA_data folder
#'##############################################################################   
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
wd_Ins <- './data/Inseason'
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
strata_file <- 'MSA_Strata.csv'
# Stock Prop info file name 
stock_prop_file <- 'MSA_prop.csv'
# Stock ID file name 
stock_id_file <- 'StockID.csv'
# Pilot Station Run  
Pilot_Run <- 'Daily_Passage_By_Species_'
# Pilot Station Var  
Pilot_Var <- 'Daily_Variance_By_Species_'
# Output EXCEL file name 
sumxlsx <- paste0('Yukon_Pilot_Chum_MSA_',Sys.Date(),'.xlsx')
# Output min_max file name 
min_max <- paste0('Pilot_d_min_max_',this.year,'.csv')
#'------------------------------------------------------------------------------
# inSeason only 
isTRUE(inSeason){
strata_file <- 'MSA_Strata_ins.csv'
# Stock Prop info file name 
stock_prop_file <- 'MSA_prop_ins.csv'  
Pilot_Run <- 'Daily Passage By Species'
# Pilot Station Var  
Pilot_Var <- 'Daily Variance By Species'  
}

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

# Standard Output Stock figures 
ststockID <- c(2,7,8,10,11,19)
# ststocks needed to read as character
ststocks <- as.character(ststockID)

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
rstr <- read.csv(file.path(wd_MSA,strata_file),stringsAsFactors = FALSE)
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
MSA <- read.csv(file.path(wd_MSA,stock_prop_file),stringsAsFactors = FALSE)
# Extract primary groupID
MSAs <- MSA[MSA$grpID %in% stgrpID,]
# Change Long to Wide format
MSAL <- dcast(MSAs, Year+Strata~grpID, value.var='Mean')
# Clean data  
MSAL <-grpclean(MSAL)
# Standardize MSA proportion, so that total will be 1.0  
MSAL[,-c(1:2)] <- MSAL[,-c(1:2)]/rowSums(MSAL[,-c(1:2)],na.rm=TRUE)
# Merge the stock proportion with sanple size:  needed for simulation
MSAL <- merge(MSAL,rstr[,c('Year','Strata','Sample_Size')],by=c('Year','Strata'))

#'------------------------------------------------------------------------------
###  2.3: Read Pilot Station Run, var, and stratum Info ---- 
#       This creates Daily Pilot run and var with strata info
#'------------------------------------------------------------------------------
#  If PostSeason is TRUE, then read all historical Pilot data. 
if(iSTRUE(PostSeason)){ 
  # Create list file
  Pilot.list <- list()  
  for(i in 1:ny){
    Pilot.list[[i]] <- read.Pilot.data(rstr,years[i])  
  }
  # Convert list file to to data.frame
  Pilot <- as.data.frame(do.call(rbind,Pilot.list))
#  Otherwise, default is read this.year's data 
 }else{
   Pilot <- read.Pilot.data(rstr,this.year) 
  }

#'==============================================================================
#  3.0: Data Preparation ----  
#'==============================================================================
# Sum of Pilot Passage by each designated stratum 
Pilot.st <- aggregate(cbind(Run, Var)~Year+Strata+stbreak+sf, FUN=sum,data=Pilot)
#'------------------------------------------------------------------------------
##  3.1 Pilot.d: Run by all strata by year:  PRIMARY GROUPS ----    
#'------------------------------------------------------------------------------
# Add daily stock proportion to Pilot data: Note this eliminate run without GSI/MSA
Pilot.d <- merge(Pilot.st, MSAL,by = c('Year','Strata'))
# Calculate daily run by stock 
Pilot.d[,stgrpIDn] <-Pilot.d[,stgrpIDn]*Pilot.d$Run

#'------------------------------------------------------------------------------
##  3.2 Pilot.t, Pilot.tp : Annual total stock run and proportion:  ALL GROUPS ----
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
##  3.3 Annual-Sampling Strata: stock run and proportion: ALL GROUPS ----
#  Output: Pilot.m, Pilot.mp: 
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
##  3.4 Annual-summer-fall: stock run and proportion: ALL GROUPS  ----
#  Output Pilot.sf, Pilot.sfp  
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
#'------ File output -----------------------------------------------------------
if(PostSeason==TRUE){
# Read existing file
#  temp <- read.csv(paste0(wd_Sum,'Pilot_sfp.csv'),header=TRUE)
# Remove this.year's data if existed=  
#  temp <- temp[temp$Year<this.year,]
# Rename column
#  grpname <- names(temp)[-c(1:3)]
#  names(temp)[-c(1:3)] <- substr(grpname,2,3)
#  Pilot.sfp <- rbind(temp, Pilot.sfp)
  write.csv(Pilot.sfp,paste0(wd_Sum,'Pilot_sfp.csv'),na='',row.names=FALSE)
}

#'-------------------------------------------------------------------------------
##  3.5: Summer and Fall stock proportion by standard strata ----   
#  Output file: Pilot.sft:  
#'-------------------------------------------------------------------------------
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
#'----- File output ------------------------------------------------------------
if(PostSeason==TRUE){
#  temp <- read.csv(paste0(wd_Sum,'Pilot_sft.csv'),header=TRUE)
  # Remove this.year's data if existed=  
#  temp <- temp[temp$Year<this.year,]
  # Add this year's data  
#  Pilot.sft <- rbind(temp, Pilot.sft)
write.csv(Pilot.sft,paste0(wd_Sum,'Pilot_sft.csv'),na='',row.names=FALSE)
}

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
##  Function: ciout:  CI Summary function ---- 
#'------------------------------------------------------------------------------
 ciout <- function(datM,ci,year=NA,season=NA){
  datM <- grpclean(datM,year)
  # Add additional stocks   
  datM <- add.sum(datM,year)
  if(is.na(season)){
  } else if(season=='s'){
  datM <- summer.p(datM)
  } else if(season=='f'){
  datM <- fall.p(datM)
  }   
# Calculate CI 
  temp <-  data.frame(s.functions(datM,ci))
# Add stock group id
  temp$grpID <- as.numeric(rownames(temp))
  return(temp)
  }

#'------------------------------------------------------------------------------
## 4.1  Summarize Pilot data by Sampling-summer-fall strata ----
#'------------------------------------------------------------------------------
# Summarize by summer vs. fall
Pilot.st.y <-  aggregate(cbind(Run,Var) ~ Year+Strata+sf, FUN=sum,data=Pilot.st)

#'-------------------------------------------------------------------------------
#  Bootstrap Simulation by of this year 
#'-------------------------------------------------------------------------------
MSA.y <- MSAL[MSAL$Year==this.year,]
# Pilot st 
Temp.st <- Pilot.st.y[Pilot.st.y$Year ==this.year,]
temp.ci <- sim.ci(MSA.y,Temp.st,sgrpIDn,nrep,ci,this.year)
#'-------------------------------------------------------------------------------
# ci update for all years for Postseason 
#'-------------------------------------------------------------------------------
if(isTRUE(PostSeason)){
  if(isTRUE(ciOverwrite)){
  temp.ci <- list()
  for(j in 1:ny){
  # Stock prop
  MSA.y <- MSAL[MSAL$Year==years[j],]
  # Pilot st 
  Temp.st <- Pilot.st.y[Pilot.st.y$Year==years[j],]
  temp.ci[[j]] <- sim.ci(MSA.y,Temp.st,sgrpIDn,nrep,ci,years[j])
    } # End for Year [j]
  } else  #Postseason is TRUE but ciOverwrite is FALSE
    {
  MSA.y <- MSAL[MSAL$Year==this.year,]
  # Pilot st 
  Temp.st <- Pilot.st.y[Pilot.st.y$Year ==this.year,]
  temp.ci <- sim.ci(MSA.y,Temp.st,sgrpIDn,nrep,ci,this.year)
  
  }
 } 

#'===============================================================================
#  5.0: Data Output  ----
#'===============================================================================
#'-------------------------------------------------------------------------------
## 5.1 temp.m: mean run and proportion by Year and strata and group ----
#'-------------------------------------------------------------------------------
# Extract mean passage and proportion and combine with CI 
temp.m <- melt(Pilot.m[,-3],id.vars = c('Year','Strata'), variable.name = "grpID", value.name = "mean")
temp.mp <- melt(Pilot.mp[,-3],id.vars = c('Year','Strata'), variable.name = "grpID", value.name = "p")
temp.m <- merge(temp.m,temp.mp,by=c('Year','Strata','grpID'))

#'------------------------------------------------------------------------------
## 5.2  Strata 100 ----
#  temp.t: mean run and proportion by Year group
#'------------------------------------------------------------------------------
# Extract total mean passage and proportion and combine with CI 
temp.t <- melt(Pilot.t[,-2],id.vars = c('Year'), variable.name = "grpID", value.name = "mean")
temp.tp <- melt(Pilot.tp[,-2],id.vars = c('Year'), variable.name = "grpID", value.name = "p")
temp.t <- merge(temp.t,temp.tp,by=c('Year','grpID'))
temp.t$Strata <- 100

#'-------------------------------------------------------------------------------
## 5.3  Strata 101,102 ----
# temp.sf: mean run and proportion by Year and summer-fall group
#'-------------------------------------------------------------------------------
temp.sf <- melt(Pilot.sf[,-3],id.vars = c('Year','sf'), variable.name = "grpID", value.name = "mean")
temp.sfp <- melt(Pilot.sfp[,-3],id.vars = c('Year','sf'), variable.name = "grpID", value.name = "p")
temp.sf <- merge(temp.sf,temp.sfp,by=c('Year','sf','grpID'))
temp.sf$sf <- temp.sf$sf+100
names(temp.sf)[2] <- 'Strata'

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
#  temp.sfs: mean proportion by Year summer-fall
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
#  temp.tf: mean proportion by Year summer-fall 
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
# combine 
temp.m <- rbind(temp.m, temp.t,temp.sf,temp.ts,temp.sfs,temp.tf,temp.sff)
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
## 5.8 mlist: this.year's summary data ----
#'-------------------------------------------------------------------------------  
mlist <- sumdata(temp.m,temp.ci)
#'-------------------------------------------------------------------------------
### Save Data Postseason CSV, xlsx for PostSeason----
#'-------------------------------------------------------------------------------  
if(isTRUE(PostSeason)){
  if(isTRUE(ciOverwrite)){
  # Save all into list file    
  mlist <- list()
  for(i in 1:ny){
    # Save to the list 
    mlist[[i]] <- sumdata(temp.m[temp.m$Year==year[i]],temp.ci[[i]])
   }
  names(mlist) <- years
  for(i in 1:ny){
    write.csv(mlist[[i]],paste0(wd_Sum,'Pilot_MSA_Sum_',years[i],'.csv'),na='',row.names=FALSE)
  } 
  } else {
  write.csv(sumdata(temp.m,temp.ci),paste0(wd_Sum,'Pilot_MSA_Sum_',this.year,'.csv'),na='',row.names=FALSE)
  }
 }

#'------------------------------------------------------------------------------
##  EXCEL table output ----
#'------------------------------------------------------------------------------
if (isTRUE(PostSeason)){
  EXlist <- list()   
  for(i in 1:ny){
      EXlist[[i]] <- read.csv(paste0(wd_Sum,'Pilot_MSA_Sum_',years[i],'.csv'),stringsAsFactors = FALSE)
      }    
  write.xlsx(EXlist,sumxlsx,rowNames=FALSE) 
}

#'-------------------------------------------------------------------------------
#  6.0 Pilot.sd:Estimate mean stock proportion by standard strata ----
#'-------------------------------------------------------------------------------
# Calculate sum by sample strata 	
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

if(PostSeason==TRUE){
#'-------------------------------------------------------------------------------
#  7.0 Pilot.sd.min.max:Estimate mean stock proportion by standard strata ----  
#'-------------------------------------------------------------------------------
Pilot.d.min.max <- aggregate(percent~group+stbreak, FUN=function(x) c(min=min(x),max=max(x),mean=mean(x)),data=Pilot.sd) 
# Change to dataframe
Pilot.d.min.max <- do.call(data.frame,Pilot.d.min.max)
# Rename Column
names(Pilot.d.min.max)[3:5] <- c('Min','Max','Mean')
Pilot.d.min.max<- merge(Pilot.d.min.max,stockID, by.x = 'group', by.y = 'grpID')
#------ File output ------------------------------------------------------------
  write.csv(Pilot.d.min.max,paste0(wd_Sum,min_max),na='',row.names=FALSE)
}

