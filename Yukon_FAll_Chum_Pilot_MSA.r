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
################################################################################
################################################################################
#  Changes in Primary stock groups
#  1999 - 2002: 3, 4, 5, 6. 20, 21, 22
#  2004 - 2007: 3, 4, 6, 8, 10, 11, 13, 14
#  2008 - present: 3, 4, 6, 7, 8, 10, 11, 13, 14
################################################################################
################################################################################
################################################################################
#   Model running and file Setting Instructions 
#   Input Files needed
#   1. MSA_Strata.csv:  Include sampling strata, dates, and sample size 
#   2. MSA_prop.csv:  Include Stock prop by strata groupID 
#   3. StockID.csv:  Include groupID, stock name, displqy order 
#   4. Daily_Passage_By_Species_yyyy.csv:  Daily Pilot Passage by species 
#   5. Daily_Variance_By_Species_yyyy.csv:  Daily Pilot Passage Variance by species 
#   Notes:
#   Pilot passage and variance files are located in Pilot folder 
#   Other files are located in main directory
################################################################################   
#===============================================================================
#  1.0: Clear Memory and Set Working Environment 
#===============================================================================
# Clear up existing files 
rm(list = ls(all = TRUE))
# Add packages needed 
library(openxlsx)   # Used to create EXCEL output file
library(reshape2)   # Used to transpose data file 
library(ggplot2)    # Used for ggplot graphics 
library(lemon)      # Used for ggplot: better figures (facet_rep_wrap)
#palette('Okabe-Ito')  # Change color palette color blinds friendly
options(scipen=999)   # Prevent R using scientific notation xxx e-yyy
# Functions Source
#-------------------------------------------------------------------------------
# Setup file directories 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Standard 
#-------------------------------------------------------------------------------
# Main <- 'C:/Projects/Yukon_River/Fall_Chum/MSA/' 
# fdr <- paste0(Main, 'R_functions/')  # R functions
# wd_MSA <- paste0(Main,'data/MSA_data/') # MSA data
# wd_Plt <- paste0(Main,'/data/Pilot_data/') #Pilot data
# wd_Sum <- paste0(Main,'/data/Summary_data/') #Summary data used for figures
#-------------------------------------------------------------------------------
# Rprojects 
#-------------------------------------------------------------------------------
fdr <- './R_functions/'
wd_MSA <- './data/MSA_data/'
wd_Plt <- './data/Pilot_data/'
wd_Sum <- './data/Summary_data/'

#-------------------------------------------------------------------------------
# Import Source files 
#-------------------------------------------------------------------------------
source(paste0(fdr,'Yukon_Chum_MSA_functions.R'))  # Function sets used for MSA analyses
source(paste0(fdr,'Yukon_Chum_MSA_ReadData.R'))  # Function sets used for MSA analyses
source(paste0(fdr,'ggplot_theme.R'))  # ggplot themes 

#-------------------------------------------------------------------------------
#  1.1: Set MSA and Pilot data file names 
#-------------------------------------------------------------------------------
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
sumxlsx <- paste0('Yukon_Pilot_Chum_MSA','.xlsx')

#-------------------------------------------------------------------------------
#  1.2: Set Standard Stock Groups 
#-------------------------------------------------------------------------------
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
ststocks <- as.character(ststockID)

#-------------------------------------------------------------------------------
#  1.3: Specify Simulation and outputs 
#-------------------------------------------------------------------------------
# Do you want to run simulation to get CI?  TRUE or FALSE 
Sim <- TRUE
# Set the number of simulation replilcates: default 100000
nrep <- 10000
# % CI range
ci <- 90
# Do you want to produce output Files?
output <- FALSE

#===============================================================================
#  2.0: Data Read and summarize 
#===============================================================================
#-------------------------------------------------------------------------------
# Read MSA data data : 
#       This function reads Pilot data, combine with strata data 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#  2.1: Read Strata Data: This creates file rstr
#-------------------------------------------------------------------------------
# Read MSA Strata data
rstr <- read.csv(paste0(wd_MSA,strata_file),stringsAsFactors = FALSE)
# Convert Date to Date format 
rstr$Strata_Start_Date <- as.Date(rstr$Strata_Start_Date,'%m/%d/%Y')
rstr$Strata_End_Date <- as.Date(rstr$Strata_End_Date,'%m/%d/%Y')
# sort data by Start date
rstr <- rstr[order(rstr$Strata_Start_Date),]
# Find the number of years in the data 
years <- unique(rstr$Year)
# number of years
ny <- length(years)

#-------------------------------------------------------------------------------
#  2.2: Read MSA Data: This creates file: MSAL 
#       MSAL will be used for the rest of the analyses 
#-------------------------------------------------------------------------------
# Read MSA group ID table 
stockID <-  read.csv(paste0(wd_MSA,stock_id_file),stringsAsFactors = FALSE)
# Read MSA prop table 
MSA <- read.csv(paste0(wd_MSA,stock_prop_file),stringsAsFactors = FALSE)
# Extract primary groupID
MSAs <- MSA[MSA$grpID %in% stgrpID,]
# Change Long to Wide format
MSAL <- dcast(MSAs, Year+Strata~grpID, value.var='Mean')
# Clean data  
MSAL <-grpclean(MSAL)

# Standardize MSA proportion, so that total will be 1.0  
MSAL[,-c(1:2)] <- MSAL[,-c(1:2)]/rowSums(MSAL[,-c(1:2)],na.rm=TRUE)
MSAL <- merge(MSAL,rstr[,c('Year','Strata','Sample_Size')],by=c('Year','Strata'))

#-------------------------------------------------------------------------------
#  2.3: Read Pilot Station Run, var, and stratum Info : 
#       This creates Daily Pilot run and var with strata info
#-------------------------------------------------------------------------------
# Create a list file
Pilot.list <- list()
for(i in 1:ny){
Pilot.list[[i]] <- read.Pilot.data(rstr,years[i])  
}
# Convert list file to to data.frame
Pilot <- as.data.frame(do.call(rbind,Pilot.list))

#===============================================================================
#  3.0: Data Preparation  
#===============================================================================
# Sum Pilot Passage by each designated stratum 
Pilot.st <- aggregate(cbind(Run, Var)~Year+Strata+stbreak+sf, FUN=sum,data=Pilot)
#-------------------------------------------------------------------------------
#  3.1 Pilot.d: Run by all strata by year:  PRIMARY GROUPS     
#-------------------------------------------------------------------------------
# Add daily stock proportion to Pilot data: Note this eliminate run without GSI/MSA
Pilot.d <- merge(Pilot.st, MSAL,by = c('Year','Strata'))
# Calculate daily run by stock 
Pilot.d[,stgrpIDn] <-Pilot.d[,stgrpIDn]*Pilot.d$Run

#-------------------------------------------------------------------------------
#  3.2 Pilot.t, Pilot.tp : Annual total stock run and proportion:  ALL GROUPS 
#-------------------------------------------------------------------------------
temp <- Pilot.d[,c('Year','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
Pilot.t <- aggregate(.~Year, FUN=function(x) sum(x,na.rm=TRUE),data=temp) 
Pilot.t <- grpclean(Pilot.t)
# Add additional stock groups
Pilot.t  <- add.sum(Pilot.t)
#------- Proportion ------------------------------------------------------------
Pilot.tp <- Pilot.t
Pilot.tp[,-c(1:2)] <- Pilot.tp[,-c(1:2)]/Pilot.t$Run

#-------------------------------------------------------------------------------
#  3.3 Pilot.m, Pilot.mp: Annual-Sampling Strata: stock run and proportion: ALL GROUPS  
#-------------------------------------------------------------------------------
# Calculate sum by sample strata 	
temp <- Pilot.d[,c('Year','Strata','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
#------ Mean Passage -----------------------------------------------------------
Pilot.m <- aggregate(.~Year+Strata, FUN=sum,data=temp) 
Pilot.m <- grpclean(Pilot.m)
# Add additional stock groups
Pilot.m  <- add.sum(Pilot.m)
#------ Mean Proportion --------------------------------------------------------
Pilot.mp <- Pilot.m
Pilot.mp[,-c(1:3)] <- Pilot.mp[,-c(1:3)]/Pilot.m$Run

#-------------------------------------------------------------------------------
#  3.4 Pilot.sf, Pilot.sfp: Annual-summer-fall: stock run and proportion: ALL GROUPS  
#-------------------------------------------------------------------------------
# Calculate sum by summer fall  	
temp <- Pilot.d[,c('Year','sf','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
#------ Mean Passage -----------------------------------------------------------
Pilot.sf <- aggregate(.~Year+sf, FUN=sum,data=temp) 
# Add additional stock groups
Pilot.sf <- grpclean(Pilot.sf)
Pilot.sf  <- add.sum(Pilot.sf)
#------ Mean Proportion --------------------------------------------------------
Pilot.sfp <- Pilot.sf
Pilot.sfp[,-c(1:2)] <- Pilot.sfp[,-c(1:2)]/Pilot.sf$Run
#------ File output ------------------------------------------------------------
if(output==TRUE){
  write.csv(Pilot.sfp,paste0(wd_Sum,'Pilot_sfp.csv'),na='',row.names=FALSE)
}

#-------------------------------------------------------------------------------
#  3.5: Pilot.sft: Summer and Fall stock proportion by standard strata       
#-------------------------------------------------------------------------------
# Summarize stock run by standard strata
temp <- add.sum(Pilot.d)
temp[is.na(temp)] <- 0
Pilot.sft <- aggregate(.~Year+stbreak, FUN=sum,data=temp[,c('Year','stbreak','Run','2','9')]) 
names(Pilot.sft) <- c('Year','stbreak','Run','Summer','Fall')
# Calculate stock proportion by standard strata
Pilot.sft[,c('Summer','Fall')]  <- 100*Pilot.sft[,c('Summer','Fall')] /Pilot.sft$Run 
#------ File output ------------------------------------------------------------
if(output==TRUE){
write.csv(Pilot.sft,paste0(wd_Sum,'Pilot_sft.csv'),na='',row.names=FALSE)
}

#===============================================================================
# 4.0 Calculate CI Range of passage and proportion by stock group and strata 
#    Data used: MSAL, 
#             Pilot.st
#             Pilot.m Pilot.mp
#             Pilot.t Pilot.tp
#             Pilot.sf Pilot.sfp
#             Pilot.ts 
#             Pilot.sfs
#             Pilot.tf
#             Pilot.tff
#             Pilot.sfs
#             stgrpIDn
#  Functions Used: grpclean, add.sum, ciout
#===============================================================================
#-------------------------------------------------------------------------------
#  Function: ciout:  CI Summary function 
#-------------------------------------------------------------------------------
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


# Create temporal  list file 
temp.ci <- list()
#-------------------------------------------------------------------------------
# 4.1  Summarize Pilot data by Sampling-summer-fall starata 
#-------------------------------------------------------------------------------
# By Summer vs. fall
Pilot.st.y <-  aggregate(cbind(Run,Var) ~ Year+Strata+sf, FUN=sum,data=Pilot.st)
#-------------------------------------------------------------------------------
#  Bootstrap Simulation by year and strata 
#-------------------------------------------------------------------------------
for(j in 1:ny){
#-------------------------------------------------------------------------------
# 4.1  Extract Stock prop and Pilot passage for each year 
#-------------------------------------------------------------------------------
# Stock prop
MSA.y <- MSAL[MSAL$Year==years[j],]
# Pilot st 
Temp.st <- Pilot.st.y[Pilot.st.y$Year==years[j],]
#-------------------------------------------------------------------------------
# 4.3  Create Empty Matrix by strata by PRIMARY GROUP
#-------------------------------------------------------------------------------
# season total
t.sim <- matrix(0,nrow =nrep,ncol=length(stgrpIDn))
colnames(t.sim) <- stgrpIDn
# by summer season  
st.sim <- matrix(0,nrow =nrep,ncol=length(stgrpIDn))
colnames(st.sim) <- stgrpIDn
# fall season
ft.sim <- matrix(0, nrow =nrep,ncol=length(stgrpIDn))
colnames(ft.sim) <- stgrpIDn
#-------------------------------------------------------------------------------
# 4.4 Bootstrap Simulation by strata for each Year  
#-------------------------------------------------------------------------------
# Simulation 
pilot.st.sim <- list()
# Extract the number of sampling strata 
nst <- min(max(Temp.st$Strata),max(MSA.y$Strata))

for(i in 1:nst){
# Calculate sample size: 1/2 of actual sample size to incorporate GSI stock ID Error  
  sn <- MSA.y[MSA.y$Strata==i,'Sample_Size']/2
# Extract primary stock proportion 
  p <-  MSA.y[MSA.y$Strata==i,stgrpIDn]
# change NA to zero  
  p[is.na(p)] <- 0
# Simulate stock proportion as multinomial distribution   
  p.sim <- t(rmultinom(nrep, sn, p)/sn)
# Extract Pilot number of a stratum
  st <- Temp.st[Temp.st$Strata==i,]
# Simulate run based on normal distribution 
  r.sim <- with(st,rnorm(nrep,Run,sqrt(Var)))
# Multiply simulated Run with simulated stock proportion to get simulated run by stock 
  r.sim <- p.sim*r.sim

#-------------------------------------------------------------------------------
# 4.4.1 ADD PRIMARY GROUP SIM to TOTAL   
#-------------------------------------------------------------------------------
# Add matrix to crate total 
  t.sim <- t.sim+r.sim    
# Add matrix to summer or fall strata   
  if(st$sf==1){
  st.sim <- st.sim+r.sim 
  } else if (st$sf==2){
  ft.sim <- ft.sim+r.sim 
  }  
#-------------------------------------------------------------------------------
# 4.4.2  Clean data and caculate SECONDARY GROUPS 
#-------------------------------------------------------------------------------
# clean data and calculate secondary stock groups for proportion and run 
  p.sim <- grpclean(p.sim,years[j])
  p.sim <- add.sum(p.sim,years[j])
  r.sim <- grpclean(r.sim,years[j])
  r.sim <- add.sum(r.sim,years[j])
# Calculate CI range 
  r.sim.ci <-  data.frame(s.functions(r.sim,ci))
  p.sim.ci <-  data.frame(s.functions(p.sim,ci))
# Add grpIsD name 
  r.sim.ci$grpID <- as.numeric(rownames(r.sim.ci))
  p.sim.ci$grpID <- as.numeric(rownames(p.sim.ci))
# Combine the run and prop by gropID
  temp <- merge(r.sim.ci,p.sim.ci,by=c('grpID'))
# Add strata number
  temp$Strata <- i
# Save to list file 
  pilot.st.sim[[i]] <- temp
 }  # End Strata CI calculation for each year [i]

#-------------------------------------------------------------------------------
# 4.5 Summarize for each Year  
#  t.sim: total passage by stock 
#  st.sim: total passage by stock: summer 
#  ft.sim: total passage by stock: fall
#-------------------------------------------------------------------------------
# Calculate total stock proportion
  t.sim.p  <- t.sim/rowSums(t.sim)
# Summer season
if(sum(st.sim)>0) {st.sim.p  <- st.sim/rowSums(st.sim)} else st.sim.p <- st.sim
# Fall season
if(sum(ft.sim)>0) {ft.sim.p  <- ft.sim/rowSums(ft.sim)} else ft.sim.p <- ft.sim

# Calculate passage and proportion by stock CI 
  temp.t <- ciout(t.sim,ci,years[j])
  temp.tp <- ciout(t.sim.p,ci,years[j])
  temp.st <- ciout(st.sim,ci,years[j])
  temp.stp <- ciout(t.sim.p,ci,years[j])
  temp.ft <- ciout(ft.sim,ci,years[j])
  temp.ftp <- ciout(ft.sim.p,ci,years[j])
  temp.103 <- ciout(t.sim,ci,years[j],'s')  
  temp.104 <- ciout(st.sim,ci,years[j],'s')  
  temp.105 <- ciout(ft.sim,ci,years[j],'s')  
  temp.106 <- ciout(t.sim,ci,years[j],'f')  
  temp.107 <- ciout(st.sim,ci,years[j],'f')  
  temp.108 <- ciout(ft.sim,ci,years[j],'f')  
# Combine the run and prop by gropID
  temp.t <- merge(temp.t,temp.tp,by=c('grpID'))  
  temp.st <- merge(temp.st,temp.stp,by=c('grpID'))  
  temp.ft <- merge(temp.ft,temp.ftp,by=c('grpID'))  
# Create dummy table with NA  for strata group 103-108  
  temp.sna <-temp.103
  temp.sna[,c('LCI','UCI')] <- NA
  temp.fna <-temp.106
  temp.fna[,c('LCI','UCI')] <- NA
  temp.103 <- merge(temp.sna,temp.103,by=c('grpID'))  
  temp.104 <- merge(temp.sna,temp.104,by=c('grpID'))  
  temp.105 <- merge(temp.sna,temp.105,by=c('grpID'))  
  temp.106 <- merge(temp.fna,temp.106,by=c('grpID'))  
  temp.107 <- merge(temp.fna,temp.107,by=c('grpID'))  
  temp.108 <- merge(temp.fna,temp.108,by=c('grpID'))  
# Add strata number
# Season Total 
  temp.t$Strata <- 100
# For Summer 
  temp.st$Strata <- 101
# For Fall 
  temp.ft$Strata <- 102
# Proportion by summer / Fall   
  temp.103$Strata <- 103 
  temp.104$Strata <- 104 
  temp.105$Strata <- 105 
  temp.106$Strata <- 106 
  temp.107$Strata <- 107 
  temp.108$Strata <- 108 
  # Save CI by Strata for each year 
  temp.ci[[j]] <- rbind(do.call(rbind, lapply(pilot.st.sim, as.data.frame)),temp.t,temp.st,temp.ft,
  temp.103,temp.104,temp.105,temp.106,temp.107,temp.108) 
 } # End for Year [j]

#===============================================================================
#  5.0: Data Output  
#===============================================================================
#-------------------------------------------------------------------------------
#  temp.m: mean run and proportion by Year and strata and group 
#-------------------------------------------------------------------------------
# Extract mean passage and proportion and combine with CI 
temp.m <- melt(Pilot.m[,-3],id.vars = c('Year','Strata'), variable.name = "grpID", value.name = "mean")
temp.mp <- melt(Pilot.mp[,-3],id.vars = c('Year','Strata'), variable.name = "grpID", value.name = "p")
temp.m <- merge(temp.m,temp.mp,by=c('Year','Strata','grpID'))

#-------------------------------------------------------------------------------
#  temp.t: mean run and proportion by Year group (Strata 100)
#-------------------------------------------------------------------------------
# Extract total mean passage and proportion and combine with CI 
temp.t <- melt(Pilot.t[,-2],id.vars = c('Year'), variable.name = "grpID", value.name = "mean")
temp.tp <- melt(Pilot.tp[,-2],id.vars = c('Year'), variable.name = "grpID", value.name = "p")
temp.t <- merge(temp.t,temp.tp,by=c('Year','grpID'))
temp.t$Strata <- 100

#-------------------------------------------------------------------------------
#  temp.sf: mean run and proportion by Year and summer-fall group (Strata 101,102)
#-------------------------------------------------------------------------------
temp.sf <- melt(Pilot.sf[,-3],id.vars = c('Year','sf'), variable.name = "grpID", value.name = "mean")
temp.sfp <- melt(Pilot.sfp[,-3],id.vars = c('Year','sf'), variable.name = "grpID", value.name = "p")
temp.sf <- merge(temp.sf,temp.sfp,by=c('Year','sf','grpID'))
temp.sf$sf <- temp.sf$sf+100
names(temp.sf)[2] <- 'Strata'

#-------------------------------------------------------------------------------
#  temp.ts: mean proportion by Year  (Strata 103)
#-------------------------------------------------------------------------------
# Strata 103
Pilot.ts <- summer.p(Pilot.t,1)
temp.ts <- melt(Pilot.ts,id.vars = c('Year'), variable.name = "grpID", value.name = "p")
temp.ts$mean <- NA
temp.ts$Strata <- 103

#-------------------------------------------------------------------------------
#  temp.sfs: mean proportion by Year summer-fall  (Strata 104,105)
#-------------------------------------------------------------------------------
# Strata 104 105
Pilot.sfs <- summer.p(Pilot.sf,2)
# Strata 104 105
temp.sfs <- melt(Pilot.sfs,id.vars = c('Year','sf'), variable.name = "grpID", value.name = "p")
temp.sfs$mean <- NA
temp.sfs$sf <- temp.sfs$sf+103
names(temp.sfs)[2] <- 'Strata'

#-------------------------------------------------------------------------------
#  temp.tf: mean proportion by Year summer-fall  (Strata 106)
#-------------------------------------------------------------------------------
# Strata 106
Pilot.tf <- fall.p(Pilot.t,1)
# Strata 106
temp.tf <- melt(Pilot.tf,id.vars = c('Year'), variable.name = "grpID", value.name = "p")
temp.tf$mean <- NA
temp.tf$Strata <- 106

#-------------------------------------------------------------------------------
#  temp.sff: mean proportion by Year summer-fall  (Strata 107,108)
#-------------------------------------------------------------------------------
# Strata 107 108
Pilot.sff <- fall.p(Pilot.sf,2)
# Strata 107 108
temp.sff <- melt(Pilot.sff,id.vars = c('Year','sf'), variable.name = "grpID", value.name = "p")
temp.sff$mean <- NA
temp.sff$sf <- temp.sff$sf+106
names(temp.sff)[2] <- 'Strata'
#-------------------------------------------------------------------------------
#  temp.m: Combine ALL Summary DATA 
#-------------------------------------------------------------------------------
# combine 
temp.m <- rbind(temp.m, temp.t,temp.sf,temp.ts,temp.sfs,temp.tf,temp.sff)
# Add group ID
temp.m$grpID <- as.numeric(as.character(temp.m$grpID))

#-------------------------------------------------------------------------------
# Save data into List Data 
#-------------------------------------------------------------------------------
mlist <- list()
for(i in 1:ny){
# Mean run and proportion  
temp <- temp.m[temp.m$Year==years[i],]
# CI of run and proportion
tempci <- temp.ci[[i]]
# combine per strata and annual data per year 
temp <- merge(temp,tempci,by=c('Strata','grpID'))
# Add name 
names(temp)[6:9] <- c('LCI.m','UCI.m','LCI.p','UCI.p')
# Merge stock ID Name 
temp <- merge(stockID,temp, by=c('grpID'))
# Sort data by SortID 
temp <- temp[order(temp$Strata,temp$SortID),
              c('Year','Strata','SortID','grpID','GroupName','mean','LCI.m','UCI.m','p','LCI.p','UCI.p')]
# Save to the list 
mlist[[i]] <- temp
}
# Put name 
names(mlist) <- years

if(output==TRUE){
#-------------------------------------------------------------------------------
#  Pilot CSV output
#-------------------------------------------------------------------------------
for(i in 1:ny){
  write.csv(mlist[[i]],paste0(wd_Sum,'Pilot_MSA_Sum_',years[i],'.csv'),na='',row.names=FALSE)
}
#-------------------------------------------------------------------------------
#  EXCEL table output
#-------------------------------------------------------------------------------
write.xlsx(mlist,sumxlsx,rowNames=FALSE) 
}

#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
#  6.0 Pilot.sd:Estimate mean stock proportion by standard strata   
#-------------------------------------------------------------------------------
# Calculate sum by sample strata 	
temp <- Pilot.d[,c('Year','stbreak','Run',stgrpIDn)]
temp[is.na(temp)] <- 0
#------ Mean Proportion --------------------------------------------------------
Pilot.sd <- aggregate(.~Year+stbreak, FUN=sum,data=temp) 
Pilot.sd <- grpclean(Pilot.sd)
# Add additional stock groups
Pilot.sd  <- add.sum(Pilot.sd)
# Reduce data to standad stocks 
Pilot.sd <- Pilot.sd[,c('Year','stbreak','Run',ststocks)]  
# Calculate stock proportion by standard strata
Pilot.sd[,ststocks]  <- 100*Pilot.sd[,ststocks] /Pilot.sd$Run 
# Transpose from wide to long 
Pilot.sd <- melt(Pilot.sd[,c('Year','stbreak',ststocks)], 
                 id.vars = c('Year','stbreak'), variable.name = "group", value.name = "percent")
# Change 0 percent to NA
Pilot.sd$percent[Pilot.sd$percent==0]<-NA
Pilot.d.min.max <- aggregate(percent~group+stbreak, FUN=function(x) c(min=min(x),max=max(x),mean=mean(x)),data=Pilot.sd) 
# Change to dataframe
Pilot.d.min.max <- do.call(data.frame,Pilot.d.min.max)
# Rename Column
names(Pilot.d.min.max)[3:5] <- c('Min','Max','Mean')
Pilot.d.min.max<- merge(Pilot.d.min.max,stockID, by.x = 'group', by.y = 'grpID')
#------ File output ------------------------------------------------------------
if(output==TRUE){
  write.csv(Pilot.d.min.max,paste0(wd_Sum,'Pilot_d_min_max.csv'),na='',row.names=FALSE)
}




#-------------------------------------------------------------------------------
#  Plot stock proportion by Strata 
#-------------------------------------------------------------------------------
if(fig2==TRUE){
Pilot.stp <- Pilot.m[,c('Year','Strata','Run', ststocks)]
Pilot.stp[,ststocks] <- 100*Pilot.stp[,ststocks] /Pilot.stp$Run
Pilot.stpl <- melt(Pilot.stp[,c('Year','Strata',ststocks)], 
            id.vars = c('Year','Strata'), variable.name = "group", value.name = "percent") 
Pilot.stpl <- Pilot.stpl[order(Pilot.stpl$Year,Pilot.stpl$Strata),]
gname <- stockID[stockID$grpID %in%ststockID,c('GroupName')]      
names(gname) <- ststockID
# Base plot 			
for(k in seq(1,20,4)){
par(mfrow=c(4,1),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l') 
for(j in 1:ny){
temp <- Pilot.stpl[Pilot.stpl$Year==years[j],]
plot(percent~Strata, type ='o',col=1,ylim=c(0,100), data=temp[temp$group==ststockID[1],])
for (i in 2:6){
lines(percent~Strata, type ='o',col=i,data=temp[temp$group==ststockID[i],])
}
title(main = years[j])
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
   plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
   legend('bottom',legend = gname, col = c(1:6),  pch=1, lwd = 1, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')
}

# ggplot
if(gg=TRUE){
    p1 <- ggplot(Pilot.stpl,aes(x=Strata,y=percent,color=group))+geom_line()
    p1+facet_wrap(~Year,scale='free')+theme_simple()+xlim(1,12)
	
	+
	scale_x_continuous(breaks=c(1:12))
    p1 
	}
}

#-------------------------------------------------------------------------------
#  Plot mean stock proportion by standard strata 
#-------------------------------------------------------------------------------
if(fig2==TRUE){
# Extract data with standard strata 
  Pilot.stp <- Pilot.m[,c('Year','Strata','Run', ststocks)]
# Convert numbers to % proportion  
  Pilot.stp[,ststocks] <- 100*Pilot.stp[,ststocks] /Pilot.stp$Run
# Change Wide to Long format.   
  Pilot.stpl <- melt(Pilot.stp[,c('Year','Strata',ststocks)], 
                     id.vars = c('Year','Strata'), variable.name = "group", value.name = "percent") 
  Pilot.stpl <- Pilot.stpl[order(Pilot.stpl$Year,Pilot.stpl$Strata),]
  gname <- stockID[stockID$grpID %in%ststockID,c('GroupName')]      
  names(gname) <- ststockID
  # Base plot 			
  for(k in seq(1,20,4)){
    par(mfrow=c(4,1),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l') 
    for(j in 1:ny){
      temp <- Pilot.stpl[Pilot.stpl$Year==years[j],]
      plot(percent~Strata, type ='o',col=1,ylim=c(0,100), data=temp[temp$group==ststockID[1],])
      for (i in 2:6){
        lines(percent~Strata, type ='o',col=i,data=temp[temp$group==ststockID[i],])
      }
      title(main = years[j])
    }
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
    legend('bottom',legend = gname, col = c(1:6),  pch=1, lwd = 1, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')
  }
  
  # ggplot
  if(gg=TRUE){
    p1 <- ggplot(Pilot.stpl,aes(x=Strata,y=percent,color=group))+geom_line()
    p1+facet_wrap(~Year,scale='free')+theme_simple()+xlim(1,12)
    
    +
      scale_x_continuous(breaks=c(1:12))
    p1 
  }
}



#===============================================================================
#  Stock run proportion by standard Strata       
#===============================================================================
if(fig3==TRUE){
windows(record=TRUE)
# Base Plot 
gname <- stockID[stockID$grpID %in%ststockID,c('GroupName')]      
names(gname) <- ststockID
if(gg=TRUE){
  # ggplot2 	
  ggplot(data = Pilot.d.min.max, aes(x=stbreak, y=Mean,group=(GroupName))) + theme_simple() + 
    scale_x_continuous( breaks=c( 1:9 ),labels=stbl)+ ylim(0, 100)+
    theme(axis.text.x = element_text(size=10))+
    geom_line(aes(color=GroupName)) + 
    geom_point(aes(shape=GroupName, color=GroupName ))+
    labs(title = "",y='Stock proportion',x="Date",shape = "", color = "")
  
} else{

par(mfrow=c(1,1),mar = c(2,2,2,2),oma = c(3,3,3,3),yaxs='i',bty='l')
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
mtext('Stock %', side = 2, line = 1, outer = TRUE)
mtext("Dates ", side = 1, line = 1, outer = TRUE)
}


}


#-------------------------------------------------------------------------------
#  3.2 Summer vs. Fall
#=------------------------------------------------------------------------------ 
if(fig4==TRUE){
  windows(record=TRUE)
if(gg==TRUE){
  # ggplot2 	
  #  Create long data 
  Pilot.sfl <- melt(Pilot.sft,id.vars=c('Year','stbreak'), 
                    measure.vars=c("Summer", "Fall"),
                    variable.name='SF', value.name='percent')
  Pilot.sfl2 <- dcast(Pilot.sfl,Year+SF ~ stbreak)
  Pilot.sfl3 <- melt(Pilot.sfl2,id.vars=c('Year','SF'), 
                     variable.name='stbreak', value.name='percent')
  # ggplot
  p <- ggplot() + theme_simple() + 
    facet_rep_wrap( ~factor(Year)) +
    #   facet_wrap( ~factor(Year),scale='free') + 
    theme(axis.text.x = element_text(size=10))+
    labs(title = "Summer vs. Fall\n")+  xlab("Season Strata")+
    geom_line(data = Pilot.sfl3, aes( x=as.numeric(stbreak),y=percent,color=SF ) )+
    geom_point(data = Pilot.sfl3, aes( x=as.numeric(stbreak),y=percent,color=SF ),size=2)
# All season
  p1 <- p+scale_x_continuous( breaks=c( 1:9 ),labels=stbl) + ylim(0, 100)
# Just around 7/19  
  p2 <- p+scale_x_continuous( breaks=c( 4:6 ),labels=stbl) + ylim(0, 100)
  p1
  p2
  
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
  
}
}
