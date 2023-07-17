#===============================================================================
#  Yukon_Chum_MSA_ReadData.R
#  Written by 
#  Toshihide "Hamachan" Hamazaki
#  This includes Data reading function sets 
#===============================================================================

#-------------------------------------------------------------------------------
# function: read.Pilot.data 
# Read Pilot Station Run, var data : 
#       This function reads Pilot data, combine with strata data (rstr)
#       Pilot Station data should be named: 
#       Daily_Passage_By_Species_year.csv
#       Daily_Variance_By_Species_year.csv
#-------------------------------------------------------------------------------
read.Pilot.data <- function(rstr,year,inSeason=FALSE){
# Read Pilot Station Run data
  if(isTRUE(inSeason)){
    run <- read.csv(file.path(wd_Ins,Pilot_Run_Ins),stringsAsFactors = FALSE)  
    rvar <- read.csv(file.path(wd_Ins,Pilot_Var_Ins),stringsAsFactors = FALSE)
 # Remove the last 6 lines
  run <- run[1:(dim(run)[1]-6),]
# If fall chum column does not exist, add fall chum  
   if(is.na(names(run)['FCHUM'])){run$FCHUM<-0} 
  # Remove the last 6 lines
  rvar <- rvar[1:(dim(rvar)[1]-2),]
# If fall chum column does not exist, add fall chum  
  if(is.na(names(rvar)['FCHUM'])){rvar$FCHUM<-0}
  } else{
  run <- read.csv(file.path(wd_Plt,paste0(Pilot_Run,year,'.csv')),stringsAsFactors = FALSE)
  rvar <- read.csv(file.path(wd_Plt,paste0(Pilot_Var,year,'.csv')),stringsAsFactors = FALSE)
  }
# change Date read as date 
  run$Date <- as.Date(run$Date,'%m/%d/%Y')
  rvar$Date <- as.Date(rvar$Date,'%m/%d/%Y')
# Change NA to zero 
  run[is.na(run)] <- 0
  rvar[is.na(rvar)] <- 0
# Sum Chum run 
  run$Run <- run$SCHUM+run$FCHUM
# Sum Chinook Variance
  rvar$Var <- rvar$SCHUM+rvar$FCHUM
# Combine run and variance by date 
  Pilot <- merge(run[,c('Date','Run')],rvar[,c('Date','Var')], by='Date',all=TRUE)
# Add Year column
  Pilot$Year <- year
# Get strata data 
  rstr.y <- rstr[which(rstr$Year==year),]
# extract start date of each sampling stratum
  if(isTRUE(inSeason)){
  breaks <- rstr.y$Strata_End_Date
  breaks <- c(min(Pilot$Date),breaks+1)
  }else{
# Adjust first day 
  breaks <- rstr.y$Strata_Start_Date
#Add first and last dates 
  if(year < 2004) {
# 1999-2002 sampling started and ended in the middle of the season 
# The first day of the strata is the first sampling date 
# The last day of the strata is the the last sampling date  
  breaks <- c(breaks, max(rstr.y$Strata_End_Date)+1)
  } else if (year < 2008) {
# 2004-2007 sampling started in the middle of the season and ended close to the end of the season 
# The first day of the strata is the first sampling date 
# The last day of the strata is the the last sampling date or the end of the season   
    breaks <- c(breaks, max(max(Pilot$Date),max(rstr.y$Strata_End_Date))+1) 
  } else {
# 2008-present sampling strted and  ended close to the begining and the end of the season 
# The first day of the strata is the first date of the season  
# The last day of the strata is the the last sampling date or the end of the season   
    breaks <- c(min(Pilot$Date),breaks[-1], max(max(Pilot$Date),max(rstr.y$Strata_End_Date))+1)   
   }
   }
# Add strata data to Pilot data 
  Pilot$Strata <- cut(Pilot$Date,breaks,right=FALSE,labels=FALSE)
# Pilot Passage dates out of sampling periods are NA strata 
# Change strata NA to zero (The dates before sampling started became strata 0. 
#  Pilot$Strata[is.na(Pilot$Strata)] <- 0 
# Create standard strata dates 
  stbreak <- stb(year)
# Add standard strata data to Pilot data 
  Pilot$stbreak <- cut(Pilot$Date,stbreak,right=FALSE,labels=FALSE)
# Create strata between summer and fall 
  sfbreak <- stsf(year)
  Pilot$sf <- cut(Pilot$Date,sfbreak,right=FALSE,labels=FALSE)
# Output data 
  out <- Pilot
}

read.Pilot.ins.data <- function(rstr,this.year){
# Read Pilot Station Run data
  run <- read.csv(file.path(wd_Ins,Pilot_Run),stringsAsFactors = FALSE)
# Remove the last 6 lines
  run <- run[1:(dim(run)[1]-6),]
# If fall chum column does not exist, add fall chum  
   if(is.na(names(run)['FCHUM'])){run$FCHUM<-0}
# Read Pilot Station Variance data 
  rvar <- read.csv(file.path(wd_Ins,Pilot_Var),stringsAsFactors = FALSE)
# Remove the last 6 lines
  rvar <- rvar[1:(dim(rvar)[1]-2),]
# If fall chum column does not exist, add fall chum  
  if(is.na(names(rvar)['FCHUM'])){rvar$FCHUM<-0}
# change Date read as date 
  run$Date <- as.Date(run$Date,'%m/%d/%Y')
  rvar$Date <- as.Date(rvar$Date,'%m/%d/%Y')
# Change NA to zero 
  run[is.na(run)] <- 0
  rvar[is.na(rvar)] <- 0
# Sum Chum run 
  run$Run <- run$SCHUM+run$FCHUM
# Sum Chinook Variance
  rvar$Var <- rvar$SCHUM+rvar$FCHUM
# Combine run and variance by date 
  Pilot <- merge(run[,c('Date','Run')],rvar[,c('Date','Var')], by='Date',all=TRUE)
# Add Year column
  Pilot$Year <- this.year
# Get strata data 
  rstr.y <- rstr[which(rstr$Year==this.year),]
# extract start date of each sampling stratum
  breaks <- rstr.y$Strata_End_Date
# Adjust first day 
  breaks <- c(min(Pilot$Date),breaks+1)   
# Add strata data to Pilot data 
  Pilot$Strata <- cut(Pilot$Date,breaks,right=FALSE,labels=FALSE)
# Pilot Passage dates out of sampling periods are NA strata 
# Remove NA strata 
# Change strata NA to zero (The dates before sampling started became strata 0. 
#  Pilot$Strata[is.na(Pilot$Strata)] <- 0 
# Create standard strata dates 
  stbreak <- stb(this.year)
# Add standard strata data to Pilot data 
  Pilot$stbreak <- cut(Pilot$Date,stbreak,right=FALSE,labels=FALSE)
# Create strata between summer and fall 
  sfbreak <- stsf(this.year)
  Pilot$sf <- cut(Pilot$Date,sfbreak,right=FALSE,labels=FALSE)
# Output data 
  out <- Pilot
}

