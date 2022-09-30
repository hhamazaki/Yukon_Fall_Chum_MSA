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
read.Pilot.data <- function(rstr,year){
# Read Pilot Station Run data
  run <- read.csv(paste0(wd_Plt,Pilot_Run,year,'.csv'),stringsAsFactors = FALSE)
# Read Pilot Station Variance data 
  rvar <- read.csv(paste0(wd_Plt,Pilot_Var,year,'.csv'),stringsAsFactors = FALSE)
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
  breaks <- rstr.y$Strata_Start_Date
#Add last dates 
# 1999-2002 sampling ended before the end of the season so that the last day 
# of the sampling date is the last date
# Since 2004 sampling ended close to the  end of the season so that the last day 
# of sampling date is end of Pilot survey 
  if(year < 2004) {breaks <- c(breaks, max(rstr.y$Strata_End_Date)+1)
  } else { breaks <- c(breaks, max(max(Pilot$Date),max(rstr.y$Strata_End_Date))+1) }
# Add strata data to Pilot data 
  Pilot$Strata <- cut(Pilot$Date,breaks,right=FALSE,labels=FALSE)
# Pilot Passage dates out of sampling periods are NA strata 
# Change strata NA to zero (The dates before sampling started became strata 0. 
  Pilot$Strata[is.na(Pilot$Strata)] <- 0 
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

