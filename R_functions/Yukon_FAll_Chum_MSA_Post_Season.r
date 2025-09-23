#'##############################################################################
#   Yukon River Fall Chum Salmon: Post-Season Stratified estimate 
#   Author:  Toshihide "Hammachan" Hamazaki
#   Date: 
#   Description
#   This program reads Pilot Station genetic and Run data and estimates 
#   Run and stratified proportion by strata and run 
#'##############################################################################
#'------------------------------------------------------------------------------
#' Output file names 
#'------------------------------------------------------------------------------ 
# Output Summary EXCEL file name 
sumxlsx <- paste0('Yukon_Pilot_Chum_MSA_',Sys.Date(),'.xlsx')
# Output JTC EXCEL file name 
jtcxlsx <- paste0('JTC_MSA_',Sys.Date(),'.xlsx')

# Output Annual Stock proportions by summer and fall:
sf_p <- paste0('Pilot_sfp_',this.year,'.csv')
# Output Annual Summer vs. Fall Stock proportions by standard break:
sf_t <- paste0('Pilot_sft_',this.year,'.csv')
# Output min_max file name 
min_max <- paste0('Pilot_d_min_max_',this.year,'.csv')
if(filesave){
# Save all MSA proportion data 
write.csv(MSA,file.path(wd_MSA,paste0('Pilot_MSA_prop_',min(years),'-',max(years),'.csv')), na='',row.names=FALSE)
# Save all MSA strata data 
write.csv(MSA,file.path(wd_MSA,paste0('Pilot_MSA_strata_',min(years),'-',max(years),'.csv')), na='',row.names=FALSE)
}

#'------------------------------------------------------------------------------
#  CI update for overwrite 
#'------------------------------------------------------------------------------
  if(isTRUE(ciOverwrite)){
  temp.ci <- list()
  mlist <- list()
  for(j in 1:ny){
  # Stock prop
  MSA.y <- MSAL[MSAL$Year==years[j],]
  # Pilot st 
  Temp.st <- Pilot.st.y[Pilot.st.y$Year==years[j],]
  temp.ci <- sim.ci(MSA.y,Temp.st,sgrpIDn,nrep,ci,years[j])
  mlist <- sumdata(temp.m[temp.m$Year==years[j],],temp.ci) 
  write.csv(mlist,file.path(wd_Sum,paste0('Pilot_MSA_Sum_',years[j],'.csv')), na='',row.names=FALSE)
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
  if(filesave){  
  write.xlsx(EXlist,file.path(wd_Out,sumxlsx),rowNames=FALSE)
  }
#'------------------------------------------------------------------------------
###  EXCEL Annual JTC MSA table output ------------
#'------------------------------------------------------------------------------
# Change Pilot Data from list to data.frame 
  Pilot.df <- as.data.frame(do.call(rbind,EXlist))

if(filesave){
#'------------------------------------------------------------------------------
###  Stratification Output  ------------
#'------------------------------------------------------------------------------
### Strata 102 -----------------------------------------------------------------
# Group ID: 2,9
st102.s <- Pilot.df[with(Pilot.df,which(Strata==102 & grpID %in% c(2,9,10,11,15,19,16,13))), ]

### JTC Table A7  ---------------
# Stock proportions and number: 'Summer','Fall','Tanana Fall','Border U.S.','Fall U.S.','Canada','Mainstem+ Upper Canada','Porcupine'
# Change long to wide 
JTC.A7.p <-dcast(st102.s, Year~grpID,value.var='p') 
# Extract and arrange columns 
JTC.A7.p <- JTC.A7.p[,c('Year','2','9','10','11','15','19','16','13')]
names(JTC.A7.p)[-1] <- c('Summer','Fall','Tanana Fall','Border U.S.','Fall U.S.','Canada','Mainstem+ Upper Canada','Porcupine')
### JTC Table A7 (Number) ---------------
JTC.A7.n <-dcast(st102.s, Year~grpID,value.var='mean') 
JTC.A7.n <- JTC.A7.n[,c('Year','2','9','10','11','15','19','16','13')]
names(JTC.A7.n)[-1] <- c('Summer','Fall','Tanana Fall','Border U.S.','Fall U.S.','Canada','Mainstem+ Upper Canada','Porcupine')

### Table 108 ----------------
# Stock proportions and number: Fall: Tanana,Border US, Canada, Mainstem Canada, Porcupine
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

### Table 103 ----------------
# Stock proportions and number:Summer:  Lower, Middle, Tanana
# Extract strata  103
st103 <- Pilot.df[Pilot.df$Strata==103,]
# Change long to wide
st103 <- dcast(st103, Year~GroupName,value.var='p')
# Extract total Summer and Merge 
T.summer <-  Pilot.df[Pilot.df$Strata==100 & Pilot.df$grpID==2,]
st103.n <- merge(st103,T.summer[,c('Year','mean')], by = 'Year')
# Multiply to the numbers 
st103.n[,c(2:4)] <-st103.n[,c(2:4)]*st103.n[,6] 

### Table 103: summer ------
out.excel <- list()
out.excel$JTC.A7.p.St102 <- JTC.A7.p
out.excel$JTC.A7.n.St102 <- JTC.A7.n
out.excel$St108.p <- st108.w
out.excel$St108.n <- st108.wm
out.excel$St103.p <- st103
out.excel$St103.n <- st103.n[,-5]
write.xlsx(out.excel,file.path(wd_Out,jtcxlsx),rowNames=FALSE)
    
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
