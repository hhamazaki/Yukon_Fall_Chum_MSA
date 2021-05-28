#===============================================================================
#  Function sets: 
#===============================================================================
#-------------------------------------------------------------------------------
#  stb:  Set Standard Stock and Strata 
#-------------------------------------------------------------------------------
# Set standard strata dates 
stb <- function(year){
  stdates <- c('05/15/','06/22/','06/29/','07/10/','07/19/','07/28/','08/06/','08/15/','08/25/','09/15/')
  stdates <- paste0(stdates,year)
  stdates <- as.Date(stdates,'%m/%d/%Y')
  return(stdates) 
}

# Figure date titles
stbl <- c('6/1-\n6/21','6/22-\n6/28','6/29-\n7/9','7/10-\n7/18','7/19-\n7/27','7/28-\n8/5',
          '8/6-\n8/14','8/15-\n8/24','8/25-\n9/7')
#-------------------------------------------------------------------------------
#  stsf:  Set Standard Summer and Fall Seasn 
#-------------------------------------------------------------------------------
# Standard Summer vs. fall date: July 19 is the standard summer/fall separate date
stsf <- function(year){
  stdates <- paste0(c('05/15/','07/19/','9/30/'),year)
  stdates <- as.Date(stdates,'%m/%d/%Y')
  return(stdates) 
} 

#-------------------------------------------------------------------------------
#  grpclean: 
#  This function isolates primary stock group
#  NA seocndary stock group data if present
#-------------------------------------------------------------------------------
grpclean <- function(datM,Year=NA) {
  if(is.na(Year)){
    datM[which(datM$Year <= 2002),as.character(setdiff(stgrpID,stgrp99))] <- NA
    datM[which(datM$Year > 2002 & datM$Year <= 2007),as.character(setdiff(stgrpID,stgrp04))] <- NA 
    datM[which(datM$Year > 2007),as.character(setdiff(stgrpID,stgrp08))] <- NA 
  } else if (Year<=2002){
    datM[,as.character(setdiff(stgrpID,stgrp99))] <- NA
  } else if (Year > 2002 & Year <= 2007){
    datM[,as.character(setdiff(stgrpID,stgrp04))] <- NA 
  } else if (Year > 2007){
    datM[,as.character(setdiff(stgrpID,stgrp08))] <- NA 
  }
  return(datM)
}

#-------------------------------------------------------------------------------
#  add.sum  to calculate secondary stock groups
#  NA if component stocks are missing
#-------------------------------------------------------------------------------
add.sum <- function(datM,Year=NA) {
  #	5 Total Middle: Baseline (1999-2002), NA (2004-2007)  7 Uppkoy+Main +  8 Tanana (2008-present)
  datM[,'5'] <- ifelse(is.na(datM[,'5']),rowSums(datM[,c('7','8')]),datM[,'5']) 
  #	10 (P) Tanana Fall (2004-present):  
  #		(1999-2002)  20 Toklat + 21 Upper Tanana
  datM[,'10'] <- rowSums(datM[,c('10','20','21')],na.rm=TRUE) 
  #   1 Upper Canada (1999-present): 3 White + 6 Teslin
  id1 <-  rowSums(datM[,c('3','6')])  # Upper CA	 
  #	2 Total Summer (1999-present): 
  #		(1999-2002)  4 Lower Summer + 5 Middle Sumer  
  #		(2004-2007)  23 Lower Summer+UpperKoy+Main + 8 Tanana Summer  
  #		(2007-present)  4 Lower Summer + 5dat Middle Summer 
  if(is.na(Year)){ 	
    id2 <-  ifelse(datM$Year > 2002 & datM$Year <= 2007, 
                   rowSums(datM[,c('4','8')]),
                   rowSums(datM[,c('4','5')]))
  } else if(Year > 2002 & Year <= 2007){ 
    id2 <- rowSums(datM[,c('4','8')])
  } else {
    id2 <- rowSums(datM[,c('4','5')])
  }
  
  #	12 Border CA (2004-present):  13 Porcupine CA + 14 Mainstem CA 				
  id12 <-	rowSums(datM[,c('13','14')]) 
  #	15 Fall US (2004-present)
  #		(2004- present): 10 Tanana Fall + 11 Border US	
  id15 <-	rowSums(datM[,c('10','11')] ) 
  #	16 Mainstem+Upper CA (2004-present): 1 Upper Canada + 14 Mainsten CA
  id16 <- id1+datM[,c('14')] 
  #	18 Total USA (2004-present): 2 Total Summer + 15 Fall US
  id18 <-	id2 + id15  
  #	19 Total Canada (2004-present): 1 Upper Canada + 12 Border CA 
  id19 <- id1 + id12
  #   9 Total Fall (1999-present)
  #		(1999-2002) 1 Upper Canada + 10 Tanana Fall + 22 Border US&CA
  #		(2004-present):  15 Fall US + 19 Total Canada  
  if(is.na(Year)){ 	
    id9 <- ifelse(datM$Year < 2004,
                  id1+rowSums(datM[,c('10','22')]),
                  id15 + id19)
  } else if (Year < 2004){
    id9 <- id1+rowSums(datM[,c('10','22')])
  } else{
    id9 <- id15 + id19
  }
  #	17 Border US+Canada (1999-present): 
  #		(1999-2002) 1 Upper Canada + 22 Border US&CA 
  #		(2004-present) 11 Border US + 19 Total Canada
  if(is.na(Year)){ 
    id17 <-	ifelse(datM$Year < 2004,
                   id1+datM[,c('22')],
                   datM[,c('11')] + id19)
  } else if (Year < 2004){
    id17 <- id1+datM[,c('22')]
  } else {
    id17 <- datM[,c('11')] + id19
  }
  #   23 (P) Lower Summer+UpperKoy+Mai (2004-present)
  #		(2004-2007) move from 4 
  #		(2008+present)  4 Lower Summer +7 UpperKoy+Mai  
  if(is.na(Year)){ 
    id23 <- ifelse(datM$Year > 2002 & datM$Year <= 2007,
                   datM[,'4'],
                   rowSums(datM[,c('4','7')]))
  } else if(Year > 2002 & Year <= 2007){
    id23 <- datM[,'4']
  } else {
    id23 <- rowSums(datM[,c('4','7')])
  }
  # Clean group 4 from 2004-2007
  if(is.na(Year)){ 
    datM[which(datM$Year > 2002 & datM$Year <= 2007),'4'] <- NA 	
  } else if(Year > 2002 & Year <= 2007){ 
    datM[,'4'] <- NA 	
  }  	
  
  # Combine all summed stock groups    
  temp <- cbind(id1,id2,id9,id12,id15,id16,id17,id18,id19,id23)
  # Add grpID name 
  colnames(temp) <- c('1','2','9','12','15','16','17','18','19','23')
  # Combine with datM	
  datM <- cbind(datM,temp)
  return(datM)
}

#-------------------------------------------------------------------------------
#  summer.p, fall.p  proporion of individual stock among summer/fall groups
#  NA if component stocks are missing
#  cn: number of colums that has Year and strata info 
#-------------------------------------------------------------------------------
summer.p <-function(datM,cn=NA){
  if(is.na(cn)){
    #  Divide by total summer 
    mp <- datM[,c('4','5','8')]/datM[,'2'] 
  } else{
    cl <- names(datM)[c(1:cn)]
    mp <- cbind(datM[,cl],datM[,c('4','5','8')]/datM[,'2'])
    names(mp) <- c(cl,'4','5','8')
  }
  return(mp)
}

fall.p <- function(datM,cn=NA){
  if(is.na(cn)){
    #  Divide by total summer 
    mp <- datM[,c('11','19','10')]/datM[,'9'] 
  }else{
    cl <- names(datM)[c(1:cn)]
    mp <- cbind(datM[,cl],datM[,c('11','19','10')]/datM[,'9'])
    names(mp) <- c(cl,'11','19','10')
  }
  return(mp)
}




#-------------------------------------------------------------------------------
#  tcol function:  This is used to make shade colors 
#-------------------------------------------------------------------------------
tcol <- function(color, percent = 50, name = NULL) {
  #	  color = color name
  #	   percent = % transparency (0-100) 
  #	   name = an optional name for the color
  # 		Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  return(t.col)
}

#-------------------------------------------------------------------------------
#  s.functions  to summarize output standard descriptive statistics
#  ci: Confidence interval percentage
#  m4, m5 numerical CI lower and upper range 
#-------------------------------------------------------------------------------
s.functions <- function(datamatrix,ci){
  pci <- (1-ci/100)/2
  #   m1 <- apply(datamatrix,2,mean)  # Mean 
  #   m2 <- apply(datamatrix,2,median) # Median 
  #   m3 <- apply(datamatrix,2,sd)     # SD 
  m4 <- apply(datamatrix,2,function(x) quantile(x,pci,na.rm=TRUE))    # Lower CI
  m5 <- apply(datamatrix,2,function(x) quantile(x,1-pci,,na.rm=TRUE))  # Upper CI  
  #   m <- t(rbind(m2,m4,m5,m1,m3)) 
  #   colnames(m) <- c('Median','LCI','UCI','Mean','SD')
  m <- t(rbind(m4,m5)) 
  colnames(m) <- c('LCI','UCI')   
  return(m)  
}

