#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.6  reshapeWL: Reshape from wide to long  -----  
#  idvar:  Variable name(s) on the column
#  timevar:  Column name for a new id column
#  v.names:  Column nume for a new value column 
#  Note: Remove all extra columns you don't use for reshaping 
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
reshapeWL <- function(df,idvar,timevar,r.names,v.names='value'){
  df <- df[,c(idvar,r.names)]
  out <- reshape(df,direction='long',idvar=idvar,varying=r.names,
                 timevar = timevar,v.names=v.names,times=r.names)
  rownames(out) <- NULL				 
  return(out)
  }  

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.7 reshapeLW: Reshape from wide to long  -----  
#  idvar:  Variable names on that stay
#  timevar:  Column names that becomes wide column 
#  v.names:  Valune Column name that fill the data  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
reshapeLW <- function(df,idvar,timevar,v.names){
   df <- df[,c(idvar,timevar,v.names)]
   if(length(timevar)>1){
   df$times <- do.call(paste, c(df[,timevar], sep=".")) } else {
   df$times <- df[,timevar]}
   df <- df[,c(idvar,'times',v.names)]
   w <- reshape(df,direction='wide',idvar=idvar,timevar='times',v.names=v.names)
   wname <- unique(df$times)
   names(w)[-c(1:length(idvar))] <- wname
   rownames(w) <- NULL	
   return(w)
}
