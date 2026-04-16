#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.6  reshapeWL: Reshape from wide to long  -----  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
reshapeWL <- function(df,idvar,timevar,v.names){
  dfname <- names(df)[names(df)!=idvar]
  out <- reshape(df,direction='long',idvar=idvar,varying=dfname,
                 timevar = timevar,v.names=v.names,times=dfname)
  return(out)}  

#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.7 reshapeLW: Reshape from wide to long  -----  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
reshapeLW <- function(df,idvar=idvar,v.names=v.names,timevar=timevar){
  dfs <- df[c(idvar,timevar,v.names)]
   vhead <-paste0(v.names,'.')  
   w <- reshape(dfs,direction='wide',timevar=timevar,v.names=v.names,idvar=idvar)
   wname <- names(w)[-c(1:length(idvar))]
   wname <- gsub(vhead,'',wname)
   names(w)[-c(1:length(idvar))] <- wname
   return(w)
}
