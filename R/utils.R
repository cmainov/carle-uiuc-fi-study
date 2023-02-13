###---------------------------------------------------
###   RESULT-GENERATING AND OTHER HELPER FUNCTIONS
###---------------------------------------------------

#####################################
########## %notin% operator #########
#####################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
`%notin%` <- Negate( `%in%` )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

#####################################
###### list to rbind function #######
#####################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
list_it <- function( list ) {
  do.call( "rbind" , list )
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

####################################################################################################
#################################### Quantile Cutting Function #####################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

quant_cut<-function(var,x,df){
  
  xvec<-vector() # initialize null vector to store
  
  for (i in 1:x){
    xvec[i]<-i/x
  }
  
  qs<-c(min(df[[var]],na.rm=T), quantile(df[[var]],xvec,na.rm=T))
  
  df[['new']]=x+1 # initialize variable
  
  for (i in 1:(x)){
    df[['new']]<-ifelse(df[[var]]<qs[i+1] & df[[var]]>=qs[i],
                        c(1:length(qs))[i],
                        ifelse(df[[var]]==qs[qs==max(qs)],x,df[['new']]))
  }
  
  return(df[['new']])
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------







####################################################################################################
#################################### Trend Variable Function #######################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

trend_func<-function(rank.var,cont.var,df,trend.var,x){
  
  df[[trend.var]] = 1
  
  medians<-vector()
  
  for (i in 1:x){
    
    newdf<-df[df[[rank.var]]==i,]
    
    medians[i]<-median(newdf[[cont.var]],na.rm=T)
    
    df[[trend.var]]<-ifelse(df[[rank.var]]==i,medians[i],df[[trend.var]])
    
  }
  
  return(df)
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




####################################################################################################
################################## Table 1 (Continuous Variables) ##################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

tab1.var.mean<-function(var.name,df,table.var.name,strata.var=NULL,strata.level=NULL){
  
  if(is.null(strata.var)==T){
    df<- data.frame( df )
  }
  else {
    df<-data.frame( df[df[[strata.var]]==strata.level,] )
  }
  
  
  rowvar.name<-c(table.var.name)
  rowvar.mean<-c(paste0(round(mean(df[[var.name]],na.rm=T),digits=1),' (',round(sd(df[[var.name]], na.rm = T),digits=1),')'))
  
  partial.table<-data.frame(cbind(rowvar.name,rowvar.mean))
  colnames(partial.table)<-c('Characteristic','Frequency (%) or Mean (SD)')
  return(partial.table)
  
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




####################################################################################################
################################# Table 1 (Categorical Variables) ##################################
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

tab1.var.freq<-function(var.name,df,table.var.name,strata.var=NULL,strata.level=NULL){ #var.name is quoted string of how
  #variable is stored in dfset, df=is the dfset stored in R environment
  #and table.var.name is a character string of how that section of table 1 should be titled
  #strata.var is the variable, quoted, to stratify on, and strata.level is a quoted string
  #containing the level of strata.var that is to be examined
  
  
  df <- data.frame( df )
  
  
  
  if (is.null(strata.var)==T){
    
    df2<-data.frame( df )
    
    df2[[ var.name ]] <- factor( df2[[var.name]] )
  }
  
  if (is.null(strata.var)==F) {
    df2<-data.frame( df[df[[strata.var]] %in% strata.level,] )
    
    df2[[ var.name ]] <- factor(df2[[ var.name ]], 
                                levels =c( levels( factor( df2[[var.name]] ) ), levels( factor( df[[var.name]] ) )[which( levels( factor( df[[var.name]] ) ) %notin% levels( factor( df2[[var.name]] ) ) ) ] ))
    
  }
  
  rowvar.name<-vector()
  levelvec<-levels(df2[[var.name]])[order(levels(df2[[var.name]]))]
  
  for (i in 1:length(levelvec)){
    rowvar.name[i]<-paste0(levelvec[i])
  }
  
  # add to variable name header
  rowvar.name <- c(table.var.name,rowvar.name)
  
  rowvar.freq<-vector()
  for (i in 1:length(levelvec)){
    rowvar.freq[i]<-paste0(table(df2[[var.name]])[levelvec[i]],' (',round(100*table(df2[[var.name]])[levelvec[i]]/sum(table(df2[[var.name]]), na.rm = T),digits=1),')')
  }
  
  rowvar.freq<-c('',rowvar.freq)
  rowvar.freq<-ifelse(rowvar.freq=='NA (NA)',paste0('0 (0.0)'),rowvar.freq)
  
  partial.table<-data.frame(cbind(rowvar.name,rowvar.freq))
  colnames(partial.table)<-c('Characteristic','Frequency (%) or Mean (SD)')
  return(partial.table)
  
  
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
