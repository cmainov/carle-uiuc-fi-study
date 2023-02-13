##------------------------------------------------------------
###   02-GENERATE TABLES 1 AND 2
###------------------------------------------------------------

library( tidyverse )

source( "R/utils.R" )

### (0.0) Read-in Wrangled Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d.epi <- readRDS( "../02-data-wrangled/01-data-scores.rds" )

# Again, note data are not hosted publicly on the repository. They are stored on my
# local machine for privacy concerns.

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (1.0) Table 1 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## NOTE: Table 1 will consist of three column summarizing the epidemiological characteristics of the study sample
# and stratified on food security status

## (1.1) Continuous variables that will be used to generate the table ##
these.1 <- c( "age", "yrs_since_dx", "number_household", "chaos_score", "financial_skills_index",
              "factg_total", "factg_pwb", "factg_ewb", "factg_swb", "factg_fwb",
              "pred.fv6.ce", "pred.fiber", "pred.pcf" )

## (1.2)  Their names in the table
tnames.1 <- c( "Age", "Years Since Cancer Diagnosis", "Household Size", "CHAOS Score",
               "Financial Skills Index", "FACT-G (Total)", "FACT-G (Physical)", "FACT-G (Emotional)", 
               "FACT-G (Social)", "FACT-G (Functional)", "F & V Cup Eq. (Adj. for age/sex)",
               "% Energy from Fiber", "% Energy from Fat" )

## (1.3) Categorical variables that will be used to generate the table and their names
these.2 <- c( "pat_sex","pat_race_ethnicity", "pat_income_new",
              "pat_marital_status", "pat_education", "financial_support",
              "tx_type", "pat_smoking_recode", "pat_alcohol_recode" )

tnames.2 <- c( "Sex", "Race/Ethnicity", "Income", "Marital Status", 
               "Highest Education Level Attained", "Financially Support Others Outside Home",
               "Treatment Type", "Smoking Status", "Drinking Status" )


## for-loop to generate table (continuous variables only)
# outer loop will loop through the three datasets (to generate three columns) and the inner loop
# will loop through the variables, which will result in a separate row for each variable in the table


d.in <- data.frame()  # initialize data.frame for loop to store rows of the table
d.in.fs <- data.frame()
d.in.fi <- data.frame()
for ( i in 1: length( these.1 ) ) {
  
  d.in <- rbind( d.in, tab1.var.mean( var.name = these.1[i],
                                      df = d.epi,
                                      table.var.name = tnames.1[i],
                                      strata.var = NULL,
                                      strata.level = NULL ) ) 
  
  # subset on food insecure
  d.in.fi <- rbind( d.in.fi, tab1.var.mean( var.name = these.1[i],
                                         df = d.epi,
                                         table.var.name = tnames.1[i],
                                         strata.var = "fi_binary",
                                         strata.level = "Low FI" ) ) 
  
  # subset on food secure
  d.in.fs <- rbind( d.in.fs, tab1.var.mean( var.name = these.1[i],
                                         df = d.epi,
                                         table.var.name = tnames.1[i],
                                         strata.var = "fi_binary",
                                         strata.level = "High FI" ) ) 
  
}



# for-loop for categorical variables

d.in.2 <- data.frame()     
d.in.fs.2 <- data.frame()
d.in.fi.2 <- data.frame()
for ( i in 1: length( these.2 ) ) {
  
  d.in.2 <- rbind( d.in.2, tab1.var.freq( var.name = these.2[i],
                                          df = d.epi,
                                          table.var.name = tnames.2[i],
                                          strata.var = NULL,
                                          strata.level = NULL ) )
  
  # subset on food insecure
  d.in.fi.2 <- rbind( d.in.fi.2, tab1.var.freq( var.name = these.2[i],
                                          df = d.epi,
                                          table.var.name = tnames.2[i],
                                          strata.var = "fi_binary",
                                          strata.level = "Low FI" ) ) 
  
  # subset on food secure
  d.in.fs.2 <- rbind( d.in.fs.2, tab1.var.freq( var.name = these.2[i],
                                             df = d.epi,
                                             table.var.name = tnames.2[i],
                                             strata.var = "fi_binary",
                                             strata.level = "High FI" ) ) 
}



# merge as rows and reorder rows for final presentation
c.1 <- cbind( d.in, d.in.fi, d.in.fs ) [, c(1,2,4,6) ] 
c.2 <- cbind( d.in.2, d.in.fi.2, d.in.fs.2 ) [, c(1,2,4,6) ] 

# bind and get table 1
t.1 <- rbind( c.1, c.2 )

# polish significant digits
for( i in c( 2:4 ) ){  
  
  t.1[,i] <- str_replace( t.1[,i], "\\(0\\)", "(0.0)" )          # a zero in parentheses
  
  t.1[,i] <- str_replace( t.1[,i], "(?<=\\(\\d)\\)", ".0)" )          # closing parentheses preceded by an opening parentheses followed by a single digit
  
  t.1[,i] <- str_replace( t.1[,i], "(?<=\\(\\d\\d)\\)", ".0)" )        # closing parentheses preceded by an opening parentheses followed by two digits
  
}

# colnames

fi.n <- table( d.epi$fi_binary )[2]
fs.n <- table( d.epi$fi_binary )[1]
n.tot <- sum( table( d.epi$fi_binary ) )

t.1 <- data.frame( t.1 )
colnames( t.1 ) <- c( "Characteristic", 
                      paste0( "Combined Sample (n = ", n.tot, ")" ),
                      paste0( "Food Insecure (n = ", fi.n, ")" ),
                      paste0( "Food Secure (n = ", fs.n, ")" ) )
                              

## Wilcoxon Rank Sum and Chi-Square/Fisher's Exact Test p values##

p.vals <- vector()
for( i in 1:length( these.1 ) ){
  
  ## Wilcoxon Rank Sum test for Categorical variables
  f1 <- formula( paste0( these.1[i], "~ fi_binary")) # write formula
  test <-  wilcox.test( f1, data = d.epi, alternative = "two.sided" )
  p.vals[i] <- test$p.value # store p value
  
  
  t.1[ which(t.1$Characteristic == tnames.1[i] ), "p" ] <- test$p.value
}

for (i in 1:length( these.2 ) ){
  
  ## Chi-square trest of independence for categorical variables
  two.tab <- table( eval( parse( text = paste0( "d.epi$", these.2[i]) ) ),
                                 d.epi$fi_binary)
  
  # fisher's exact test if there is at least one cell with a count less than 5
  if( sum( as.vector( two.tab ) < 5 ) > 0 ){
    test.cat <- fisher.test( two.tab, simulate.p.value = T )
  }
  
  # chi-square test if there are no cells with less than count of 5
  if( sum( as.vector( two.tab ) < 5 ) == 0 ){
    test.cat <- chisq.test( two.tab, simulate.p.value = T )
  }
  t.1[ which(t.1$Characteristic == tnames.2[i] ), "p" ] <- test.cat$p.value
  
}


## Clean up p-values column ##

t.1 <- t.1 %>%
  mutate( p = ifelse( p < 0.05 & p >= 0.01, paste0( round( p, 2), "*" ),
                      ifelse( p < 0.01, paste0( "< 0.01**" ), round( p, 2 ))))


t.1[,5] <- str_replace( t.1[,5], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
t.1[,5] <- str_replace( t.1[,5], "^1$", "0.99" ) # round down probabilities = 1

t.1[,5][ is.na( t.1[,5] ) ] <- ""
## Save Table 1 ##

write.table( t.1, "../04-tables-figures/01-table-1.txt", sep = "," )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### Table 2 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Table 2 will be similar to Table 1. We will stratify by two levels of each diet index (median-split)
# and look at the distribution of the covariates across those two levels to see if the diet indices may be correlated
# with any of the characteristics. Finally, we will add another dimension by making two tables--one for cases and one 
# for controls.


# first make variables splitting observations at the median
names.q2 <- paste0( c( "factg_total", "pred.fv6.ce", "pred.fiber", "pred.pcf" ), ".q2" )
i.names <- c( "factg_total", "pred.fv6.ce", "pred.fiber", "pred.pcf" )
i.names.2 <- c( "FACT-G", "F&V Cups", "% Energy Fiber", "% Energy Fat" )

# split outcome variables at the median for "high"/"low" categorization
for( i in 1:length(names.q2)){
  
  d.epi[, names.q2 ] <- quant_cut(var = i.names[i], df = d.epi, x = 2)
  
}



## Table 2 for Cases ##

# for-loop for continuous variables

t.out.1 <- list()
for( g in 1:length( i.names ) ) {
  
  d.out.3 <- list()                                               # initialize list to store columns of Table 2
  for ( j in 1:2 ) {                                              # index on 1st/2nd quantile
    
    d.in <- data.frame()     
    for ( i in 1: length( these.1 ) ) {
      
      d.in <- rbind( d.in, tab1.var.mean( var.name = these.1[i],
                                          df = d.epi,
                                          table.var.name = tnames.1[i],
                                          strata.var = names.q2[g],
                                          strata.level = c( 1,2 )[[j]] ) ) 
      
    }
    
    d.out.3[[j]] <- d.in
    
    
  }
  
  t.out.1[[g]] <- do.call( "cbind", d.out.3 )
  
}

# column bind controls and cases separate
final.bind <- do.call( "cbind", t.out.1 )[ , -c( 3, 5, 7, 9, 11, 13, 15 )]


# for-loop for categorical variables

t.out.2.cat <- list()
for( g in 1:length( names.q2 ) ) {
  
  d.out.4.cat <- list()
  for ( j in 1:2 ) {                                              # index on case or control status
    
    d.in.cat <- data.frame()
    for ( i in 1: length( these.2 ) ) {
      
      d.in.cat <- rbind( d.in.cat, tab1.var.freq( var.name = these.2[i],
                                                  df = d.epi,
                                                  table.var.name = tnames.2[i],
                                                  strata.var = names.q2[g],
                                                  strata.level = c( 1,2 )[[j]] ) )
      
    }
    
    d.out.4.cat[[j]] <- d.in.cat
    
  }
  
  t.out.2.cat[[g]] <- do.call( "cbind", d.out.4.cat )
  
}

# column bind controls and cases separate
final.bind.cat <- do.call( "cbind", t.out.2.cat )[ , -c( 3, 5, 7, 9, 11, 13, 15 )]


## (Put Together Final Table 2 ###

t.2 <- bind_rows( final.bind, final.bind.cat )[ c( 1:5,14:61 ), ]


## polish significant digits ##

for( i in c( 2:9 ) ){  # columns 2:9 only
  t.2[,i] <- str_replace( t.2[,i], "\\(0\\)", "(0.0)" )          # a zero in parentheses
  
  t.2[,i] <- str_replace( t.2[,i], "(?<=\\(\\d)\\)", ".0)" )          # closing parentheses preceded by an opening parentheses followed by a single digit
  
  t.2[,i] <- str_replace( t.2[,i], "(?<=\\(\\d\\d)\\)", ".0)" )        # closing parentheses preceded by an opening parentheses followed by two digits
  
}

### Column names ### 

nms <- c( paste0( i.names.2, " M1" ), paste0( i.names.2, " M2" ) ) 

# arrange names in correct order
nms <- nms[ c( 1, 5, 2, 6, 3, 7, 4, 8 ) ]

colnames( t.2 ) <- c( "Characteristic", nms )

## Save Table 2 ##
write.table( t.2, "../04-tables-figures/02-table-2.txt", sep = "," )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

