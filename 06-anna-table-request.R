### This is a special table Anna asked me to put together for a dept
### presentation at KU Med

library( tidyverse )

source( "R/utils.R" )

### (0.0) Read-in Wrangled Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d.epi <- readRDS( "../02-data-wrangled/01-data-scores.rds" ) %>%
  mutate( time.since.cat = ifelse( yrs_since_dx > 3, ">3 years",
                                   ifelse( yrs_since_dx <= 3, "<= 3 years", NA ) ),
          hhsizecat = ifelse( number_household >= 3, ">=3",
                              ifelse( number_household < 3, "< 3", NA ) ),
          financial_status_effect = ifelse( financial_status_effect %in% c( "Quite a bit", "Very much"), "Quite a bit/Very much",
                                            ifelse( financial_status_effect %in% c( "A little", "Somewhat","Not at all"), "Little/Somewhat/Not at all",
                                                    financial_status_effect ) ) )

# Again, note data are not hosted publicly on the repository. They are stored on my
# local machine for privacy concerns.

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




## NOTE: Table 1 will consist of three column summarizing the epidemiological characteristics of the study sample
# and stratified on food security status

## (1.1) Continuous variables that will be used to generate the table ##
these.1 <- c( "factg_total", "factg_pwb", "factg_ewb", "factg_swb", "factg_fwb" )

## --------- End Subsection --------- ##


## (1.2)  Their names in the table
tnames.1 <- c( "Financial Skills Index", "FACT-G (Total)", "FACT-G (Physical)", "FACT-G (Emotional)", 
               "FACT-G (Social)", "FACT-G (Functional)" )

## --------- End Subsection --------- ##


## (1.3) Categorical variables that will be used to generate the table and their names
these.2 <- c( "pat_sex","time.since.cat", "hhsizecat", "worse_financial_status",
              "financial_status_effect" )

tnames.2 <- c( "Sex", "Time Since CA Diagnosis", "Household Size",
               "Since your cancer diagnosis, are you worse off regarding your Financial Status?",
               "How much is this due to your cancer diagnosis and treatment?" )

## --------- End Subsection --------- ##


## (1.4) Generate tables with internal helper function ##

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

## --------- End Subsection --------- ##


## (1.5) Clean up tables ##

# merge as rows and reorder rows for final presentation
c.1 <- cbind( d.in, d.in.fi, d.in.fs ) [, c(1,2,4,6) ] 
c.2 <- cbind( d.in.2, d.in.fi.2, d.in.fs.2 ) [, c(1,2,4,6) ] 

# bind and get table 1
t.1 <- rbind( c.2, c.1 )

# polish significant digits
for( i in c( 2:4 ) ){  
  
  t.1[,i] <- str_replace( t.1[,i], "\\(0\\)", "(0.0)" )          # a zero in parentheses
  
  t.1[,i] <- str_replace( t.1[,i], "(?<=\\(\\d)\\)", ".0)" )          # closing parentheses preceded by an opening parentheses followed by a single digit
  
  t.1[,i] <- str_replace( t.1[,i], "(?<=\\(\\d\\d)\\)", ".0)" )        # closing parentheses preceded by an opening parentheses followed by two digits
  
}

# column names

fi.n <- table( d.epi$fi_binary )[2]
fs.n <- table( d.epi$fi_binary )[1]
n.tot <- sum( table( d.epi$fi_binary ) )

t.1 <- data.frame( t.1 )
colnames( t.1 ) <- c( "Characteristic", 
                      paste0( "Combined Sample (n = ", n.tot, ")" ),
                      paste0( "Food Insecure (n = ", fi.n, ")" ),
                      paste0( "Food Secure (n = ", fs.n, ")" ) )

## --------- End Subsection --------- ##


## (1.6) Wilcoxon Rank Sum and Chi-Square/Fisher's Exact Test p values ##

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

## --------- End Subsection --------- ##


## (1.7) Clean up p-values column ##

t.1 <- t.1 %>%
  mutate( p = ifelse( p < 0.05 & p >= 0.01, paste0( round( p, 2), "*" ),
                      ifelse( p < 0.01, paste0( "< 0.01**" ), round( p, 2 ))))


t.1[,5] <- str_replace( t.1[,5], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
t.1[,5] <- str_replace( t.1[,5], "^1$", "0.99" ) # round down probabilities = 1

t.1[,5][ is.na( t.1[,5] ) ] <- ""

## --------- End Subsection --------- ##


## (1.8) Save Table 1 ##

write.table( t.1, "../04-tables-figures/11-anna-table-1-request.txt", sep = "," )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

