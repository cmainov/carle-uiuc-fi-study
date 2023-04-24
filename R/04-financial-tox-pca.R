
library( tidyverse )
library( FactoMineR ) # for MCA procedure
library( factoextra ) # for biplot
source( "R/utils.R" )

### (0.0) Read-in Wrangled Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d <- readRDS( "../02-data-wrangled/01-data-scores.rds" )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (1.0) Descriptives for Financial Toxicity Variables ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## (1.1) Recode some variables ##

d.2 <- d %>%
  # recode levels for mark all that apply questions
  mutate( how_pay_out_of_pocket___1 = ifelse( how_pay_out_of_pocket___1 == "Checked", 
                                              "I used my income and/or savings", 
                                              how_pay_out_of_pocket___1 ),
          how_pay_out_of_pocket___2 = ifelse( how_pay_out_of_pocket___2 == "Checked", 
                                              "I borrowed money from family or friends", 
                                              how_pay_out_of_pocket___2 ),
          how_pay_out_of_pocket___3 = ifelse( how_pay_out_of_pocket___3 == "Checked", 
                                              "I borrowed money against my house", 
                                              how_pay_out_of_pocket___3 ),
          how_pay_out_of_pocket___4 = ifelse( how_pay_out_of_pocket___4 == "Checked", 
                                              "I left some of my medical bills unpaid", 
                                              how_pay_out_of_pocket___4 ),
          how_pay_out_of_pocket___5 = ifelse( how_pay_out_of_pocket___5 == "Checked", 
                                              "I increased my credit card debt", 
                                              how_pay_out_of_pocket___5 ),
          how_pay_out_of_pocket___6 = ifelse( how_pay_out_of_pocket___6 == "Checked", 
                                              "Other", 
                                              how_pay_out_of_pocket___6 ),
          
          # dichotomize "How much is this due to your cancer?" questions
          health_insurance_effect = ifelse( health_insurance_effect %in% c( "Quite a bit", "Very much"), "Quite a bit/Very much",
                                            ifelse( health_insurance_effect %in% c( "A little", "Somewhat","Not at all"), "Little/Somewhat/Not at all",
                                                    health_insurance_effect ) ),
          employment_status_effect = ifelse( employment_status_effect %in% c( "Quite a bit", "Very much"), "Quite a bit/Very much",
                                            ifelse( employment_status_effect %in% c( "A little", "Somewhat","Not at all"), "Little/Somewhat/Not at all",
                                                    employment_status_effect ) ),
          financial_status_effect = ifelse( financial_status_effect %in% c( "Quite a bit", "Very much"), "Quite a bit/Very much",
                                            ifelse( financial_status_effect %in% c( "A little", "Somewhat","Not at all"), "Little/Somewhat/Not at all",
                                                    financial_status_effect ) ),
          
          # re-code pat_treatment into binary, yes/no, variable for treatment status
          pat_treatment = ifelse( pat_treatment %in% c( "Currently receiving treatment for a cancer",
                                                        "Receiving treatment for cancer that has returned" ), "Receiving treatment",
                                  ifelse( pat_treatment %in% c( "I am not receiving treatment" ), "Not receiving treatment",
                                          pat_treatment ) ),
          
          # recode out of pocket costs to binary (split at $5000)
          paid_out_of_pocket_binary = ifelse( paid_out_of_pocket %in% c( "less than $500", "$500 - $2000", "$2001 - 5000" ),
                                                 "<= $5000",
                                                ifelse( paid_out_of_pocket %in% c( "$5001 - $10,000", "more than $10,000" ),
                                                        "> $5000", paid_out_of_pocket ) ) )
          
          
          

## --------- End Subsection --------- ##


## (1.2) Select variables and generate table##

# these variables
these.tox <- c( "pat_treatment", "worse_insurance", "health_insurance_effect", "worse_employment_status",
             "employment_status_effect", "worse_financial_status", "financial_status_effect",
             "paid_out_of_pocket_binary", "how_pay_out_of_pocket___1", "how_pay_out_of_pocket___2",
             "how_pay_out_of_pocket___3", "how_pay_out_of_pocket___4", "how_pay_out_of_pocket___5", 
             "how_pay_out_of_pocket___6", "debt_from_treatment",
             "go_without_meds", "take_less_meds", "miss_dr_appoint" ) # note that we removed
# the "If other please specify" open-ended question from this set (variable #162)

# names for presentation in table
q.names <- c( "Treatment Status",
              "Since your cancer diagnosis, are you worse off regarding Health Insurance?",
              "How much is this due to your cancer diagnosis and treatment?",
              "Since your cancer diagnosis, are you worse off regarding your Employment Status?",
              "How much is this due to your cancer diagnosis and treatment?",
              "Since your cancer diagnosis, are you worse off regarding your Financial Status?",
              "How much is this due to your cancer diagnosis and treatment?",
              "How much have you paid out-of-pocket for medical expenses related to your cancer?",
              "How did you pay for these out-of-pocket expenses?--Selection 1",
              "How did you pay for these out-of-pocket expenses?--Selection 2",
              "How did you pay for these out-of-pocket expenses?--Selection 3",
              "How did you pay for these out-of-pocket expenses?--Selection 4",
              "How did you pay for these out-of-pocket expenses?--Selection 5",
              "How did you pay for these out-of-pocket expenses?--Selection 6",
              "Do you currently have debt from your cancer treatment?",
              "Because of the cost of your cancer treatment, did you have to go without medication?",
              "Because of the cost of your cancer treatment, did you have to take less than the fully prescribed amount of a prescription?",
              "Because of the cost of your cancer treatment, did you have to miss a doctor's appointment?")


# for-loop for categorical variables

d.in <- data.frame()     
d.in.fs <- data.frame()
d.in.fi <- data.frame()
for ( i in 1: length( these.tox ) ) {
  
  d.in <- rbind( d.in, tab1.var.freq( var.name = these.tox[i],
                                          df = d.2,
                                          table.var.name = q.names[i],
                                          strata.var = NULL,
                                          strata.level = NULL ) )
  
  # subset on food insecure
  d.in.fi <- rbind( d.in.fi, tab1.var.freq( var.name = these.tox[i],
                                                df = d.2,
                                                table.var.name = q.names[i],
                                                strata.var = "fi_binary",
                                                strata.level = "Low FS" ) ) 
  
  # subset on food secure
  d.in.fs <- rbind( d.in.fs, tab1.var.freq( var.name = these.tox[i],
                                                df = d.2,
                                                table.var.name = q.names[i],
                                                strata.var = "fi_binary",
                                                strata.level = "High FS" ) ) 
}

## --------- End Subsection --------- ##


## (1.3) Clean up tables ##

# merge as rows and reorder rows for final presentation
c.1 <- cbind( d.in, d.in.fi, d.in.fs ) [, c(1,2,4,6) ] 


# remove rows that have the "unchecked" response counts
c.1 <- c.1[ which( !str_detect( c.1[,1], "Unchecked" ) ), ]

# there were no subjects that responded in the affirmative to "borrowing money against my house", so will remove that row from table
c.1 <- c.1[ which( !str_detect( c.1[,1], "Selection 3" ) ), ]

## --------- End Subsection --------- ##

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### (2.0) Financial Toxicity Composite Score Using MCA ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (2.1) Select variables for MCA ##

# these variables will be used in the MCA (note: branching logic questions were removed given lower response rates to those)
these.mca <- c( "worse_insurance", "worse_employment_status", "worse_financial_status",
                "paid_out_of_pocket_binary", "how_pay_out_of_pocket___1", "how_pay_out_of_pocket___2",
                "how_pay_out_of_pocket___4", "how_pay_out_of_pocket___5", 
                "how_pay_out_of_pocket___6", "debt_from_treatment",
                "take_less_meds", "miss_dr_appoint", "pat_treatment" ) # note that we removed
# the "If other please specify" open-ended question from this set (variable #162) AND
# the how_pay_out_of_pocket___3 variable (see note above) in section 1.3
# note: we also removed go_without_meds from the MCA list of variables given that only 1 subject responded in the affirmative

# NOTE that variables with names ending in "_effect" having missings that reflect the skip pattern

## --------- End Subsection --------- ##


## (2.2) Fix variables that having meaningful skip patterns so they are not missing ##

# this section was removed

## --------- End Subsection --------- ##


## (2.3) MCA ##

mca.tox <- FactoMineR::MCA( d.2[ these.mca] )

# print eigenvalues
mca.tox$eig

# print biplot
fviz_mca_biplot( mca.tox, 
                 repel = TRUE, # to repel labels close to one another
                 ggtheme = theme_minimal())


# get scores
ind.coords <- get_mca_ind( mca.tox )

d.4 <- cbind( d.2,ind.coords$coord[ ,1:3] ) # first three dimensions only

# get contributions of each of the variables to the components
col.contrib <- get_mca_var( mca.tox )

# loadings matrix for components 1 and 2
col.load <- sweep(mca.tox$var$coord,2,sqrt(mca.tox$eig[1:ncol(mca.tox$var$coord),1]),FUN="/")[,1:2]
# reference: https://stats.stackexchange.com/questions/262703/r-multiple-correspondence-analysis-loadings 

## --------- End Subsection --------- ##


## (2.4) Add MCA dimension scores to table ##

these.mca.2 <- c( "Dim.1", "Dim.2", "Dim.3" )
mca.names <- c( "MCA Dimension 1", "MCA Dimension 2", "MCA Dimension 3" ) 

## for-loop to generate table (continuous variables only)
# outer loop will loop through the three datasets (to generate three columns) and the inner loop
# will loop through the variables, which will result in a separate row for each variable in the table


d.in <- data.frame()  # initialize data.frame for loop to store rows of the table
d.in.fs <- data.frame()
d.in.fi <- data.frame()
for ( i in 1: length( these.mca ) ) {
  
  d.in <- rbind( d.in, tab1.var.mean( var.name = these.mca.2[i],
                                      df = d.4,
                                      table.var.name = mca.names[i],
                                      strata.var = NULL,
                                      strata.level = NULL,
                                      round.to = 2 ) ) 
  
  # subset on food insecure
  d.in.fi <- rbind( d.in.fi, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.4,
                                            table.var.name = mca.names[i],
                                            strata.var = "fi_binary",
                                            strata.level = "Low FS",
                                            round.to = 2 ) ) 
  
  # subset on food secure
  d.in.fs <- rbind( d.in.fs, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.4,
                                            table.var.name = mca.names[i],
                                            strata.var = "fi_binary",
                                            strata.level = "High FS",
                                            round.to = 2 ) ) 
  
}


# merge as rows and reorder rows for final presentation
c.2 <- cbind( d.in, d.in.fi, d.in.fs ) [, c(1,2,4,6) ] 


# row bind with other table of toxicity variables
c.3 <- rbind( c.1, c.2 )

## (1.6) Wilcoxon Rank Sum and Chi-Square/Fisher's Exact Test p values ##

p.vals <- vector()
for( i in 1:length( these.mca.2 ) ){
  
   d.this <- data.frame( d.4 )
  ## Wilcoxon Rank Sum test for Categorical variables
  f1 <- formula( paste0( these.mca.2[i], "~ fi_binary")) # write formula
  test <-  wilcox.test( f1, data = d.this, alternative = "two.sided" )
  p.vals[i] <- test$p.value # store p value
  
  
  c.3[ which(c.3$Characteristic == mca.names[i] ), "p" ] <- test$p.value
}

for (i in 1:length( these.tox ) ){
  
  ## Chi-square test of independence for categorical variables
  two.tab <- table( eval( parse( text = paste0( "d.4$", these.tox[i]) ) ),
                    d.4$fi_binary)
  
  # fisher's exact test if there is at least one cell with a count less than 5
  if( sum( as.vector( two.tab ) < 5 ) > 0 ){
    test.cat <- fisher.test( two.tab, simulate.p.value = T )
  }
  
  # chi-square test if there are no cells with less than count of 5
  if( sum( as.vector( two.tab ) < 5 ) == 0 ){
    test.cat <- chisq.test( two.tab, simulate.p.value = T )
  }
  c.3[ which(c.3$Characteristic == q.names[i] ), "p" ] <- test.cat$p.value
  
}

## --------- End Subsection --------- ##


## (2.5) Clean up p-values column ##

c.3 <- c.3 %>%
  mutate( p = ifelse( p < 0.05 & p >= 0.01, paste0( round( p, 2), "*" ),
                      ifelse( p < 0.01, paste0( "< 0.01**" ), round( p, 2 ))))


c.3[,5] <- str_replace( c.3[,5], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
c.3[,5] <- str_replace( c.3[,5], "^1$", "0.99" ) # round down probabilities = 1

c.3[,5][ is.na( c.3[,5] ) ] <- ""

## --------- End Subsection --------- ##


## (2.6) Make ordination plot for MCA scores ##

ggplot( data = data.frame( d.4 ) %>% filter( !is.na(fi_binary)), mapping = aes( x = Dim.1, y = Dim.2 ) ) +
  geom_point( aes( col = fi_binary ) ) +
  stat_ellipse( aes( color = fi_binary ) ) +
  theme_classic() +
  theme( legend.title = element_blank())

ggsave( "../04-tables-figures/mca-ordination-plot.png" )

## --------- End Subsection --------- ##


## (2.8) Mean Time Since Dx row in table ##
fi.levs <- c( "", "Low FS","High FS" )

age.fr <- data.frame(NA)
for( i in seq_along( fi.levs ) ){
  
  if (i == 1){
  age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
               df = d.4,
               table.var.name = "Years Since Diagnosis",
               strata.var = NULL,
               strata.level = NULL ) )
  }
  
  if (i != 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.4,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = "fi_binary",
                                            strata.level = fi.levs[i] ) )
  }
}

# wilcoxon test
f.fi <- formula( paste0( "yrs_since_dx", "~ fi_binary")) # write formula
test <-  wilcox.test( f.fi, data = d.4, alternative = "two.sided" )
age.fr <- cbind( age.fr, round( test$p.value, 2) ) # store p value
age.fr <- setNames(  age.fr[,c(2,3,5,7,8)],names( c.3 ) )
# add to table

c.3 <- rbind( age.fr, c.3 )

## --------- End Subsection --------- ##


## (2.9) Save Table ##

# descriptive
write.table( c.3, "../04-tables-figures/05-table-financial-tox.txt", sep = "," )

# loadings
write.csv( col.load, "../04-tables-figures/07-mca-loadings.csv" )


# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (3.0) Create Table Stratified on Treatment Status ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (3.1) Generate tables ##

# for-loop for categorical variables

d.in.ts <- data.frame()     
d.in.nt <- data.frame()
d.in.t <- data.frame()
for ( i in 1: length( these.tox ) ) {
  
  d.in.ts <- rbind( d.in.ts, tab1.var.freq( var.name = these.tox[i],
                                      df = d.2,
                                      table.var.name = q.names[i],
                                      strata.var = NULL,
                                      strata.level = NULL ) )
  
  # subset on food insecure
  d.in.t <- rbind( d.in.t, tab1.var.freq( var.name = these.tox[i],
                                            df = d.2,
                                            table.var.name = q.names[i],
                                            strata.level = "Receiving treatment",
                                            strata.var = "pat_treatment" ) )  
  
  # subset on food secure
  d.in.nt <- rbind( d.in.nt, tab1.var.freq( var.name = these.tox[i],
                                            df = d.2,
                                            table.var.name = q.names[i],
                                            strata.level = "Not receiving treatment",
                                            strata.var = "pat_treatment" ) ) 
}

## --------- End Subsection --------- ##


## (3.2) Clean up tables ##

# merge as rows and reorder rows for final presentation
t.1 <- cbind( d.in.ts, d.in.t, d.in.nt ) [, c(1,2,4,6) ] 


# remove rows that have the "unchecked" response counts
t.1 <- t.1[ which( !str_detect( t.1[,1], "Unchecked" ) ), ]

# there were no subjects that responded in the affirmative to "borrowing money against my house", so will remove that row from table
t.1 <- t.1[ which( !str_detect( t.1[,1], "Selection 3" ) ), ]

## --------- End Subsection --------- ##


## (3.3) Add MCA dimension scores to table ##

d.in.ts<- data.frame()  # initialize data.frame for loop to store rows of the table
d.in.t <- data.frame()
d.in.nt <- data.frame()
for ( i in 1: length( these.mca ) ) {
  
  d.in.ts <- rbind( d.in.ts, tab1.var.mean( var.name = these.mca.2[i],
                                      df = d.4,
                                      table.var.name = mca.names[i],
                                      strata.var = NULL,
                                      strata.level = NULL,
                                      round.to = 2 ) ) 
  
  # subset on food insecure
  d.in.t <- rbind( d.in.t, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.4,
                                            table.var.name = mca.names[i],
                                           strata.level = "Receiving treatment",
                                           strata.var = "pat_treatment",
                                            round.to = 2 ) ) 
  
  # subset on food secure
  d.in.nt <- rbind( d.in.nt, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.4,
                                            table.var.name = mca.names[i],
                                            strata.level = "Not receiving treatment",
                                            strata.var = "pat_treatment",
                                            round.to = 2 ) ) 
  
}


# merge as rows and reorder rows for final presentation
t.2 <- cbind( d.in.ts, d.in.t, d.in.nt ) [, c(1,2,4,6) ] 


# row bind with other table of toxicity variables
t.3 <- rbind( t.1, t.2 )

## --------- End Subsection --------- ##


## (3.4) Wilcoxon Rank Sum and Chi-Square/Fisher's Exact Test p values ##

p.vals <- vector()
for( i in 1:length( these.mca.2 ) ){
  
  d.this <- data.frame( d.4 )
  ## Wilcoxon Rank Sum test for Categorical variables
  f1 <- formula( paste0( these.mca.2[i], "~ pat_treatment")) # write formula
  test <-  wilcox.test( f1, data = d.this, alternative = "two.sided" )
  p.vals[i] <- test$p.value # store p value
  
  
  t.3[ which(t.3$Characteristic == mca.names[i] ), "p" ] <- test$p.value
}

for (i in 1:length( these.tox ) ){
  
  ## Chi-square test of independence for categorical variables
  two.tab <- table( eval( parse( text = paste0( "d.4$", these.tox[i]) ) ),
                    d.4$pat_treatment)
  
  # fisher's exact test if there is at least one cell with a count less than 5
  if( sum( as.vector( two.tab ) < 5 ) > 0 ){
    test.cat <- fisher.test( two.tab, simulate.p.value = T )
  }
  
  # chi-square test if there are no cells with less than count of 5
  if( sum( as.vector( two.tab ) < 5 ) == 0 ){
    test.cat <- chisq.test( two.tab, simulate.p.value = T )
  }
  t.3[ which(t.3$Characteristic == q.names[i] ), "p" ] <- test.cat$p.value
  
}

## --------- End Subsection --------- ##


## (3.5) Clean up p-values column ##

t.3 <- t.3 %>%
  mutate( p = ifelse( p < 0.05 & p >= 0.01, paste0( round( p, 2), "*" ),
                      ifelse( p < 0.01, paste0( "< 0.01**" ), round( p, 2 ))))


t.3[,5] <- str_replace( t.3[,5], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
t.3[,5] <- str_replace( t.3[,5], "^1$", "0.99" ) # round down probabilities = 1

t.3[,5][ is.na( t.3[,5] ) ] <- ""

## --------- End Subsection --------- ##


## (3.6) Mean Time Since Dx row in table ##
pat.levs <- c( "", "Receiving treatment","Not receiving treatment" )

age.fr <- data.frame(NA)
for( i in seq_along( fi.levs ) ){
  
  if (i == 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.4,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = NULL,
                                            strata.level = NULL ) )
  }
  
  if (i != 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.4,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = "pat_treatment",
                                            strata.level = pat.levs[i] ) )
  }
}

# wilcoxon test
f.pat <- formula( paste0( "yrs_since_dx", "~ pat_treatment")) # write formula
test <-  wilcox.test( f.pat, data = d.4, alternative = "two.sided" )
age.fr <- cbind( age.fr, round( test$p.value, 2) ) # store p value
age.fr <- setNames(  age.fr[,c(2,3,5,7,8)],names( t.3 ) )
# add to table

t.3 <- rbind( age.fr, t.3 )

## --------- End Subsection --------- ##


## (3.7) Save Table ##

# descriptive
write.table( t.3, "../04-tables-figures/06-table-financial-tox-treatment.txt", sep = "," )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### (4.0) Stratify Tables in Section 1.0 by Treatment Status ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# i.e., this will create two tables

## (4.1) Generate Tables ##


# first create two dataset that subset the two categories of treatment status
d.rec <- d.4 %>% filter( pat_treatment == "Receiving treatment")
d.nrec <- d.4 %>% filter( pat_treatment == "Not receiving treatment")

# generate empty frame to hold results
d.in.rec <- data.frame()     
d.in.rec.fs <- data.frame()
d.in.rec.fi <- data.frame()
d.in.nrec <- data.frame()     
d.in.nrec.fs <- data.frame()
d.in.nrec.fi <- data.frame()
for ( i in 1: length( these.tox ) ) {
  
  
  ## receiving treatment ##
  d.in.rec <- rbind( d.in.rec, tab1.var.freq( var.name = these.tox[i],
                                      df = d.rec,
                                      table.var.name = q.names[i],
                                      strata.var = NULL,
                                      strata.level = NULL ) )
  
  # subset on food insecure
  d.in.rec.fi <- rbind( d.in.rec.fi, tab1.var.freq( var.name = these.tox[i],
                                            df = d.rec,
                                            table.var.name = q.names[i],
                                            strata.var = "fi_binary",
                                            strata.level = "Low FS" ) ) 
  
  # subset on food secure
  d.in.rec.fs <- rbind( d.in.rec.fs, tab1.var.freq( var.name = these.tox[i],
                                            df = d.rec,
                                            table.var.name = q.names[i],
                                            strata.var = "fi_binary",
                                            strata.level = "High FS" ) ) 
  
  ## not receiving treatment ##
  d.in.nrec <- rbind( d.in.nrec, tab1.var.freq( var.name = these.tox[i],
                                             df = d.nrec,
                                             table.var.name = q.names[i],
                                             strata.var = NULL,
                                             strata.level = NULL ) )
  
  # subset on food insecure
  d.in.nrec.fi <- rbind( d.in.nrec.fi, tab1.var.freq( var.name = these.tox[i],
                                                   df = d.nrec,
                                                   table.var.name = q.names[i],
                                                   strata.var = "fi_binary",
                                                   strata.level = "Low FS" ) ) 
  
  # subset on food secure
  d.in.nrec.fs <- rbind( d.in.nrec.fs, tab1.var.freq( var.name = these.tox[i],
                                                   df = d.nrec,
                                                   table.var.name = q.names[i],
                                                   strata.var = "fi_binary",
                                                   strata.level = "High FS" ) ) 
  
}

## --------- End Subsection --------- ##


## (4.2) Clean up tables ##

# merge as rows and reorder rows for final presentation
t.rec <- cbind( d.in.rec, d.in.rec.fi, d.in.rec.fs ) [, c(1,2,4,6) ] 
t.nrec <- cbind( d.in.nrec, d.in.nrec.fi, d.in.nrec.fs ) [, c(1,2,4,6) ] 


# remove rows that have the "unchecked" response counts
t.rec <- t.rec[ which( !str_detect( t.rec[,1], "Unchecked" ) ), ]
t.nrec <- t.nrec[ which( !str_detect( t.nrec[,1], "Unchecked" ) ), ]

# there were no subjects that responded in the affirmative to "borrowing money against my house", so will remove that row from table
t.rec <- t.rec[ which( !str_detect( t.rec[,1], "Selection 3" ) ), ]
t.nrec <- t.nrec[ which( !str_detect( t.nrec[,1], "Selection 3" ) ), ]

## --------- End Subsection --------- ##


## (4.3) Add MCA Dimension Scores ##

# generate empty frame to hold results
d.in.rec <- data.frame()     
d.in.rec.fs <- data.frame()
d.in.rec.fi <- data.frame()
d.in.nrec <- data.frame()     
d.in.nrec.fs <- data.frame()
d.in.nrec.fi <- data.frame()

for ( i in 1: length( these.mca ) ) {
  
  
  ## receiving treatment ##
  d.in.rec <- rbind( d.in.rec, tab1.var.mean( var.name = these.mca.2[i],
                                      df = d.rec,
                                      table.var.name = mca.names[i],
                                      strata.var = NULL,
                                      strata.level = NULL,
                                      round.to = 2 ) ) 
  
  # subset on food insecure
  d.in.rec.fi <- rbind( d.in.rec.fi, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.rec,
                                            table.var.name = mca.names[i],
                                            strata.var = "fi_binary",
                                            strata.level = "Low FS",
                                            round.to = 2 ) ) 
  
  # subset on food secure
  d.in.rec.fs <- rbind( d.in.rec.fs, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.rec,
                                            table.var.name = mca.names[i],
                                            strata.var = "fi_binary",
                                            strata.level = "High FS",
                                            round.to = 2 ) ) 
  
  
  ## not receiving treatment ##
  
  d.in.nrec <- rbind( d.in.nrec, tab1.var.mean( var.name = these.mca.2[i],
                                              df = d.nrec,
                                              table.var.name = mca.names[i],
                                              strata.var = NULL,
                                              strata.level = NULL,
                                              round.to = 2 ) ) 
  
  # subset on food insecure
  d.in.nrec.fi <- rbind( d.in.nrec.fi, tab1.var.mean( var.name = these.mca.2[i],
                                                    df = d.nrec,
                                                    table.var.name = mca.names[i],
                                                    strata.var = "fi_binary",
                                                    strata.level = "Low FS",
                                                    round.to = 2 ) ) 
  
  # subset on food secure
  d.in.nrec.fs <- rbind( d.in.nrec.fs, tab1.var.mean( var.name = these.mca.2[i],
                                                    df = d.nrec,
                                                    table.var.name = mca.names[i],
                                                    strata.var = "fi_binary",
                                                    strata.level = "High FS",
                                                    round.to = 2 ) ) 
  
  
}


# merge as rows and reorder rows for final presentation
t.rec2 <- cbind( d.in.rec, d.in.rec.fi, d.in.rec.fs ) [, c(1,2,4,6) ] 
t.nrec2 <- cbind( d.in.nrec, d.in.nrec.fi, d.in.nrec.fs ) [, c(1,2,4,6) ] 

# row bind with other table of toxicity variables
t.rec3 <- rbind( t.rec, t.rec2 )
t.nrec3 <- rbind( t.nrec, t.nrec2 )

## --------- End Subsection --------- ##


## (4.4) Wilcoxon Rank Sum and Chi-Square/Fisher's Exact Test p values ##

p.vals.rec <- vector()
p.vals.nrec <- vector()
for( i in 1:length( these.mca.2 ) ){
  
  d.this.rec <- data.frame( d.rec )
  d.this.nrec <- data.frame( d.nrec )
  ## Wilcoxon Rank Sum test for Categorical variables
  f1 <- formula( paste0( these.mca.2[i], "~ fi_binary")) # write formula
  test.rec <-  wilcox.test( f1, data = d.this.rec, alternative = "two.sided" )
  test.nrec <-  wilcox.test( f1, data = d.this.nrec, alternative = "two.sided" )
  
    p.vals.rec[i] <- test.rec$p.value # store p value
    p.vals.nrec[i] <- test.nrec$p.value # store p value
    
  
  t.rec3[ which(t.rec3$Characteristic == mca.names[i] ), "p" ] <- test.rec$p.value
  t.nrec3[ which(t.nrec3$Characteristic == mca.names[i] ), "p" ] <- test.nrec$p.value
  
  }

for (i in 1:length( these.tox ) ){
  
  ## Chi-square test of independence for categorical variables
  two.tab.rec <- table( eval( parse( text = paste0( "d.rec$", these.tox[i]) ) ),
                    d.rec$fi_binary)
  
  two.tab.nrec <- table( eval( parse( text = paste0( "d.nrec$", these.tox[i]) ) ),
                        d.nrec$fi_binary)
  
  # fisher's exact test if there is at least one cell with a count less than 5
  if( sum( as.vector( two.tab.rec ) < 5 ) > 0 ){
    test.cat.rec <- fisher.test( two.tab.rec, simulate.p.value = T )
  }
  
  if( sum( as.vector( two.tab.nrec ) < 5 ) > 0 ){
    test.cat.nrec <- fisher.test( two.tab.nrec, simulate.p.value = T )
  }
  
  # chi-square test if there are no cells with less than count of 5
  if( sum( as.vector( two.tab.rec ) < 5 ) == 0 ){
    test.cat.rec <- chisq.test( two.tab.rec, simulate.p.value = T )
  }
  t.rec3[ which(t.rec3$Characteristic == q.names[i] ), "p" ] <- test.cat.rec$p.value

  if( sum( as.vector( two.tab.nrec ) < 5 ) == 0 ){
    test.cat.nrec <- chisq.test( two.tab.nrec, simulate.p.value = T )
  }
  t.nrec3[ which(t.nrec3$Characteristic == q.names[i] ), "p" ] <- test.cat.nrec$p.value
  
}

## --------- End Subsection --------- ##


## (4.5) Clean up p-values column ##

t.rec3 <- t.rec3 %>%
  mutate( p = ifelse( p < 0.05 & p >= 0.01, paste0( round( p, 2), "*" ),
                      ifelse( p < 0.01, paste0( "< 0.01**" ), round( p, 2 ))))


t.rec3[,5] <- str_replace( t.rec3[,5], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
t.rec3[,5] <- str_replace( t.rec3[,5], "^1$", "0.99" ) # round down probabilities = 1

t.rec3[,5][ is.na( t.rec3[,5] ) ] <- ""


t.nrec3 <- t.nrec3 %>%
  mutate( p = ifelse( p < 0.05 & p >= 0.01, paste0( round( p, 2), "*" ),
                      ifelse( p < 0.01, paste0( "< 0.01**" ), round( p, 2 ))))


t.nrec3[,5] <- str_replace( t.nrec3[,5], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
t.nrec3[,5] <- str_replace( t.nrec3[,5], "^1$", "0.99" ) # round down probabilities = 1

t.nrec3[,5][ is.na( t.nrec3[,5] ) ] <- ""


## --------- End Subsection --------- ##


## (4.6) Mean Time Since Dx row in table (rec) ##

fi.levs <- c( "", "Low FS","High FS" )

age.fr <- data.frame(NA)
for( i in seq_along( fi.levs ) ){
  
  if (i == 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.this.rec,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = NULL,
                                            strata.level = NULL ) )
  }
  
  if (i != 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.this.rec,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = "fi_binary",
                                            strata.level = fi.levs[i] ) )
  }
}

# wilcoxon test
f.fi <- formula( paste0( "yrs_since_dx", "~ fi_binary")) # write formula
test <-  wilcox.test( f.fi, data = d.this.rec, alternative = "two.sided" )
age.fr <- cbind( age.fr, round( test$p.value, 2) ) # store p value
age.fr <- setNames(  age.fr[,c(2,3,5,7,8)],names( t.rec3 ) )
# add to table

t.rec3 <- rbind( age.fr, t.rec3 )

## --------- End Subsection --------- ##


## (4.7) Mean Time Since Dx row in table (nrec) ##
fi.levs <- c( "", "Low FS","High FS" )

age.fr <- data.frame(NA)
for( i in seq_along( fi.levs ) ){
  
  if (i == 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.this.nrec,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = NULL,
                                            strata.level = NULL ) )
  }
  
  if (i != 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.this.nrec,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = "fi_binary",
                                            strata.level = fi.levs[i] ) )
  }
}

# wilcoxon test
f.fi <- formula( paste0( "yrs_since_dx", "~ fi_binary")) # write formula
test <-  wilcox.test( f.fi, data = d.this.nrec, alternative = "two.sided" )
age.fr <- cbind( age.fr, round( test$p.value, 2) ) # store p value
age.fr <- setNames(  age.fr[,c(2,3,5,7,8)],names( t.nrec3 ) )
# add to table

t.nrec3 <- rbind( age.fr, t.nrec3 )

## --------- End Subsection --------- ##


## (4.8) Save Tables ##

# descriptive
write.table( t.rec3, "../04-tables-figures/07a-table-financial-tox-fi-treatment.txt", sep = "," )

# descriptive
write.table( t.nrec3, "../04-tables-figures/07b-table-financial-tox-fi-treatment.txt", sep = "," )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (5.0) Create Table Stratified on DIM 2 Score ( Hi-lo) ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## (5.1) Generate tables ##

# create binary variable for the MCA dimension score (Dim 2)

d.score <- d.4 %>%
  mutate( dim2.binary = as.factor( paste0( quant_cut("Dim 2", 2, df = d.4 ) ) ) )

# for-loop for categorical variables

d.in.score <- data.frame()     
d.in.low <- data.frame()
d.in.hi <- data.frame()
for ( i in 1: length( these.tox ) ) {
  
  d.in.score <- rbind( d.in.score, tab1.var.freq( var.name = these.tox[i],
                                            df = d.score,
                                            table.var.name = q.names[i],
                                            strata.var = NULL,
                                            strata.level = NULL ) )
  
  # subset on food insecure
  d.in.hi <- rbind( d.in.hi, tab1.var.freq( var.name = these.tox[i],
                                          df = d.score,
                                          table.var.name = q.names[i],
                                          strata.level = "2",
                                          strata.var = "dim2.binary" ) )  
  
  # subset on food secure
  d.in.low <- rbind( d.in.low, tab1.var.freq( var.name = these.tox[i],
                                            df = d.score,
                                            table.var.name = q.names[i],
                                            strata.level = "1",
                                            strata.var = "dim2.binary" ) ) 
}

## --------- End Subsection --------- ##


## (5.2) Clean up tables ##

# merge as rows and reorder rows for final presentation
l1 <- cbind( d.in.score, d.in.low, d.in.hi ) [, c(1,2,4,6) ] 


# remove rows that have the "unchecked" response counts
l1 <- l1[ which( !str_detect( l1[,1], "Unchecked" ) ), ]

# there were no subjects that responded in the affirmative to "borrowing money against my house", so will remove that row from table
l1 <- l1[ which( !str_detect( l1[,1], "Selection 3" ) ), ]

## --------- End Subsection --------- ##


## (5.3) Add MCA dimension scores to table ##

d.in.score<- data.frame()  # initialize data.frame for loop to store rows of the table
d.in.hi <- data.frame()
d.in.low <- data.frame()
for ( i in 1: length( these.mca ) ) {
  
  d.in.score <- rbind( d.in.score, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.score,
                                            table.var.name = mca.names[i],
                                            strata.var = NULL,
                                            strata.level = NULL,
                                            round.to = 2 ) ) 
  
  # subset on food insecure
  d.in.hi <- rbind( d.in.hi, tab1.var.mean( var.name = these.mca.2[i],
                                          df = d.score,
                                          table.var.name = mca.names[i],
                                          strata.level = "2",
                                          strata.var = "dim2.binary",
                                          round.to = 2 ) ) 
  
  # subset on food secure
  d.in.low <- rbind( d.in.low, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.score,
                                            table.var.name = mca.names[i],
                                            strata.level = "1",
                                            strata.var = "dim2.binary",
                                            round.to = 2 ) ) 
  
}


# merge as rows and reorder rows for final presentation
l2 <- cbind( d.in.score, d.in.low, d.in.hi ) [, c(1,2,4,6) ] 


# row bind with other table of toxicity variables
l3 <- rbind( l1, l2 )

## --------- End Subsection --------- ##


## (5.4) Wilcoxon Rank Sum and Chi-Square/Fisher's Exact Test p values ##

p.vals <- vector()
for( i in 1:length( these.mca.2 ) ){
  
  d.hihis <- data.frame( d.score )
  ## Wilcoxon Rank Sum test for Categorical variables
  f1 <- formula( paste0( these.mca.2[i], "~ dim2.binary")) # write formula
  test <-  wilcox.test( f1, data = d.hihis, alternative = "two.sided" )
  p.vals[i] <- test$p.value # store p value
  
  
  l3[ which(l3$Characteristic == mca.names[i] ), "p" ] <- test$p.value
}

for (i in 1:length( these.tox ) ){
  
  ## Chi-square test of independence for categorical variables
  two.hiab <- table( eval( parse( text = paste0( "d.score$", these.tox[i]) ) ),
                    d.score$dim2.binary)
  
  # fisher's exact test if there is at least one cell with a count less than 5
  if( sum( as.vector( two.hiab ) < 5 ) > 0 ){
    teslcat <- fisher.test( two.hiab, simulate.p.value = T )
  }
  
  # chi-square test if there are no cells with less than count of 5
  if( sum( as.vector( two.hiab ) < 5 ) == 0 ){
    teslcat <- chisq.test( two.hiab, simulate.p.value = T )
  }
  l3[ which(l3$Characteristic == q.names[i] ), "p" ] <- teslcat$p.value
  
}

## --------- End Subsection --------- ##


## (5.5) Clean up p-values column ##

l3 <- l3 %>%
  mutate( p = ifelse( p < 0.05 & p >= 0.01, paste0( round( p, 2), "*" ),
                      ifelse( p < 0.01, paste0( "< 0.01**" ), round( p, 2 ))))


l3[,5] <- str_replace( l3[,5], "(\\d\\.\\d)$", "\\10" ) # match digit, period, digit,end and add a 0 before the end
l3[,5] <- str_replace( l3[,5], "^1$", "0.99" ) # round down probabilities = 1

l3[,5][ is.na( l3[,5] ) ] <- ""

## --------- End Subsection --------- ##

## (2.8) Mean Time Since Dx row in table ##
pat.levs <- c( "", "1","2" )

age.fr <- data.frame(NA)
for( i in seq_along( fi.levs ) ){
  
  if (i == 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.score,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = NULL,
                                            strata.level = NULL ) )
  }
  
  if (i != 1){
    age.fr <- cbind( age.fr, tab1.var.mean( var.name = "yrs_since_dx",
                                            df = d.score,
                                            table.var.name = "Years Since Diagnosis",
                                            strata.var = "dim2.binary",
                                            strata.level = pat.levs[i] ) )
  }
}

# wilcoxon test
f.pat <- formula( paste0( "yrs_since_dx", "~ dim2.binary")) # write formula
test <-  wilcox.test( f.pat, data = d.score, alternative = "two.sided" )
age.fr <- cbind( age.fr, round( test$p.value, 2) ) # store p value
age.fr <- setNames(  age.fr[,c(2,3,5,7,8)],names( l3 ) )
# add to table

l3 <- rbind( age.fr, l3 )

## --------- End Subsection --------- ##

## (5.6) Save Table ##

# descriptive
write.table( l3, "../04-tables-figures/08-table-financial-tox-mca-score-hi-lo.txt", sep = "," )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### (6.0) Save Data with MCA Scores and Recategorized Variables for Later Use ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# rename MCA dimension variables
d.4 %>% 
  rename( mca.dim.1 = `Dim 1`,
          mca.dim.2 = `Dim 2`,
          mca.dim.3 = `Dim 3` ) %>%
  # save
saveRDS( ., "../03-data-rodeo/01-data-mca-recat.rds")

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

