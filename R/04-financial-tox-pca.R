
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
                                              how_pay_out_of_pocket___6 ) )

## --------- End Subsection --------- ##


## (1.2) Select variables and generate table##

# these variables
these.tox <- c( "worse_insurance", "health_insurance_effect", "worse_employment_status",
             "employment_status_effect", "worse_financial_status", "financial_status_effect",
             "paid_out_of_pocket", "how_pay_out_of_pocket___1", "how_pay_out_of_pocket___2",
             "how_pay_out_of_pocket___3", "how_pay_out_of_pocket___4", "how_pay_out_of_pocket___5", 
             "how_pay_out_of_pocket___6", "debt_from_treatment",
             "go_without_meds", "take_less_meds", "miss_dr_appoint" ) # note that we removed
# the "If other please specify" open-ended question from this set (variable #162)

# names for presentation in table
q.names <- c( "Since your cancer diagnosis, are you worse off regarding Health Insurance?",
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
                                                strata.level = "Low FI" ) ) 
  
  # subset on food secure
  d.in.fs <- rbind( d.in.fs, tab1.var.freq( var.name = these.tox[i],
                                                df = d.2,
                                                table.var.name = q.names[i],
                                                strata.var = "fi_binary",
                                                strata.level = "High FI" ) ) 
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

# these variables will be used in the MCA
these.mca <- c( "worse_insurance", "health_insurance_effect", "worse_employment_status",
                "employment_status_effect", "worse_financial_status", "financial_status_effect",
                "paid_out_of_pocket", "how_pay_out_of_pocket___1", "how_pay_out_of_pocket___2",
                "how_pay_out_of_pocket___4", "how_pay_out_of_pocket___5", 
                "how_pay_out_of_pocket___6", "debt_from_treatment",
                "go_without_meds", "take_less_meds", "miss_dr_appoint" ) # note that we removed
# the "If other please specify" open-ended question from this set (variable #162) AND
# the how_pay_out_of_pocket___3 variable (see note above) in section 1.3

# NOTE that variables with names ending in "_effect" having missings that reflect the skip pattern

## --------- End Subsection --------- ##


## (2.2) Fix variables that having meaningful skip patterns so they are not missing ##

d.3 <- d.2 %>%
  mutate( health_insurance_effect = ifelse( !is.na( worse_insurance ) & is.na( health_insurance_effect ),
                                            "N/A", health_insurance_effect ),
          employment_status_effect = ifelse( !is.na( worse_employment_status ) & is.na( employment_status_effect ),
                                            "N/A", employment_status_effect ),
          financial_status_effect = ifelse( !is.na( worse_financial_status ) & is.na( financial_status_effect ),
                                             "N/A", financial_status_effect ) )

## --------- End Subsection --------- ##


## (2.3) MCA ##

mca.tox <- FactoMineR::MCA( d.3[ these.mca] )

# print eigenvalues
mca.tox$eig

# print biplot
fviz_mca_biplot( mca.tox, 
                 repel = TRUE, # to repel labels close to one another
                 ggtheme = theme_minimal())


# get scores
ind.coords <- get_mca_ind( mca.tox )

d.4 <- cbind( d.3,ind.coords$coord[ ,1:3] ) # first three dimensions only

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
                                            strata.level = "Low FI",
                                            round.to = 2 ) ) 
  
  # subset on food secure
  d.in.fs <- rbind( d.in.fs, tab1.var.mean( var.name = these.mca.2[i],
                                            df = d.4,
                                            table.var.name = mca.names[i],
                                            strata.var = "fi_binary",
                                            strata.level = "High FI",
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


## (2.7) Save Table ##

# descriptive
write.table( c.3, "../04-tables-figures/05-table-financial-tox.txt", sep = "," )

# loadings
write.csv( col.load, "../04-tables-figures/07-mca-loadings.csv" )

