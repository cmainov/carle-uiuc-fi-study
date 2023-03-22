
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

ind.coords$coord

# get contributions of each of the variables to the components
col.contrib <- get_mca_var( mca.tox )

col.contrib$contrib
