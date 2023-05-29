##------------------------------------------------------------
###   07-REGRESSION ANALYSIS-FINANCIAL TOX. AND FI (ANNA)
###------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we fit regression (logistic) models
# that assess the relationship between financial toxicity (measured by the MCA dimension 2 score)
# and food insecurity. All models adjust for relevant covariates.
#
# INPUT DATA FILES: 
# i. "../03-data-rodeo/01-data-mca-recat.rds"
#
#
# OUTPUT FILES: 
# i. "../04-tables-figures/14-fin-tox-and-fi-results.txt"
#
# **A Special Note**: The data and tables are not being hosted on this GitHub repository given privacy concerns
# Relative paths are used for obtaining the data from a local folder on my machine.
#
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

library( tidyverse )
library( car ) # to compute variance inflation factor (VIF)

# load in helper functions
source( "R/utils.R" )



### (0.0) Read-in Wrangled Data w/ Dim 2 Score ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d <- readRDS( "../03-data-rodeo/01-data-mca-recat.rds" )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



### (1.0) Model Fitting ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## (1.1) Specify formula ##

f1 <-  fi_binary ~ I( mca.dim.2/sd(d$mca.dim.2,na.rm = T ) ) + inc_pov_binary + age + 
  pat_sex + food_assist_yn  + malnutrition_index + disease_stage + chaos_score

## --------- End Subsection --------- ##


## (1.2) Fit the model ##

m1 <- glm( f1, data = d, family = binomial )

## (1.3) Print the model (using `res_or` from the utils.R file) ##

r1 <- res_or( model.obj = m1, 
              term = "mca.dim.2", 
              y.var = "Food Insecurity",
              stratum = "All" )  

## --------- End Subsection --------- ##


## (1.4) Save the result ##

write.table( r1, "../04-tables-figures/14-fin-tox-and-fi-results.txt", sep = "," )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------





