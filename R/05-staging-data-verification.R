library(readxl)

# bring in data Mahima provided and keep only pertinent columns
stage.d <- read_xlsx( "../01-data-raw/Cancer Insecurity Staging Verification.xlsx") %>%
  rename( record_id = `Subject ID`,
          tnm.stage = `TNM grading`,
          clinical.stage = `Clinical Staging`) %>%
  select( record_id, tnm.stage, clinical.stage )

# bring in data we have been working with
d <- readRDS( "../02-data-wrangled/01-data-scores.rds" )

# join them
d.2 <- left_join(d, stage.d )

# print them
View( d.2 %>%
  select( disease_stage, tnm.stage, clinical.stage ) )


# check number of missings in each variable
sapply( d.2[, c( "disease_stage", "tnm.stage", "clinical.stage")],
        function( x ) sum( is.na( x ) ) )
        