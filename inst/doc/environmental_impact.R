## ----setupknitr, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
load(system.file(file.path("extdata", "environmental_impact_vignette.rda"), package = 'iotables'))

## ----setup, echo=FALSE, message=FALSE-----------------------------------------
library(iotables)
library(dplyr, quietly=TRUE)
library(tidyr, quietly=TRUE)

## ----getiotable, eval=FALSE---------------------------------------------------
#  BE <- iotable_get(source = "naio_10_cp1700", geo = "BE",
#                    year =2015,
#                    labelling = "short", unit = "MIO_EUR",
#                    stk_flow = "TOTAL")

## ----getairpol, eval=FALSE----------------------------------------------------
#  ghg <- airpol_get(airpol="GHG", geo="BE", year=2020, unit="THS_T")

## ----ghgindicators------------------------------------------------------------
be_io <- BE %>%
  bind_rows( ghg %>% rename (prod_na = .data$indicator) )

ghg_indicator <- input_indicator_create ( 
  data_table  = be_io,
  input_vector = "GHG_emission")

## ----ghgindicator-------------------------------------------------------------
# Only the top 5 is printed: 
ghg_indicator %>%
  vector_transpose( .keep = TRUE ) %>%
  rename ( GHG_emission_indicator = .data$value ) %>%
  arrange ( -.data$GHG_emission_indicator ) %>%
  top_n(5)

## ----getco2indicators, eval=FALSE---------------------------------------------
#  co2 <- airpol_get(airpol="CO2", geo="BE", year=2020, unit = "THS_T")

## ----co2indicators------------------------------------------------------------
be_io_c <- BE %>%
  bind_rows( co2 %>% rename (prod_na = .data$indicator) )

co2_indicator <- input_indicator_create ( 
  data_table  = be_io_c,
  input_vector = "CO2_emission")

# Only the top 5 is printed: 
co2_indicator %>% 
  vector_transpose( .keep = TRUE ) %>%
  rename ( CO2_emission_indicator = .data$value ) %>%
  arrange ( -.data$CO2_emission_indicator ) %>%
  top_n(5)

## ----getmethaneindicators-----------------------------------------------------
methane <- airpol_get (airpol = "CH4", geo="BE", year = 2020, unit = "THS_T")

## ----methaneindicators--------------------------------------------------------
be_io_m <- BE %>%
  bind_rows( methane %>% rename (prod_na = .data$indicator) )

methane_indicator <- input_indicator_create ( 
  data_table  = be_io_m,
  input_vector = "CH4_emission")

# Only the top 5 is printed: 
methane_indicator %>% 
  vector_transpose( .keep = TRUE ) %>%
  rename ( CH4_emission_indicator = .data$value ) %>%
  arrange ( -.data$CH4_emission_indicator ) %>%
  top_n(5)

## ----ghgmultiplier, message=FALSE---------------------------------------------
I_be <- input_coefficient_matrix_create( 
     data_table = BE, 
     digits = 4) %>%
  leontieff_inverse_create()


ghg_multipliers <- multiplier_create ( 
  input_vector    = ghg_indicator,
  Im              = I_be,
  multiplier_name = "GHG_multiplier", 
  digits = 4 )

# Only the top 5 is printed: 
ghg_multipliers %>%
  vector_transpose( .keep = TRUE ) %>%
  rename ( GHG_multiplier = .data$value ) %>%
  arrange ( -.data$GHG_multiplier ) %>%
  top_n(5)

## ----savevignettedata, eval=FALSE---------------------------------------------
#  save(methane, co2, ghg, BE, file = file.path(".." , "inst", "extdata", "environmental_impact_vignette.rda") )

