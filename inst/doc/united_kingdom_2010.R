## ----setup knitr, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=TRUE------------------------------------------------------
library(iotables)
library(dplyr, quietly = TRUE) 
library(tidyr, quietly = TRUE)
library(testthat, quietly = TRUE)

## ----read_control_data, eval=FALSE--------------------------------------------
#  uk_2010_data <- iotables_download ( source = "uk_2010" )
#  save ( uk_2010_data, file = file.path('data-raw', 'uk_2010_data.rda'))
#  uk_test_results <- iotables:::uk_2010_results_get()
#  #saved as package data

## ----downloading_excel_data---------------------------------------------------
uk_siot              <- iotable_get (labelled_io_data = uk_2010_data, 
                                     source = "uk_2010_siot")
uk_published_coeff   <- iotable_get (labelled_io_data = uk_2010_data, 
                                     source = "uk_2010_siot")
uk_published_inverse <- iotable_get (labelled_io_data = uk_2010_data, 
                                     source = "uk_2010_siot")

## ----compare_coeff------------------------------------------------------------
uk_input_coeff <- input_coefficient_matrix_create(data_table = uk_siot)

uk_coeff_published <- select (uk_input_coeff, 1 ) %>%
  left_join ( iotable_get ( labelled_io_data = uk_2010_data, 
                            source = "uk_2010_coeff" ) %>%
                mutate ( across(where(is.factor), as.character)), 
              by = "prod_na")

test_that("correct data is returned", {
  expect_equal(round(uk_input_coeff[,2:8], 8),
               round(uk_coeff_published  [,2:8], 8)) })

## ----compare_inverse----------------------------------------------------------
uk_calculated_inverse <- leontieff_inverse_create(uk_input_coeff)
uk_published_inverse <- select (uk_calculated_inverse, 1 ) %>%
  left_join ( iotable_get ( labelled_io_data = uk_2010_data, 
                        source = "uk_2010_inverse" ) %>%
                mutate ( across(where(is.factor), as.character)), 
              by = "prod_na")

test_that("correct data is returned", {
  expect_equal(round(uk_calculated_inverse[,2:8], 8),
               round(uk_published_inverse  [,2:8], 8)) })

## ----compare_effects----------------------------------------------------------
employment_effect_results <- uk_test_results %>%
  select ( .data$uk_row_label, .data$`Employment cost effects`)

primary_inputs_uk <- coefficient_matrix_create(
    data_table  = uk_siot, 
    total       = 'output', 
    return_part = 'primary_inputs')

employment_input <- filter (primary_inputs_uk , .data$prod_na == "D1")

employment_effects <- direct_effects_create( employment_input, uk_calculated_inverse) %>%
  pivot_longer( -all_of("prod_na"), names_to = "prod" ) %>%
  mutate ( prod_na = .data$prod ) %>%
  select ( -.data$prod ) %>%
  left_join ( metadata_uk_2010 %>%
                select (all_of(c("prod_na", "uk_row_label"))),
              by = "prod_na") %>%
  left_join ( employment_effect_results, by = 'uk_row_label') %>%
  filter ( !is.na(uk_row_label )) %>%
  relocate ( .data$value, .after = .data$uk_row_label )


iotables:::create_knitr_table (
      data_table =  employment_effects[1:10,], 
      digits = 4,
      caption = "Comparison of Calculated And Published Employment Cost Effects", 
      col.names = c("industry code", "row label", "calculated", "published"), 
      col_width = c(2,11,3,3))

## ----gva_effects--------------------------------------------------------------
uk_siot2 <- uk_siot %>%
  filter (.data$prod_na %in% c("B2A3G", "D1", "D29X39"))  %>%
  summarise ( across(where(is.numeric), sum, na.rm = TRUE) ) %>%
  cbind ( data.frame ( prod_na = "GVA"), . ) %>%
  rbind ( uk_siot, .)

gva_effect_results <- uk_test_results %>%
  select ( .data$uk_row_label, .data$`GVA effects`)

gva_input <- coefficient_matrix_create(
   data_table  = uk_siot2, 
   total       = 'output', 
   return_part = 'primary_inputs') %>%
  filter ( prod_na == "GVA" )

gva_effects <- direct_effects_create( gva_input, uk_calculated_inverse ) %>%
  pivot_longer( -all_of("prod_na"), names_to = "prod" ) %>%
  mutate ( prod_na = .data$prod ) %>%
  select ( -.data$prod ) %>%
  left_join ( metadata_uk_2010 %>%
                select (all_of(c("prod_na", "uk_row_label"))),
              by = "prod_na") %>%
  left_join ( gva_effect_results, by = 'uk_row_label' ) %>%
  filter ( !is.na(uk_row_label )) %>%
  relocate ( .data$value, .after = .data$uk_row_label )

iotables:::create_knitr_table (
      data_table =  gva_effects[1:10,], 
      digits = 4,
      caption = "Comparison of Calculated And Published GVA Effects", 
      col.names = c("industry code", "row label", "calculated", "published"), 
      col_width = c(2,11,3,3))

## ----compare_emp_multipliers--------------------------------------------------
empc_multiplier_results <- uk_test_results %>%
  select ( .data$uk_row_label, .data$`Employment cost multiplier`)

empc_indicator_uk <- coefficient_matrix_create(
   data_table  = uk_siot, 
   total       = 'output', 
   return_part = 'primary_inputs') %>%
   filter ( prod_na == 'D1')

empc_multipliers <- input_multipliers_create(
  input_requirements = empc_indicator_uk,
  uk_calculated_inverse) %>%
  pivot_longer( -all_of("prod_na"), names_to = "prod" ) %>%
  mutate ( prod_na = .data$prod ) %>%
  select ( -.data$prod ) %>%
  left_join ( metadata_uk_2010 %>%
                select (all_of(c("prod_na", "uk_row_label"))),
              by = "prod_na") %>%
  left_join ( empc_multiplier_results, by = 'uk_row_label' ) %>%
  filter ( !is.na(uk_row_label ))  %>%
  relocate ( .data$value, .after = .data$uk_row_label )

iotables:::create_knitr_table (
      data_table =  empc_multipliers [1:10,], digits = 4,
      caption = "Comparison of Calculated And Published Employment Cost Multipliers", 
      col.names = c("industry code", "row label", "calculated", "published"), 
      col_width = c(2,11,3,3))

## ----gva_comp-----------------------------------------------------------------
gva_multipliers <- input_multipliers_create(
  input_requirements = gva_input,
  uk_calculated_inverse) %>%
  pivot_longer( -all_of("prod_na"), names_to = "prod" ) %>%
  mutate ( prod_na = .data$prod ) %>%
  select ( -.data$prod ) %>%
  left_join ( metadata_uk_2010 %>%
                select (all_of(c("prod_na", "uk_row_label"))),
              by = "prod_na") %>%
  left_join ( uk_test_results %>%
                select ( .data$uk_row_label, .data$`GVA multiplier`), by = 'uk_row_label' ) %>%
  filter ( !is.na(uk_row_label )) %>%
  relocate ( .data$value, .after = .data$uk_row_label )


iotables:::create_knitr_table (
  data_table =  gva_multipliers [1:10,], digits = 4,
  caption = "Comparison of Calculated And Published GVA Multipliers", 
  col.names = c("industry code", "row label", "calculated", "published"), 
  col_width = c(2,11,3,3))

iotables:::create_knitr_table (
  data_table =  gva_multipliers [1:10,], 
  digits = 4,
  caption = "Comparison of Calculated And Published GVA Multipliers", 
  col.names = c("industry code", "row label", 
                "calculated", "published"), 
  col_width = c(2,11,3,3))

## ----compare_output_multipliers-----------------------------------------------
uk_output_multipliers <- output_multiplier_create(uk_input_coeff) %>%
  pivot_longer( -all_of("prod_na"), names_to = "prod" ) %>%
  mutate ( prod_na = .data$prod ) %>%
  select ( -.data$prod ) %>%
  left_join ( metadata_uk_2010 %>%
                select (all_of(c("prod_na", "uk_row_label"))),
              by = "prod_na") %>%
  left_join ( uk_test_results %>%
                select ( .data$uk_row_label, .data$`Output multiplier`), by = 'uk_row_label' ) %>%
  filter ( !is.na(uk_row_label )) %>%
  relocate ( .data$value, .after = .data$uk_row_label )


iotables:::create_knitr_table (
      data_table =  uk_output_multipliers [1:10,], 
      digits = 4,
      caption = "Comparison of Calculated And Published Output Multipliers", 
      col.names = c("industry code", "row label",
                    "calculated", "published"), 
      col_width = c(2,11,3,3))

