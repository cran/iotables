## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(iotables)
require(dplyr)

## ----download, eval=FALSE-----------------------------------------------------
#  #Not run
#  not_included_directory <- file.path('..', 'not_included')
#  if ( ! dir.exists(not_included_directory) ) dir.create (not_included_directory)
#  #The contents of the 'not_included' directory can be found on GitHub,
#  #but they are not released and distributed with the package.
#  
#  naio_10_cp1700 <- iotables_download(
#    "naio_10_cp1700", #SIOT
#    data_directory = not_included_directory)
#  
#  # For inclusion in the package, the files must be smaller.
#  # Reducing the size of the bulk files will not affect
#  # the demonstration.
#  
#  naio_10_cp1700 <- naio_10_cp1700 %>%
#    dplyr::filter ( geo %in% c("CZ", "SK")) %>%
#    dplyr::filter ( year %in% c(2010, 2015))
#  
#  #Conforming employment data both sexes from 15 years old, year 2015.
#  #prod_na vocabulary for product x product conformity
#  emp_cz <- employment_get(geo = "CZ", year = "2015", sex = "Total",
#    age = "Y_GE15", labelling = "prod_na",
#    data_directory = not_included_directory,
#    force_download = TRUE)
#  
#  #Conforming employment data #both sexes from 15 years old, year 2017.
#  emp_sk <- employment_get(geo = "SK", year = "2017", sex = "Total",
#    age = "Y_GE15", labelling = "prod_na",
#    data_directory = not_included_directory,
#    force_download = TRUE)
#  
#  save (naio_10_cp1700, emp_sk, emp_cz,
#        file = file.path('..', 'inst', 'extdata',
#                         'naio_10_product_x_product.rda'))

## ----load---------------------------------------------------------------------
#load from pre-saved file to increase running speed
load (system.file('extdata', 
                  'naio_10_product_x_product.rda', 
                  package = 'iotables') )


## ----preprocess---------------------------------------------------------------
cz_io <-  iotable_get ( labelled_io_data = naio_10_cp1700, 
                         source = "naio_10_cp1700", geo = "CZ", 
                         year = 2015, unit = "MIO_NAC", 
                         stk_flow = "TOTAL",
                         labelling = "short" )

sk_io <-  iotable_get ( labelled_io_data = naio_10_cp1700, 
                        source = "naio_10_cp1700", geo = "SK", 
                        year = 2010, unit = "MIO_EUR", 
                        stk_flow = "TOTAL",
                        labelling = "short" )

cz_input_flow <- input_flow_get( data_table = cz_io )

sk_input_flow <- input_flow_get( data_table = sk_io)

cz_output <- output_get( data_table = cz_io)
sk_output <- output_get( data_table = sk_io)

## ----inputcoeff, results='asis'-----------------------------------------------
input_coeff_matrix_cz <- input_coefficient_matrix_create(
  data_table = cz_io
)

input_coeff_matrix_sk <- input_coefficient_matrix_create(
  data_table = sk_io
)

knitr::kable(head(input_coeff_matrix_cz[,1:8]))

## ----leontieff, results='asis'------------------------------------------------
L_cz <- leontieff_matrix_create( input_coeff_matrix_cz  )
I_cz <- leontieff_inverse_create( input_coeff_matrix_cz )

L_sk <- leontieff_matrix_create( input_coeff_matrix_sk  )
I_sk <- leontieff_inverse_create( input_coeff_matrix_sk )

knitr::kable(head(I_cz[,1:8]))

## ----direct, results='asis'---------------------------------------------------
primary_inputs_cz <- coefficient_matrix_create(data_table = cz_io, 
                                              total = 'output', 
                                              return = 'primary_inputs') 

primary_inputs_sk <- coefficient_matrix_create(data_table = sk_io, 
                                              total = 'output', 
                                              return = 'primary_inputs')

direct_cz <- direct_effects_create( primary_inputs_cz, I_cz )  
direct_sk <- direct_effects_create( primary_inputs_sk, I_sk )  

knitr::kable (head(direct_cz[,1:8]), digits = 4)

## ----total, results='asis'----------------------------------------------------
primary_inputs_cz <- coefficient_matrix_create(data_table = cz_io, 
                                              total = 'output', 
                                              return = 'primary_inputs') 

primary_inputs_sk <- coefficient_matrix_create(data_table = sk_io, 
                                              total = 'output', 
                                              return = 'primary_inputs')

multipliers_cz <- input_multipliers_create( primary_inputs_cz, I_cz )  
multipliers_sk <- input_multipliers_create( primary_inputs_sk, I_sk ) 

knitr::kable (head(multipliers_cz[,1:8]), digits = 4)

## ----employmenteffect, results='asis', message=FALSE--------------------------
#New function is needed to add employment vector to SIOT
names ( emp_sk )[1] <- 'prod_na'
names ( emp_cz )[1] <- 'prod_na'

emp_indicator_sk <- rbind ( 
  sk_io[, 1:66], 
  emp_sk) %>% coefficient_matrix_create(., 
       return_part = 'primary_inputs') %>%
  filter ( prod_na == "employment_total" )

emp_indicator_cz <- full_join ( 
  cz_io, 
  emp_cz) %>% coefficient_matrix_create(., 
       return_part = 'primary_inputs') %>%
  filter ( prod_na == "employment_total" )


emp_effect_sk <- direct_effects_create( emp_indicator_sk, I_sk )  
emp_effect_cz <- direct_effects_create( emp_indicator_cz, I_cz )  

knitr::kable (emp_effect_cz[1:8], digits = 5)

## ----employmentindicator, results='asis'--------------------------------------
#New function is needed to add employment vector to SIOT

emp_multiplier_sk <- input_multipliers_create( emp_indicator_sk, I_sk )  
emp_multiplier_cz <- input_multipliers_create( emp_indicator_cz, I_cz )  

knitr::kable (emp_multiplier_cz[1:8], digits=5)

## ----output_multipliers, results='asis'---------------------------------------

output_multipliers_cz <- output_multiplier_create (input_coeff_matrix_cz)
output_multipliers_sk <- output_multiplier_create (input_coeff_matrix_sk)

knitr::kable (head(output_multipliers_cz[,1:8]), digits=4)

## ----backward, results='asis'-------------------------------------------------
cz_bw <- backward_linkages(I_cz)
sk_bw <- backward_linkages(I_sk)

knitr::kable (head(cz_bw[,1:8]), digits=4)

## ----output_coeff, results='asis', eval=FALSE---------------------------------
#  output_coeff_cz <- output_coefficient_matrix_create(
#    io_table = cz_io, total = "tfu", digits = 4)
#  
#  output_coeff_sk <- output_coefficient_matrix_create(
#    io_table = sk_io, total = "tfu")
#  
#  knitr::kable (head(output_coeff_cz[,1:8]))

## ----forward, results='asis', eval=FALSE--------------------------------------
#  cz_fw <- forward_linkages ( output_coeff_cz )
#  sk_fw <- forward_linkages( output_coeff_sk )
#  
#  knitr::kable (head(cz_fw), digits=4)

## ----reproduction_data, eval=FALSE--------------------------------------------
#  require(xlsx)
#  cz_file_name <- file.path("..", "not_included", "CzechRep_test.xlsx")
#  #Czech Republic data
#  xlsx::write.xlsx ( cz_io, file = cz_file_name, sheetName = "io_table",
#                     col.names=TRUE, row.names=TRUE, append=FALSE)
#  xlsx::write.xlsx ( cz_output, file = cz_file_name, sheetName = "cz_output",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( input_coeff_matrix_cz, file = cz_file_name,
#                     sheetName = "input_coeff_matrix_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( L_cz, file = cz_file_name, sheetName = "L_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( I_cz, file = cz_file_name, sheetName = "I_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( direct_cz, file = cz_file_name,
#                     sheetName = "direct_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( multipliers_cz, file = cz_file_name,
#                     sheetName = "multipliers_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_effect_cz, file = cz_file_name,
#                     sheetName = "emp_effect_cz_2015",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_indicator_cz, file = cz_file_name,
#                     sheetName = "emp_indicator_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_multiplier_cz, file = cz_file_name,
#                     sheetName = "emp_multiplier_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( cz_bw, file = cz_file_name,
#                     sheetName = "cz_backward_linkages",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( output_coeff_cz, file = cz_file_name,
#                     sheetName = "output_coeff_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( cz_fw, file = cz_file_name,
#                     sheetName = "cz_forward_linkages",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( output_multipliers_cz, file = cz_file_name,
#                     sheetName = "output_multipliers_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  
#  
#  sk_file_name <- file.path("..", "not_included", "SlovakRep_test.xlsx")
#  #Czech Republic data
#  xlsx::write.xlsx ( sk_io, file = sk_file_name, sheetName = "io_table",
#                     col.names=TRUE, row.names=TRUE, append=FALSE)
#  xlsx::write.xlsx ( sk_output, file = sk_file_name, sheetName = "sk_output",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( input_coeff_matrix_sk, file = sk_file_name,
#                     sheetName = "input_coeff_matrix_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( L_sk, file = sk_file_name, sheetName = "L_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( I_sk, file = sk_file_name, sheetName = "I_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( direct_sk, file = sk_file_name,
#                     sheetName = "direct_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( multipliers_sk, file = sk_file_name,
#                     sheetName = "multipliers_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_effect_sk, file = sk_file_name,
#                     sheetName = "emp_effect_sk_2015",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_indicator_sk,file = sk_file_name,
#                     sheetName = "emp_indicator_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_multiplier_sk, file = sk_file_name,
#                     sheetName = "employment_multipliers_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( sk_bw, file = sk_file_name,
#                     sheetName = "sk_backward_linkages",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( output_coeff_sk, file = sk_file_name,
#                     sheetName = "output_coeff_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( sk_fw, file = sk_file_name,
#                     sheetName = "sk_forward_linkages",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( output_multipliers_sk, file = sk_file_name,
#                     sheetName = "output_multipliers_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)

