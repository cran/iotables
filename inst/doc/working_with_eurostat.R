## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(iotables)
require(dplyr)

## ----download, eval=FALSE------------------------------------------------
#  #Not run
#  not_included_directory <- file.path('..', 'not_included')
#  if ( ! dir.exists(not_included_directory) ) dir.create (not_included_directory)
#  #The contents of the 'not_included' directory can be found on GitHub,
#  #but they are not released and distributed with the package.
#  
#  naio_10_cp1700 <- iotables_download("naio_10_cp1700", #SIOT
#                                      data_directory = not_included_directory )
#  
#  #For inclusion in the package, the files must be smaller. Reducing the size of
#  #the bulk files will not affect the demonstration
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

## ----load----------------------------------------------------------------
#load from pre-saved file to increase running speed

load (system.file('extdata', 'naio_10_product_x_product.rda', package = 'iotables') )


## ----preprocess----------------------------------------------------------
message ( "IO tables")
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

message ( "Use tables")
cz_use <- use_table_get( labelled_io_table = cz_io, 
                         source = "naio_10_cp1700", geo = "CZ", 
                         year = 2015, unit = "MIO_NAC", 
                         stk_flow = "TOTAL",
                         labelling = "short")

sk_use <- use_table_get( labelled_io_table = sk_io, 
                         source = "naio_10_cp1700", geo = "SK", 
                         year = 2010, unit = "MIO_EUR", 
                         stk_flow = "TOTAL",
                         labelling = "short")

message ( "Output vectors")
cz_output <- output_get( labelled_io_table = cz_io)
sk_output <- output_get( labelled_io_table = sk_io)

## ----inputcoeff, results='asis'------------------------------------------
message ("Czech Republic")
input_coeff_matrix_cz <- input_coefficient_matrix_create(
  cz_use, 
  cz_output
)

message ("Slovak Republic")
input_coeff_matrix_sk <- input_coefficient_matrix_create(
  sk_use, 
  sk_output
)

knitr::kable(head(input_coeff_matrix_cz[,1:8]))

## ----leontieff, results='asis'-------------------------------------------
L_cz <- leontieff_matrix_create( input_coeff_matrix_cz  )
I_cz <- leontieff_inverse_create( L_cz )

L_sk <- leontieff_matrix_create( input_coeff_matrix_sk  )
I_sk <- leontieff_inverse_create( L_sk )

knitr::kable(head(I_cz[,1:8]))

## ----direct, results='asis'----------------------------------------------
message ( "Czech Republic:")
direct_cz <- direct_effects_create( labelled_io_table = cz_io, 
                                    digits = 4)

message ( "Slovak Republic:")
direct_sk <- direct_effects_create( labelled_io_table = sk_io )

knitr::kable (head(direct_cz[,1:8]))

## ----total, results='asis'-----------------------------------------------
message ( "Czech Republic:")
multipliers_cz <- input_multipliers_create( direct_cz[, 1:62], I_cz)

message ( "No messages.")
message ( "Slovak Republic:")
multipliers_sk <- input_multipliers_create( direct_sk, I_sk, digits = 4)

knitr::kable (head(multipliers_cz[,1:8]))

## ----employmentindicator, results='asis'---------------------------------
message("Slovak Republic")
emp_indicator_sk  <- input_indicator_create( 
  input_matrix = emp_sk, 
  output_vector = sk_output)

message("\nCzech Republic")
emp_indicator_cz  <- input_indicator_create( 
  input_matrix = emp_cz, 
  output_vector = cz_output)

message("\nSlovak Republic")
employment_multipliers_sk <- multiplier_create ( 
  input_vector    = emp_indicator_sk,
  Im              = I_sk,
  multiplier_name = "employment_multiplier", 
  digits = 4 ) %>% 
  tidyr::gather (industry, values, !!2:ncol(.))

message("\nCzech Republic")
employment_multipliers_cz <- multiplier_create ( 
  input_vector    = emp_indicator_cz,
  Im              = I_cz,
  multiplier_name = "employment_multiplier", 
  digits = 4 ) %>% 
  tidyr::gather (industry, values, !!2:ncol(.)) %>% 
  dplyr::mutate ( values = values * 1000 )

knitr::kable (head(employment_multipliers_cz))

## ----output_multipliers, results='asis'----------------------------------

output_multipliers_cz <- output_multiplier_create (input_coeff_matrix_cz)
output_multipliers_sk <- output_multiplier_create (input_coeff_matrix_sk)

knitr::kable (head(output_multipliers_cz[,1:8]))

## ----backward, results='asis'--------------------------------------------
cz_bw <- backward_linkages(I_cz)
sk_bw <- backward_linkages(I_sk)

knitr::kable (head(cz_bw[,1:8]))

## ----output_coeff, results='asis'----------------------------------------

output_coeff_cz <- output_coefficient_matrix_create( 
  io_table = cz_io, type = "final_demand", digits = 4)

output_coeff_sk <- output_coefficient_matrix_create( 
  io_table = sk_io, type = "final_demand")

knitr::kable (head(output_coeff_cz[,1:8]))

## ----forward, results='asis'---------------------------------------------
cz_fw <- forward_linkages ( output_coeff_cz )
sk_fw <- forward_linkages( output_coeff_sk )

knitr::kable (head(cz_fw))

## ----reproduction_data, eval=FALSE---------------------------------------
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
#  xlsx::write.xlsx ( emp_cz, file = cz_file_name,
#                     sheetName = "emp_cz_2015",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_indicator_cz, file = cz_file_name,
#                     sheetName = "emp_indicator_cz",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( employment_multipliers_cz, file = cz_file_name,
#                     sheetName = "employment_multipliers_cz",
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
#  xlsx::write.xlsx ( emp_sk, file = sk_file_name,
#                     sheetName = "emp_sk_2015",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( emp_indicator_sk,file = sk_file_name,
#                     sheetName = "emp_indicator_sk",
#                     col.names=TRUE, row.names=TRUE, append=TRUE)
#  xlsx::write.xlsx ( employment_multipliers_sk, file = sk_file_name,
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

