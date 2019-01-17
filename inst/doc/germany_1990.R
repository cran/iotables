## ----setup, include=TRUE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(iotables)
require(dplyr); require(tidyr)

## ----iotables------------------------------------------------------------
data_table <- iotable_get( labelling = "iotables" )
input_flow <- input_flow_get ( 
                  data_table = data_table, 
                  households = FALSE)

de_output <- primary_input_get ( data_table, "output" )
print (de_output[c(1:4)])

## ----inputcoeff, echo=FALSE----------------------------------------------
de_input_coeff <- input_coefficient_matrix_create( 
     data_table = data_table, 
     digits = 4)

##which is equivalent to 

de_input_coeff <- coefficient_matrix_create( data_table, 
                             total = "output", 
                             return_part = "products", 
                             households = FALSE,
                             digits = 4)

print ( de_input_coeff[1:3, 1:3])

## ----outputcoeff, echo=FALSE---------------------------------------------
de_out <- output_coefficient_matrix_create ( 
                                    io_table = data_table, 
                                    total = 'tfu',
                                    digits = 4)

#Rounding is slightly different in the manual
print ( de_out[1:3, 1:3] )

## ----leontieff-----------------------------------------------------------
L_de <- leontieff_matrix_create ( technology_coefficients_matrix =
                                 de_input_coeff )
I_de <- leontieff_inverse_create(de_input_coeff)
I_de_4 <- leontieff_inverse_create(de_input_coeff, digits = 4)
print (I_de_4[,1:3])

## ----employment_indicator------------------------------------------------
de_emp <- primary_input_get ( data_table,
                              primary_input = "employment_domestic_total" )

de_emp_indicator <- input_indicator_create ( 
    data_table  = data_table,
    input_vector = "employment_domestic_total")

print ( tidyr::gather( de_emp_indicator, indicators, values, !!2:ncol(de_emp_indicator))[,-1] )

## ----gva_indicator-------------------------------------------------------
de_gva <- primary_input_get ( data_table,
                              primary_input = "gva") 

de_gva_indicator  <- input_indicator_create( 
    data_table  = data_table,
    input_vector = "gva")

print( tidyr::gather(de_gva_indicator, indicators, values,!!2:ncol(de_gva_indicator))[,-1]  ) 

## ----input_indicator-----------------------------------------------------
direct_effects_de <- coefficient_matrix_create(data_table  = data_table, 
                                               total       = 'output', 
                                             return_part = 'primary_inputs')
print ( direct_effects_de[1:6,1:4])

## ----inputmultipliers----------------------------------------------------
input_reqr <- coefficient_matrix_create(
    data_table  = iotable_get (), 
    total       = 'output', 
    return_part = 'primary_inputs') 

multipliers <- input_multipliers_create(
  input_requirements = input_reqr,
  inverse = I_de)

knitr::kable(multipliers, digits= 4)

## ----employment_multiplier-----------------------------------------------
de_emp_indicator <- input_indicator_create (
  data_table = data_table, 
  input = 'employment_domestic_total') 

employment_multipliers <- multiplier_create ( 
  input_vector    = de_emp_indicator,
  Im              = I_de,
  multiplier_name = "employment_multiplier", 
  digits = 4 )

print (tidyr::gather(employment_multipliers,
              multipliers, values, !!2:ncol(employment_multipliers))[-1])

## ----outputmult----------------------------------------------------------
de_input_coeff <- input_coefficient_matrix_create( 
                         data_table = iotable_get(), 
                         digits = 4)
                           
output_multipliers <- output_multiplier_create ( 
                        input_coefficient_matrix = de_input_coeff )

print (tidyr::gather(output_multipliers, multipliers, values)[-1,])

## ----backward------------------------------------------------------------
de_coeff <- input_coefficient_matrix_create( iotable_get(), digits = 4)
I_de <- leontieff_inverse_create ( de_coeff )

de_bw <- backward_linkages(I_de)
print (tidyr::gather(de_bw, backward_linkages, values)[-1,])

## ----forward-------------------------------------------------------------
de_out <- output_coefficient_matrix_create ( 
  data_table, "final_demand", digits = 4
  )
                                    
forward_linkages ( output_coefficient_matrix = de_out )

