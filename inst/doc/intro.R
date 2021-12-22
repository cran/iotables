## ----setupknitr, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,  message=FALSE----------------------------------------------------
library(iotables)
library(dplyr, quietly = T)
library(tidyr, quietly = T)

## ----iotables-----------------------------------------------------------------
germany_io <- iotable_get( labelling = "iotables" )
input_flow <- input_flow_get ( 
                  data_table = germany_io, 
                  households = FALSE)

de_output <- primary_input_get ( germany_io, "output" )
print (de_output[c(1:4)])

## ----inputcoeff, echo=TRUE----------------------------------------------------
de_input_coeff <- input_coefficient_matrix_create( 
     data_table = germany_io, 
     digits = 4)

## which is equivalent to:
de_input_coeff <- coefficient_matrix_create( 
  data_table = germany_io, 
  total = "output", 
  return_part = "products", 
  households = FALSE,
  digits = 4)

print (de_input_coeff[1:3, 1:3])

## ----outputcoeff, echo=FALSE--------------------------------------------------
de_out <- output_coefficient_matrix_create ( 
                                    io_table = germany_io, 
                                    total = 'tfu',
                                    digits = 4)

# Rounding is slightly different in the Eurostat manual:
print(de_out[1:3, 1:3])

## ----leontieff----------------------------------------------------------------
L_de <- leontieff_matrix_create ( 
  technology_coefficients_matrix = de_input_coeff 
  )
I_de   <- leontieff_inverse_create(de_input_coeff)
I_de_4 <- leontieff_inverse_create(de_input_coeff, digits = 4)
print (I_de_4[,1:3])

## ----employment_indicator-----------------------------------------------------
de_emp <- primary_input_get(germany_io,
                            primary_input = "employment_domestic_total" )

de_emp_indicator <- input_indicator_create ( 
    data_table   = germany_io,
    input_vector = "employment_domestic_total")

vector_transpose(de_emp_indicator)

## ----gva_indicator------------------------------------------------------------
de_gva <- primary_input_get ( germany_io,
                              primary_input = "gva") 

de_gva_indicator  <- input_indicator_create( 
    data_table  = germany_io,
    input_vector = "gva")

vector_transpose(de_gva_indicator)

## ----input_indicator----------------------------------------------------------
direct_effects_de <- coefficient_matrix_create(
  data_table  = germany_io, 
  total       = 'output', 
  return_part = 'primary_inputs')

direct_effects_de[1:6,1:4]

## ----inputmultipliers---------------------------------------------------------
input_reqr <- coefficient_matrix_create(
    data_table  = iotable_get (), 
    total       = 'output', 
    return_part = 'primary_inputs') 

multipliers <- input_multipliers_create(
  input_requirements = input_reqr,
  inverse = I_de)

multipliers

## ----employment_multiplier----------------------------------------------------
de_emp_indicator <- input_indicator_create (
  data_table = germany_io, 
  input = 'employment_domestic_total') 

employment_multipliers <- multiplier_create ( 
  input_vector    = de_emp_indicator,
  Im              = I_de,
  multiplier_name = "employment_multiplier", 
  digits = 4 )

print (tidyr::gather(employment_multipliers,
              multipliers, values, 
              !!2:ncol(employment_multipliers))[-1])

## ----outputmult---------------------------------------------------------------
de_input_coeff <- input_coefficient_matrix_create( 
                         data_table = iotable_get(), 
                         digits = 4)
                           
output_multipliers <- output_multiplier_create ( 
                        input_coefficient_matrix = de_input_coeff )

vector_transpose(output_multipliers)

## ----backward-----------------------------------------------------------------
de_coeff <- input_coefficient_matrix_create(iotable_get(), digits = 4)
I_de <- leontieff_inverse_create (de_coeff)

vector_transpose(backward_linkages(I_de))

## ----forward------------------------------------------------------------------
de_out <- output_coefficient_matrix_create (
  io_table = germany_io, total = "final_demand", digits = 4
  )
                                    
forward_linkages (output_coefficient_matrix = de_out)

