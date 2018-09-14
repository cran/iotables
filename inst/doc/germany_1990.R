## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(iotables)
require(dplyr)

## ----iotables------------------------------------------------------------
de_use    <- use_table_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR", 
                       households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR",
                       households = FALSE, labelling = "iotables")
print (de_output[c(1:4)])

## ----inputcoeff, echo=FALSE----------------------------------------------
de_input_coeff <- input_coefficient_matrix_create( de_use, de_output, digits = 4)

print ( de_input_coeff[1:3, 1:3])

## ----outputcoeff, echo=FALSE---------------------------------------------
#Sample data table has not total column
io_table <- iotable_get () 
io_table <- io_table [1:which(tolower(io_table[,1]) =="total" ), ]

output_bp <- dplyr::select ( io_table, output_bp )
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])
io_table <- cbind (io_table, output_bp)

de_out <- output_coefficient_matrix_create ( io_table = io_table, 
                                    type = 'final_demand',
                                    digits = 4)

#Rounding is slightly different in the manual
print ( de_out[1:3, 1:3] )

## ----leontieff-----------------------------------------------------------
L_de <- leontieff_matrix_create( technology_coefficients_matrix =
                                de_input_coeff )
I_de <- leontieff_inverse_create(L_de)
print (I_de[,1:3])

## ----employment_indicator------------------------------------------------
de_emp <- primary_input_get ( input = "employment_total",
                              source = "germany_1990", geo = "DE",
                              year = 1990,  
                              households = FALSE, labelling = "iotables")

de_emp_indicator <- input_indicator_create (de_emp, de_output)
print ( de_emp_indicator[1:4])


## ----gva_indicator-------------------------------------------------------
de_gva <- gva_get ( source = "germany_1990" )

de_gva_indicator  <- input_indicator_create( 
    input_matrix = de_gva, 
    output_vector = de_output)

print( de_gva_indicator ) 

## ----input_indicator-----------------------------------------------------
io_table <- iotable_get () 
#Total column should not be missing:
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])

labelled_io_table <- io_table

direct_effects_de <- direct_effects_create ( labelled_io_table = io_table ) 
print ( direct_effects_de[1:6,1:4])

## ----gva_multipliers-----------------------------------------------------

de_gva_multiplier <- multiplier_create( 
                            Im = I_de, 
                            input_vector = de_gva_indicator, 
                            multiplier_name = 'gva_multiplier' )
print ( de_gva_multiplier )

## ----inputmultipliers----------------------------------------------------
#Total column should not be missing
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])

labelled_io_table <- io_table

direct_effects_de <- direct_effects_create ( labelled_io_table = io_table ) 

#remove the total column from direct effects to conform with the inverse [,-8]

multipliers <- input_multipliers_create(
  direct_effects = direct_effects_de [, -8],
  inverse = I_de, 
  labelled = TRUE)

print (multipliers[1:3, 1:3])

## ----employment_multiplier, warning=FALSE--------------------------------

de_emp <- primary_input_get ( input = "employment_total",
                              source = "germany_1990", geo = "DE",
                              year = 1990,  
                              households = FALSE, labelling = "iotables")

de_emp_indicator <- input_indicator_create (de_emp, de_output)

employment_multipliers <- multiplier_create ( 
  input_vector    = de_emp_indicator,
  Im              = I_de,
  multiplier_name = "employment_multiplier", 
  digits = 4 )

print (employment_multipliers)


## ----outputmult----------------------------------------------------------
de_input_coeff <- input_coefficient_matrix_create( 
                           de_use, de_output, digits = 4)
                           
output_multipliers <- output_multiplier_create ( 
                        input_coefficient_matrix = de_input_coeff )

print (output_multipliers)

## ----type2---------------------------------------------------------------
de_use_2    <- use_table_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR", 
                       households = TRUE, labelling = "iotables")

de_output_2 <- output_get ( source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR",
                       households = TRUE, labelling = "iotables")

gva_de_2 <- gva_get (source = "germany_1990", geo = "DE",
                       year = 1990, unit = "MIO_EUR",
                       households = TRUE, labelling = "iotables")

de_input_coeff_2 <- input_coefficient_matrix_create( de_use_2, de_output_2)

gva_indicator_de_2  <- input_indicator_create( 
    input_matrix = gva_de_2, 
    output_vector = de_output_2)

L_de_2 <- leontieff_matrix_create( technology_coefficients_matrix =
                                de_input_coeff_2 )
I_de_2 <- leontieff_inverse_create(L_de_2)

gva_multiplier_2 <- multiplier_create( Im = I_de_2,
                                       input_vector = gva_indicator_de_2)

print ( gva_multiplier_2 )

## ----backward------------------------------------------------------------
de_bw <- backward_linkages(I_de)
print (de_bw)

## ----forward-------------------------------------------------------------
#You need a table that has a total column and either the total 
#intermediate use or final use
#This is usually the case with Eurostat tables, but with the Germany data
#file total must be added.

io_table <- iotable_get () 
io_table <- io_table [1:which(tolower(io_table[,1]) =="total" ), ]

output_bp <- dplyr::select ( io_table, output_bp )
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])
io_table <- cbind (io_table, output_bp)

de_out <- output_coefficient_matrix_create ( 
  io_table, "final_demand", digits = 4
  )
                                    
forward_linkages ( output_coefficient_matrix = de_out )
#'

