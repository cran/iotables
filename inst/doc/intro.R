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
germany_io <- iotable_get(labelling = "iotables")
input_flow <- input_flow_get(
  data_table = germany_io,
  households = FALSE
)

de_output <- primary_input_get(germany_io, "output")
print(de_output[c(1:4)])

## ----inputcoeff, echo=TRUE----------------------------------------------------
de_input_coeff <- input_coefficient_matrix_create(
  data_table = germany_io,
  digits = 4
)

## which is equivalent to:
de_input_coeff <- coefficient_matrix_create(
  data_table = germany_io,
  total = "output",
  return_part = "products",
  households = FALSE,
  digits = 4
)

print(de_input_coeff[1:3, 1:3])

## ----outputcoeff, echo=FALSE--------------------------------------------------
de_out <- output_coefficient_matrix_create(
  data_table = germany_io,
  total = "tfu",
  digits = 4
)

# Rounding is slightly different in the Eurostat manual:
print(de_out[1:3, 1:3])

## ----leontief-----------------------------------------------------------------
L_de <- leontief_matrix_create(
  technology_coefficients_matrix = de_input_coeff
)
I_de <- leontief_inverse_create(de_input_coeff)
I_de_4 <- leontief_inverse_create(
  technology_coefficients_matrix = de_input_coeff,
  digits = 4
)
print(I_de_4[, 1:3])

## ----employment_indicator-----------------------------------------------------
de_emp <- primary_input_get(germany_io,
  primary_input = "employment_domestic_total"
)

de_emp_indicator <- input_indicator_create(
  data_table = germany_io,
  input_row  = "employment_domestic_total"
)

vector_transpose_longer(de_emp_indicator)

## ----gva_indicator------------------------------------------------------------
de_gva <- primary_input_get(germany_io,
  primary_input = "gva"
)

de_gva_indicator <- input_indicator_create(
  data_table  = germany_io,
  input_row   = "gva"
)

vector_transpose_longer(de_gva_indicator)

## ----input_indicator----------------------------------------------------------
direct_effects_de <- coefficient_matrix_create(
  data_table  = germany_io,
  total       = "output",
  return_part = "primary_inputs"
)

direct_effects_de[1:6, 1:4]

## ----inputmultipliers---------------------------------------------------------
input_reqr <- coefficient_matrix_create(
  data_table  = iotable_get(),
  total       = "output",
  return_part = "primary_inputs"
)

multipliers <- input_multipliers_create(
  input_requirements = input_reqr,
  Im = I_de
)

multipliers

## ----employment_multiplier----------------------------------------------------
de_emp_indicator <- input_indicator_create(
  data_table = germany_io,
  input = "employment_domestic_total"
)

employment_multipliers <- multiplier_create(
  input_vector = de_emp_indicator,
  Im = I_de,
  multiplier_name = "employment_multiplier",
  digits = 4
)

vector_transpose_longer(employment_multipliers,
  values_to = "employment_multipliers"
)

## ----outputmult---------------------------------------------------------------
de_input_coeff <- input_coefficient_matrix_create(
  data_table = iotable_get(),
  digits = 4
)

output_multipliers <- output_multiplier_create(input_coefficient_matrix = de_input_coeff)

vector_transpose_longer(output_multipliers,
  values_to = "output_multipliers"
)

## ----backward-----------------------------------------------------------------
de_coeff <- input_coefficient_matrix_create(iotable_get(), digits = 4)
I_de <- leontief_inverse_create(de_coeff)

vector_transpose_longer(backward_linkages(I_de),
  values_to = "backward_linkage_strength"
)

## ----ghoshinverse-------------------------------------------------------------
de_out <- output_coefficient_matrix_create(
  data_table = germany_io,
  total = "final_demand",
  digits = 4
)

ghosh_inverse_create(de_out, digits = 4)[, 1:4]

## ----forwardlinkages----------------------------------------------------------
forward_linkages(output_coefficient_matrix = de_out)

## ----emissions----------------------------------------------------------------
data("germany_airpol")
emissions_de <- germany_airpol[, -3] %>%
  vector_transpose_wider(
    names_from = "iotables_col",
    values_from = "value"
  )

## ----emissionsprint-----------------------------------------------------------
emissions_de

## ----outputbp-----------------------------------------------------------------
output_bp <- output_get(germany_io)

## ----emmissioncoeffs----------------------------------------------------------
emission_coeffs <- germany_io %>%
  supplementary_add(emissions_de) %>%
  input_indicator_create(input_row = as.character(emissions_de$airpol), digits = 4)

## ----emmissioncoeffsprint-----------------------------------------------------
emission_coeffs[-1, 1:3]

## ----CO2multiplier------------------------------------------------------------
multiplier_create(
  input_vector = emission_coeffs[2, ],
  Im = I_de,
  multiplier_name = "CO2_multiplier",
  digits = 4
)

## ----emissionmultipliers------------------------------------------------------
names(emission_coeffs)[1] <- names(I_de)[1]
emission_multipliers <- cbind(
  key_column_create(
    names(emission_coeffs)[1],
    gsub("_indicator", "_multiplier", unlist(emission_coeffs[-1, 1]))
  ),
  do.call(
    rbind,
    lapply(
      2:nrow(emission_coeffs),
      function(x) equation_solve(emission_coeffs[x, ], I_de)
    )
  ) %>% as.data.frame()
)

emission_multipliers[, -1] <- round(emission_multipliers[, -1], 4)

emission_multipliers[1:3, 1:4]

## ----finaldemand--------------------------------------------------------------
final_demand <- rowSums(germany_io[1:6, 9:13])

## ----emissioncontent----------------------------------------------------------
emission_content <- as.data.frame(
  round(as.matrix(emission_multipliers[1:3, -1]) %*% diag(final_demand), 0)
)
names(emission_content) <- names(emission_multipliers[, -1])

emission_content <- data.frame(
  iotables_row = gsub("_multiplier", "_content", emission_multipliers[1:3, 1])
) %>%
  cbind(emission_content)

emission_content[, 1:4]

