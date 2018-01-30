## ----croatiaiotable, echoe=TRUE, results='asis', message = FALSE---------
library (iotables); library (dplyr); require (knitr)
hr_io_1800 <- iotable_get ( source = "croatia_2010_1800", geo = "HR",
                         year = 2010, unit = "T_NAC", 
                         labelling  = "short")


knitr::kable(head(hr_io_1800, 3))

## ----croatiaiotable2, echoe=TRUE, results='asis', message = FALSE--------
require(iotables); require (knitr)
hr_use_1800 <- use_table_get ( source = "croatia_2010_1800", geo = "HR",
                               year = 2010, unit = "T_NAC", 
                               labelling = "iotables", 
                               keep_total = FALSE)
output_vector_hr <- output_get(source = "croatia_2010_1800", geo = "HR",
                               year = 2010, unit = "T_NAC", 
                               labelling = "iotables", 
                               keep_total = FALSE)
knitr::kable(head(hr_use_1800, 3))

## ----croatiaiotable3, echoe=TRUE, results='asis', message = FALSE--------
require(iotables); require (knitr)
hr_coeff <- input_coefficient_matrix_create( 
  input_flow = hr_use_1800,
  output = output_vector_hr, 
  digits = 6)

#In case we used 'short' type labels:
#hr_coeff <- hr_coeff %>%
#  select ( -U ) %>%
#  filter ( t_rows2 != "CPA_U")

#In case we used 'iotables' type labels:
hr_coeff <- hr_coeff %>%
  select ( -extraterriorial_organizations ) %>%
  filter ( iotables_row != "extraterriorial_organizations")

L_hr <- leontieff_matrix_create( technology_coefficients_matrix =
                                hr_coeff )
I_hr <- leontieff_inverse_create(L_hr)
knitr::kable(head(hr_coeff, 3))

## ----croatiaemployment, echoe=TRUE, results='asis', message = FALSE------
kable (head(iotables::croatia_employment_2013, 5))

## ----croatiaemployment2, echoe=TRUE, results='asis', message = FALSE-----
multiplier_name <- data.frame ( 
  iotables_row = "employment", stringsAsFactors = FALSE)

hr_emp <- croatia_employment_2013 %>%
  dplyr::select ( iotables_row, employment ) %>%
  tidyr::spread ( iotables_row, employment, !!2:ncol(.) )  %>%
  dplyr::select ( -total, -extraterriorial_organizations ) %>%  
  cbind(data.frame ( 
  iotables_row = "employment", stringsAsFactors = FALSE), .) 

kable (hr_emp[,1:5])
##Only the first 5 columns are shown to preserve space.

## ----employmentmultipliers, echoe=TRUE, results='asis', message = FALSE----
hr_emp_indicator <- input_indicator_create (
  input_matrix = hr_emp, 
  output_vector = select (output_vector_hr, -extraterriorial_organizations  )                        )

employment_multipliers_hr <- multiplier_create ( 
  input_vector    = hr_emp_indicator,
  Im              = I_hr, 
  multiplier_name = "employment_multiplier",
  digits          = 4 ) %>%
  tidyr::gather ( t_cols2, values, !! 2:ncol(.)) %>%
  mutate ( values = values * 1000 )%>%
  arrange (., desc(values))

kable(head(employment_multipliers_hr,23))
##Only the first 5 columns are shown to preserve space.

