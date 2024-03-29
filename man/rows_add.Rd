% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rows_add.R
\name{rows_add}
\alias{rows_add}
\title{Add conforming row(s)}
\usage{
rows_add(data_table, rows_to_add, row_names = NULL, empty_fill = 0)
}
\arguments{
\item{data_table}{A symmetric input-output table, a use table, 
a margins or tax table retrieved by the  \code{\link{iotable_get}}
function.}

\item{rows_to_add}{A data frame or a named numeric vector.}

\item{row_names}{An optional name or vector of names for the key column. Defaults to \code{NULL}.}

\item{empty_fill}{What should happen with missing column values? Defaults to \code{0}. If you want
to avoid division by zero, you may consider a very small value such as 0.000001.}
}
\value{
An extended \code{data_table} with the new row(s) binded.
}
\description{
Add a conforming row, or elements of a conforming row to a names matrix.
}
\details{
If you want to add a single row manually, you can input a named numeric vector or a 
data frame with a single row. For multiple rows, input them as wide form data frame (see examples.)
}
\examples{
rows_to_add <- data.frame(iotables_row      = "CO2_emission", 
                          agriculture_group =  10448, 
                          industry_group    =  558327, # -> construction is omitted
                          trade_group       =  11194)

rows_add (iotable_get(), rows_to_add = rows_to_add)

rows_add (iotable_get(), 
          rows_to_add = c(industry_group    =  1534, 
                          trade_group       =  4),
          row_names    = "CH4_emission" )
}
\seealso{
Other iotables processing functions: 
\code{\link{conforming_vector_create}()},
\code{\link{household_column_get}()},
\code{\link{iotable_year_get}()},
\code{\link{key_column_create}()},
\code{\link{matrix_round}()},
\code{\link{output_get}()},
\code{\link{primary_input_get}()},
\code{\link{supplementary_add}()},
\code{\link{total_tax_add}()},
\code{\link{vector_transpose_longer}()},
\code{\link{vector_transpose_wider}()}
}
\concept{iotables processing functions}
