% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empty_remove.R
\name{empty_remove}
\alias{empty_remove}
\title{Symmetrically remove empty rows and columns}
\usage{
empty_remove(data_table)
}
\arguments{
\item{data_table}{A symmetric input-output table, or a symmetric part of a
use table or a supply table.}
}
\value{
A tibble/data.frame with a key row and a symmetric matrix, 
after removing all empty columns and rows at the same time.
}
\description{
Symmetrically remove columns with only zero values or with missing values.
}
\examples{
test_table <- input_coefficient_matrix_create(iotable_get(source = "germany_1995"))
test_table[, 2] <- 0
empty_remove (test_table)
}
