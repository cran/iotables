#' Create a Leontief matrix
#' 
#' Create a Leontief matrix from technology matrix after some basic error 
#' handling. Most likely you will need this function as a step to invoke 
#' the  function to create its inverse: 
#' \code{\link{leontief_inverse_create}}.
#' @param technology_coefficients_matrix A technology coefficient 
#' matrix created by the \code{\link{input_coefficient_matrix_create}} or 
#' \code{\link{output_coefficient_matrix_create}}.
#' @importFrom dplyr mutate across
#' @family analytic object functions
#' @return A Leontief matrix of data.frame class. The column names are 
#' ordered, and the row names are in the first, auxiliary metadata column.
#' @examples 
#' tm <- input_flow_get ( 
#'   data_table = iotable_get(), 
#'   households = FALSE)
#' L <- leontief_matrix_create( technology_coefficients_matrix = tm )
#' @export 

leontief_matrix_create <- function ( technology_coefficients_matrix ) { 

  key_column <- as.character(unlist (technology_coefficients_matrix[,1]))
  key_column
  
  total_row <- which(c("total", 'cpa_total') %in% tolower(key_column))
  total_col <- which(c("total", 'cpa_total') %in% tolower(names(technology_coefficients_matrix)))
  if ( length(total_row) > 0 ) {
    technology_coefficients_matrix <-  technology_coefficients_matrix[-total_row,]
  }
  
  if ( length(total_col) > 0 ) {
    technology_coefficients_matrix <- technology_coefficients_matrix[,-total_col]
  }
  
  Tm <- as.matrix (technology_coefficients_matrix[,2:ncol(technology_coefficients_matrix )])
  
  if ( nrow(Tm) != ncol(Tm)) stop("Error: the input matrix is not symmetric.")
  
  IminusA <- diag( nrow(Tm) ) - Tm
  
  if ( sum(vapply(IminusA, function(x) sum(is.nan(x)), numeric(1))) > 0 ) {
    warning ("Warning: There are invalid elements in the Leontief-matrix.")
  }
 
 Leontief <- cbind(
   as.data.frame(technology_coefficients_matrix [,1]),
   as.data.frame(IminusA)
   )

 names ( Leontief)[1] <- names (technology_coefficients_matrix)[1]
 Leontief[,1] <- as.character(Leontief[,1])
 
 Leontief
}

#' @rdname leontief_matrix_create
#' @export
leontieff_matrix_create <- function (technology_coefficients_matrix) {
  
  .Deprecated(new = leontief_matrix_create(technology_coefficients_matrix), 
              msg = "leontieff_matrix_create() is spelled correctly as leontief_matrix_create()")
  
  leontief_matrix_create(technology_coefficients_matrix)
  
}
