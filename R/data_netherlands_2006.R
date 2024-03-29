#' Simple input-output table for the Netherlands, 2006.
#'
#' This simplified SIOT is taken from the  Science Policy Integration for
#' Coastal Systems Assessment project's input-output multiplier 
#' specification sheet. It is used as a simple example SIOT for 
#' controlled analytical results. The column names were slightly altered 
#' to resemble more the current Eurostat conventions and the main example 
#' dataset \code{\link{germany_1995}}.
#' @format A data frame with 14 observations and 13 variables.
#' @source Source: Input-Output Multipliers Specification Sheet and Supporting
#' Material in the Spicosa Project Report
#' @usage data(netherlands_2006)
#' @format A data frame of 13 observations in 14 variables. 
#' \describe{
#'   \item{prod_na}{Product name, simplified, following the Eurostat conventions}
#'   \item{agriculture_group}{Simple aggregated agricultural products}
#'   \item{mining_group}{Simple aggregated mining products}
#'   \item{manufacturing_group}{Simple aggregated manufacturing products}
#'   \item{construction_group}{Construction}
#'   \item{utilities_group}{Simple aggregated utilities products/services}  
#'   \item{services_group}{Simple aggregated services products}
#'   \item{TOTAL}{Column / row sums, simple summary, not included in the original source}
#'   \item{final_consumption_private}{Simple aggregated final private use}
#'   \item{final_consumption_households}{Simple aggregated final household consumption}
#'   \item{final_consumption_government}{Simple aggregated final government consumption}    
#'   \item{gross_fixed_capital_formation}{Gross fixed capital formation 'GFCF'}   
#'   \item{exports}{Simple aggregated exports}
#'   \item{total_use}{Simple aggregated total use}              
#' }
#' @family Validation datasets
"netherlands_2006"

