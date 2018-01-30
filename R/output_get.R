#' Get an output vector
#' 
#' @param source A data source, for example "naio_10_cp1700". Possible codes are "naio_10_cp1700",
#' "naio_10_cp1750", "naio_10_pyp1700", "naio_10_pyp1750", "naio_cp17_r2", "naio_17_agg_60_r2", 
#' "naio_17_agg_10_r2", "croatia_2010_1700", "croatia_2010_1800", 
#' "croatia_2010_1900". For further information consult the 
#' \href{http://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/methodology/symmetric-input-output-tables}{Eurostat Symmetric Input-Output Tables} page.
#' @param geo A country code or a country name.  For example, "SK" or as "Slovakia".
#' @param year A numeric variable containing the year. Defaults to 2010, because this year has the most data. 
#' @param unit A character string containing the currency unit, defaults to "MIO_NAC" (million national currency unit). The alternative is "MIO_EUR". 
#' @param stk_flow Defaults to "DOM", alternative "IMP". 
#' @param households If you need to make household demand endogenous, or "close the households off", TRUE selects 
#' wages and final household consumption. This is needed for induced-effects calculations.
#' @param labelling Defaults to "iotables" which gives standard row and column names regardless of the
#' source of the table, or if it is a product x product, industry x industry or product x industry table.
#' The alternative is "short" which is the original short row or column code of Eurostat or OECD.
#' @param keep_total Logical variable. Defaults to FALSE and removes the totaling row and column from the matrix.  
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join mutate_if arrange
#' @importFrom tidyr gather spread 
#' @importFrom forcats fct_reorder
#' @examples 
#' output_hr <- output_get(source = "croatia_2010_1800", geo = "HR",
#'                         year = 2010, unit = "T_NAC", labelling = "iotables")
#' @export 

output_get <- function ( source = "germany_1990", geo = "DE",
                            year = 1990, unit = "MIO_EUR",
                            households = FALSE,  
                            labelling = "iotables" , 
                            stk_flow = "DOM", 
                            keep_total = FALSE) {  
  ##Initialize variables ------------
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; values = NULL ;.= NULL #non-standard evaluation creates a varning in build. 
  iotables_row =NULL; iotables_col = NULL; prod_na = NULL; induse = NULL
  unit_input <- unit ; geo_input <- geo; stk_flow_input <- stk_flow
  source_inputed <- source 
  
  tmp_rds <- paste0(tempdir(), "\\", source, "_", labelling, ".rds")
  
  ##Exception handling --------------
  if (source == "croatia_2010_1900") {
    stop("The table croatia_2010_1900 is an import table and has no output field.")
  }
  if (! labelling %in% c("iotables", "short")) {
    stop("Only iotables or original short columns can be selected.")
  }
  
  if ( source == "germany_1990") {
    labelled_io_table <- iotable_get ( source = "germany_1990", 
                                       geo = geo_input, year = year, 
                                       unit = unit_input, labelling = labelling )     # use germany example 
    output_vector <- labelled_io_table[16,]
    if (households == TRUE ) {
      output_vector <- output_vector [1,1:8]
    } else {
      output_vector <- output_vector [1,1:7]
    }
    return ( output_vector )  #return simplified example table and do not run rest of the code
  } else {
    if ( tmp_rds %in% list.files (path = tempdir()) ) {
      labelled_io_table <- readRDS( tmp_rds ) #if already downloaded and saved as rds 
    } else { 
      labelled_io_table <- iotable_get ( source = source, 
                                         geo = geo_input, year = year, 
                                         unit = unit_input, labelling = labelling,
                                         stk_flow = stk_flow_input) }
  } # use eurostat files 
  
  labelled_io_table <- labelled_io_table %>% 
    mutate_if ( is.factor, as.character)
  
  if (households == TRUE) {
    household_consumption_col <- which ( names (labelled_io_table ) %in% 
                                           c('final_consumption_households', 'P3_S14'))
    
    if (length( household_consumption_col) > 1 ) {
      warning ( "Beware, more household consumption items were found in the table.")
    }
    if ( length( household_consumption_col) == 0 ) {
      stop ( "No household consumption data was found.")
    }
    output_row <- which (labelled_io_table[[1]] %in%  
                                     c('output_bp', 'P1', 'output') )
    if (source == "naio_cp17_r2") {
      output_row <- which (labelled_io_table[[1]] %in%  
               c('total', "CPA_TOTAL") )
    }
    
       if ( length( output_row) == 0 ) {
      stop ( "No output data was found.")
    }
    if (keep_total == TRUE) {
      message ( "Households were added to the matrix.")
      output_vector <- labelled_io_table[    output_row[1] , 
                                             c(1:67, household_consumption_col[1]) ] 
      output_vector [1,68] <- 0
    } else {
      message ( "Households were added to the matrix.")
      message ( "Total column was removed from the matrix.")
      output_vector <- labelled_io_table[    output_row[1] , 
                                             c(1:66, household_consumption_col[1]) ] 
      output_vector [1,67] <- 0
    }
 
    
  } else {    #no households case
    output_row <- which (labelled_io_table[[1]] %in%  
                           c('output_bp', 'P1', 'output') )
    if (source == "naio_cp17_r2") {
      output_row <- which (labelled_io_table[[1]] %in%  
                             c('total', "CPA_TOTAL") )
    }
    
    if ( length( output_row) == 0 ) {
      stop ( "No output data was found.")
    }
    if (keep_total == TRUE) {
      output_vector <- labelled_io_table[ output_row[1], 1:67 ] 
    } else {
      output_vector <- labelled_io_table[ output_row[1], 1:66 ]
      message ( "Total column was removed from the matrix.")
    }
    
  } # end of no household case 
  return ( output_vector ) 
}

