#' Various internal functions to work with IOT metadata, including the labelling 
#' vocabularies, row and column ordering. 
#' None of these functions should be exported.
#' @keywords internal
getdata <- function(...) {
  e <- new.env()
  name <- utils::data(..., envir = e)[1]
  e[[name]]
}

#' @keywords internal
define_prod_ind <- function() {
  ## Define which sources follow the prod_ind vocabulary.
  ## The rest of the potential sources follows the induse vocabulary, except for the Croatia 
  ## replication data.
  
  c("naio_10_cp1700", "naio_10_cp1750", "naio_10_pyp1700",
    "naio_10_pyp1750", "naio_10_cp15", "naio_10_cp16",
    "naio_10_cp1610", "naio_10_cp1620", "naio_10_cp1630", 
    "naio_10_pyp1620", "naio_10_pyp1630", "germany_1995")
}

#' @keywords internal
adjust_stk_flow <- function( stk_flow, source ) {
  if ( source %in% c("naio_10_cp1620",  "naio_10_cp1630", 
                     "naio_10_pyp1620", "naio_10_pyp1630")
  ) {
    'TOTAL'  # Tax and margin tables only have one version 
    
  } else {
    stk_flow
  }
}

#' @keywords internal
#' @autoglobal
get_vocabulary_prod_ind <- function() {
  getdata ("metadata") %>%  # For tables that follow prod_ind vocabulary
    filter( variable == "prod_na") %>%
    dplyr::rename ( prod_na = code) %>%
    dplyr::rename ( prod_na_lab = label ) %>%
    dplyr::rename ( row_order = numeric_label ) %>%
    dplyr::rename ( iotables_row = iotables_label )
}

#' @keywords internal
#' @autoglobal
get_vocabulary_induse <- function() {
  getdata("metadata") %>%  # For tables that follow the induse vocabulary
    filter( variable == "induse") %>%
    dplyr::rename ( induse = code) %>%
    dplyr::rename ( induse_lab = label )%>%
    dplyr::rename ( col_order = numeric_label ) %>%
    dplyr::rename ( iotables_col = iotables_label )
}

#' @keywords internal
#' @autoglobal
get_vocabulary_t_rows <- function() {
  getdata("metadata") %>%
    filter( variable == "t_rows") %>%
    dplyr::rename ( t_rows2 = code) %>%
    dplyr::rename ( t_rows2_lab = label ) %>%
    dplyr::rename ( row_order = numeric_label ) %>%
    dplyr::rename ( iotables_row = iotables_label )
}

#' @importFrom dplyr rename
#' @keywords internal
#' @autoglobal
get_vocabulary_t_cols <- function() {
  getdata("metadata") %>%
    filter( variable == "t_cols") %>%
    dplyr::rename ( t_cols2 = code) %>%
    dplyr::rename ( t_cols2_lab = label ) %>%
    dplyr::rename ( col_order = numeric_label ) %>%
    dplyr::rename ( iotables_col = iotables_label )
}

#' @keywords internal
#' @autoglobal
get_metadata_rows <- function(source) {
  prod_ind <- define_prod_ind()
  trow_tcol <- croatia_files <- c('croatia_2010_1700', 'croatia_2010_1800', 
                                  'croatia_2010_1900')
  
  uk_tables <- c("uk_2010_siot", "uk_2010_use", "uk_2010_imports",
                 "uk_2010_coeff", "uk_2010_inverse")
  
  if ( source %in% prod_ind ) {
    get_vocabulary_prod_ind()
  }  else if ( source %in% trow_tcol ) {
    get_vocabulary_t_rows()
  } else if ( source %in% uk_tables ) {
    getdata("metadata_uk_2010") %>%
      filter ( !is.na(.data$uk_row)) %>%
      select ( -all_of(c("uk_col", "uk_col_label", "induse", "col_order")) ) %>%
      mutate ( uk_row = gsub("\\.", "-", as.character(.data$uk_row))) %>%
      mutate ( uk_row = gsub(" & ", "-", as.character(.data$uk_row)))
  } else {
    stop("Don't know which row name vocabulary to use.")
  }
}

#' @keywords internal
#' @autoglobal
get_metadata_cols <- function(source) {
  prod_ind <- define_prod_ind()
  
  trow_tcol <- croatia_files <- c('croatia_2010_1700', 'croatia_2010_1800', 
                                  'croatia_2010_1900')
  
  uk_tables <- c("uk_2010_siot", "uk_2010_use", "uk_2010_imports", "uk_2010_coeff", "uk_2010_inverse")
  
  if ( source %in% prod_ind ) {
    get_vocabulary_induse()
  }  else if ( source %in% trow_tcol ) {
    get_vocabulary_t_cols()
  } else if ( source %in% uk_tables ) {
    getdata("metadata_uk_2010")  %>%
      filter( !is.na(.data$uk_col)) %>%
      select( -uk_row, -uk_row_label, -prod_na, -row_order) %>%
      mutate ( uk_col = gsub("\\.", "-", as.character(.data$uk_col))) %>%
      mutate ( uk_col = gsub(" & ", "-", as.character(.data$uk_col))) %>%
      mutate ( uk_col = trimws(.data$uk_col, 'both'))
  }
}
