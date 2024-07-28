#' Internal functions used in write_fwf and read_matc_fwf that isolates file widths
#' @param lo fwf_layout dataframe
#' @param x record id referenced in fwf_layout
#' @keywords internal
#' @export
#'

getWidths <- function(lo,x){
  #lo %>% dplyr::filter(.data$record_id == x) %>% dplyr::pull(.data$width)
  lo$width[lo$record_id == toupper(x)]
}

#' Internal functions used in write_fwf and read_matc_fwf that isolates file widths
#' @param lo fwf_layout dataframe
#' @param x record id referenced in fwf_layout
#' @keywords internal
#' @export
#'

getFields <- function(lo,x){
  #lo %>% dplyr::filter(.data$record_id == x) %>% dplyr::pull(.data$data_element_name)
  lo$data_element_name[lo$record_id == toupper(x)]


}
#' Internal functions used in write_fwf and read_matc_fwf that isolates file widths
#' @param lo fwf_layout dataframe
#' @param x record id referenced in fwf_layout
#' @keywords internal
#' @export

getColType <- function(lo,x){
  #reads all columns as strings
  #paste0(rep('c',nrow(lo %>% dplyr::filter(.data$record_id == x))), collapse = "")
  paste0(rep('c',nrow(lo[lo$record_id == toupper(x),])), collapse = "")
}
