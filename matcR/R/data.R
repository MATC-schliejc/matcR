#'
#' Fixed width file layout for WTCS Reporting
#'
#' A dataset containing WTCS record types and their field layout
#'
#' @format A data frame with 9 variables:
#'  \describe{
#'    \item{record_id}{A file layout designation - Alpha Char Are Uppercase }
#'    \item{position_start}{numeric starting point of the data element}
#'    \item{position_end}{numeric ending point of the data element}
#'    \item{width}{the number of characters or numbers of the data element}
#'    \item{data_element_order}{the order of the data element in the record}
#'    \item{data_element_name}{The column name from the inputed data frame to be written to the fwf}
#'    \item{data_element_desc}{A description of the data element}
#'    \item{fwf_justification}{This indicates the side the data element is added from.}
#'    \item{notes}{historical changes or other information}
#'    }
#'  @source \url{https://mywtcs.wtcsystem.edu/grants-data-reporting/data-reporting-manuals/client-reporting-system/}
#'  @examples
#'  usethis::use_data(fwf_layouts)  #creates .rda file
#'  data(fwf_layouts)  #lazy loading
"fwf_layouts"

