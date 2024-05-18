#' A Function to create a fixed width file for the WTCS portal or IPEDS
#'
#' @import dplyr
#' @import readr
#' @import utils
#' @importFrom rlang .data
#' @importFrom rlang as_name
#' @param file Name of input file including path
#' @param rcd Record types to format from the file
#' @param ldf Is the output in a list dataframe?  alternative is individual dataframes loaded to Global Env.
#' @return A list of dataframes when ldf is TRUE or a boolean of TRUE
#' @export
#'
read_matc_fwf <- function(file,rcd = 'CLIENT',ldf = FALSE){
  #Purpose:  This converts a WTCS formatted fixed-width text file to a dataframe
  #Args: file_pattern: WTCS formatted filename or partial filename to convert
  #      folder: the parent folder to begin search for file_pattern
  #      rcd: The record layout to apply to conversion (multiple are accepted)
  #      ldf: defines output organization: TRUE = all records in a single list of dataframes; FALSE = all separate records
  #Usage:  If ldf == FALSE then do not assign it to a object
  #        If ldf == TRUE then assign it to a list object (ex: s1 <- loadWTCS(file,rcd = 'S1',ldf = TRUE)
  #Requirements: This relys on an updated copy of {_WTCS Record Layout.RDS}.  See also the excel version used to create the RDS
  #              Uses functions in the helper.R file
  #ToDo: add function to search for files (both the fwf & layout)
  ## @importFrom stats filter



  #--- NORMALIZE ARGUMENTS -----------------------##
  rcd <- toupper(rcd)  #WTCS naming conventions expect uppercase which is also in the layout file


  #--- LOAD RECORD LAYOUT ------------------------##
  #usethis::use_data('fwf_layouts', internal = TRUE)
  #fwf_layouts <- get0("fwf_layouts", envir = asNamespace("matcR"))
  data("fwf_layouts", package = "matcR", envir = environment())
  if(is.null(fwf_layouts)){stop('Error loading fwf_layouts.  Countact Package Author.')}


  #--- OPTIONAL DEFAULT RECORD TYPE GROUPINGS ----##
  if(rcd[1] == 'CLIENT') { rcd <- c('S1','S2','S3','S4','S5','S6','S7','S8','S9') }

  #--- VALIDATE RCD ARGUMENT ---------------------##
  valid_record_id  <- unlist(fwf_layouts %>% select(.data$record_id) %>% unique())
  if(any(rcd %in% valid_record_id)==FALSE) {stop("Invalid record(s) type selected.  \nPlease reference fwf_layouts for valid record types.")}
  rm(valid_record_id)


  #--- CHECK THAT FILE EXISTS --------------------##

  #Do I need this section?
  #if(file.exists(filename))

  #Use below to hard code an RDS file
  #l <- 'S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data\\_WTCS File Layouts\\_RECORD LAYOUTS.RDS'
  #--- MAIN FUNCTION -----------------------------##

  rcd_list <- list()

  suppressWarnings(
    for(i in 1:length(rcd)){
      rcd_list[i] <- list (readr::read_fwf(file,
                                    na = ' ',
                                    readr::fwf_widths(c(getWidths(fwf_layouts,rcd[i])),
                                               c(getFields(fwf_layouts,rcd[i]))),
                                    getColType(fwf_layouts,rcd[i])) %>%
                             #dplyr::filter(.data$record_id == rcd[i])
                             dplyr::filter(across(any_of("record_id"), ~. == rcd[i]))
                              )
      cat("Processed the",rcd[i],"Record\n")
      #print(rcd_list[i])
    }
  )


  #readr::fwf_widths(getWidths(fwf_layouts,rcd[i]),
  #                  c(getFields(fwf_layouts,rcd[i])))



  # the read_fwf will cause parse errors when reading s6 because the row length is short


  names(rcd_list) <- tolower(rcd)  #switch to lower case for R script data frame naming conventions


  if(ldf == FALSE){
    cat('Moving',length(names(rcd_list)),'dataframe to global environment.')
    list2env(rcd_list ,.GlobalEnv) #moves all dataframes to global environment
    #return(TRUE)
    return()
  } else {
    return(rcd_list)
  }
  return(0)
}


