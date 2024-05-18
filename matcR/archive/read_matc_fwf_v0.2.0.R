#' A Function to create a fixed width file for the WTCS portal or IPEDS
#'
#' @import dplyr
#' @import readr
#' @import utils
#' @importFrom rlang .data
#' @importFrom rlang as_name
#' @param file_pattern A partial filename as string.
#' @param folder folder location of the file to load
#' @param rcd Record types to format from the file
#' @param ldf Is the output in a list dataframe?  alternative is individual dataframes loaded to Global Env.
#' @return A boolean of TRUE
#' @export
#'
read_matc_fwf <- function(file_pattern = 'WIX.CLIENT',folder = getwd(),rcd = 'client',ldf = FALSE){
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

  #usethis::use_data('fwf_layouts', internal = TRUE)
  fwf_layouts <- get0("fwf_layouts", envir = asNamespace("matcR"))

  file_name <- NULL

  #utils::globalVariables("fwf_layouts")
  #load(fwf_layouts)
  #--- ERROR CHECK FOR WTCS RECORDTYPES ----------##

  #try
  #chk <- c('S1','S2','S3','S4','S5','S6','S7','S8','S9','C','X1','X2','X3','G','FW')


  #--- OPTIONAL DEFAULT RECORD TYPE GROUPINGS ----##
  if(rcd[1] == 'client') { rcd <- c('S1','S2','S3','S4','S5','S6','S7','S8','S9') }


  #--- ARGS ERROR CHECKS -------------------------##
  rcd <- toupper(rcd)  #WTCS naming conventions expect uppercase which is also in the layout file

  valid_record_id  <- fwf_layouts %>% select(.data$record_id) %>% unique()

  if(any(rcd %in% as.character(valid_record_id[,1]))==FALSE) {stop("Invalid record(s) type selected.  \nPlease reference fwf_layouts for valid record types.")}

  #--- GET FWF FOR COLUMN PROCESSING
  df <- file.info(
    list.files(folder,
               pattern = file_pattern,
               ignore.case = TRUE,
               full.names=TRUE,
               recursive=TRUE,
               include.dirs=FALSE))


  file_name <- rownames(df[with(df, order(desc(as.POSIXct(ctime)))),])[1]  #retrieves the most reciently modified file
  mtime <- df[with(df, order(desc(as.POSIXct(ctime)))),4][1]  #last modified time
  if(nrow(df)==0){
    stop("FILE NOT FOUND IN:",folder,'\n')
  }else{
    cat('Processing: ',file_name,'  last modified:',as.character(mtime) ,'\n')
  }
  rm(df)




  #-----------------------------------------------##





  #--- LOAD RECORD LAYOUT ------------------------##
  # This looks for the most recently edited RDS file in c:\covid


  #Use below to hard code an RDS file
  #l <- 'S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data\\_WTCS File Layouts\\_RECORD LAYOUTS.RDS'
  #--- MAIN FUNCTION -----------------------------##


  rcd_list <- list()



  getWidths(fwf_layouts,rcd[1])
  #getFields(fwf_layouts,rcd[1])
  #getColType(fwf_layouts,rcd[1])

  suppressWarnings(
    for(i in 1:length(rcd)){
      cat("Processing",rcd[i],"Record\n")
      rcd_list[i] <- list (readr::read_fwf(file_name,
                                    na = ' ',
                                    readr::fwf_widths(c(getWidths(fwf_layouts,rcd[i])),
                                               c(getFields(fwf_layouts,rcd[i]))),
                                    getColType(fwf_layouts,rcd[i])) %>%
                             dplyr::filter(.data$record_id == rcd[i])
                           )
    }
  )
  # the read_fwf will cause parse errors when reading s6 because the row length is short

  names(rcd_list) <- tolower(rcd)  #switch to lower case for R script data frame naming conventions


  if(ldf == FALSE){
    cat('Moving',length(names(rcd_list)),'dataframe to global environment.')
    list2env(rcd_list ,.GlobalEnv) #moves all dataframes to global environment
    return(TRUE)
  } else {
    return(rcd_list)
  }
  return(0)
}


