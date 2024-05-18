#' A Function to create a fixed width file for the WTCS portal or IPEDS
#'
#' @importFrom logging logwarn
#' @param file_pattern A partial filename as string.
#' @param folder folder location of the file to load
#' @return A fully qualified path to one of more files
#' @export
#'


##~~~~~~~~~~~~~~~~~~~~~}  getFileName {~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#ARGUMENTS:
#         1. a case-insensitive string of the partial file name
#         2. a folder path (all subfolders will be checked)
#USAGE:   getFileName('the name of the file')




get_file_path <- function(file_pattern = 'all',folder = getwd()) {
  ####  To Do: customize Descr column name
  ####  Notes:
  ####      1. fuzzy match file name search
  ####      2. Could result in error if multiple file types exist
  ####      3. ignors open excel files (~$ suffix)
  ####
  ####  Arguments:
  ####      file_name:  a string pattern in the file name
  ####      folder:  starting point for searching.  Includes all subdirectories

  ####  Troubleshooting
  #file_pattern <- NULL
  #folder <- getwd()

  if(file_pattern == 'all') {
    l <-  file.info(
      list.files(folder,
                 ignore.case = TRUE,
                 full.names=TRUE,
                 recursive=TRUE,
                 include.dirs=TRUE))
    #print(rownames(l[with(l, order(desc(as.POSIXct(ctime)))),]))
  }else{
    l <- file.info(
      list.files(folder,
                 pattern = file_pattern,
                 ignore.case = TRUE,
                 full.names=TRUE,
                 recursive=TRUE,
                 include.dirs=FALSE))
    l$file <- basename(rownames(l))
    l <- l[!grepl("^~\\$", l$file),]   #removes open excel files from list
    l <- rownames(l[with(l, order(desc(as.POSIXct(mtime)))),])[1]  #retrieves the most reciently modified file
    if(is.na(l)){
      cat(paste0("NOT FOUND: ",file_pattern," -> looking in " ,folder,'\n'))
      if("logger" %in% (.packages())){
        logging::logwarn(paste0("NOT FOUND: ",file_pattern," -> looking in ",folder,'\n'),logger = 'Get')
      }
      l <- NULL
    }else{
      cat(paste0("Found File: ",l,'\n'))
    }
  }
  return(l)
}
