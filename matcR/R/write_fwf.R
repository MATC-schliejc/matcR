#' A Function to create a fixed width file for the WTCS portal or IPEDS
#'
#' @import tidyverse
#' @import utils
#' @importFrom rlang .data
#' @importFrom rlang as_name
#' @param x A dataframe with a column called record_id matching a layout file id
#' @param output_file_name The name of the export file
#' @param type use WTCS layout file or IPEDS layout file
#' @return A boolean of TRUE
#' @export
#'

write_fwf <- function(x,output_file_name,type = 'WTCS'){
  #create  dynamic code / function to create script to write out fwf file
  ##https://stackoverflow.com/questions/21514092/how-to-call-expression-result-after-paste-command-in-r



  #AGRS: x - a list of dataframes or a dataframe containing all columns with all data_element_names from Record Layout file
  #          #note:  how to know what layout to use if a df is passed to function??? does df need a column called record_id?
  #      output_file_name - text file name, user digression
  #      type - WTCS layout or IPEDS

  #USAGE EXAMPLE: write_fwf(iEF2b,paste0('iEF2b-',FISCAL_YEAR,'.txt'),record_layout = 'iEF2b')
  #               write_fwf(crs,'test_write_fwf.txt','WTCS Record Layouts')
  #notes for record layout:
  #      fwf_justification is the side the spaces go on.
  #      source for creating fwf file is cmadden@edisonohio.edu
  #           https://www.airweb.org/eAIR/techtips/Pages/Importing-Exporting-Fixed-Data.aspx


  #require('tidyverse')
  #testing:  output_file_name = 'test.txt'

  #https://r-pkgs.org/data.html  reference for using internal .rda files
 # usethis::use_data(fwf_layouts, internal = TRUE)
 # usethis::use_data(fwf_layouts)

   # Validate input
  if (!inherits(x, "data.frame") && !inherits(x, "list")) {
    stop("x must be a data frame or a list of data frames")
  }
  if (!is.character(output_file_name)) {
    stop("output_file_name must be a character string")
  }
  if (!type %in% c("WTCS", "IPEDS")) {
    stop("type must be either 'WTCS' or 'IPEDS'")
  }



  fwf_layouts <- get0("fwf_layouts", envir = asNamespace("matcR"))
  if(is.null(fwf_layouts)){stop('Error loading fwf_layouts.  Countact Package Author.')}

  final <- NULL

  #inherits(x, "data.frame") #FALSE if ldf, TRUE if df

  if(inherits(x, "data.frame")) {
        cat('Yes, this is a df\n')
       # y <- deparse(substitute(x))  #the value of y is x
        y <- as.character(x[1,1])
        #rcd <- fwf_layouts %>% stats::filter(.data$record_id == toupper(y)) #is an list element from rcd; not sure this will work...
        rcd <- fwf_layouts[fwf_layouts$record_id == toupper(y),]
       # n <- paste0("str_trunc(str_pad(",deparse(substitute(x)),"$",rcd$data_element_name,',',rcd$width,",'",rcd$fwf_justification,"'),",rcd$width,",'",rcd$fwf_justification,"',ellipsis='')",collapse=', ')
       # n2 <-paste0("final <- c(sprintf(paste0(rep('%s',ncol(",deparse(substitute(x)),")),collapse = ''),",n,"))")
        n <- paste0("str_trunc(str_pad(x$",rcd$data_element_name,',',rcd$width,",'",rcd$fwf_justification,"'),",rcd$width,",'",rcd$fwf_justification,"',ellipsis='')",collapse=', ')
        n2 <-paste0("final <- c(sprintf(paste0(rep('%s',ncol(x)),collapse = ''),",n,"))")

        eval(parse(text = n2))
        write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE)

        cat('Completed writing ',y,' to ',output_file_name)
        rm(final,n,n2)
        return(TRUE)
  } else if (inherits(x,"list")){
    #Processing a list of dataframes
    # This assumes the list elements are dataframes

    rcds <- names(x)  #lists all df index names in a ldf  --- will replace rcd in loop
    for(i in rcds){
      cat('Writing ',toupper(i),'\n')
      rcd <- fwf_layouts[fwf_layouts$record_id == toupper(i),]
      #rcd <- subset(fwf_layouts,.data$record_id == toupper(i))  #is an list element from rcds
      if(!nrow(rcd)){stop('Record id not found in fwf_layouts.rda \n')}  # if 0 rows stop
      n <- paste0("str_trunc(str_pad(x[['",rlang::as_name(i)[1],"']]$",rcd$data_element_name,',',rcd$width,",'",rcd$fwf_justification,"'),",rcd$width,",'",rcd$fwf_justification,"',ellipsis='')",collapse=', ')
      n2 <-paste0("final <- c(sprintf(paste0(rep('%s',ncol(x[['",rlang::as_name(i)[1],"']])),collapse = ''),",n,"))")
      eval(parse(text = n2))
      write.table(as.data.frame(final),
                  output_file_name,
                  append = TRUE,
                  col.names = FALSE,
                  row.names = FALSE,
                  quote = FALSE)
    rm(final,n,n2)
    }
    return(TRUE)
  } else {#cat("Object Class is not available for Processing\n")   #cat generates error here
          return(FALSE)
    }
}
