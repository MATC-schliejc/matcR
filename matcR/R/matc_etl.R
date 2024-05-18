#' ---------------------------------------------------------------------#
#'  Purpose: Extract, transform & load Data
#'
#'  Created by: John Schliesmann
#'  Created Date: 2/15/2024
#'
#'  Pipeline Anatomy:
#'   pipeline       = R hash map (also the env name)
#'   [[]]           = the df or ldf name to reference in r script
#'   list(          = begins the meaningful info to ingest data
#'     name         = the file name, url, or db of data to ingest
#'     fun          = the name of the function in {Data Pre-Processing Functions.R}
#'     container    = the destination environment for the ingested data
#'
#'
#'  Modifications:
#'        1.  The replaces the old loaddata() function
#'
#'  Requirements:
#'        1. The pre-processing function must be in place before ETL execution
#'
#'  Notes:
#'        1. Data structure is setup so R project folders are at the same level as FY
#'        2. pipline naming conventions for informing extract_wrapper
#'
#'  To Do:
#'        1. improve handling of mutliple years during pre-processing???
#'        2. maybe add code to check pipeline structure
#' ----------------------------------------------------------------------#
#'
#' @import purrr
#' @import googlesheets4
#' @param pipeline R script or hash env
#' @param funs R script containing preprocessing functions
#' @return NULL
#' @export
#'
matc_etl <- function(pipeline,funs){
  #Args:
  #    pipeline - full pathname to a pipeline file (R script)
  #    funs - full pathname to a script of pre-processing functions referenced in the pipeline

  e <- new.env()
  source(funs, local = e)
  source(pipeline, local = e)

  # Function to preprocess and append dataframes
  etl <- function(key, env) {

    #support functions -------------------####
    extract_wrapper <- function(x,fun){

      #handle google sheets

      if(grepl("^https?://.+", x) ||
         class(x)[1] == 'sheets_id' ||
         class(x)[1] == 'drive_id') {
        gs4_obj <- tryCatch(gs4_get(x),
                            gargle_error_request_failed = function(e) e
        )

        if(inherits(url, "error"))
        {
          #logwarn(paste0("Could not find:",x),logger = 'Get')
        } else {
          #loginfo(paste0('Found Google Sheet:',gs4_obj$name),logger = 'Get')
          return(fun(gs4_obj$spreadsheet_id))
        }
      } else {
        #handle local files
        file_path = get_file_path(x,folder = dirname(getwd()))
        if(is.null(file_path)){#logwarn(paste0(x," not loaded"),logger = 'Get')
        } else {
          return(fun(file_path))
        }
      }
    }

    initialize_container <- function(item) {
      # Create the container environment if it does not exists.
      if (!exists(item$container, envir = .GlobalEnv, mode = "environment")) {
        assign(item$container, new.env(), envir = .GlobalEnv)
      }
    }
    #-----------------------------------------------####

    item <- env[[key]]
    #print("Processing:",item$name,"with",item$fun)
    #decide where to put each pipeline entry
    if(item$container %in% c('GlobalEnv','.GlobalEnv','')){
      target_container <- globalenv()
    } else {
      initialize_container(item) # Ensure environment/container exists
      target_container <- get(item$container, envir = globalenv())
    }

    # Execute the pre-processing function for each key in the pipeline
    df <- extract_wrapper(item$name,item$fun)

    # Assign the DataFrame to the target container using the key as the index

    assign(key, df, envir = target_container)
  }

  # Iterate through the keys in the pipeline and process each
  keys <- ls(e$pipeline)
  purrr::walk(keys, ~etl(.x, e$pipeline))

  #CleanUp
  on.exit({
    # Remove all objects in the environment `e`
    rm(list = ls(envir = e), envir = e)
  }, add = TRUE)
  return(NULL)
}

