#' Internal Connection Functions
#'
#' Internally used to facilitate back-end connection to Etrade
#'
#' @import rstudioapi sodium
#'
#' @name et_connect
NULL

#' @describeIn et_connect sets system variables in order to use default app of this package
#' @export
et_default_app <- function(){
   secret::get_secret("etrade_app", key = openssl::read_key(Sys.getenv("RALGO_KEY_PATH")))
}


#' @describeIn et_connect retrieves or initializes authentication to Etrade
et_connect <- function(){
   path <- Sys.getenv("ETRADE_PATH")

   ## note that initializing a session object will store it
   if(is.null(path) | path == "" | !file.exists(path)){
      etconn <- ETRADE$new()$start()

      tryCatch({
         saveRDS(etconn, Sys.getenv("ETRADE_PATH"))
      }, error = function(c){
         stop("Could not save etconn after successful renew.
              \nSave path should have been saved on start up...
              Check valid path with: Sys.getenv('ETRADE_PATH')")
    })

  }else{
    ## read in saved session object
    tryCatch({
      etconn <- readRDS(path)
    }, error = function(c){
      stop("Cannot read previously stored connection from :\n\n", path)
    })
  }

  ## renew access if expired
  if(!etconn$remaining_dur > 0){
    tryCatch({
      etconn$renew_access()
    }, error = function(c){
      stop("Access expired and could not run renew_access(). Attempting to reset...")
    })
  }
  return(etconn)
}

#' #' @describeIn et_connect retrieves or initializes authentication to Etrade
#' #' @export
#' et_connect <- rg_connect

#' @describeIn et_connect returns ERROR if no access is found
stop_for_access <- function(){
  session <- et_connect()

  if(session$check_access() == "INACTIVE"){
    print_stamp("SESSION EXPIRED", "*")
    stop("Session expires at midnight OR after 2 hours of inactivity\n\nAuthenticate with etrade_connect()", call. = FALSE)
  }
  invisible(TRUE)
}
