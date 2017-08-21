#' Internal Connection Functions
#'
#' Internally used to facilitate back-end connection to Etrade
#'
#' @name et_connect
NULL

#' @describeIn et_connect sets system variables in order to use default app of this package
#' @export
et_default_app <- function(){
   op <- getOption("secret.vault")
   vpath <- system.file("vault", package = "ralgotools")
   on.exit(options(secret.vault = op))

   key <- openssl::read_key(Sys.getenv("RALGO_KEY_PATH"))
   app <- secret::get_secret(name = "etrade_app", key = key)
   return(app)
}


# et_request_token <- function(app){
#    url <- "https://etws.etrade.com/oauth/request_token"
#
#    sign <- httr::oauth_signature(
#       url = url,
#       app = app,
#       other_params = c(oauth_callback="oob")
#    )
#
#    oahead <- httr::oauth_header(sign)
#    rtoken <- httr::content(httr::GET(url, oahead))
#    return(rtoken)
# }
#
# et_auth_access <- function(app, req_token){
#
#    url_access <- "https://etws.etrade.com/oauth/access_token"
#
#    lurl             <- httr::parse_url("https://us.etrade.com/e/t/etws/authorize?key={key}&token={token}")
#    lurl$query$key   <- app$key
#    lurl$query$token <- req_token$oauth_token
#    verifier <- httr::oauth_exchanger(httr::build_url(lurl))$code
#
#    sign <- httr::oauth_signature(
#       url          = url_access,
#       app          = app,
#       token        = req_token$oauth_token,
#       token_secret = req_token$oauth_secret,
#       other_params = c(oauth_verifier = verifier)
#    )
#
#
#    httr::content(httr::GET(url_access, httr::oauth_header(sign)))
# }
#
#
#
# app <- et_default_app()
# req_token <- et_request_token(app)
# access_token <- et_auth_access(app, req_token)


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
      message("Access expired and could not run renew_access(). Deleting old connection...")
       file.remove(Sys.getenv("ETRADE_PATH"))
       stop("Connection deleted. Try re-running function", call. = FALSE)
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
