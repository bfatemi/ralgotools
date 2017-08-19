#' Internal Connection Functions
#'
#' Internally used to facilitate back-end connection to Etrade
#'
#' @name et_connect
NULL

#' @describeIn et_connect sets system variables in order to use default app of this package
#' @export
et_default_app <- function(){
   secret::get_secret("etrade_app", key = openssl::read_key(Sys.getenv("RALGO_KEY_PATH")))
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
# app <- et_default_app()
# req_token <- et_request_token(app)
#
#
#
# lurl             <- httr::parse_url("https://us.etrade.com/e/t/etws/authorize?key={key}&token={token}")
# lurl$query$key   <- app$key
# lurl$query$token <- req_token$oauth_token
#
# url <- httr::build_url(lurl)
#
# listen <- function(env) env$QUERY_STRING
# server <- httpuv::startServer("0.0.0.0", 7166, list(call = listen))
#
# httr::BROWSE(request_url)
# httpuv::stopServer(server)

# set_callback("request", old_req)

# r <- GET(url)

# rawToChar(r$content)
# httr::oauth_exchanger(url)
# oauth <- httr::oauth_listener(url)


# et_authorize_app <- function(app, req_token){
#    url <- "https://us.etrade.com/e/t/etws/authorize"
#
#
#
#    sign <- httr::oauth_signature(
#       url = url,
#       app = app,
#       token = req_token$oauth_token,
#       token_secret = req_token$oauth_token_secret,
#       other_params = c(oauth_callback="oob")
#    )
#
#    httr::oauth_exchanger()
#
#
#    # head <- httr::add_headers(.headers = list(key = app$key, token = req_token$oauth_token))
#    httr::content(httr::GET(url, query = list(key = app$key, token = req_token$oauth_token)))
#
#
#    lurl             <- httr::parse_url(private$url_auth)
#    lurl$query$key   <- private$app$key
#    lurl$query$token <- private$oauth_token
#
#    url   <- httr::build_url(lurl)
#    oauth <- httr::oauth_listener(url)
#    private$setAccessSign(oauth$oauth_verifier)
#
#    private$token  <- httr::content(
#       httr::GET(private$url_access, httr::oauth_header(private$sign_acc))
#    )
# }


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
