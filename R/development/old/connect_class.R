#' Etrade Session Management
#'
#' Functions to manage the active session. Including functions to begin or renew a stale session
#'
#' @import httr
#' @import data.table
#' @importFrom lubridate ymd_hms
#' @importFrom R6 R6Class
#'
#' @name etrade_session
#'
ETRADE <- R6::R6Class(
   classname = "ETRADE"
)

private <- list(start_time    = NULL,
                end_time      = NULL,
                last_accessed = NULL,
                url_auth      = "https://us.etrade.com/e/t/etws/authorize?key={key}&token={token}",
                url_request   = "https://etws.etrade.com/oauth/request_token",
                url_access    = "https://etws.etrade.com/oauth/access_token",
                url_renew     = "https://etws.etrade.com/oauth/renew_access_token",
                app           = do.call(httr::oauth_app, et_default_app()),
                token         = NULL,
                sign_req      = NULL,
                sign_acc      = NULL,
                oauth_token   = NULL,
                oauth_secret  = NULL,
                session_path  = NULL)

if(Sys.getenv("ETRADE_PATH") == "")
   Sys.setenv(ETRADE_PATH = paste0(Sys.getenv("RALGO_DIR"), "/.etrade"))


private$start_time    <- Sys.time()
private$end_time      <- lubridate::ymd_hms(paste0(as.Date(private$start_time), " 23:59:59"), tz = Sys.timezone())
private$last_accessed <- private$start_time

# saveRDS(private, Sys.getenv("ETRADE_PATH"))

## API CALL ARGS - required by etrade_api
api_arg = function(url){

   # AUTH HEADER - get header at time of api call. Note this function is called by api_arg
   oahead <- httr::oauth_header(
      httr::oauth_signature(
         url          = url,
         app          = private$app,
         token        = private$access_token$oauth_token,
         token_secret = private$access_token$oauth_token_secret
      )
   )

   list(url = url, oahead, config = httr::accept_json())
}

#
# SET ACCESS REQUEST SIGNATURE
#
private$sign_req <- httr::oauth_signature(
   url          = private$url_request,
   app          = private$app,
   other_params = c(oauth_callback="oob")
)

oahead <- httr::oauth_header(private$sign_req)
rtoken <- httr::content(httr::GET(private$url_request, oahead))

private$oauth_token  <- rtoken$oauth_token
private$oauth_secret <- rtoken$oauth_token_secret

#
# GET/SET ACCESS TOKEN SIGNATURE
#
lurl             <- httr::parse_url(private$url_auth)
lurl$query$key   <- private$app$key
lurl$query$token <- private$oauth_token

url   <- httr::build_url(lurl)
oauth <- httr::oauth_listener(url)

private$sign_acc <- httr::oauth_signature(
   url          = private$url_access,
   app          = private$app,
   token        = private$oauth_token,
   token_secret = private$oauth_secret,
   other_params = c(oauth_verifier = oauth$oauth_verifier)
)


#
# GET/SET ACCESS TOKEN
#
private$token  <- httr::content(httr::GET(private$url_access,
                                          httr::oauth_header(private$sign_acc)))

# TIMESTAMP ACCESS
self$last_accessed <- Sys.time()

   # if( !private$setRequestToken() )
   #    stop("Problem setting request token")
   #
   # if( !private$setAccessToken() )
   #    stop("Problem setting access token")




      ## TOKEN STATUS - active variable because it checks the status
      check_access = function(private){

         if(is.null(private$token))
            return("NO SESSION FOUND")

         if(self$remaining_dur < 0)
            return("EXPIRED")

         if(difftime(Sys.time(), self$last_accessed, units = "hour") > 2)
            return("INACTIVE")

         ## Every user call is causing this function to run twice in a row. Figure out why
         # message("Remaining duration in session: ", self$remaining_dur)
         return("ACTIVE")
      }

      ##
      ## RENEW INACTIVE - token inactive if hasnt been used in 2 hours
      ##
      renew_access = function(){
         sign <- httr::oauth_signature(
            url          = private$url_renew,
            app          = private$app,
            token        = private$token$oauth_token,
            token_secret = private$token$oauth_token_secret
         )
         resp <- httr::GET(private$url_renew, httr::oauth_header(sign))
         httr::stop_for_status(resp)

         self$last_accessed <- Sys.time()
         return(TRUE)
      }

      ##
      ## ETRADE APP - user facing, mostly used for debugging right now
      ##
      etrade_app = function(){
         return(private$app)
      }
   )


   # ACTIVE LIST -------------------------------------------------------------


   active = list(

      ##
      ## ACCESS_TOKEN - active variable because it checks the status
      ##
      access_token = function(){
         status <- self$check_access()

         if(status == "NO SESSION FOUND")
            self$start()

         if(status == "EXPIRED")
            stop("Access has expired. Reconnect with XYZ")
         if(status == "INACTIVE")
            self$renew_access()

         self$last_accessed <- Sys.time()
         return(private$token)
      },

      ##
      ## REMAINING DURATION - active variable because it calculates when called
      ##
      remaining_dur = function(){
         difftime(self$end_time, Sys.time(), units = "hour")
      }
   )
)


   # PRIVATE LIST ------------------------------------------------------------

   # private = list(
   #
   #    url_auth     = "https://us.etrade.com/e/t/etws/authorize?key={key}&token={token}",
   #    url_request  = "https://etws.etrade.com/oauth/request_token",
   #    url_access   = "https://etws.etrade.com/oauth/access_token",
   #    url_renew    = "https://etws.etrade.com/oauth/renew_access_token",
   #    app          = NULL,
   #    token        = NULL,
   #    sign_req     = NULL,
   #    sign_acc     = NULL,
   #    oauth_token  = NULL,
   #    oauth_secret = NULL,
   #    session_path = NULL,








#    )
#
# )
