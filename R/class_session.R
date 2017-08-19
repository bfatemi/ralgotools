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
   classname = "ETRADE",

   public = list(
      start_time    = NULL,
      end_time      = NULL,
      last_accessed = NULL,


      ## Start Session
      start = function(){
         private$setApp()

         if( !private$setRequestToken() )
            stop("Problem setting request token")

         if( !private$setAccessToken() )
            stop("Problem setting access token")

         self$start_time    <- Sys.time()
         self$end_time      <- lubridate::ymd_hms(paste0(as.Date(self$start_time), " 23:59:59"), tz = Sys.timezone())
         self$last_accessed <- self$start_time
         saveRDS(self, Sys.getenv("ETRADE_PATH"))
         invisible(self)
      },

      ## INITIALIZE SESSION
      ##  - Set environmental variables/creating dir if does not exist
      initialize = function(){
         if(Sys.getenv("RALGO_DIR") == "")
            Sys.setenv(RALGO_DIR = paste0(Sys.getenv("R_LIBS_USER"), "/ralgo_data"))

         if(Sys.getenv("ETRADE_PATH") == "")
            Sys.setenv(ETRADE_PATH = paste0(Sys.getenv("RALGO_DIR"), "/.etrade"))

         if(!dir.exists(Sys.getenv("RALGO_DIR")))
            dir.create(Sys.getenv("RALGO_DIR"))
      },

      ## API CALL ARGS - required by etrade_api
      api_arg = function(url){
         list(url = url, private$getAuthHeader(url), config = httr::accept_json())
      },

      ## TOKEN STATUS - active variable because it checks the status
      check_access = function(){

         if(is.null(private$token))
            return("NO SESSION FOUND")

         if(self$remaining_dur < 0)
            return("EXPIRED")

         if(difftime(Sys.time(), self$last_accessed, units = "hour") > 2)
            return("INACTIVE")

         ## Every user call is causing this function to run twice in a row. Figure out why
         # message("Remaining duration in session: ", self$remaining_dur)
         return("ACTIVE")
      },

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
      },

      ##
      ## ETRADE APP - user facing, mostly used for debugging right now
      ##
      etrade_app = function(){
         return(private$app)
      }
   ),


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
   ),


   # PRIVATE LIST ------------------------------------------------------------

   private = list(

      url_auth     = "https://us.etrade.com/e/t/etws/authorize?key={key}&token={token}",
      url_request  = "https://etws.etrade.com/oauth/request_token",
      url_access   = "https://etws.etrade.com/oauth/access_token",
      url_renew    = "https://etws.etrade.com/oauth/renew_access_token",
      app          = NULL,
      token        = NULL,
      sign_req     = NULL,
      sign_acc     = NULL,
      oauth_token  = NULL,
      oauth_secret = NULL,
      session_path = NULL,

      ##
      ## ETRADE APP - set/get. Default if no params provided
      ##
      setApp = function(appname = NULL, key = NULL, secret = NULL){

         if(is.null(appname) | is.null(appname) | is.null(appname)){
            private$app <- do.call(httr::oauth_app, et_default_app())
            return(TRUE)
         }else{
            stop("App parameters required: appname, key, secret")
         }

         args <- list(appname = appname, key = key, secret = secret)
         private$app <- do.call(httr::oauth_app, args)
         return(TRUE)
      },


      ###
      ### REQUEST SET - signature, then token
      ###
      setRequestSign = function(){
         private$sign_req <- httr::oauth_signature(
            url          = private$url_request,
            app          = private$app,
            other_params = c(oauth_callback="oob")
         )
         return(TRUE)
      },
      setRequestToken = function(){
         private$setRequestSign()
         oahead <- httr::oauth_header(private$sign_req)
         rtoken <- httr::content(httr::GET(private$url_request, oahead))

         private$oauth_token  <- rtoken$oauth_token
         private$oauth_secret <- rtoken$oauth_token_secret
         return(TRUE)
      },

      ###
      ### ACCESS SET - signature, then token
      ###
      setAccessSign = function(verifier){
         private$sign_acc <- httr::oauth_signature(
            url          = private$url_access,
            app          = private$app,
            token        = private$oauth_token,
            token_secret = private$oauth_secret,
            other_params = c(oauth_verifier = verifier)
         )
         return(TRUE)
      },
      setAccessToken = function(){
         lurl             <- httr::parse_url(private$url_auth)
         lurl$query$key   <- private$app$key
         lurl$query$token <- private$oauth_token

         url   <- httr::build_url(lurl)
         oauth <- httr::oauth_listener(url)
         private$setAccessSign(oauth$oauth_verifier)

         private$token  <- httr::content(
            httr::GET(private$url_access, httr::oauth_header(private$sign_acc))
         )

         self$last_accessed <- Sys.time()
         return(TRUE)
      },


      ###
      ### AUTH HEADER - get header at time of api call. Note this function is called by api_arg
      ###
      getAuthHeader = function(url){
         httr::oauth_header(
            httr::oauth_signature(
               url          = url,
               app          = private$app,
               token        = self$access_token$oauth_token,
               token_secret = self$access_token$oauth_token_secret
            )
         )
      }
   )

)

