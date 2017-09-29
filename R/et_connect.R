#' Connect to Etrade
#'
#' Function to Launch the Authorization process to connect to etrade and grant
#' access to trading with ralgotools.
#'
#' @import httr
#' @import shiny
#'
#' @name et_connect
NULL

#' @describeIn et_connect Launches application to faciliate 2-factor authentication
#' @export
EtradeConnect <- function(){
   appDir <- system.file("shiny-connect", "AppConnect", package = "ralgotools")
   if(appDir=="")
      stop("Count not find application to launch. Try Reinstalling 'ralgotools'", call. = FALSE)
   shiny::runApp(appDir)
}

#' @describeIn et_connect returns etrade application saved in this package
et_app <- function(){
   httr::oauth_app(appname = "ralgo",
             key = "8b8aeb67a64e91979d11247855d1d9b0",
             secret = "69d25b800ca84a90be4347469f9aff66",
             redirect_uri = "http://localhost:1410")
}


#' @describeIn et_connect function to ask for and retrieve request token from etrade
et_request_token <- function(app){
   url  <- "https://etws.etrade.com/oauth/request_token"
   sign <- httr::oauth_signature(url, app = app, other_params = c(oauth_callback="oob"))
   resp <- httr::GET(url, httr::oauth_header(sign))
   return(httr::content(resp))
}


#' @describeIn et_connect Construct url for authorization call to Etrade
et_auth_url <- function(app, rtoken){
   url <- "https://us.etrade.com/e/t/etws/authorize"
   httr::modify_url(url = url, query = list(key = app$key, token = rtoken$oauth_token))
}


#' @describeIn et_connect Internal Streamlined Connection Without Shiny Application for Testing Purposes
et_connect <- function(){
   app    <- et_app()
   rtoken <- et_request_token(app)

   url_acc <- "https://etws.etrade.com/oauth/access_token"
   authUrl <- et_auth_url(app, rtoken)

   BROWSE(authUrl)

   verifier <- readline(prompt = "Enter Access Code Sent as a Text Message: ")

   sign <-httr::oauth_signature(
      url          = url_acc,
      app          = app,
      token        = rtoken$oauth_token,
      token_secret = rtoken$oauth_token_secret,
      other_params = c(oauth_verifier = verifier)
   )
   resp <- httr::GET(url_acc, httr::oauth_header(sign))
   content(resp)
}




