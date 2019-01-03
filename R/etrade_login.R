

#' Basic Etrade Login Function
#'
#' @return Access token for continued api use
#' @export
#'
#' @import httr rstudioapi
#'
etrade_login <- function(){

   ## DEFINE APPLICATION
   app <- oauth_app(appname = "ralgo",
                    key = "644b829eead8b987365c37a5d4717920",
                    secret = "e1569af52ea44ddd27e7d9f5391ba12f")

   ##
   ## GET REQUEST TOKEN (PART 1 OF OAUTH)
   #@
   url.request <- "https://api.etrade.com/oauth/request_token"
   sign <- oauth_signature(url = url.request,
                           app = app,
                           other_params = c(oauth_callback="oob"))
   resp <- GET(url.request, oauth_header(sign))
   rtoken <- content(resp)

   ##
   ## GET AUTHORIZE TOKEN (PART 2 OF OAUTH)
   #@
   url.authorize <- modify_url(url = "https://us.etrade.com/e/t/etws/authorize",
                               query = list(
                                  key = app$key,
                                  token = rtoken$oauth_token
                               ))

   bool_permit <- showQuestion(title = "Secure and Encrypted Authentication",
                               message = "Continue to Etrade.com for Secure Authentication",
                               ok = "Yes", cancel = "No")

   if(bool_permit){
      browseURL(url.authorize)
      verifier <- showPrompt("Enter Access Code",
                             "Copy and Paste Access Code Provided by Etrade and continue",
                             default = "XXXX")

      url.access <- "https://api.etrade.com/oauth/access_token"
      sign <- oauth_signature(
         url          = url.access,
         app          = app,
         token        = rtoken$oauth_token,
         token_secret = rtoken$oauth_token_secret,
         other_params = c(oauth_verifier = verifier)
      )
      resp <- GET(url_acc, oauth_header(sign))
      return(content(resp))
   }
}
