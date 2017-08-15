#' Etrade API Call
#'
#' A function to assist with lower-level etrade API calls.
#'
#' @param module The etrade API module to reach (one of: "accounts", "market", "order", "notification")
#' @param api The API endpoint to reach within the module
#' @param query list of other arguments for the body of the call
#' @param session An internal session object
#' @param verb A verb function from package httr (GET (default), POST, etc)
#' @param norun boolean (default = FALSE). If true, returns unevaluated api call with
#'    environment captured, that can be executed later with function \code{eval_api}
#' @param api_call unevaluted api call retuned by \code{etrade_api(..., norrun = TRUE)}
#' @param url send in entire url string instead of using module and api
#'
#' @import data.table
#' @importFrom rlang env
#' @importFrom pryr make_call
#' @importFrom httr GET parse_url build_url accept_json stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_c str_split
#'
#' @name internal_api
NULL

#' @describeIn internal_api the function that creates an etrade api call
#' @export
etrade_api <- function(module = c("accounts", "market", "order", "notification"),
                       api = NULL,
                       session = NULL,
                       query = NULL,
                       verb = httr::GET,
                       norun = FALSE,
                       url = NULL){

   if(is.null(api) & is.null(url))
      stop("api and url are NULL")
   if(is.null(session))
      stop("no session object provided")

   if(is.null(url))
      url <- etrade_url(module, api, query)

   args <- session$api_arg(url)
   apiCall <- pryr::make_call(`verb`, args, envir = rlang::env())

   if(norun)
      return(apiCall)

   resp <- eval(apiCall)
   httr::stop_for_status(resp)
   parsed <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
   return(parsed)
}

#' @describeIn internal_api A helper function to build Etrade API call
etrade_url <- function(module=NULL, api, query=NULL){
   tmp <- stringr::str_split(module, "/")[[1]]
   mod <- stringr::str_c(tmp[tmp!=""], collapse = "/")

   tmp <- stringr::str_split(api, "/")[[1]]
   api <- stringr::str_c(tmp[tmp!=""], collapse = "/")

   url <- paste0("https://etws.etrade.com/", mod, "/rest/", api)

   purl <- httr::parse_url(url)
   purl$query <- query
   httr::build_url(purl)
}

#' @describeIn internal_api A helper function that evaluates an etrade API call
eval_api <- function(api_call){
   resp <- eval(api_call)
   httr::stop_for_status(resp)
   jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
}

