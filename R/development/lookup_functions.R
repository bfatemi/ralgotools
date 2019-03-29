#' Lookup Etrade Functions
#'
#' Authenticated Functions to Lookup Summary Stats Regarding Current Assets.
#'
#' @param asDT Boolean. Default FALSE will return a character vector. TRUE will return
#' a one-column data.table
#'
#' @import data.table
#'
#' @name lookup_functions
NULL

#' @describeIn lookup_functions get tickers as vector or column
#' @export
lu_my_tickers <- function(asDT = FALSE){
   aid   <- list_accounts()$accountId
   posll <- list_positions(aid)
   tcks  <- sort(unique(unlist(sapply(posll, names))))
   if(asDT)
      return(data.table(ticker = tcks))
   return(tcks)
}

#' @describeIn lookup_functions aggregates and calculates return and weights of all assets owned
#' @export
lu_portfolio_stat <- function(){
   aid    <- list_accounts()$accountId
   posDT  <- list_positions(aid, asDT = TRUE)
   statDT <- posDT[, list(symbol = get("symbol"),
                          wt = get("qty")/sum(get("qty")),
                          mu = (get("marketValue") - get("costBasis"))/get("costBasis"))]
   statDT[, lapply(.SD, sum), "symbol"]
}

#' @describeIn lookup_functions calculates and prints total net return
#' @export
lu_performance_pct <- function(){
   tks <- lu_my_tickers()
   pDT <- lu_portfolio_stat()
   pct <- pDT[, round(sum(get("wt") * get("mu")), 4)]
   lab <- c("Gain: +", "Loss: -")[c(pct > 0, pct < 0)]

   msg <- paste0(lab, pct*100, "%")
   print_stamp(msg, "#")
   invisible(msg)
}





