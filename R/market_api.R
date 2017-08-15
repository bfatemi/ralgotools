#' Get Up-to-Date Quote Information
#'
#' This function will return a data.table containing quotes and additional
#' information, such as Fundementals or Intraday prices, for the provided
#' ticker or tickers.
#'
#' @param tickers A character vector of tickers
#' @param detailFlag Optional. Can specify a subset of data provided by API.
#'        Defaults to 'ALL' which returns all variables for given tickers
#' @param companyname A character vector specifying the company name to look the ticker up
#' @param type One of "EQ" for equity or "MF" for mutual fund
#'
#' @import data.table
#'
#' @return A dataset of class data.table
#'
#' @name market_api
#' @examples
#' # et_quote(c("BAC", "GOOG"))
NULL

#' @describeIn market_api Function to retrieve quotes and other information
#' @export
et_quote <- function(tickers = NULL, detailFlag = c("ALL", "FUNDAMENTAL","INTRADAY", "WEEK_52")){
   session <- et_connect()

   if(is.null(tickers))
      stop("No tickers supplied to getQuote", call. = FALSE)

   ## Construct API call and get raw data
   ##
   if(length(tickers) > 1)
      ticks <- paste0("quote/", paste(tickers, collapse = ","))
   else{
      ticks <- paste0("quote/", tickers)
   }
   dFlag   <- match.arg(detailFlag, c("ALL", "FUNDAMENTAL","INTRADAY", "WEEK_52"))
   rData   <- etrade_api(module = "market",
                         api = ticks,
                         query = list(detailFlag = dFlag),
                         session = session)

   ## Transform each ticker list item and bind together into table
   qData <- rData$quoteResponse$quoteData
   lname <- gsub("_", "", tolower(dFlag))

   if(length(tickers) > 1){

      rowsList <- lapply(qData, function(tick){
         if(!is.null(tick$errorMessage))
            return(NULL)
         res <- c(tick$product, tick[[lname]])
         setDT(res)
         return(res)
      })

      resDT <- rbindlist(rowsList, fill=TRUE)

   }else{
      resDT <- c(qData$product, qData[[lname]])
      setDT(resDT)
   }

   ## flag each row that has valid data
   resDT[, c("bValid") := TRUE]

   ## Check for missing data and return
   ind <- !str_extract(tickers, "[A-Z]+") %in% resDT$symbol
   if(sum(ind) > 0){
      warning(paste0("No data returned: ", paste0(tickers[ind], collapse = ", ")))
      tmp <- data.table(symbol = tickers[ind])
      tmp[, c("bValid") := FALSE]
      return(rbindlist(list(resDT, tmp[]), fill=TRUE))
   }
   resDT[]
}




#' @describeIn market_api Function search for ticker based on potential company name
#' @export
et_find_ticker <- function(companyname = NULL, type = c("EQ", "MF")){
   session <- et_connect()

   if(is.null(companyname))
      stop("No company name given to look up", call. = FALSE)

   stype <- match.arg(type, c("EQ", "MF"))

   rData   <- etrade_api(module = "market",
                         api = "productlookup",
                         query = list(company = companyname, type=stype),
                         session = session)

   plist <- rData$productLookupResponse$productList
   if(length(companyname) > 1)
      return(rbindlist(lapply(plist, setDT)))
   return(as.data.table(plist))
}

#' @describeIn market_api Function to pull all tickers in active account and retrieve quote and additional info for in real-time
#' @export
et_quote_all <- function(){
   keepCols <- c("symbol", "companyName",
                 "lastTrade", "ask", "askSize", "bid", "bidSize",
                 "high", "low", "highAsk", "highBid", "lowAsk", "lowBid",
                 "annualDividend", "dividend", "eps", "estEarnings", "exDivDate",
                 "prevDayVolume", "prevClose",
                 "open", "numTrades", "todayClose", "totalVolume")
   et_quote(lu_all_tickers())[, keepCols, with=FALSE]
}
