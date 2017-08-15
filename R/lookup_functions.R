#' Analysis Helpers
#'
#' Functions that assist with interactive exploration and analysis. Includes quick lookup
#' wrappers around main functions.
#'
#' Includes account specific functions as well as general information lookup helpers.
#'
#' @param asDT Boolean. Default FALSE will return a character vector. TRUE will return
#' a one-column data.table
#'
#' @import data.table
#' @importFrom httr GET
#' @importFrom XML readHTMLTable
#' @importFrom stringr str_replace_all str_detect str_to_lower str_extract
#'
#' @name lookup_functions
NULL

#' @describeIn lookup_functions get tickers as vector or column
#' @export
lu_all_tickers <- function(asDT = FALSE){
   stop_for_access()
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
   stop_for_access()
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
   tks <- lu_all_tickers()
   pDT <- lu_portfolio_stat()
   pct <- pDT[, round(sum(get("wt") * get("mu")), 4)]
   lab <- c("Gain: +", "Loss: -")[c(pct > 0, pct < 0)]

   msg <- paste0(lab, pct*100, "%")
   print_stamp(msg, "#")
   invisible(msg)
}



#' @describeIn lookup_functions List all SIC codes
#' @export
lu_all_SIC <- function(){
   url_sic  <- "https://www.sec.gov/info/edgar/siccodes.htm"
   html_txt <- rawToChar(httr::GET(url_sic)$content)

   ll      <- XML::readHTMLTable(html_txt)
   indexDT <- sapply(ll, function(i) "SICCode" %in% as.character(i[1:10, 1]))

   dtList <- lapply(ll[indexDT], function(i) {
      setDT(i) # set as data.table (important for row indexing below)

      # which rows contain the real column names? clean and set names
      ind  <- which(as.character(i[1:30, get("V1")]) == "SICCode")
      nam  <- as.character(unlist(i[ind]))
      cnam <- stringr::str_replace_all(nam, "[^A-Za-z]", "_")
      setnames(i, cnam)

      # remove row of names since those were just set
      DT <- i[-ind]

      # remove any columns that have no character/numeric entries
      rmcol_ind <-
         unlist(DT[, sapply(.SD,
                            function(col) {
                               vec <- col[col != "" & !is.na(col)]
                               sum(stringr::str_detect(vec, "^[^A-Za-z0-9]*$")) == length(vec)
                            })])

      res <- DT[, -which(rmcol_ind), with = FALSE]
      res[, c("SICCode") := suppressWarnings(as.numeric(as.character(get("SICCode"))))]
      res[!is.na(get("SICCode"))]
   })

   sicDT <- rbindlist(dtList, use.names = TRUE, fill = FALSE)
   setnames(sicDT, c("sic_code", "ad_office", "industry"))
   setkeyv(sicDT, "sic_code")

   # for some reason, the url sends back 2 of every row
   SIC <- sicDT[, .N, c(colnames(sicDT))][, !"N"]
   return(SIC)
}



#' @describeIn lookup_functions List all ETFs
#' @export
lu_etfs <- function(){
   DT <- fread("http://www.nasdaq.com/investing/etfs/etf-finder-results.aspx?download=Yes")
   return(DT)
}



#' @describeIn lookup_functions List companies listed on NSDQ, NYSE, and AMEX exchanges.
#' @export
lu_all_pubco <- function(){
   llurl <- list(
      nsdq = "http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download",
      nyse = "http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download",
      amex = "http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=AMEX&render=download"
   )

   rbindlist(lapply(llurl, function(url){

      # read url and drop a column of links and an unknown extra col with all NAs
      DT <- fread(rawToChar(httr::GET(url)$content), drop = c("V10", "Summary Quote"))

      # set friendly names, add new column identifying the exchange
      setnames(DT, stringr::str_to_lower(stringr::str_replace_all(colnames(DT), " ", "_")))
      exch_nam <- stringr::str_extract(url, "(?<=exchange\\=).+(?=\\&render\\=download)")
      resDT <- cbind(exchange = exch_nam, DT)

      numcols <- c("lastsale", "marketcap", "adr_tso", "ipoyear")
      resDT[, (numcols):=lapply(numcols, function(i) suppressWarnings(as.numeric(get(i))))]
      resDT
   }), use.names = TRUE, fill = TRUE)
}


