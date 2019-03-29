#' Retrieve Data Resources from RankFile
#'
#' Functions that return data resources such as all companies on an exchance, all ETFs, SIC codes, etc.
#'
#' @param refresh A boolean. If TRUE (default is FALSE) then data stored locally will be refreshed.
#' @param fname The name of the file to retrieve and parse from rankandfile
#'
#' @section Citation:
#' The data returned is from \url{http://rankandfiled.com/}
#'
#' @import data.table
#' @importFrom httr GET content stop_for_status
#' @importFrom stringr str_split str_split_fixed
#'
#' @return Data.table with data resources
#'
#' @name rank_file_helpers
NULL

rf_files <- c(
   "sic_naics",
   "cik_ticker",
   "cusip_ticker",
   "private_funds",
   "13f_cusips",
   "file_numbers",
   "cik_lei",
   "investment_advisers",
   "edgar_state_country",
   "mmf_cusips",
   "funds"
)


#' @describeIn rank_file_helpers A function that retrieves serialized data from \url{www.rankandfiled.com}
#' @export
getRankfileCSV <- function(fname = NULL){
   if(is.null(fname))
      stop("Provide file name")

   url <- paste0("http://rankandfiled.com/static/export/", fname, ".csv")
   r   <- GET(url)

   if(http_error(r))
      stop("Problem with the filename or URL location: \n  ", url)

   content(r, "text", encoding = "UTF-8")
}



#' @describeIn rank_file_helpers A function to pull SEC listed idenfier information for all companies
#' @export
rf_industry <- function(){
   fread(input = getRankfileCSV("sic_naics"), sep2 = "\\|")
}


#' @describeIn rank_file_helpers A function to pull SEC listed idenfier information for all companies
#' @export
rf_tickers <- function(refresh = FALSE){

   dDir  <- paste0(system.file(package="ralgo"), "/data/")
   fpath <- paste0(dDir, "tickdata")

   if(!dir.exists(dDir))
      dir.create(dDir)


   if(refresh == TRUE | !file.exists(fpath)){

      r <- httr::GET("http://rankandfiled.com/static/export/cik_ticker.csv")
      httr::stop_for_status(r)
      rdat <- httr::content(r, "text", encoding = "UTF-8")
      DT <- as.data.table(stringr::str_split_fixed(stringr::str_split(rdat, "\\n")[[1]], "\\|", 8))


      for(k in 1:ncol(DT))
         set(DT, i = which(DT[, k, with=FALSE] == ""), j = k, value = NA_character_)


      ## grab new column names and compare against old just in case
      cnames <- as.character(DT[1])
      setnames(DT, cnames) # 1st row are col names
      tickdata <- DT[!is.na(get("Ticker")) & !is.na(get("Name"))][-1]

      stopifnot(file.create(fpath))
      save(tickdata, file = fpath, compress = "bzip2")
      print("Data Refreshed")
   }

   if(!exists("tickdata"))
      load(fpath)
   return(tickdata)
}
