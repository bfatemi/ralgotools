#' Functions for the Accounts API
#'
#' These functions wrap calls to the Accounts API. See details for additional information.
#'
#' @param aid An accountId required to look up information specific to one or more accounts.
#' @param postype The underlying asset type to retreive
#' @param asDT TBD
#'
#' @import data.table
#'
#' @name AccountAPI
NULL


#' @describeIn AccountAPI list all accounts for the authenticated user
#' @export
list_accounts <- function(){
   session <- et_connect()
   rawData <- etrade_api(module = "accounts", api = "accountlist", session = session)
   resll   <- lapply(rawData$json.accountListResponse$response, setDT)
   return(rbindlist(resll))
}

#' @describeIn AccountAPI Retrieve balance information specific to one or more accounts for the authenticated user
#' @export
list_balances <- function(aid=NULL, asDT = FALSE){

   if(is.null(aid))
      stop("Please provide accountID. Use list_accounts() to retrieve.", call. = FALSE)

   session <- et_connect()
   rawacct <- lapply(aid, function(id) etrade_api("accounts", paste0("accountbalance/", id), session))

   resultList <- lapply(rawacct, function(i){
      tryCatch({
         ll     <- i$json.accountBalanceResponse
         id_pos <- sapply(ll, length) == 1
         id_row <- sapply(ll, function(i) is.data.table(i) | is.list(i))
         tmpll  <- c(list(accountInfo = as.data.table(ll[id_pos])), ll[id_row])

         res <- lapply(tmpll, function(k){
            vals <- unlist(k)
            data.table(lineitem = names(vals), value = vals)
         })
         return(res)

      }, error = function(c){
         warning("parsing went wrong for aid: ", i$json.accountBalanceResponse$accountId)
         return(NULL)
      })
   })
   names(resultList) <- aid

   if(asDT){
      aids    <- names(resultList)
      tmpList <- lapply(aids, function(id){
         rll <- resultList[[id]]
         rdt <- rbindlist(
            lapply(names(rll), function(r) cbind(lineitem_type = r, rll[[r]])),
            use.names = TRUE,
            fill = TRUE
         )
         return(cbind(accountId = id, rdt))
      })
      resultDT <- rbindlist(tmpList, use.names = TRUE, fill = TRUE)
      return(resultDT)
   }
   return(resultList)
}

#' @describeIn AccountAPI List account positions
#' @export
list_positions <- function(aid = NULL,
                           postype = c("EQ", "ALL", "OPTN", "INDX", "MF", "FI"),
                           asDT = FALSE){

   if(is.null(aid))
      stop("Please provide accountID. Use list_accounts() to retrieve.", call. = FALSE)


   session <- et_connect()

   ## 25 is max. Need to check if there are more than 25 positions in account
   urls        <- paste0("accountpositions/", aid, "?count=25&marker=")
   names(urls) <- aid
   module      <- "accounts"

   resll <- sapply(urls, function(api){
      count <- 0
      marker    <- "%20"

      output <- list()
      while(marker != ""){
         rawData <- etrade_api(module, paste0(api, marker), session)
         rpos    <- rawData$json.accountPositionsResponse
         marker  <- gsub(" ", "%20", rpos$marker)
         count   <- count + rpos$count
         output  <- c(output, rpos$response)
      }
      return(output)

   }, simplify = FALSE, USE.NAMES = TRUE)

   if(asDT){
      get_prow <- function(pos){
         pid <- pos$productId
         pDT <- data.table(symbol = pid$symbol, pos_type = pid$typeCode)
         DT  <- as.data.table(pos[names(pos) != "productId"])
         return(cbind(pDT, DT))
      }

      tmp   <- lapply(resll, function(r) rbindlist(lapply(r, get_prow), use.names = TRUE, fill = TRUE))
      tmp   <- lapply(names(tmp), function(nam) cbind(accountId = nam, tmp[[nam]]))
      resDT <- rbindlist(tmp, use.names = TRUE, fill = TRUE)

      resDT[, "costBasis"    := as.numeric(get("costBasis"))]
      resDT[, "qty"          := as.numeric(get("qty"))]
      resDT[, "currentPrice" := as.numeric(get("currentPrice"))]
      resDT[, "marketValue"  := as.numeric(get("marketValue"))]
      setkeyv(resDT, "symbol")
      return(resDT[])

   }else{
      resultList <- lapply(resll, function(i){
         names(i) <- sapply(i, function(j) j$productId$symbol)
         return(i)
      })
      return(resultList)
   }
}







