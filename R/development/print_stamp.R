#' Utility to Print on Console
#'
#' @param msg Message to print
#' @param sym Symbol to use for readability
#'
#' @return NULL
print_stamp <- function (msg, sym = "#"){
   if (stringr::str_length(msg)%%2 == 1)
      msg <- stringr::str_c(msg, " ")

   msg       <- stringr::str_c(" ", msg, " ")
   scount    <- stringr::str_length(msg)
   cushion   <- ceiling(scount * 1.3) - scount
   cushion   <- cushion + cushion%%2
   topcount  <- scount + cushion - 1
   sidecount <- 3
   hdft      <- stringr::str_c(rep(sym, topcount), collapse = "")
   spaces    <- stringr::str_c(rep(" ", topcount - 1), collapse = "")
   sides     <- rep(sym, sidecount)
   grid_col  <- topcount + 1
   grid_row  <- sidecount + 2

   tmp <- stringr::str_c(c(hdft, stringr::str_c(sides, spaces), hdft), sym, collapse = "\n")
   txt <- stringr::str_split(stringr::str_split(tmp, "\n")[[1]],
                             "")
   pad.l <- c(sym, rep(" ", cushion/2 - 1))
   pad.r <- c(rep(" ", cushion/2 - 1), sym)
   txt[[3]] <- c(pad.l, stringr::str_split(msg, "")[[1]], pad.r)
   cat("\n\n")
   cat(paste0(sapply(txt, function(itxt) paste0(c(itxt, "\n"), collapse = "")), collapse = ""))
   cat("\n")
}
