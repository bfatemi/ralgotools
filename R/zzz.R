.onLoad <- function(libname, pkgname){
   Sys.setenv("HTTR_PORT" = 7166)
   Sys.setenv("HTTR_SERVER_PORT" = 7166)
   Sys.setenv("RALGO_DIR" = paste0(Sys.getenv("R_LIBS_USER"), "/ralgo_data"))

   if(!dir.exists(Sys.getenv("RALGO_DIR")))
      dir.create(Sys.getenv("RALGO_DIR"))


}

.onAttach <- function(libname, pkgname){
   ralgo_key_path <- paste0(Sys.getenv("RALGO_DIR"), "/ralgo.pk")

   if(!file.exists(ralgo_key_path)){
      packageStartupMessage("\nNo access key found...\n\nPlace given key in directory and reload package: ", Sys.getenv("RALGO_DIR"), "/\n")
      return(invisible(FALSE))
   }

   packageStartupMessage("\nAccess Key Identified\nSetting session environment variables...\n")

   Sys.setenv("RALGO_KEY_PATH" = ralgo_key_path)
   Sys.setenv("ETRADE_PATH" = paste0(Sys.getenv("RALGO_DIR"), "/.etrade"))

   # Check access
   tryCatch({
      et_default_app()
      # secret::get_secret("etrade_app", openssl::read_key(ralgo_key_path))
   }, error = function(c){
      stop("Access may be old. Please request new key.", call. = FALSE)
   })

   return(invisible(TRUE))

}
