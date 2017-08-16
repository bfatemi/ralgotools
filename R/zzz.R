.onLoad <- function(libname, pkgname){
   Sys.setenv("HTTR_PORT" = "localhost")
   Sys.setenv("HTTR_SERVER_PORT" = 1410)
   invisible(TRUE)
}

.onAttach <- function(libname, pkgname){
   Sys.setenv("HTTR_PORT" = "localhost")
   Sys.setenv("HTTR_SERVER_PORT" = 1410)

   path <- normalizePath(paste0(.libPaths()[1], "/ralgo_data"), mustWork = FALSE)
   Sys.setenv("RALGO_DIR" = path)

   if(!dir.exists(Sys.getenv("RALGO_DIR")))
      dir.create(Sys.getenv("RALGO_DIR"))

   ralgo_key_path <- normalizePath(paste0(Sys.getenv("RALGO_DIR"), "/ralgo.pk"), mustWork = FALSE)

   if(!file.exists(ralgo_key_path)){
      packageStartupMessage("\nNo access key found...\n\nPlace given key in directory and reload package: ", Sys.getenv("RALGO_DIR"), "/\n")
      return(invisible(FALSE))
   }

   packageStartupMessage("\nAccess Key Identified\nSetting session environment variables...\n")

   Sys.setenv("RALGO_KEY_PATH" = ralgo_key_path)
   Sys.setenv("ETRADE_PATH" = normalizePath(paste0(Sys.getenv("RALGO_DIR"), "/.etrade"), mustWork = FALSE))

   # Check access
   tryCatch({
      et_default_app()
   }, error = function(c){
      stop("Access may be old. Please request new key.", call. = FALSE)
   })

   return(invisible(TRUE))

}
