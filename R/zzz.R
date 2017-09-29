# .onLoad <- function(libname, pkgname){
#    Sys.setenv("HTTR_SERVER" = "localhost")
#    Sys.setenv("HTTR_SERVER_PORT" = 7166)
#
#    # set ralgo directory path
#    basedir <- Sys.getenv("R_USER", Sys.getenv("R_HOME"))
#    rDir <- normalizePath(paste0(basedir, "/ralgo_data"), mustWork = FALSE)
#    Sys.setenv("RALGO_DIR" = rDir)
#
#    # create dir if does not exist
#    if(!dir.exists(rDir)) dir.create(rDir)
#
#    # set path for private key used for ralgo
#    key_path <- normalizePath(paste0(rDir, "/ralgo.pk"), mustWork = FALSE)
#    Sys.setenv("RALGO_KEY_PATH" = key_path)
#
#    # set path for session object
#    session_path <- normalizePath(paste0(rDir, "/.etrade"), mustWork = FALSE)
#    Sys.setenv("ETRADE_PATH" = session_path)
#
#    # check if access exists and error if does not
#    if( !file.exists( key_path ) ){
#       packageStartupMessage("\nNo access key found...\n\nPlace given key in directory and reload package: ", rDir, "/\n")
#       return(invisible(FALSE))
#    }
#
#    invisible(TRUE)
# }
#
# .onAttach <- function(libname, pkgname){
#    packageStartupMessage("\nAccess Key Identified\nSetting session environment variables...\n")
#    return(invisible(TRUE))
# }
