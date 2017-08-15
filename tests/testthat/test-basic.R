context("test decryption")

test_that("Get etrade app", {
   skip_on_travis()
   skip_on_cran()

   # Set path to package vault and secret key
   Sys.setenv("RALGO_KEY_PATH" = "C:\\Users\\Bobbyf\\Documents\\R\\win-library\\3.3\\ralgo_data\\ralgo.pk")
   options(secret.vault = normalizePath(paste0(devtools::inst("ralgotools"), "/vault")))

   expect_equal(class(et_default_app()), "list")
})
