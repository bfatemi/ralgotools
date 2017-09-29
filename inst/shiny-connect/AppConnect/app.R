
ui <- fluidPage(
   theme = shinytheme("superhero"),
   tags$head(
      tags$style(HTML("p {
                      display: block;
                      margin-top: 0;
                      margin-bottom: 0;
                      margin-left: 1em;
                      margin-right: 1em;}"))
      ),

   titlePanel("Connect to Etrade"),

   HTML("<div style = 'background-color: #29343f; border-radius: 10px;'>
        <br/><p>You will need to request the following information from Etrade.
        If you need additional information, hit the help button below.
        Once you have recieved this from Etrade, save it here and hit submit.
        You typically will only have to save this information once, but
        keep it stored in case you need to input it here again.</p><br/></div>"),

   htmlOutput("curName"),
   br(),
   actionButton("submitrequest", " Submit",       icon = icon("launch", class = NULL, lib = "font-awesome")),
   actionButton("close",         " Close Window", icon = icon("times", class = NULL, lib = "font-awesome"))
   )




server <- function(input, output, session) {
   ## Helper functions to walk user through getting code
   ##
   confirm_modal <- function(title){
      showModal(modalDialog(title = title,
                            "Click Close to return to R, or click outside this dialogue box to go back to app",
                            easyClose = TRUE,
                            footer = tagList(modalButton("Back"), actionButton("exit", "Close"))
      ))
   }



   app <- et_app()

   show_app_info <- function(){
      tmp.html <- "<br/><div style = 'background-color: #29343f; border-radius: 10px; width: 70%'>"
      cid     <- paste0(tmp.html, app$key, "</div>")
      csecret <- paste0(tmp.html, app$secret, "</div>")
      name    <- paste0(tmp.html, app$appname, "</div>")
      HTML(paste(paste("<br/>Current App Name :", name),
                 paste("Current Client ID  :", cid),
                 paste("Current Secret Key :", csecret),
                 sep = '<br/>'))
   }
   output$curName <- renderUI(show_app_info())

   observeEvent(input$close, {
      stopApp()
   })
   observeEvent(input$erase, {
      end_session()
      confirm_modal("Session Ended")
   })


   # Return the UI for a modal dialog with data selection input. If 'failed' is
   # TRUE, then display a message that the previous value was invalid.
   CodeModal <- function(blankcode = FALSE, failed = FALSE) {
      modalDialog(
         textInput("code", "Enter Etrade Code", placeholder = 'Code Sent via SMS'),
         span('Authenticate Through Etrade Login in Popout Window'),

         if(blankcode)
            div(tags$b("No code detected", style = "color: red;")),

         if(failed)
            div(tags$b("Could not authenticate with provided code", style = "color: red;")),

         footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
         )
      )
   }




   app     <- et_app()
   rtoken  <- et_request_token(app)
   url_acc <- "https://etws.etrade.com/oauth/access_token"
   authUrl <- et_auth_url(app, rtoken)

   observeEvent(input$submitrequest, {
      BROWSE(authUrl)
      showModal(CodeModal())
   })



   # When OK button is pressed, attempt to load the code to retrieve final access
   observeEvent(input$ok, {
      verifier <- input$code

      if(verifier != ""){

         sign <-httr::oauth_signature(
            url          = url_acc,
            app          = app,
            token        = rtoken$oauth_token,
            token_secret = rtoken$oauth_token_secret,
            other_params = c(oauth_verifier = verifier)
         )
         resp <- httr::GET(url_acc, httr::oauth_header(sign))
         # print(content(resp))

         confirm_modal("Access Granted")

         stopApp(content(resp))

      }else{
         showModal(CodeModal(blankcode = TRUE))
      }
   })

}
shinyApp(ui = ui, server = server)


