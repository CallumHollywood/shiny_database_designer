
server <- function(input, output, session){
  
  ## live_hash        = a constant reading of the URL address bar
  ## output$main_ui   = the server side ui sent to the front end 'ui.R'
  ## How it works?....
  ## Events (like a click on an actionbutton) are set up throughout the project which update the url # address.
  ## 'live_hash' reacts to update to the new url address and if the condtions are met then output$main_ui will
  ## update to call the appropriate module - which it sends to ui.R
  
  live_hash <- reactive({getUrlHash()})
  
  output$main_ui <- renderUI({
    
    if(live_hash() == ""){
      landpageUI("lndpge")
    } else if(live_hash() == "#abcxyz"){
      abcxyzUI("abcxyz")
    } else if(live_hash() == "#db_control"){
      database_controlUI("dbcntrl")
    } 
    
  })
  
  
  ## CallModules            ####
  #----------------------------#
  
  module_lndpge             <-  callModule(landpage,         "lndpge")
  module_abcxyz             <-  callModule(abcxyz,           "abcxyz")
  module_dbcntrl            <-  callModule(database_control, "dbcntrl")
  
}