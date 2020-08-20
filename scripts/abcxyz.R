

abcxyzUI <- function(id){
  
  ns <- NS(id)
  tagList(
    bannerUI(ns("bnnr_abcxyz")),
    uiOutput(ns("land_message"))
  )
}

# abcxyz <- function(id){
#   moduleServer(
#     id,
#     function(input, output, session){
#       # output$land_message <- renderUI(tagList("Ready to rock and roll!"))
#       return(list())
#     }
#   )
# }

abcxyz <- function(input, output, session){

  module_banner <- callModule(banner, "bnnr_abcxyz",
                              titl_indx = 1)
  
  output$land_message <- renderUI({
    
    tagList("Ready to rock and roll!")
  
  })

}