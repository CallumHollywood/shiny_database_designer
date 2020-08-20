


landpageUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    banner_landUI("bnnr_lnd"),
    fluidRow(
      column(width = 12,
             align = "center",
             h2("Welcome"),
             br()
      )
    ),
    fluidRow(
      column(width = 5),
      column(width = 2,
             align = "center",
             fluidRow(h6("")),
             fluidRow(
               column(width = 12,
                      align = "center",
                      actionButton(ns("btn_goto_module"),
                                   "Go To a Module",
                                   width = "100%")
               )
             ),
             br(),
             
             column(width = 12,
                    align = "center",
                    actionButton(ns("btn_goto_db_control"),
                                 "Go to DB Control",
                                 width = "100%")
             )
             
             )
      
    ),
    column(width = 5)
  )
}




landpage <- function(input, output, session){
  
  
  
  
  observeEvent(input$btn_goto_module, {
    
    updateQueryString(
      paste0(getQueryString(), "#abcxyz"),
      "push"
    )
    
  })
  
  observeEvent(input$btn_goto_db_control, {
    
    updateQueryString(
      paste0(getQueryString(), "#dbcntrl"),
      "push"
    )
    
  })
  
  
}

