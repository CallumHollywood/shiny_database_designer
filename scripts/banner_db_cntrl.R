



banner_db_cntrlUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(width = 1,
             align = "center",
             a(img(src="mds_logo.jpg"), href="https://mds-consulting.co.za/", target = "_blank") 
             ),
      column(width = 3,
             align = "left",
             h3("R Zero to Hero")
             ),
      column(width = 4,
             uiOutput(ns("outpt_db_cntrl"))
             ),
     column(
      width = 2,
      align = "right",
      fluidRow(tags$a(href="", h3("HOME"))
               ),
      fluidRow(
        radioButtons(ns("btn_dev_state"),
                     label = NULL,
                     choices = c("dev",
                                 "prod"),
                     selected = "dev",
                     inline = T
                     )
      )
      
      
       )
    ),
    column(
      width = 1
    ),
    hr()
    
  )
  
}


banner_db_cntrl <- function(input, output, session){
  
  # dev_state <- reactive({})
  

  
  
  output$outpt_db_cntrl <- renderUI({
    
    tagList(
      h3("DB Control Room")
    )
    
  })
  
  
  
  return(list(dev_state = reactive({input$btn_dev_state})))
  
  
  
  
  
}