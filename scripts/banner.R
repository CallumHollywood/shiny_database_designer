



bannerUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(width = 1,
             align = "center",
             style = "padding: 20px 0px 0px 0px",
             a(img(src="logo.png", height="50%", width="50%"), href="", target = "_blank") 
             ),
      column(width = 3,
             align = "left",
             h3("")
             ),
      column(width = 4,
             align = "center",
             uiOutput(ns("outpt_bnnr_titl"))
             ),
     column(
      width = 2,
      align = "right",
      fluidRow(
        tags$a(href="", h3("HOME"))
      )
      
      
       )
    ),
    column(
      width = 1
    ),
    hr()
    
  )
  
}


banner <- function(input, output, session, titl_indx){
  
  

  
  
  bnnr_titl <- reactive({
    
    tibble(
      title = c(
        "ABCXYZ"
        )
    ) %>% 
      slice(titl_indx) %>% 
      pull(title)
    
  })
  
  output$outpt_bnnr_titl <- renderUI({
    
    tagList(
      br(),
      h3(bnnr_titl())
    )
    
  })
  
  
  return(list())
  
}