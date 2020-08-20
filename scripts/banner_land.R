

# control for the banner on the site land page #
#----------------------------------------------#


banner_landUI <- function(id){
  
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
             br()
             ),
      column(
        width = 2,
        align = "right")
    ,
    column(
      width = 1
    )
    ),
    hr()
  )
  
}


banner_land <- function(input, output, session){
  
  
}