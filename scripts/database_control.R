

#### << -- STATIC DATA -- >>     #####
#------------------------------------#

db_schema_list <- sort(
  c("maintain",
    "users",
    "content"
  ))


#### << -- END STATIC DATA -- >> #####
#------------------------------------#

database_controlUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    banner_db_cntrlUI(ns("bnnr_db")),
    fluidRow(h4("Database Control")),
    column(width = 3,
           align = "left",
           uiOutput(ns("outpt_slt_db_cntrl_options"))
           
           ),
    column(width = 9,
           uiOutput(ns("outpt_db_cntrl_main"))
           )
    
  )
}




database_control <- function(input, output, session){
  
  
  #### << -- CALL MODULES  -- >>  

  module_banner <- callModule(
    banner_db_cntrl,
    "bnnr_db"
    )
  
  
  
  #### << -- REACTIVES -- >> ####
  
  
  csv_upload_rctv <- reactive({
    
    req(input$file_upload_csv)
    
    read_csv(input$file_upload_csv$datapath)
    
    
  })
  
  
  #### << -- OBSERVE EVENTS -- >>####
  
  
  view_records <- reactiveValues(table = NULL)
  
  observeEvent(input$btn_view_records_exct,{

    view_records$table <- dbGetQuery(con,
                                paste0(
                                  "SELECT * FROM ",
                                  input$slt_view_records_schema,
                                  ".",
                                  input$slt_view_records_tables,
                                  " limit ",
                                  input$slt_view_records_limit
                                )
    )
    
  })
  
  
  output$outpt_view_records_table <- renderTable({

    if(is.null(view_records$table))
      return(NULL)

    view_records$table

  })
  
  
  
  
  observeEvent(input$btn_drop_table_exct, {
    
    dbGetQuery(con,
               paste0(
                 "DROP TABLE ",
                 input$slt_drop_table_schema,
                 ".",
                 input$slt_drop_table_tables,
                 ";"
               )
    )
    
  })
  
  
  observeEvent(input$btn_upload_csv_to_database,{
    
    req(input$slt_upload_csv_schema)
    req(input$txt_pass_upload_csv_table_name)
    
    appender <- tibble(
      append = ifelse(input$btn_upload_csv_schema_overwrite == "append", T, F),
      overwrite = ifelse(input$btn_upload_csv_schema_overwrite == "overwrite", T, F)
    )
    
    
    dbWriteTable(con,
                 name = c(input$slt_upload_csv_schema, input$txt_pass_upload_csv_table_name),
                 value = csv_upload_rctv(),
                 row.names = F,
                 append = appender$append,
                 overwrite = appender$overwrite
    )
    
  })
  
  
  #### << -- OUTPUTS -->> ####
  
  
  output$outpt_db_cntrl_main <- renderUI({
    
    ns <- session$ns
    
    if(is.null(input$slt_db_cntrl_options))
      return(NULL)
    
    if(input$slt_db_cntrl_options == "upload csv"){
      tagList(
        uiOutput(ns("outpt_db_cntrl_upload_csv"))
              )
    } else  if(input$slt_db_cntrl_options == "drop table"){
      tagList(
        uiOutput(ns("outpt_db_cntrl_drop_table"))
      )
    } else  if(input$slt_db_cntrl_options == "view records"){
      tagList(
        uiOutput(ns("outpt_db_cntrl_view_records"))
      )
    } else if(input$slt_db_cntrl_options == "delete records"){
      tagList(
        uiOutput(ns("outpt_db_cntrl_delete_records"))
      )
    } else if(input$slt_db_cntrl_options == "diarise agenda"){
      tagList(
        uiOutput(ns("outpt_db_cntrl_diarise_agenda"))
      )
    }
    
    
  })
  
  
  agenda_class_days_rctv <- reactive({
    
    dbGetQuery(
      con,
      paste0("select * from maintain.class_presentation_agenda")
    ) %>% 
      select(class) %>% 
      arrange(class) %>% 
      distinct() 
    
    
  })
  
  
  output$outpt_agenda_class_days_tbl <- DT::renderDataTable({
    
    x <- agenda_class_days_rctv()
    
    DT::datatable(
      x, 
      rownames = F,
      selection = "single",
      escape = F,
      options = list(dom = 't',
                     pageLength = 50, 
                     info = FALSE,
                     lengthMenu = list(c(15, -1), c("50", "All")) )
    ) %>% 
      formatStyle(names(x),
                  backgroundColor = "#2B3E4F"
      ) 
    
  })
  
  
  
  output$outpt_db_cntrl_diarise_agenda <- renderUI({
    
    ns <- session$ns
    
    tagList(
      
      column(width = 7,
             DT::dataTableOutput(ns("outpt_agenda_class_days_tbl"))
             )
      
    )
    
    
  })
  
  
  
  
  
  
  
  output$outpt_file_upload_csv <- DT::renderDataTable({
    
    
    
    DT::datatable(
      csv_upload_rctv(),
      rownames = F
    ) %>%
      formatStyle(names(csv_upload_rctv()),
                  backgroundColor = "#2B3E4F"
      )
    
    
  })
  
  output$outpt_slt_view_records_schema <- renderUI({
    
    ns <- session$ns
    
    tagList(
      selectInput(ns("slt_view_records_schema"),
                  "slt_view_records_schema",
                  choices = c(db_schema_list, ""),
                  selected = ""
      ),
      hr(),
      selectInput(ns("slt_view_records_tables"),
                  "slt_view_records_tables",
                  # "slt_drop_table_tables",
                  choices = "",
                  selected = ""
      ),
      hr(),
      numericInput(ns("slt_view_records_limit"),
                   "slt_view_records_limit",
                   min = 10,
                   max = 100,
                   value = 20,
                   step = 10
                   ),
      hr(),
      actionButton(ns("btn_view_records_exct"),
                   "btn_view_records_exct")
      
    )
    
  })
  
  
  output$outpt_slt_delete_records_schema <- renderUI({
    
    
    ns <- session$ns
    
    tagList(
      selectInput(ns("slt_delete_records_schema"),
                  "slt_delete_records_schema",
                  choices = c(db_schema_list, ""),
                  selected = ""
      ),
      hr(),
      selectInput(ns("slt_delete_records_tables"),
                  "slt_delete_records_tables",
                  # "slt_drop_table_tables",
                  choices = "",
                  selected = ""
      ),
      hr(),
      actionButton(ns("btn_delete_records_exct"),
                   "btn_delete_records_exct")
      
    )
    
    
  })
    

  
  observeEvent(input$btn_delete_records_exct,{
    
    dbGetQuery(con,
               paste0("delete from ",
                      input$slt_delete_records_schema,
                      ".",
                      input$slt_delete_records_tables
                      )
               )
    
  })
  
  
  output$outpt_slt_drop_table_schema <- renderUI({
    
    ns <- session$ns
    
    tagList(
      selectInput(ns("slt_drop_table_schema"),
                  "slt_drop_table_schema",
                  choices = c(db_schema_list, ""),
                  selected = ""
      ),
      hr(),
      selectInput(ns("slt_drop_table_tables"),
                  "slt_drop_table_tables",
                  choices = "",
                  selected = ""
      ),
      hr(),
      actionButton(ns("btn_drop_table_exct"),
                   "btn_drop_table_exct")
    )
    
    
  })
  
  
  
  observeEvent(input$slt_view_records_schema,{
    
    schema_tables <- dbGetQuery(con,
                                paste0(
                                  "SELECT table_name FROM information_schema.tables WHERE table_schema = '",
                                  input$slt_view_records_schema,
                                  "'"
                                )
    ) 
    
    # schema_tables <- schema_tables[,1]
    schema_tables <- schema_tables$table_name
    
    # print(str(schema_tables))
    
    updateSelectInput(session,
                      "slt_view_records_tables",
                      "slt_view_records_tables",
                      choices = schema_tables,
                      selected = schema_tables[1]
    )
    
  })
  
  
  observeEvent(input$slt_delete_records_schema,{
    
    schema_tables <- dbGetQuery(con,
                                paste0(
                                  "SELECT table_name FROM information_schema.tables WHERE table_schema = '",
                                  input$slt_delete_records_schema,
                                  "'"
                                )
    ) 
    
    # schema_tables <- schema_tables[,1]
    schema_tables <- schema_tables$table_name
    
    # print(str(schema_tables))
    
    updateSelectInput(session,
                      "slt_delete_records_tables",
                      "slt_delete_records_tables",
                      choices = schema_tables,
                      selected = schema_tables[1]
    )
    
  })
  
  
  
  
  
  
  observeEvent(input$slt_drop_table_schema,{
    
    schema_tables <- dbGetQuery(con,
                                paste0(
                                  "SELECT table_name FROM information_schema.tables WHERE table_schema = '",
                                  input$slt_drop_table_schema,
                                  "'"
                                )
                                ) 
    
    # schema_tables <- schema_tables[,1]
    schema_tables <- schema_tables$table_name
    
    # print(str(schema_tables))
    
    updateSelectInput(session,
                      "slt_drop_table_tables",
                      "slt_drop_table_tables",
                      choices = schema_tables,
                      selected = schema_tables[1]
                      )
    
  })
  

  
  output$outpt_slt_upload_csv_schema <- renderUI({
    
    
    ns <- session$ns
    
    tagList(
    
      fileInput(ns("file_upload_csv"),
                "file_upload_csv"),
      hr(),
    selectInput(ns("slt_upload_csv_schema"),
                "slt_upload_csv_schema",
                choices = db_schema_list,
                selected = db_schema_list[1]
                ),
    hr(),
    textInput(ns("txt_pass_upload_csv_table_name"),
              "txt_pass_upload_csv_table_name",
              value = ""
              ),
    hr(),
    radioButtons(ns("btn_upload_csv_schema_overwrite"),
                 "btn_upload_csv_schema_overwrite",
                 choices = c("append", "overwrite"),
                 selected = "append"
                 ),
    actionButton(ns("btn_upload_csv_to_database"),
                 "btn_upload_csv_to_database")
    
    )
    
  })
  
  
  
  output$outpt_db_cntrl_upload_csv <- renderUI({
    
    ns <- session$ns
    
    tagList(
             column(width = 3,
                    column(width = 12,
                           align = "left",
                           wellPanel(
                             h6("Upload CSV to the DB"),
                             hr(),
                             uiOutput(ns("outpt_slt_upload_csv_schema"))
                           )
                           )
                    ),
             column(width = 9,
                    column(width = 12,
                           align = "center",
                           DT::dataTableOutput(ns("outpt_file_upload_csv"))
                           )
                    )
      
    )
    
  })
  
  
  output$outpt_db_cntrl_drop_table <- renderUI({
    
    ns <- session$ns
    
    tagList(
      column(width = 3,
             column(width = 12,
                    align = "left",
                    wellPanel(
                      h6("Drop Table from DB"),
                      hr(),
                      uiOutput(ns("outpt_slt_drop_table_schema"))
                    )
             )
      ),
      column(width = 9,
             column(width = 12,
                    align = "center"
                    # , DT::dataTableOutput(ns("outpt_file_upload_csv"))
             )
      )
      
    )
    
  })
  
  
  output$outpt_db_cntrl_view_records <- renderUI({
    
    ns <- session$ns
    
    tagList(
      column(width = 3,
             column(width = 12,
                    align = "left",
                    wellPanel(
                      h6("Select Table to view"),
                      hr(),
                      uiOutput(ns("outpt_slt_view_records_schema")) # outpt_slt_drop_table_schema
                    )
             )
      ),
      column(width = 9,
             column(width = 12,
                    align = "center"
                     , tableOutput(ns("outpt_view_records_table"))
             )
      )
      
    )
    
  })
  
  
  
  output$outpt_db_cntrl_delete_records <- renderUI({
    
    ns <- session$ns
    
    tagList(
      column(width = 3,
             column(width = 12,
                    align = "left",
                    wellPanel(
                      h6("Select Table to delete all records from"),
                      hr(),
                      uiOutput(ns("outpt_slt_delete_records_schema")) # outpt_slt_drop_table_schema
                    )
             )
      ),
      column(width = 9,
             column(width = 12,
                    align = "center"
                    , tableOutput(ns("outpt_view_records_table"))
             )
      )
      
    )
    
    
    
    
    
    
  })

  
  output$outpt_slt_db_cntrl_options <- renderUI({
    
    ns <- session$ns
    
    db_cntrl_options <- sort(c(
                          "upload csv"     ,
                          "view records"   ,
                          "delete records" ,
                          "drop table"     ,
                          "diarise agenda"
                          ))
    
    tagList(
      selectInput(ns("slt_db_cntrl_options"),
                  "slt_db_cntrl_options",
                  choices = db_cntrl_options,
                  selected = "view records"
                  )
    )
    
  })
  
  
}

