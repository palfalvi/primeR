
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(rhandsontable)
library(tidyverse)

shinyServer(function(input, output, global, session) {
  
  statusCode <- reactiveValues()
  ## Hide submission box~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  shinyjs::hide("box1")
  
  ## ValueBox~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$empty_info <- renderValueBox({
    valueBox(subtitle = paste( "ID:", input$empty_primers),
             value = all_primers[all_primers$`No.` == input$empty_primers,2],
              icon = icon(name = "flask", lib = "font-awesome"), width = 12
              )
  })
    
    
  ## Data table to browse primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$browse_primers <- DT::renderDataTable(datatable(all_primers) %>% 
                                                 formatStyle(columns = 'stock conc.' ,
                                                             target = "row",
                                                             background = styleColorBar(all_primers$`stock conc.`, angle = -90, color = 'lightblue'),
                                                             backgroundSize = '98% 88%',
                                                             backgroundRepeat = 'no-repeat',
                                                             backgroundPosition = 'center'), 
                                               server = TRUE)
  
  
  ##Disable file uploading without proper username and a file uploaded~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disable("file_upload")
  observe({
     if (!is_null(input$file_primer_update) && nchar(input$submit_user) > 3 ) { 
       # Change the following line for more examples
       shinyjs::enable("file_upload")
     } else {
       shinyjs::disable("file_upload")
     }
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  ##Reset file for uploading~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  observeEvent(input$file_reset, {
    shinyjs::reset("file_primer_update")
    shinyjs::hide("box1")
    check_data <- NULL
    file1 <- NULL
    statusCode$code = 0
  })    
 
  ##Reset manual table input~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  observeEvent(input$manual_reset, {
    shinyjs::reset("manual_input")
    output$manual_input <- renderRHandsontable(
      rhandsontable(manualIn, readOnly = FALSE, selectCallback = TRUE, width = '100%', colHeaders = c("Primer Name", "Primer Sequence", "Concentration (μM)", "Comments"))
    )
    check_data <- NULL
    shinyjs::hide("box1")
    statusCode$code = 0
  })   
  
  ##Disable BLAST search without a fasta sequence is provided~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disable("primer_search")
  observe({
    if (nchar(input$blast_query) > 5) { 
      # Change the following line for more examples
      shinyjs::enable("primer_search")
    } else {
      shinyjs::disable("primer_search")
    }
  })
    
  
  ## Manual input with handsontable~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
    manualIn <- tibble(
      name = character(2),
      seq = character(2),
      conc = double(2),
      comm = character(2)
    )

   output$manual_input <- renderRHandsontable(
     rhandsontable(manualIn, readOnly = FALSE, selectCallback = TRUE, width = '100%', colHeaders = c("Primer Name", "Primer Sequence", "Concentration (μM)", "Comments"))
   )
   
   ## Check manual input ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   observeEvent(input$manual_check, {
     check_data <- hot_to_r(input$manual_input) %>% isolate() %>% as_tibble() %>% filter(name != "", seq != "", conc > 0)
     
     if(nrow(check_data) == 0) {
       showModal(modalDialog(title = "No or invalid entries!",
                             "Please make sure you entered at least one valid line with name, sequence and concentration.",
                             easyClose = TRUE,
                             footer = NULL, 
                             size = "l"))
       return()
     } else {}
   output$check_table <- 
     DT::renderDataTable(
     check_data %>% DT::datatable(), server = TRUE)
   shinyjs::show("box1")
   statusCode$code = 1
   }
   )
   
  ## Check file upload~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   observeEvent(input$upload_check, {
     
     if(is.null(input$file_primer_update)) {
       showModal(modalDialog(title = "No file selected!",
                             "Please upload a .csv file with valid format.",
                             easyClose = TRUE,
                             footer = NULL, 
                             size = "l"))
       return()
     } else {
       file1 <- input$file_primer_update
       check_data <- read_csv(file1$datapath, col_names = c("a", "name", "seq", "conc", "comm"), skip = 1) %>% filter(name != "", seq != "", conc > 0, !is.null(conc))
       shinyjs::show("box1")
    }
     
     if(nrow(check_data) == 0) {
       showModal(modalDialog(title = "No or invalid entries!",
                             "Please make sure you entered at least one valid line with name, sequence and concentration.",
                             easyClose = TRUE,
                             footer = NULL, 
                             size = "l"))
       return()
     } else {}
     output$check_table <- 
       DT::renderDataTable(
         check_data,  server = TRUE)
     statusCode$code = 1
   }
   )
   
   
   ##Status update~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   statuscolor <- reactive({
   if (stringr::str_length(input$submit_user) < 3){
     "red"
   } else if (statusCode$code == 1){
     "green"
   } else {
     "aqua"
   }
     })
   
   statusText <- reactive({
     
   })
   
  output$status <- renderUI(
    valueBox("Let's start with your name", icon = icon("hand-o-left"), color = statuscolor(), value = "Start", width = 12)
  )
  
  
  
  ##Report finished primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$finished_primer_btn, {
    showModal(modalDialog(title = paste("Confirm finished primer", all_primers[all_primers$`No.` == input$empty_primers,2], "(id:", input$empty_primers, ")"), 
                          footer = NULL, size = "l", easyClose = FALSE,  {
                            tagList( 
      HTML("WARNING! Your decisions will have consequences!"),
      br(),
      actionButton("action_finished", "Confirm", icon = icon("trash")),
      modalButton("Cancel", icon = icon("times")))
      
    }))
  })
  
  observeEvent(input$action_finished, {
    removeModal()
    showNotification( paste("Finished primer submitted:\n", all_primers[all_primers$`No.` == input$empty_primers,2], "(id:", input$empty_primers, ")"),
                            type = "error", duration = 10)
  })
  
  observeEvent(input$cancel_finished, {
    removeModal()
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

})