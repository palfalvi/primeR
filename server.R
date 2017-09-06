
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
  statusCode$code <- 0
  ## Hide submission box~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  shinyjs::hide("box1")
  
  ## ValueBox~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$empty_info <- renderValueBox({
    valueBox(subtitle = paste( "ID:", input$empty_primers),
             value = all_primers() %>% filter(ID == input$empty_primers) %>% .[,2],
              icon = icon(name = "flask", lib = "font-awesome"), width = 12
              )
  })
    
    
  ## Data table to browse primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_primers <- shiny::reactiveFileReader(1000,
                            session,
                            filePath = "primers.sqlite",
                            readFunc = function(x) {
                              con <- dbConnect(SQLite(), x)
                              con %>% tbl("primers") %>% as_data_frame() -> table_out
                              dbDisconnect(con)
                              return(table_out)
                            })
  
  output$browse_primers <- DT::renderDataTable(datatable(all_primers()), 
                                               server = TRUE)
  
  
  ##Disable file uploading without proper username and a file uploaded~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disable("file_upload")
  observe({
     if (!is.null(input$file_primer_update) && nchar(input$submit_user) > 3 ) { 
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
    data$check <- NULL
    file1 <- NULL
    statusCode$code <- 0
  })    
 
  ##Reset manual table input~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  observeEvent(input$manual_reset, {
    shinyjs::reset("manual_input")
    output$manual_input <- renderRHandsontable(
      rhandsontable(manualIn, readOnly = FALSE, selectCallback = TRUE, width = '100%', colHeaders = c("Primer Name", "Primer Sequence", "Concentration (μM)", "Comments"))
    )
    data$checm <- NULL
    shinyjs::hide("box1")
    statusCode$code <- 0
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
   
   data <- reactiveValues()
   
   ## Check manual input ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   observeEvent(input$manual_check, {
     data$check <- hot_to_r(input$manual_input) %>% isolate() %>% as_tibble() %>% filter(name != "", seq != "", conc > 0)
     
     
     if(nrow(data$check) == 0) {
       showModal(modalDialog(title = "No or invalid entries!",
                             "Please make sure you entered at least one valid line with name, sequence and concentration.",
                             easyClose = TRUE,
                             footer = NULL, 
                             size = "l"))
       return()
     } else {}
   output$check_table <- 
     DT::renderDataTable(
     data$check %>% DT::datatable(), server = TRUE)
   shinyjs::show("box1")
   statusCode$code <- 1
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
       data$check <- read_csv(file1$datapath, col_names = c("a", "name", "seq", "conc", "comm"), skip = 1) %>% filter(name != "", seq != "", conc > 0, !is.null(conc))
       update_table <- reactive({data$check})
       shinyjs::show("box1")
    }
     
     if(nrow(data$check) == 0) {
       showModal(modalDialog(title = "No or invalid entries!",
                             "Please make sure you entered at least one valid line with name, sequence and concentration.",
                             easyClose = TRUE,
                             footer = NULL, 
                             size = "l"))
       return()
     } else {
       statusCode$code <- 1
       output$check_table <- 
          DT::renderDataTable(
            data$check,  server = TRUE)
     
     }
   }
   )
   
   
   ##Status update~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   statuscolor <- reactive({
   if (stringr::str_length(input$submit_user) < 3){
     "red"
   } else if (statusCode$code == 1){
     "green"
   } else {
     "yellow"
   }
     })
   
   statusText <- reactive({
     if (stringr::str_length(input$submit_user) < 3){
       "Please enter a valid name"
     } else if (statusCode$code == 1){
       "Proceed to upload"
     } else {
       "Please upload a file or enter primers manually"
     }
   })
   
   statusTitle <- reactive({
     if (stringr::str_length(input$submit_user) < 3){
       "Enter name"
     } else if (statusCode$code == 1){
       "Primers validated"
     } else {
       "Enter primer info"
     }
   })
   
  output$status <- renderUI(
    valueBox(subtitle = statusText(), icon = icon("hand-o-left"), color = statuscolor(), value = statusTitle(), width = 12)
  )
  
  
  ##Update database with submitted primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$submit_new, {
    
    showModal(modalDialog(title = paste("Confirm primer uploading", " ID later ", "(id:", input$empty_primers, ")"), 
                          footer = NULL, size = "l", easyClose = FALSE,  {
                            tagList( 
                              HTML("WARNING! Your decisions will have consequences!"),
                              br(),
                              actionButton("upload_new", "Confirm", icon = icon("upload")),
                              modalButton("Cancel", icon = icon("times")))
                            
                          }))
  })
  
  observeEvent(input$upload_new, {
    
    removeModal()
    showNotification( paste("Primer submitted:\n", "(id:", ")"),
                      type = "message", duration = 10)
    
    con <- dbConnect(SQLite(), "primers.sqlite")
    dbWriteTable(con, "primers", data$check %>% select(-comm) %>% mutate(ID = 0, 
                                                                         primer_name = name,
                                                                         sequence = seq,
                                                                         concentration = conc) %>%
                   select(ID, primer_name, sequence, concentration), append = TRUE, overwrite = FALSE)  ## check_data and primer database not in same format!!
    con %>% dbDisconnect()
    
    write_file(x = paste(">", data$check$name, "\n",
                         data$check$seq, sep = ""), 
               path = "primers.fasta", 
               append = TRUE)
    
    shinyjs::reset("file_primer_update")
    shinyjs::hide("box1")
    data$check <- NULL
    file1 <- NULL
    statusCode$code <- 0
    shinyjs::reset("manual_input")
    output$manual_input <- renderRHandsontable(
      rhandsontable(manualIn, readOnly = FALSE, selectCallback = TRUE, width = '100%', colHeaders = c("Primer Name", "Primer Sequence", "Concentration (μM)", "Comments"))
    )
    
    ## Append to fasta file
    
    ## Needs proper table format
    ## Needs output for new ids and primers 
  })
    
    

  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##Report finished primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$finished_primer_btn, {
    showModal(modalDialog(title = paste("Confirm finished primer", all_primers() %>% filter(ID == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"), 
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
    showNotification( paste("Finished primer submitted:\n", all_primers() %>% filter(ID == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"),
                            type = "error", duration = 10)
    #Add finished comment to primer
  })
  
  observeEvent(input$cancel_finished, {
    removeModal()
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  ##Report reordered primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$report_reordered, {
    showModal(modalDialog(title = paste("Confirm reordering informations", all_primers() %>% filter(ID == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"), 
                          footer = NULL, size = "l", easyClose = FALSE,  {
                            tagList( 
                              HTML("WARNING! Your decisions will have consequences!"),
                              br(),
                              actionButton("action_finished2", "Confirm", icon = icon("refresh")),
                              modalButton("Cancel", icon = icon("times")))
                            
                          }))
  })
  
  observeEvent(input$action_finished2, {
    removeModal()
    showNotification( paste("Reorder primer submitted:\n", all_primers() %>% filter(ID == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"),
                      type = "warning", duration = 10)
    #Create new entry for reordered primer
  })
  
  observeEvent(input$cancel_finished, {
    removeModal()
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  ##Report reordered primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$modify_primer_comment, {
    showModal(modalDialog(title = paste("Confirm comment modification for ", all_primers() %>% filter(ID == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"), 
                          footer = NULL, size = "l", easyClose = FALSE,  {
                            tagList( 
                              HTML("New comment: "),
                              input$new_comment,
                              br(),
                              HTML("WARNING! Your decisions will have consequences!"),
                              br(),
                              actionButton("action_finished3", "Confirm", icon = icon("refresh")),
                              modalButton("Cancel", icon = icon("times")))
                            
                          }))
  })
  
  observeEvent(input$action_finished3, {
    removeModal()
    showNotification( paste("Updated comment:\n", all_primers() %>% filter(ID == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"),
                      type = "warning", duration = 10)
    #Create new entry for reordered primer
  })
  
  observeEvent(input$cancel_finished, {
    removeModal()
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

  
  ##Update blast db based on fasta timestamp~~~~~~~~~~~~~~~~~~~~~
  
  pollUpdate <- reactivePoll(intervalMillis = 1000,
               session = session,
               checkFunc = function() {
                 if (file.exists("primers.fasta")) {
                   as.numeric(file.info("primers.fasta")$mtime[1])
                 } else {
                   ""}
               },
               valueFunc = function() {
                 file.info("primers.fasta")$mtime[1]
               })

  observeEvent(pollUpdate(), {
    create_blast_db("primers.fasta", "nucl")
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  ##Plan~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # check_data contains new primers
  # when submit button (input$submit_new) was triggered :
  # Assign new IDs for primers
  # Append data to fasta file
  # Append data to sqlite
  # Give feedback table/text on new IDs and success of submission
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
})
