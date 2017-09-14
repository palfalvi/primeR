
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
library(DBI)
library(RSQLite)
library(dbplyr)

shinyServer(function(input, output, global, session) {
  
  statusCode <- reactiveValues()
  statusCode$code <- 0
  ## Hide submission box~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  shinyjs::hide("box1")
  
  ## ValueBox~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$empty_info <- renderValueBox({
    valueBox(subtitle = paste( "id:", input$empty_primers),
             value = all_primers() %>% filter(id == input$empty_primers) %>% select(primer_name),
              icon = icon(name = "flask", lib = "font-awesome"), width = 12
              )
  })
    
    
  ## Data table to browse primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_primers <- shiny::reactiveFileReader(1000,
                            session,
                            filePath = "primers.sqlite",
                            readFunc = function(x) {
                              con <- dbConnect(SQLite(), x)
                              con %>% tbl("primers") %>% as_data_frame() %>% mutate(date = lubridate::as_date(date %>% as.numeric()),
                                                                                    empty = empty %>% as.logical()) -> table_out
                              
                              return(table_out)
                            })
  
  output$browse_primers <- DT::renderDataTable(all_primers() %>% datatable(class = 'cell-border stripe',
                                                                         rownames = FALSE),
                                                               server = TRUE)
  
  
  ## Selectize input for empty primers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$empty_primer_selection <- renderUI({
    selectizeInput("empty_primers", label = "Primer id", choices = all_primers() %>% select(id))
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  ##Disable file uploading without proper username and a file uploaded~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disable("upload_check")
  observe({
     if (!is.null(input$file_primer_update) && nchar(input$submit_user) > 3 ) { 
       # Change the following line for more examples
       shinyjs::enable("upload_check")
     } else {
       shinyjs::disable("upload_check")
     }
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##Disable manual input validation without proper username~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disable("manual_check")
  observe({
    if (nchar(input$submit_user) > 3 ) { 
      # Change the following line for more examples
      shinyjs::enable("manual_check")
    } else {
      shinyjs::disable("manual_check")
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
    data$check <- NULL
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
     data$check %>% DT::datatable(options = list(pageLength = -1,
                                                 lengthMenu = FALSE),
                                  class = 'cell-border stripe',
                                  rownames = FALSE), server = TRUE)
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
       data$check <- read_fasmac(file1$datapath)
       update_table <- reactive({data$check})
       shinyjs::show("box1")
    }
     
     if(nrow(data$check) == 0) {
       showModal(modalDialog(title = "No or invalid entries!",
                             "Please make sure you entered a proper file with at least one valid line with name, sequence and concentration.",
                             easyClose = TRUE,
                             footer = NULL, 
                             size = "l"))
       return()
     } else {
       output$check_table <- 
          DT::renderDataTable(
            data$check %>% DT::datatable(options = list(pageLength = -1,
                                                        lengthMenu = FALSE),
                                         class = 'cell-border stripe',
                                         rownames = FALSE),  server = TRUE)
       shinyjs::show("box1")
       statusCode$code <- 1
     
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
    
    showModal(modalDialog(title = paste("Confirm primer uploading. Your primers' new ID's will be visible after submission."), 
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
    dbWriteTable(con, "primers", data$check %>% as_data_frame() %>% 
                   mutate(primer_name = name,
                          sequence = seq,
                          concentration = conc,
                          comments = comm,
                          date = lubridate::today(),
                          name = isolate(input$submit_user),
                          empty = 0) %>%
                   select(primer_name, sequence, concentration, comments, date, name, empty), 
                 append = TRUE, overwrite = FALSE)  ## check_data and primer database not in same format!!
    
    data$new_primers <- con %>% 
      tbl("primers") %>% 
      as_data_frame() %>%
      semi_join(data$check %>% 
                  as_data_frame(), by = c("primer_name" = "name",
                                          "sequence" = "seq",
                                          "concentration" = "conc"))
    
    
    
    for (line in 1:nrow(data$check)) {
      write_file(x = paste("\n>", data$new_primers$id[line], "\n",
                           data$new_primers$sequence[line], sep = ""),
                 path = "primers.fasta",
                 append = TRUE)
    }
    
    output$new_primers <- renderDataTable(data$new_primers %>% mutate(date = lubridate::as_date(date %>% 
                                                                                                  as.numeric()),
                                                                      empty = empty %>% 
                                                                        as.logical()) %>% 
                                            DT::datatable(options = list(pageLength = -1,
                                                                         lengthMenu = FALSE),
                                                          class = 'cell-border stripe',
                                                          rownames = FALSE))
    
    # data$check %>% 
    #   as_data_frame() %>%
    #   glue::glue_data('\n>{name}\n{seq}') %>%
    #   rlang::as_character() %>%
    #   write_file(path = "primers.fasta",
    #              append = TRUE)
   
    shinyjs::reset("file_primer_update")
    shinyjs::hide("box1")
    data$check <- NULL
    file1 <- NULL
    statusCode$code <- 0
    shinyjs::reset("manual_input")
    output$manual_input <- renderRHandsontable(
      rhandsontable(manualIn, readOnly = FALSE, selectCallback = TRUE, width = '100%', colHeaders = c("Primer Name", "Primer Sequence", "Concentration (μM)", "Comments"))
    )
    
    ## Append to fasta file with glue?
    
    ## Needs proper table format
    ## Needs output for new ids and primers 
  })
    
    

  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##Report finished primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$finished_primer_btn, {
    showModal(modalDialog(title = paste("Confirm finished primer", all_primers() %>% filter(id == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"), 
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
    showNotification( paste("Finished primer submitted:\n", all_primers() %>% filter(id == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"),
                            type = "error", duration = 10)
    #Add finished comment to primer
  })
  
  observeEvent(input$cancel_finished, {
    removeModal()
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  ##Report reordered primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$report_reordered, {
    showModal(modalDialog(title = paste("Confirm reordering informations", all_primers() %>% filter(id == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"), 
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
    showNotification( paste("Reorder primer submitted:\n", all_primers() %>% filter(id == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"),
                      type = "warning", duration = 10)
    #Create new entry for reordered primer
  })
  
  observeEvent(input$cancel_finished, {
    removeModal()
  })
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  ##Report reordered primers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$modify_primer_comment, {
    showModal(modalDialog(title = paste("Confirm comment modification for ", all_primers() %>% filter(id == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"), 
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
  }, ignoreInit = TRUE)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  ## Blast~~~~~~~~~~~
  
  observeEvent(input$primer_search, {
    
    #output$blast_test <- renderText(input$blast_query %>% shiny::isolate())
    
    if (!fasta_validator(input$blast_query %>% isolate() %>% as.character())) {
      showModal(modalDialog(title = "No or invalid entries!",
                            "Please make sure you entered a valid fasta sequnce",
                            easyClose = TRUE,
                            footer = NULL, 
                            size = "l"))
      return()
    }
    
    output$blast_test <- renderDataTable({
      withProgress(message = "BLAST", 
                 detail = "This may take a while",
                 {
                   for (i in 1:10) {
                     incProgress(1/100)
                     Sys.sleep(0.1)
                   }
                 })
      data$blast_option <- blast_options(query = isolate(input$blast_query),
                                         blast_db = "primers.fasta",
                                         ungapped = isolate(input$exact_match),
                                         evalue = isolate(input$blast_evalue))#,
                #evalue = input$blast_evalue)
                
      blast_query_file <- paste("blast_input_", Sys.time() %>% as.numeric(),".fasta", sep = "")
                
      write_file(x = paste(">blast_query\n", isolate(input$blast_query),sep = ""), 
                 path = paste("blast_query/", blast_query_file, sep = ""), append = FALSE)
                
     data$blast <- blast_search(blast_op = data$blast_options,
                                blast_type = "blastn",
                                query = paste("./blast_query/", blast_query_file, sep = ""),
                                blast_db = "primers.fasta")
     
     data$query_length <- stringr::str_length(isolate(input$blast_query))
                
     file.remove(paste("blast_query/", blast_query_file, sep = ""))
     updateTextAreaInput(session, inputId = "blast_query", value = "")
            
     con <- dbConnect(RSQLite::SQLite(), "primers.sqlite")
     data$blast_output <- con %>% 
       tbl("primers") %>%
       collect() %>%
       mutate(sseqid = id) %>%
       inner_join(data$blast) %>%
       mutate(
         primer_len = stringr::str_length(sequence),
         query_direction = sign(qend - qstart),
         primer_direction = sign(send - sstart),
         query_seq = isolate(input$blast_query) %>% stringr::str_sub(qstart, qend)
       ) %>%
       select(id, primer_name, empty, pident, primer_len, length:send, 
              query_direction, primer_direction, query_seq, sequence, qstart, qend) %>% 
       filter(sstart == primer_len | send == primer_len) %>%
       select(id, primer_name , query_seq, sequence, pident, primer_direction, qstart, qend)
     
     DT::datatable(data$blast_output)
    })
    
    ## Blast plot
    
    output$blast_graph <- renderHighchart({
      con <- dbConnect(RSQLite::SQLite(), "primers.sqlite")
      
      
        highchart() %>%
          hc_yAxis(min = 0,
                   max = stringr::str_length(isolate(input$blast_query)),
                   title= "Query sequence",
                   allowDeciamls = FALSE,
                   crossHair = TRUE,
                   minRange = 10,
                   minorGridLineDashStyle = "longdash",
                   minorTickInterval = 1) %>%
          hc_xAxis(min = -1,
                   max = 1) %>%
          hc_add_series(type = "columnrange",
                        data = tibble(x = 0, low = 1, high = stringr::str_length(isolate(input$blast_query))),
                        hcaes(x = x,
                              low = low,
                              high = high),
                        pointPadding = 0,
                        pointWidth = 10,
                        grouping = FALSE,
                        pointPlacement = 0,
                        enableMouseTracking = FALSE) %>%
          hc_add_series(type = "columnrange",
                        data = data$blast_output %>% isolate(),
                        hcaes(x = primer_direction*0.1,
                              low = qstart,
                              high = qstart + (primer_direction * stringr::str_length(sequence))),
                        pointWidth = 5,
                        grouping = FALSE,
                        pointPlacement = 0,
                        color = "red") %>%
          hc_add_series(type = "columnrange",
                           data = data$blast_output %>% isolate(),
                        hcaes(x = primer_direction*0.1,
                              low = qstart,
                              high = qend),
                        pointWidth = 5,
                        grouping = FALSE,
                        pointPlacement = 0) %>%
          hc_chart(inverted = "x", zoomType = "y") %>%
          highcharter::hc_tooltip(pointFormat = "{point.sequence} <br> Primer: <b>{point.primer_name}</b> <br> ID: {point.id}",
                                  hideDelay = 20) 
      
      
      
    })
     
    session$onSessionEnded(function() {
      con %>% dbDisconnect()
    })
    
  })
  
  ## ~~~~~~~~~~~~~~~~
  
  
  
})
