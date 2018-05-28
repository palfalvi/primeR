
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
library(highcharter)

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
                                                                                    empty = empty ) -> table_out
                              
                              return(table_out)
                            })
  
  output$browse_primers <- DT::renderDataTable(all_primers() %>% datatable(class = 'cell-border stripe',
                                                                         rownames = FALSE,
                                                                         options = list(searchHighlight = TRUE,
                                                                         scrollX = TRUE,
                                                                         fixedColumns = TRUE
                                                                         )),
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
                                                 lengthMenu = FALSE,
                                                 dom = 't',
                                                 scrollX = TRUE,
                                                 fixedColumns = TRUE),
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
       data$check <- NULL
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
                                                        lengthMenu = FALSE,
                                                        scrollX = TRUE,
                                                        fixedColumns = TRUE
                                                        ),
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
    
    showNotification( paste("Primer submitted:\n", "(id:", ")"),
                      type = "message", duration = 10)
    
    
    
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
                                                                         lengthMenu = FALSE,
                                                                         scrollX = TRUE,
                                                                         fixedColumns = TRUE
                                                                         ),
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
    con <- dbConnect(SQLite(), "primers.sqlite")
    dbExecute(con, paste("UPDATE primers SET empty = 1 WHERE id = ", input$empty_primers,";"))
    dbDisconnect(con)
    #Add finished comment to primer
  })
  
  observeEvent(input$cancel_finished, {
    removeModal()
  })

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  ##Report modified comment~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    #Create new entry for comment
    con <- dbConnect(SQLite(), "primers.sqlite")
    dbExecute(con, paste("UPDATE primers SET comments = comments || ';", input$new_comment ,"' WHERE id = ", input$empty_primers,";", sep = ""))
    dbDisconnect(con)
    
    showNotification( paste("Primer comment updated:\n", all_primers() %>% filter(id == input$empty_primers) %>% .[,2], "(id:", input$empty_primers, ")"),
                      type = "error", duration = 10)
  })
  
  # observeEvent(input$cancel_finished, {
  #   removeModal()
  #   
  # })
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
    
    if (!fasta_validator(input$blast_query %>% isolate() %>% as.character() %>% stringr::str_remove_all("[\r\n]"))) {
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

                   
      data$blast_option <- blast_options(query = isolate(input$blast_query),
                                         blast_db = "primers.fasta",
                                         ungapped = isolate(input$exact_match),
                                         evalue = isolate(input$blast_evalue))#,
                #evalue = input$blast_evalue)
                
      blast_query_file <- paste("blast_input_", Sys.time() %>% as.numeric(),".fasta", sep = "")
                
      write_file(x = paste(">blast_query\n", isolate(input$blast_query),sep = ""), 
                 path = paste("blast_query/", blast_query_file, sep = ""), append = FALSE)
     data$blast <- NULL           
     data$blast <- blast_search(blast_op = data$blast_options,
                                blast_type = "blastn",
                                query = paste("./blast_query/", blast_query_file, sep = ""),
                                blast_db = "primers.fasta")
     
     data$query_length <- stringr::str_length(isolate(input$blast_query))
                
     file.remove(paste("blast_query/", blast_query_file, sep = ""))
     updateTextAreaInput(session, inputId = "blast_query", value = "")
                   
                 }) 
     con <- dbConnect(RSQLite::SQLite(), "primers.sqlite")
     data$blast_output <- NULL
     data$blast_output <- con %>% 
       tbl("primers") %>%
       collect() %>%
       mutate(sseqid = id) %>%
       inner_join(data$blast) %>%
       filter(!empty) %>%
       mutate(
         primer_len = stringr::str_length(sequence),
         primer_direction = sign(send - sstart),
         binding_seq = sequence %>% stringr::str_sub(ifelse(primer_direction == 1, sstart, send), ifelse(primer_direction == 1, send, sstart)),
         flagging_seq = ifelse(str_length(sequence) == str_length(binding_seq), "", sequence %>% stringr::str_sub(end = primer_len - str_length(binding_seq) )),
         alignment = map2(.x = qseq, .y = sseq, .f = function(x, y) {(str_split(x, "")[[1]] == str_split(y,"")[[1]]) %>% as.numeric()}),
         dots = pmap(list(qstart, qend, alignment, primer_direction), .f = function(x, y, z, d) { if(d == 1){seq(x, y, length.out = length(z))} else {seq(y, x, length.out = length(z))}}),
         placeholder = flagging_seq %>% str_replace_all("[a-zA-Z]", "_")
       ) %>%
       select(id, primer_name, empty, pident, primer_len, length:send, primer_direction, sequence, binding_seq, flagging_seq, alignment, dots, sseq, qseq, placeholder) %>% 
       filter(sstart == primer_len | send == primer_len) %>%
       select(id, primer_name, binding_seq, flagging_seq, sequence, pident, primer_direction, qstart, qend, gapopen, mismatch, alignment, dots, sseq, qseq, placeholder)
     
     DT::datatable(data$blast_output %>%
                     mutate(Direction = ifelse(primer_direction == 1, "Forward", "Reverse"),
                            "Binding length" = qend - qstart + 1) %>%
                     select(-flagging_seq,
                            -binding_seq,
                            -qseq,
                            -sseq,
                            -alignment,
                            -dots,
                            -placeholder,
                            -qstart,
                            -qend,
                            -primer_direction), 
                   options = list(
                     scrollX = TRUE,
                     fixedColumns = TRUE
                   )
                     )
    })
    
    ## Blast plot
    
    output$blast_graph <- renderHighchart({
      
      withProgress(message = "Summarising results", 
                   detail = "",{
      
      con <- dbConnect(RSQLite::SQLite(), "primers.sqlite")
      
        highchart() %>%
          hc_yAxis(title = list(text = "Input sequence (bp)"),
                   min = 0,
                   max = stringr::str_length(isolate(input$blast_query)),
                   title= "Query sequence",
                   allowDeciamls = FALSE,
                   crossHair = TRUE,
                   minRange = 40,
                   minorGridLineDashStyle = "longdash",
                   minorTickInterval = 1)%>%
          hc_xAxis(title = list(text = ""),
                   min = -1,
                   max = 1,
                   reversed = FALSE
                   ) %>%
          hc_add_series(#primer_matching
                     type = "columnrange",
                     enableMouseTracking = FALSE,
                     data = data$blast_output %>%
                       select(id, dots, alignment, primer_direction) %>% 
                       unnest(),
                     hcaes(low = dots - 0.5,
                           high = dots + 0.5,
                           x = primer_direction*0.1,
                           color = ifelse(alignment == 1, "green", "red")),
                     pointWidth = 4,
                     pointPlacement = 0
                   ) %>%
          hc_add_series(type = "columnrange",
                        data = tibble(x = 0, low = 1, high = stringr::str_length(isolate(input$blast_query))),
                        hcaes(x = x,
                              low = low,
                              high = high),
                        pointPadding = 0,
                        pointWidth = 10,
                        grouping = FALSE,
                        pointPlacement = 0,
                        enableMouseTracking = FALSE,
                        color = "dodgerblue") %>%
          hc_add_series(type = "columnrange",
                        data = data$blast_output %>% isolate(),
                        hcaes(x = primer_direction*0.2,
                              low = ifelse(primer_direction == 1, qstart - str_length(flagging_seq), qend),
                              high = ifelse(primer_direction == 1, qstart, qend + str_length(flagging_seq))),
                        pointWidth = 5,
                        grouping = FALSE,
                        pointPlacement = 0,
                        color = "red") %>%
          hc_add_series(type = "columnrange",
                           data = data$blast_output %>% isolate(),
                        hcaes(x = primer_direction*0.2,
                              low = qstart,
                              high = qend),
                        pointWidth = 5,
                        grouping = FALSE,
                        pointPlacement = 0,
                        fillOpacity = 0.1) %>%
          hc_chart(inverted = TRUE, zoomType = "y") %>%
          hc_tooltip(pointFormat = "5'-<i>{point.flagging_seq}</i><b>{point.sseq}</b>-3' <br>5'-{point.placeholder}<b>{point.qseq}</b>-3'<br>Primer: <b>{point.primer_name}</b><br>ID: {point.id}",
                                  hideDelay = 20,
                     headerFormat = "Primer info<br>",
                     style = list(fontFamily = "Monaco"),
                     hideDelay = 500) %>%
          hc_legend(enabled = FALSE) %>%
          hc_plotOptions(dataLabels = list(
            enable = TRUE,
            format = '<point.name>' 
          ))
                   })
      
    })
    
    output$download_blast <- downloadHandler(
      filename = function() {paste('primer_result-', Sys.Date(), '.csv', sep = "")},
      content = function(con){write_csv(data$blast_output %>% select(id,
                                                                     primer_name,
                                                                     sequence),
                                        con)})
     
    session$onSessionEnded(function() {
      con %>% dbDisconnect()
    })
    
  })
  
  ## ~~~~~~~~~~~~~~~~
  
  
  
})
