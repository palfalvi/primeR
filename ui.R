
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(rhandsontable)

  
shinyUI(function(request){
  
  dashboardPage( skin = "black",
    dashboardHeader(title = "Primer search"),
    sidebar = 
    dashboardSidebar(
      sidebarMenu(
      menuItem("Search in available primers", tabName = "check_primer", icon = icon("align-left")),
      menuItem("Submit ordered primers", tabName = "submit_primer", icon = icon("cloud-upload")),
      menuItem("Submit finished primers", tabName = "empty_primer", icon = icon("trash")),
      menuItem("Create new primers", tabName = "create_primer", icon = icon("cogs")),
      menuItem("Create CRISPR gRNA", tabName = "create_grna", icon = icon("cogs"), badgeColor = "red", badgeLabel = "soon"),
      menuItem("Brows available primers", tabName = "browse_primer", icon = icon("database")),
      menuItem("Order primers", href = "https://www.bio.fasmac.co.jp/FasmacWebSystem/ja-JP/Account/Login.mvc", icon = icon("rocket"), badgeColor = "red", badgeLabel = "Fasmac")
    )),
    body = 
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(tabName = "check_primer",
                fluidRow(
                  box(
                    title = "Inputs", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 8,
                    textAreaInput("blast_query", "Fasta query input:", cols = 10, rows = 8, resize = "vertical", placeholder = paste(">FASTA&nbsp;tcagatacagatagaca")),
                    actionButton("primer_search", "Search")
                  ),
                  box(
                    title = "Settings", 
                    status = "info", 
                    solidHeader = TRUE, 
                    collapsible = TRUE, 
                    collapsed = TRUE,
                    width = 4,
                    checkboxInput("exact_match", "Without gaps", value = 1),
                    numericInput("blast_evalue", "E-value treshold", value = 10, min = 0, max = 100)
                  )),
                fluidRow(
                  box(width = 12,
                      collapsible = TRUE,
                      status = "info",
                      solidHeader = TRUE,
                      title = "Graphical results"
                      #plotlyOutput()
                  ),
                  box(width = 12,
                    title = "Results", status = "info", solidHeader = TRUE, 
                    HTML('Here comes the result later')
                  )
                )),
        tabItem(tabName = "submit_primer",
                fluidRow(
                  box(width = 8,
                      title = "User info",
                      status = "info",
                      solidHeader = TRUE,
                      #User
                      
                      selectizeInput("submit_user", "Username:", options = auth_users, choices = c("Select" = "", auth_users)),
                      br(),
                      bookmarkButton("Save bookmark with your name")
                  ),
                  box(width = 4, id = "status", title = "Status", status = "info", solidHeader = TRUE,
                      uiOutput("status"))
                  ),
                  fluidRow(
                  tabBox(width = 12,
                      title = tagList(icon("gear"), "New entries"),
                      tabPanel(title = "Upload a file",
                               fileInput("file_primer_update", label = "Upload .csv file with primer infos", 
                                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                               ),
                               checkboxInput("fasmac_csv", label = "Fasmac .csv file", value = TRUE),
                               
                               actionButton("upload_check", "Upload file"),
                               actionButton("file_reset", "Reset file")
                             ),
                     tabPanel(title = "Manual inputs",
                              rHandsontableOutput("manual_input"),
                              br(),
                              actionButton("manual_check", "Validate primers"),
                              actionButton("manual_reset", "Reset")
                      
                  )
                )),
                fluidRow(
                  box(width = 12, id = "box1",
                      title = "Check and Submit", status = "warning", solidHeader = TRUE,
                      DT::dataTableOutput("check_table"),
                      actionButton("submit_new", "Submit", icon = icon("cloud-upload")),
                      actionButton("reset_submit", "Reset", icon = icon("ban")))
    
                ),
                
                fluidRow(  
                  box(width = 12,
                      title = "Actions",
                      status = "warning",
                      valueBox("Correctly filled lines", icon = icon("check-square-o"), color = "green", value = 0, width = 5)
                  ))),
        
        tabItem(tabName = "empty_primer",
                fluidRow(
                  box(width = 12,status = "info",
                      title = "Modify primer records", solidHeader = TRUE,
                      selectizeInput("empty_primers", label = "Primer ID", choices = all_primers$`No.`)
                  ),
                  valueBoxOutput("empty_info",width = 12),
                  box(width = 12,status = "info", solidHeader = TRUE,
                      actionButton("finished_primer_btn", "Report finished primer"),
                      actionButton("report_reordered", "Report reordered primers"),
                      actionButton("modify_primer_comment", "Modify primer comment"))
                )),
        
        tabItem(tabName = "create_primer",
                fluidRow(
                  box(width = 12,
                    title = "Inputs", status = "primary", solidHeader = TRUE,
                    textAreaInput("text", "DNA sequence:",cols = 80, rows = 10),
                    checkboxInput("full_length", "Need the full length", value = 0),
                    textInput("5_extension", "5'-overhang: "),
                    textInput("3_extension", "3'-overhang: "),
                    actionButton("primer_search", "Search")
                  )),
                fluidRow(
                  box( width = 12,
                    title = "Results", status = "info", solidHeader = TRUE, 
                    HTML('Here comes the result later')
                  )
                  )),
        tabItem(tabName = "create_grna",
                fluidRow(
                  box(width = 12,
                      HTML("<h1>Coming soon...</h1> <br><br>"),
                      HTML("Design gRNA for your CRISPR experiment to target a single site (or paired site) without haveing any off-target with <a href=http://eendb.zfgenetics.org/casot/index.php>CasOT</a>.")
                      )
                )
                ),
        tabItem(tabName = "browse_primer",
                fluidRow(
                  box(width = 12,
                    DT::dataTableOutput("browse_primers")
                  )))
      )
      
    )
  )
  
})





