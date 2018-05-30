
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
library(highcharter)

  
shinyUI(function(request){
  
  dashboardPage( skin = "black",
    dashboardHeader(title = "Primer search"),
    sidebar = 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "main_page", icon = icon("info")),
        menuItem("Submit ordered primers", tabName = "submit_primer", icon = icon("cloud-upload")),
        menuItem("BLAST in available primers", tabName = "check_primer", icon = icon("align-left")),
        menuItem("Submit finished primers", tabName = "empty_primer", icon = icon("trash")),
        #menuItem("Create new primers", tabName = "create_primer", icon = icon("cogs"), badgeColor = "red", badgeLabel = "soon"),
        #menuItem("Create CRISPR gRNA", tabName = "create_grna", icon = icon("cogs"), badgeColor = "red", badgeLabel = "soon"),
        menuItem("Browse available primers", tabName = "browse_primer", icon = icon("table")),
        menuItem("Order primers", href = "https://www.bio.fasmac.co.jp/FasmacWebSystem/ja-JP/Account/Login.mvc", icon = icon("rocket"), badgeColor = "blue", badgeLabel = "Fasmac"),
        menuItem("Report issues", href = "https://github.com/palfalvi/primeR", icon = icon("github"))
         )),
    body = 
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(tabName = "main_page",
                HTML("<h1><center>Welcome to the <i>primeR</i> database</center></h1>
                     <br><br>
                     Contact: <a href='https://github.com/palfalvi/primeR'>GitHub</a>")),
        tabItem(tabName = "check_primer",
                fluidRow(
                  box(
                    title = "Inputs", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 8,
                    textAreaInput("blast_query", "Sequence input (without header!):", cols = 10, rows = 8, resize = "vertical", placeholder = paste("cggctagctagaaac\ntcagatacagataga")),
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
                    numericInput("blast_evalue", "E-value treshold", value = 1000, min = 0, max = 10000)
                  )),
                fluidRow(
                  box(width = 12,
                      collapsible = TRUE,
                      status = "info",
                      solidHeader = TRUE,
                      title = "Graphical results",
                      highchartOutput("blast_graph")
                  ),
                  box(width = 12,
                    title = "Results", status = "info", solidHeader = TRUE, 
                    downloadButton("download_blast", "Download blast results"),
                    br(),
                    dataTableOutput("blast_test")
                  )
                )),
        tabItem(tabName = "submit_primer",
                fluidRow(
                  box(width = 8,
                      title = HTML("<i class='fa fa-user-circle'></i> User info"),
                      status = "info",
                      solidHeader = TRUE,
                      #User
                      #uiOutput("user_selection"),
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
                      DT::dataTableOutput("new_primers")
                  ))),
        
        tabItem(tabName = "empty_primer",
                fluidRow(
                  box(width = 12,status = "info",
                      title = "Modify primer records", solidHeader = TRUE,
                      uiOutput("empty_primer_selection")
                      #selectizeInput("empty_primers", label = "Primer id", choices = all_primers() %>% select(id))
                  ),
                  valueBoxOutput("empty_info",width = 12),
                  box(width = 6,status = "info", solidHeader = TRUE,
                      actionButton("finished_primer_btn", "Report finished primer", icon = icon("trash"))
                      ),
                  box(width = 6,status = "info", solidHeader = TRUE,
                      textInput("new_comment", "New comment:"),
                      actionButton("modify_primer_comment", "Modify primer comment", icon = icon("comment"))
                      )
                  )
                ),
        
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





