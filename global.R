library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(rhandsontable)

enableBookmarking(store = "server")


## Example primer file until SQL is not working
#all_primers <- read_csv("primers.csv")
auth_users <- read_csv("users.csv")
#blast_path <- ""

source(file = "helper.R")


jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

con <- DBI::dbConnect(RSQLite::SQLite(), "primers.sqlite")
