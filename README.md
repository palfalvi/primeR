#_primeR_ is an R-Shiny based website for managing one's laboratory's oligo/primer database with extra features

##Description

_primeR_ is an R-Shiny based websiteto manage your laboratory's oligos/primers. It incorporates a submission page for automatically assign your primers to the database (SQLite), which will generate an autoincrement ID for easier management and storage.

The BLAST tab is BLAST searching your query in the currently available database and filtering the outputs for "primer-compatibility" (e.g. 3' binding).


Dependencies

_primeR_ uses the following R packages:

```
shiny
shinydashboard
shinyjs
DT
rhandsontable
tidyverse
DBI
RSQLite
dbplyr
highcharter
```

Installation

Contact

[palfalvi.gergo@gmail.com](palfalvi.gergo@gmail.com)

Licence

