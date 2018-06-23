## app.R ##
library(shinydashboard)
library(shiny)
library(data.table)
library(survey)

#censusResults = fread("/home/spagan/development/CensusBM/bm-census-moop-correlation/data/Census2017Geomapwtlabels.csv",
#                      sep = "\t", fill = TRUE, na.strings=c("",NA"))
censusResults = fread("/home/spagan/development/CensusBM/bm-census-moop-correlation/data/Census2017Geomapwtlabels.csv",
                      fill = TRUE, na.strings=c("","NA"))
censusResults$weightnerds[is.na(censusResults$weightnerds)] = 0
censusDesign = svydesign(ids = ~id, weights = ~weightnerds,
                         data = censusResults)

weighted_table = function(rowvar, colvar, weights){
  rowLevels = unique(rowvar)[order(unique(rowvar))]
  rowLevels = rowLevels[!is.na(rowLevels)]
  colLevels = unique(colvar)[order(unique(colvar))]
  colLevels = colLevels[!is.na(colLevels)]
  #  wtab = matrix(0, length(rowLevels), length(colLevels))
  wtab = sapply(1:length(colLevels), function(c, w, rl, cl, rv, cv){
    sapply(1:length(rowLevels), function (r, c, w, rl, cl, rv, cv){
      sum(w[rowvar == rowLevels[r] & colvar == colLevels[c]],
          na.rm = TRUE)
    }, w = w, c = c, rl = rl, cl = cl, rv = rv, cv = cv)
  }, w = weights, rl = rowLevels, cl = colLevels, rv = rowvar, cv = colvar)
  colnames(wtab) = colLevels
  rownames(wtab) = rowLevels
  return(wtab)
}




ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Inputs", icon = icon("bar-chart-o"),
             # Input directly under menuItem
             selectInput("rowvar", "Row Variable",
                         choices = c("Gender" = "gender", "Race" = "ethno",
                                     "Age" = "agegr4", "Virgin" = "firstyear"),
                         width = '98%'),
             
             # Input inside of menuSubItem
             selectizeInput("colvar", "Column Variable",
                            choices = names(censusResults),
                            multiple = FALSE, selected = "completed",
                            width = '98%')
    )
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(dataTableOutput('table1'))
    ),
    fluidRow(
      box(dataTableOutput('table2'))
    ),
    fluidRow(
      box(dataTableOutput('table3'))
    )
  )
)

server <- function(input, output) {
  censusResults = fread("/home/spagan/development/CensusBM/bm-census-moop-correlation/data/Census2017Geomapwtlabels.csv",
                        na.strings=c("","NA"))
  censusResults$weightnerds[is.na(censusResults$weightnerds)] = 0
  censusDesign = svydesign(ids = ~id, weights = ~weightnerds,
                           data = censusResults)
  
  
  
  output$table1 <- renderDataTable({
    v1 <- censusResults[[input$rowvar]]
    v2 <- censusResults[[input$colvar]]
    weights = censusResults$weightnerds
    prop.table(weighted_table(v1, v2, weights))
  }
  )
  output$table2 <- renderDataTable({
    v1 <- censusResults[[input$rowvar]]
    v2 <- censusResults[[input$colvar]]
    weights = censusResults$weightnerds
    prop.table(weighted_table(v1, v2, weights), 1)
  }
  )
  output$table3 <- renderDataTable({
    v1 <- censusResults[[input$rowvar]]
    v2 <- censusResults[[input$colvar]]
    weights = censusResults$weightnerds
    prop.table(weighted_table(v1, v2, weights), 2)
  }
  )
}

shinyApp(ui, server)