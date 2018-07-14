## app.R ##
library(shinydashboard)
library(shiny)
library(data.table)
library(survey)
library(ggplot2)

## Settings
dataLocation = "/home/spagan/development/CensusBM/bm-census-moop-correlation/data/Census2017Geomapwtlabels.csv"


## Open input file
censusResults = fread(dataLocation,
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
      p("Density, normalized to all population"),
      box(dataTableOutput('table1'))
    ),
    fluidRow(
      p("Density, normalized by row"),
      box(dataTableOutput('table2'))
    ),
    fluidRow(
      p("Density, normalized by column"),
      box(dataTableOutput('table3'))
    ),
    fluidRow(
      p(paste("Univariate density plot for variable","ROW")),
      #box(renderPlot('plotRow'))
      plotOutput('plotRow')
    )
  )
)

server <- function(input, output) {
  censusResults = fread(dataLocation, na.strings=c("","NA"))
  censusResults$weightnerds[is.na(censusResults$weightnerds)] = 0
  censusDesign = svydesign(ids = ~id, weights = ~weightnerds,
                           data = censusResults)

  #Tables 
  output$table1 <- renderDataTable({
    v1 <- censusResults[[input$rowvar]]
    v2 <- censusResults[[input$colvar]]
    weights = censusResults$weightnerds
    prop.table(weighted_table(v1, v2, weights))
  })
  output$table2 <- renderDataTable({
    v1 <- censusResults[[input$rowvar]]
    v2 <- censusResults[[input$colvar]]
    weights = censusResults$weightnerds
    prop.table(weighted_table(v1, v2, weights), 1)
  })
  output$table3 <- renderDataTable({
    v1 <- censusResults[[input$rowvar]]
    v2 <- censusResults[[input$colvar]]
    weights = censusResults$weightnerds
    prop.table(weighted_table(v1, v2, weights), 2)
  })
  
  #Make the uni-variate distributions
  colorScheme = c("#EA008B","#CC308D","#AE608E","#909090")
  themeSetting <- theme(panel.grid.major = element_blank(),
                        panel.grid.minor.y = element_line(color="#666464", size = .1),
                        panel.background  = element_blank(), axis.line.x = element_line(color="Black", size=.75),
                        title = element_text (size = 15), legend.key = element_blank(),
                        axis.line.y = element_line(color="Black", size=.75),
                        axis.title.y = element_text(face = "bold", margin = margin(0,20,0,0), size = 14),
                        axis.title.x = element_text(face = "bold", margin = margin(20,0,0,0), size = 14),
                        axis.text = element_text(size = 13), plot.title = element_text(face = "bold", hjust = 0.5))
  colorSetting <- scale_color_manual(values=colorscheme)
  fillSetting <- scale_fill_manual(values=colorscheme)

#  output$plotRow <-renderPlot({
#      ggplot(data=censusResults, aes(x=input$rowvar,weight=weightnerds/sum(weightnerds))) +
#        themeSetting +
#        geom_bar(stat="identity", fill=colorScheme[1]) +
#        labs(title='Univariate distribution',x=input$rowvar,y='Precentage') +
#        scale_y_continuous(labels=scales::percent)
#  }, height = 400,width = 600)

  output$plotRow <-renderPlot({
    ggplot(data=censusResults, aes(x=input$rowvar)) +
      themeSetting +
      geom_bar(fill=colorScheme[1]) +
      labs(title='Univariate distribution',x=input$rowvar,y='Precentage') +
      scale_y_continuous(labels=scales::percent)
  }, height = 400,width = 600)
  
}

shinyApp(ui, server)