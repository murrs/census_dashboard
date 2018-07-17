## app.R ##
library(shiny)
library(shinythemes)
library(data.table)
library(survey)
library(plyr)
library(ggplot2)

## Settings
dataLocation = "/home/spagan/development/CensusBM/bm-census-moop-correlation/data/Census2017Geomapwtlabels.csv"


## Open input file
censusResults = fread(dataLocation, fill = TRUE, na.strings=c("","NA"))
censusResults$weightnerds[is.na(censusResults$weightnerds)] = 0

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

ui <- fluidPage(theme = shinytheme("lumen"),
  titlePanel("BM Census Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("rowvar", "Row Variable",
                  choices = c("Gender" = "gender", "Race" = "ethno",
                              "Age" = "agegr4", "Virgin" = "firstyear"),
                  width = '98%'),
      selectizeInput("colvar", "Column Variable",
                     choices = names(censusResults),
                     multiple = FALSE, selected = "completed",
                     width = '98%'),
      radioButtons("tabNorm", "Table normalization:",
                    c("All population"="All", "By row"="Row", "By column"="Col"))
    ),
    mainPanel(
      h2("Univariate distributions"),
      plotOutput(outputId = "plotRow", width = 300),
      plotOutput(outputId = "plotCol", width = 300),
      h2("Variable correlation:"),
      dataTableOutput('tableTwoVars')
    )
  )
)

server <- function(input, output) {
  
  #Filter input data, if requested
  # For now filtering is disabled, to enable it, add after the last instruction below:
  #    censusResults %>%
  #      filter(
  #        #put here conditions, e.g.
  #        #gender == input$genderFilter
  #      )

#  filteredData <- reactive({
#    req(input$rowvar)
#    req(input$colvar)
#    cat('input dataset columns:')
#    cat(colnames(censusResults))
#    cat('\n')
#    censusResults[,c(input$colvar,input$rowvar)]
#  })
#  colnames(filteredData) <- c("COL", "ROW")
 
  filteredRow <- reactive({
    req(input$rowvar)
    censusResults[, input$rowvar]
  })

  filteredCol <- reactive({
    req(input$colvar)
    censusResults[, input$colvar]
  })
  
  weights <- censusResults$weightnerds
  normWeights = sum(censusResults$weightnerds)
  
  #Make 2-variables table                         )
  output$tableTwoVars <- renderDataTable(withProgress({
    prop.table(weighted_table(filteredRow(), filteredCol(), weights),
               switch(input$tabNorm,
                      "All"=NULL,
                      "Row"=1,
                      "Col"=2))
  }, message="Reloading data... Please wait"))
  
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

  output$plotRow <-renderPlot({
    cat(paste('Input variable:', input$rowvar, '\n'))
    cat('Dataset: \n')
    cat(filteredRow())
    cat('\n')
    #cat(filteredData())
    #cat('\n')
    #cat(paste('NRows: ', nrow(filteredData())))
    #cat(filteredData()[1,])
    #cat('\n')
    ggplot() +
      themeSetting +
      geom_bar(fill=colorScheme[1], data=filteredRow(), aes_string(x=input$rowvar,weight=100*weights/normWeights)) +
      labs(x=input$rowvar,y='Precentage') +
      scale_y_continuous(labels=scales::percent)
  })

  output$plotCol <-renderPlot({
    ggplot() +
      themeSetting +
      geom_bar(fill=colorScheme[1], data=filteredCol(), aes_string(x=input$colvar,weight=100*weights/normWeights)) +
      labs(x=input$colvar,y='Precentage') +
      scale_y_continuous(labels=scales::percent)
  })
  
}

shinyApp(ui, server)