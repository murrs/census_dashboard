
library(shiny)
library(shinythemes)
library(data.table)
library(survey)
library(plyr)
library(ggplot2)

## Settings
#dataLocation = "Census2017Geomapwtlabels.csv"
#censusResults = fread(dataLocation, fill = TRUE, na.strings=c("","NA"))
varnames = fread("variable_name_lookup.csv", na.strings = "")
varnames2 = varnames$varnames[!is.na(varnames$label)]
names(varnames2) = varnames$label[!is.na(varnames$label)]

shinyUI(fluidPage(theme = shinytheme("lumen"),
  titlePanel("BM Census Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("rowvar", "Row Variable",
                  choices = varnames2,
                  width = '98%'),
      selectizeInput("colvar", "Column Variable",
                     choices = varnames2,
                     multiple = FALSE, selected = "completed",
                     width = '98%'),
      radioButtons("tabNorm", "Table normalization:",
                    c("All population"="All", "By row"="Row", "By column"="Col")),
      img(src="censuslogo.png",align="center",width="50%")
    ),
    mainPanel(
      h2("Univariate distributions"),
      fluidRow(splitLayout(cellWidths = c('50%','50%'), 
        plotOutput(outputId = "plotRow"),
        plotOutput(outputId = "plotCol")
      )),
      h2("Correlation table"),
      fluidRow(
      #dataTableOutput('tableTwoVars')
      tableOutput('tableTwoVars')
      )
    )
  )
))
