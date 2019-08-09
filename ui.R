
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
  titlePanel("BRC Census Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      img(src="censuslogo.png",align="center",width="50%"),
      # fileInput("dataLocation", "Load new CSV/TSV data file",
      #           accept = c("text/csv", 
      #                      "text/comma-separated-values,text/plain",
      #                      ".csv",
      #                      ".tsv")
      # ),
      h2("Select question to analyze"),
      selectInput("rowvar", "Analysis Question",
                  choices = varnames2,
                  width = '98%'),
      sliderInput("year", "Year:",
                  min = 2016, max = 2018,
                  value = 2018,
                  sep = "")
      # radioButtons("tabNorm", "Table normalization:",
      #               c("All population"="All", "By row"="Row", "By column"="Col")),
      
    ),
    mainPanel(
      # fluidRow(
      #   # Dynamic infoBoxes
      #   infoBoxOutput("mostCommonResponseBox"),
      #   infoBoxOutput("leastCommonResponseBox"),
      #   infoBoxOutput("responseRateBox")
      # ),
      h2("Response Distribution"),
      fluidRow(plotOutput(outputId = "plotRow", width = '90%')),
      h2("Conditional Response Distribution"),
      fluidRow(
      selectizeInput("colvar", "Column Variable",
                      choices = varnames2,
                      multiple = FALSE, selected = "completed",
                      width = '80%'),
      #dataTableOutput('tableTwoVars')
      tableOutput('tableTwoVars'),
      plotOutput(outputId = "mosaicPlot")
      
      )
    )
  )
))
