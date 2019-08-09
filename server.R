
library(shiny)
library(shinythemes)
library(data.table)
library(survey)
library(plyr)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
library(plotly)
library(scales)
library(ggmosaic) #for new chart

## Settings
#dataLocation = "C:\\Users\\Aaron\\Documents\\census\\Data\\main_results_2017\\Online survey\\csv\\Clean2017CensusFulltabMar2018.csv"
dataLocation = "census_3yr_dashboard.RData"
variableLocation = "variable_name_lookup_v2.csv"

#censusResults = data.table();
# weights=c();
# normWeights=c();
# load_new_data = function(inputFileName, forceLoad) {
#   if (!is.null(inputFileName)) {
#     if ((inputFileName != dataLocation) || forceLoad) {
#       censusResults <<- fread(dataLocation, sep = "\t", na.strings = c("", "NA"))
#       #censusResults = read.csv(dataLocation, fill = TRUE, na.strings=c("","NA"))
#       censusResults$weightnerds[is.na(censusResults$weightnerds)] = 0
#       weights <<- censusResults$weightnerds
#       normWeights <<- sum(censusResults$weightnerds)
#     }
#   }
# }

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

# makePlotData <- function(df, groupvar){
#   groupvar <- sym(groupvar)
#   df %>% group_by(!!groupvar) %>% 
#     summarize(p = sum(normWeights))
# }

## Open input files
load(dataLocation)
varnames = fread(variableLocation, na.strings = "")
# varnames$label[varnames$varnames == "weightnerds"] = "weightnerds"
# censusResults[, (varnames$varnames[is.na(varnames$label)]):=NULL]

## Main server function
shinyServer( function(input, output) {
  
  filteredRow <- reactive({
    # if (!is.null(input$dataLocation)) {
    #   load_new_data(input$dataLocation["datapath"])
    # }
    req(input$rowvar)
    censusResults[[as.character(input$year)]][[input$rowvar]]
  })

  filteredCol <- reactive({
    # if (!is.null(input$dataLocation)) {
    #   load_new_data(input$dataLocation["datapath"])
    # }
    req(input$colvar)
    censusResults[[as.character(input$year)]][[input$colvar]]
  })
  
  #Make 2-variables table                         )
  output$tableTwoVars <- function(){
    # if (!is.null(input$dataLocation)) {
    #   load_new_data(input$dataLocation["datapath"])
    # }
    v1 <- filteredRow()
    v2 <- filteredCol()
    weightsyear <- censusResults[[as.character(input$year)]][["weights"]]
    ptab <- prop.table(weighted_table(v1, v2, weightsyear), 2
               # switch(input$tabNorm,
               #        "All"=NULL,
               #        "Row"=1,
               #        "Col"=2)
               ) %>%
      as.table() %>%
      round(digits = 2) %>%
      apply(MARGIN = c(1,2), FUN = function(x){
        cell_spec(x, format = "html", bold = T, 
                  color = spec_color(x, end = 0.9, scale_from = c(0,1)))
      })

    kable(ptab, escape = FALSE, digits = 2) %>%
      kable_styling(c("striped", "condensed"), full_width = FALSE) %>%
      row_spec(row = 1:nrow(ptab), bold = TRUE) %>%
      row_spec(row = 0, angle = 0)
      
  }
  
    
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
  colorSetting <- scale_color_manual(values=colorScheme)
  fillSetting <- scale_fill_manual(values=colorScheme)

  output$plotRow <- renderPlot({
    censusyear = censusResults[[as.character(input$year)]]
    ggplot(environment = environment()) +
      themeSetting +
      geom_bar(fill = colorScheme[1], 
               data=censusyear, 
               aes_string(x = input$rowvar, 
                          weight = censusyear$normWeights)) +
      labs(x = varnames$label[varnames$varnames == input$rowvar],
           y = 'Percentage') +
      scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })

  # output$plotCol <- renderPlot({
  #   ggplot(environment = environment()) +
  #     themeSetting +
  #     geom_bar(fill = colorScheme[1], data=censusResults, 
  #              aes_string(x = input$colvar,weight=weights / normWeights)) +
  #     labs(x = varnames$label[varnames$varnames == input$colvar],
  #          y = 'Precentage') +
  #     scale_y_continuous(labels=scales::percent) +
  #     theme(axis.text.x=element_text(angle=90, hjust=1))
  # })

    # FDZ edit
    output$mosaicPlot<- renderPlot({
      censusResults[[as.character(input$year)]]%>%
        group_by_at(c(input$colvar,input$rowvar))%>%
        summarise(n = n())%>% #This part counts how many instances for each pair
        ungroup()%>%
        mutate_if(is.character,as.factor)%>% #factors are required for geom_mosaic
        ggplot() +
        themeSetting +
        geom_mosaic(aes_string(weight='n', x=paste0("product(",input$rowvar, ")"), fill=input$colvar))+
        scale_fill_manual(values=colorScheme)+
        theme(axis.text.x=element_text(angle=90, hjust=1))
        })
})
