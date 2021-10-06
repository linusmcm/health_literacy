#######################################################################
#######################################################################
#install.packages("shiny")
#install.packages("visNetwork")
#install.packages("readxl")
#install.packages("shinythemes")
#install.packages("devtools")
#install.packages('devtools')
#install.packages("RTextTools")
#install.packages("e1071")
#install.packages("RSentiment")
#require(devtools)
#install.packages("plotly")
#######################################################################
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#######################################################################
library(shiny)
library(plotly)
library(visNetwork)
library(readxl)
library(shinythemes)
library(devtools)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud2)
library(e1071)
library(RTextTools)
library(RSentiment)
#devtools::install_github("datastorm-open/visNetwork")
#######################################################################
source("global.R")
#######################################################################
fontSize <<- 14
shapeValue <<- "circle"
groupType <- c("Select All","Technology","HelloTas","Consumers","Organisation","Training")
cloudTypes <- c("Select All" ,"Launceston Fixed Session","Ulverstone Fixed Session","Individual Transcripts")
#######################################################################
options(shiny.trace=TRUE)
options(shiny.fullstacktrace=TRUE)
#options(shiny.stacktraceoffset=TRUE)
showReactLog(time = TRUE)
width <<- "1800px"
height <<- "900px"
widthNumber <<- 1800
heightNumber <<-900
smallPanel <<- 300
statementPanel <<- 600
#######################################################################
ui <- navbarPage(theme = shinytheme("yeti"),title = "Health Literacy Project",
                 tabPanel(title = "Project Information",
                          h1("Health Literacy Project - Information Page")
                          ),
                 
                 tabPanel(title = "Ripple Map",
                  column(3,
                    wellPanel(h2("Health Literacy Ripple Map"),
                              selectInput('Group', 'Select a Section: ', choices = groupType, selected = "Select All")),
                  wellPanel(
                    textOutput("group"),
                    textOutput("title"),
                    textOutput("value"),
                  textOutput("polarity"),
                textOutput("objectivity"),
              textOutput("label")),
                  wellPanel(
                    htmlOutput("statement")
                  )),
                ################################################
                mainPanel(
                visNetworkOutput("network", width = width, height = height))
                ################################################
),
tabPanel(title = "Word Cloud",
         column(3,
                wellPanel(h2("Health Literacy Word Clouds"),
                          selectInput('wcChoice', 'Please Select a Transcript: ', choices = cloudTypes, selected = "Select All"))
                
        
         ),
         mainPanel(wordcloud2Output("plot"))
         
         
))
####################################################################
####################################################################
server <- function(input, output, session) 
  {
  edges <- read_excel("dataFiles/data.xlsx", sheet = "edges")
  nodes <- read_excel("dataFiles/data.xlsx", sheet = "nodes")

  output$network <- renderVisNetwork(
    {
    visNetwork(nodes, edges, width = "100%", height = "100%") %>%
      visEvents(select = "function(properties) {  Shiny.onInputChange ('test' , properties.nodes); ;}")  %>%
      #visOptions(selectedBy = "group") %>%
      visInteraction(hover = T, hoverConnectedEdges = T, selectable = T, selectConnectedEdges = T, tooltipDelay = 300, tooltipStay = 300,
                      tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: nowrap; font-family: Arial;font-size:18px;font-color:purple; border: black; background-color: white;')%>%
      visEdges(smooth = list(forceDirection = "none", roundness = 0), arrows = "to", hoverWidth = 8 , width = 2,selectionWidth = , 8, shadow = list(enabled = TRUE, size = 5))%>%
      visNodes(shadow = list(enabled = T, size = 5), 
               font = list(face = "Sans Family" , 
                           size = fontSize, 
                           strokeWidth = 1.75, 
                           strokeColor = "white"),
               scaling = list(min = 10, 
                              max = 30, 
                              label = list(enabled = TRUE, 
                                           min = 14, 
                                           max = 30, 
                                           maxVisible = 24, 
                                           drawThreshold = 12)))%>%
      
        
      visPhysics(solver = "repulsion", 
                 minVelocity = 0.75, 
                 repulsion = list(centralGravity = 0.3,
                                          springLength = 50, 
                                          springConstant = 0.49, 
                                          nodeDistance =  400,
                                          damping = 0.03,
                                          avoidOverlap = 0.9))%>% 
              
                 
     
      visGroups(groupname = "Technology", shape = shapeValue, color = list(background = 'rgba(255,0, 0,0.7)', 
                                                                           border = 'rgb(255,0,0)', 
                                                                           highlight = list(background = 'rgba(255,0, 0,0.5)', 
                                                                                            border = 'rgb(255,0,0)'),
                                                                           hover  = list(background = 'rgba(255,0, 0,0.1)', 
                                                                                         border = 'rgb(255,0,0)') 
      )) %>%
      visGroups(groupname = "Training", shape = shapeValue, color = list(background = 'rgba(0, 80, 255, 0.8)', 
                                                                         border = 'rgb(0, 80, 255)', 
                                                                         highlight = list(background = 'rgba(0, 80, 255,0.5)', 
                                                                                          border = 'rgb(0, 80, 255)'),
                                                                         hover  = list(background = 'rgba(0, 80, 255,0.1)', 
                                                                                       border = 'rgb(0, 80, 255)') 
      ))%>%  
      visGroups(groupname = "Consumers", shape = shapeValue, color = list(background = 'rgba(0, 255, 29, 0.8)', 
                                                                          border = 'rgb(0, 255, 29)', 
                                                                          highlight = list(background = 'rgba(0, 255, 29,0.5)', 
                                                                                           border = 'rgb(0, 255, 29)'),
                                                                          hover  = list(background = 'rgba(0, 255, 29,0.1)', 
                                                                                        border = 'rgb(0, 255, 29)') 
      ))%>% 
      visGroups(groupname = "Resources", shape = shapeValue, color = list(background = 'rgba(255, 246, 0, 0.8)', 
                                                                          border = 'rgb(255, 246, 0)', 
                                                                          highlight = list(background = 'rgba(255, 246, 0,0.5)', 
                                                                                           border = 'rgb(255, 246, 0)'),
                                                                          hover  = list(background = 'rgba(255, 246, 0,0.1)', 
                                                                                        border = 'rgb(255, 246, 0)') 
      ))%>%
      visGroups(groupname = "Organisation", shape = shapeValue, color = list(background = 'rgba(249, 107, 0, 0.8)', 
                                                                             border = 'rgb(249, 107, 0)', 
                                                                             highlight = list(background = 'rgba(249, 107, 0,0.5)', 
                                                                                              border = 'rgb(249, 107, 0)'),
                                                                             hover  = list(background = 'rgba(249, 107, 0,0.1)', 
                                                                                           border = 'rgb(249, 107, 0)') 
      ))%>%
      visGroups(groupname = "HelloTas", shape = shapeValue, color = list(background = 'rgba(249, 0, 237, 0.8)', 
                                                                         border = 'rgb(249, 0, 237)', 
                                                                         highlight = list(background = 'rgba(249, 0, 237,0.5)', 
                                                                                          border = 'rgb(249, 0, 237)'),
                                                                         hover  = list(background = 'rgba(249, 0, 237,0.1)', 
                                                                                       border = 'rgb(249, 0, 237)') 
      ))
  })
  ########################################
  textOutput <- reactive({ generate_data(input$test) })
  plotWordCloud <- reactive({ generate_wordCloud(input$wcChoice) })
  #output$textOutput <- renderText({ testValue() })
  ########################################
  output$group <- renderText(
    { 
      paste("Section: ", toupper(textOutput()$group))
    })
  output$value <- renderText(
    { 
      paste("Sentiment Score: ", textOutput()$value)
    })
  output$statement <- renderText(
    { 
      paste(textOutput()$statement, textOutput()$htmlString)
    })
  output$title <- renderText(
    { 
      paste("Title: ", textOutput()$title)
    })
  output$objectivity <- renderText(
    { 
      paste("Objectivity: ", textOutput()$objectivity)
    })
  output$polarity <- renderText(
    { 
      paste("Polarity: ", textOutput()$polarity)
    })  
  ################################################################################
  ################################################################################
  output$plot <- renderWordcloud2({
    #set.seed(1234)
    #wordcloud2(plotWordCloud()$d, color = "random-dark")
    wordcloud2(plotWordCloud()$d, size = 2, minRotation = -pi/2, maxRotation = -pi/2)
    #wordcloud2(plotWordCloud()$d, size = 2  color=rep_len( c("grey", "orange"), nrow(plotWordCloud()$d)))
  })
  ################################################################################
  ################################################################################
  #observe(
  #  {
  #  gr <- input$network_selectedBy
  #  id <- nodes$id[nodes$group%in%gr]
  #  visNetworkProxy("network") %>%
  #    visFit(nodes = id, animation = list(duration = 500, easingFunction = "easeInOutQuad"))
  #  })
  observe({
    gr <- input$Group
    isolate({
      if(gr != "ALL"){
        id <- nodes$id[nodes$group%in%gr]
      }else{
        id <- NULL
      }
      visNetworkProxy("network") %>%
        visFit(nodes = id, animation = list(duration = 500, easingFunction = "easeInOutQuad"))
    })
  })
}
#return(list(title" = node$title, "value" = node$value,  "label" = node$label, "polarity" = node$polarity, "objectivity" = node$objectivity, "majorText" = node$majorText))
####################################################################
shinyApp(ui = ui, server = server)