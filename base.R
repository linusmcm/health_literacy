####################################################################
####################################################################
#setwd("F:/monash/assignment_4/assignment_4")
options(shiny.trace=TRUE)
#options(shiny.fullstacktrace=TRUE)
options(shiny.stacktraceoffset=TRUE)
#showReactLog(time = TRUE)
####################################################################
source("global.R")
install_packages()
install_libraries()

####################################################################
endType <- c("both", "north", "south")
viewType <- c("spectator","above", "bowler","half", "batsman")
####################################################################
ui <- fluidPage(theme = shinytheme("cyborg"),
                absolutePanel(
                  top = 5, left = 5, right = 5,width = 250,
                  fixed = TRUE,
                  wellPanel(
                    textOutput("textEnds"),
                    textOutput("text2"),
                    textOutput("text1"),
                    textOutput("SpeedDat")),
                  wellPanel(
                    textOutput("aHeight"),
                    textOutput("hMin"),
                    textOutput("hMax")
                  ),
                  wellPanel(
                    textOutput("aSpeed"),
                    textOutput("sMin"),
                    textOutput("sMax")
                  )),
                
                plotlyOutput("plot", height = "100%", width = "100%"),
                hr(),
                fluidRow(
                  column(3, 
                         
                         wellPanel(selectInput('ends', 'Bowling End', choices = endType, selected = "End"), selectInput('view', 'View Perspective:', choices = viewType, selected = "viewType")), 
                         wellPanel(sliderInput(inputId = "bounceH", label = "Bounce Height",   value = 50, min = 1, max = 100), sliderInput(inputId = "speedSlow", label = "Speed generated",   value = 50, min = 1, max = 100) 
                         )),
                  column(3, plotlyOutput("plotHex", height = "100%", width = "100%")),
                  column(width = 3, plotlyOutput("plotCorner", height = "100%", width = "100%")),
                  column(width = 3, plotlyOutput("plotSpeed", height = "100%", width = "100%"))
                ))
#end fluidRow 
####################################################################
####################################################################
server <- function(input, output,session) 
{
  datasetY <- reactive({  generate_data(input$bounceH,input$speedSlow, input$ends,input$view) })
  ########################################
  ########################################
  output$plot <- renderPlotly(
    { 
      p <- callGRaph(datasetY())
      ggplotly(p)
    })
  ########################################
  output$plotCorner <- renderPlotly(
    { 
      p <- linear_graph(datasetY())
      ggplotly(p)
    })
  ########################################
  output$plotSpeed <- renderPlotly(
    { 
      p <- linear_graph_speed(datasetY())
      ggplotly(p)
    })
  ########################################
  output$plotHex <- renderPlotly(
    { 
      p <- call_hex_map(datasetY()) 
      ggplotly(p)
    })
  ########################################
  output$text1 <- renderText(
    { 
      nBalls <- length(datasetY()$df$rX)
      paste("Number of Balls: ", nBalls)
    })
  ########################################
  output$textEnds <- renderText(
    { 
      paste("Current Bowling End - ", toupper(datasetY()$ends))
    })
  ########################################
  output$aHeight <- renderText(
    { 
      hMean <- paste(round(mean(datasetY()$df$sZ)*1000,1)," mm")
      paste("Av Bounce Height: ", hMean)
    })
  ########################################
  output$hMax <- renderText(
    { 
      hMax <- paste(round(max(datasetY()$df$sZ)*1000,1)," mm")
      paste("Max Bounce Height: ", hMax)
    })
  ########################################
  output$hMin <- renderText(
    { 
      hMin <- paste(round(min(datasetY()$df$sZ)*1000,1)," mm")
      paste("Min Bounce Height: ", hMin)
    })
  ########################################
  ########################################
  output$aSpeed <- renderText(
    { 
      hMean <- paste(round(mean(datasetY()$df$sSpeed),1),"km/h")
      paste("Av Speed: ", hMean)
    })
  ########################################
  output$sMax <- renderText(
    { 
      hMax <- paste(round(max(datasetY()$df$sSpeed),2),"km/h")
      paste("Max Speed: ", hMax)
    })
  ########################################
  output$sMin <- renderText(
    { 
      hMin <- paste(round(min(datasetY()$df$sSpeed),2)," km/h")
      paste("Min Speed: ", hMin)
    })
  ########################################
  output$text2 <- renderText(
    { 
      if(input$bounceH < 50)
      {
        bHeight <- "LOW"
        pCent <- datasetY()$percentile
      }
      else if (input$bounceH > 50)
      {
        bHeight <- "HIGH"
        pCent <- (100 - datasetY()$percentile )
      }
      else
      {
        bHeight <- "ALL"
        pCent <- datasetY()$percentile 
      }
      paste("Bounce Height - ",bHeight, " - ", pCent, "%")
    })
  ########################################
  ########################################
  output$SpeedDat <- renderText(
    { 
      if(input$speedSlow < 50)
      {
        bHeight <- "SLOWER"
        pCent <- datasetY()$speedPercent
      }
      else if (input$speedSlow > 50)
      {
        bHeight <- "FASTER"
        pCent <- (100 - datasetY()$speedPercent)
      }
      else
      {
        bHeight <- "ALL"
        pCent <- datasetY()$speedPercent 
      }
      paste("Speed - ",bHeight, " - ", pCent, "%")
    })
  ########################################
}
####################################################################
# Run the application 
shinyApp(ui = ui, server = server)
####################################################################
####################################################################
