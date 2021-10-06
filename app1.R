#######################################################################
#######################################################################
#install.packages("shiny")
#install.packages("visNetwork")
#install.packages("readxl")
#install.packages("shinythemes")
#######################################################################
library(shiny)
library(visNetwork)
library(readxl)
library(shinythemes)
#######################################################################
fontSize <<- 14
shapeValue <<- "circle"
#######################################################################
#options(shiny.trace=TRUE)
options(shiny.fullstacktrace=TRUE)
#options(shiny.stacktraceoffset=TRUE)
#showReactLog(time = TRUE)
#######################################################################
#######################################################################
ui <- fluidPage(theme = shinytheme("united"),
                verbatimTextOutput("shiny_return"),
                  mainPanel(visNetworkOutput("network_proxy_nodes", height = "100%", width="100%"))
                )

#######################################################################
#######################################################################
server <- function(input, output) 
{
  
  output$network_proxy_nodes <- renderVisNetwork(
    {
    edges <- read_excel("data.xlsx", sheet = "edges")
    nodes <- read_excel("data.xlsx", sheet = "nodes")
    visNetwork(nodes, edges, width = "100%", height = "100%") %>%
      visEvents(select = "function(nodes){
                  Shiny.onInputChange('click', nodes[0]);
                  ;}") %>%
      visOptions(selectedBy = "group") %>%
      visInteraction(hover = T, hoverConnectedEdges = T, selectable = T, selectConnectedEdges = T, tooltipDelay = 300, tooltipStay = 300,tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;
                     font-family: Arial;font-size:18px;font-color:purple; border: black; background-color: white;')%>%
      visEdges(arrows = "to", hoverWidth = 8 , width = 2,selectionWidth = , 8, shadow = list(enabled = TRUE, size = 5))%>%
      visNodes(shadow = list(enabled = T, size = 5), 
               font = list(face = "Arial" , 
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
      visPhysics(solver = "forceAtlas2Based", 
                 minVelocity = 0.75, 
                 forceAtlas2Based = list(gravitationalConstant = -50, 
                                         springLength = 70, 
                                         springConstant = 0.995, 
                                         damping = 0.48, 
                                         avoidOverlap =0.2))%>% 
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
  
  observe({
    gr <- input$network_proxy_nodes_selectedBy
    nodes <- read_excel("data.xlsx", sheet = "nodes")
    id <- nodes$id[nodes$group%in%gr]
    visNetworkProxy("network_proxy_nodes") %>%
      visFit(nodes = id, animation = list(duration = 500, easingFunction = "easeInOutQuad"))
  })
  
  output$shiny_return <- renderPrint({
    visNetworkProxy("network_proxy_nodes") %>%
      print(input$click)%>%
      visNearestNodes(target = input$click)
  })
  
  #######################################################################
  }
#######################################################################
shinyApp(ui = ui, server = server)
#######################################################################
#######################################################################

