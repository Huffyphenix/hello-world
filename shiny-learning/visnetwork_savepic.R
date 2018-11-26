library(shiny)
library(visNetwork)

# network data
nodes <- data.frame(id = 1:100, label = paste("Label", 1:100),
                    group = sample(LETTERS[1:3], 100, replace = TRUE))

edges <- data.frame(from = trunc(runif(100)*(100-1))+1,
                    to = trunc(runif(100)*(100-1))+1)

# Define UI
ui <- shinyUI(fluidPage(
  visNetworkOutput("network"), 
  actionButton("store_position", "Store positions !"),
  downloadLink('downloadNetwork', 'Download network')
))

# Define server
server <- shinyServer(function(input, output) {
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE) %>%
      # visPhysics(enabled = FALSE) %>%
      visEdges(smooth = FALSE)
  })
  
  # get position info
  observeEvent(input$store_position, {
    visNetworkProxy("network") %>% visGetPositions()
  })
  
  # format positions
  nodes_positions <- reactive({
    positions <- input$network_positions
    if(!is.null(positions)){
      nodes_positions <- do.call("rbind", lapply(positions, function(x){ data.frame(x = x$x, y = x$y)}))
      nodes_positions$id <- names(positions)
      nodes_positions
    } else {
      NULL
    }
  })
  
  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste('network-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      print(con)
      nodes_positions <- nodes_positions()
      if(!is.null(nodes_positions)){
        nodes_save <- merge(nodes, nodes_positions, by = "id", all = T)
      } else  {
        nodes_save <- nodes
      }
      
      visNetwork(nodes = nodes_save, edges = edges, height = "800px") %>%
        visOptions(highlightNearest = TRUE) %>% visExport() %>%
        visPhysics(enabled = FALSE) %>%
        visEdges(smooth = FALSE) %>% 
        #visSave(file=file)
        htmlwidgets::saveWidget(con)
    }
  )
})

# Run the application 
shinyApp(ui = ui, server = server)
