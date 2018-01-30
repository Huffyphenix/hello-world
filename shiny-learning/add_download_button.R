library(shiny)
ui<-pageWithSidebar(
  headerPanel('Downloading Data'),
  shinyWidgets::dropdownButton(
    
    tags$h3("Download Options"),
    
    numericInput(inputId = "width",
                label = 'Width',
                value = 4,
                min = 1,
                max = 10),
    
    numericInput(inputId = "height",
                 label = 'height',
                 value = 6,
                 min = 3,
                 max = 20),
    selectInput(inputId = 'pictype',
                label = "Selcet format for your picture",
                choices = list("PDF"="pdf","PNG"="png"),
                icon = icon("check"), 
                bigger = TRUE, status = "info", 
                animation = "jelly"),
    downloadButton(outputId = "picdownload",
                 label = "Download"
                ),
    circle = TRUE, status = "danger",
    icon = icon("gear"), width = "300px",
    right = TRUE,
    tooltip = shinyWidgets::tooltipOptions(title = "Click to download picture !")
  ),
  mainPanel(
    plotOutput('plot')
  )
)

library(ggplot2)
server <- function(input, output) {
  plotInput <- reactive({
    df <- cars
    p <-ggplot(df, aes_string(x=names(df)[1], y=names(df)[2])) +
      geom_point()
  })
  
  output$plot <- renderPlot({
    print(plotInput())
  })
  
  # output$downloadData <- downloadHandler(
  #   filename = function() { paste(input$dataset, '.csv', sep='') },
  #   content = function(file) {
  #     write.csv(datatasetInput(), file)
  #   }
  # )
  output$picdownload <- downloadHandler(
    filename = function() { paste("picture", '.',input$pictype, sep='') },
    content = function(file) {
      ggsave(file,plotInput(),device = input$pictype,width = input$width,height = input$height)
    }
  )
}
shinyApp(ui=ui,server = server)
