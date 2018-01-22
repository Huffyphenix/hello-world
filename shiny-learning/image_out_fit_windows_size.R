ui <- pageWithSidebar(
  headerPanel("renderImage example"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000,  value = 500)
  ),
  mainPanel(
    # Use imageOutput to place the image on the page
    imageOutput("myImage")
  )
)

server <- function(input, output, session) {
  
  # A dynamically-sized plot
  output$myImage <- renderImage({
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_myImage_width
    height <- session$clientData$output_myImage_height
    
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    
    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')
    
    # Generate the image file
    png(outfile, width = width*pixelratio, height = height*pixelratio,
        res = 72*pixelratio)
    hist(rnorm(input$obs))
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  # This code reimplements many of the features of `renderPlot()`.
  # The effect of this code is very similar to:
  # renderPlot({
  #   hist(rnorm(input$obs))
  # })
}
shinyApp(ui=ui,server=server)
