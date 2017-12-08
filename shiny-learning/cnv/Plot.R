# call in ui by PlotInput("cnv_pie",..) OR  PlotInput("cnv_bar",..)
# call in ser by callModule(Plot,"cnv_pie",...) OR ...
PlotInput <- function(id,width,height){
  ns<-NS(id)
  
  tagList(
    plotOutput(ns("plot")),
    hr()
  #sliderInput(ns("num"),label = "Select size of number",min=10,max = 100,value = 50)
  )
  
}

Plot <- function(input,output,session){
  #size <- reactive(as.numeric(input$num))
  # x <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  #   })
  # y <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  # })
  x<-rnorm(50)
  y<-nrow(50)
  
  output$plot <-renderPlot({
    plot(x,y)
  }) # fun argument decide what function will be called.

  
}