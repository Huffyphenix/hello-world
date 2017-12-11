library(shiny)

source("cancer.R")
source("cancerType.R")
source("plot.R")

ui<-fluidPage(
  titlePanel("TTTTTEST"),
  tabsetPanel(id = "content",
    tabPanel(title = "cnv_pie",PlotInput("cnv_pie",width="100%",height="420px")),
    tabPanel(title = "cnv_pie1",PlotInput("cnv_pie1",width="100%",height="420px")),
    tabPanel(title = "cnv_pie1",PlotInput("cnv_pie2",width="100%",height="420px"))
  )
  # plotOutput("plot"),
  # sliderInput("num",label = "Select size of number",min=10,max = 100,value = 50)
)

server <- function(input,output){
  callModule(Plot,"cnv_pie")
  callModule(Plot,"cnv_pie1")
  callModule(Plot,"cnv_pie2")
  # x <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  # })
  # 
  # y <- reactive({
  #   input$num %>% as.numeric %>% rnorm()
  # })
  # 
  # output$plot <-renderPlot({
  #   plot(x(),y())
  # }) 
}
shinyApp(ui,server)
