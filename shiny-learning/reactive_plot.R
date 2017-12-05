####### README#######
####### Reactive plot#######
#Use R to generate a reactive plot
####### README#######


library(shiny)

ui <- fluidPage(
  fluidRow(
  plotOutput(outputId = "pl", click = 'pl_click', dblclick = 'pl_dclick', hover = 'pl_hover', brush = 'pl_brush'),
  column(3, textOutput('dtclk', container=pre)),
  column(3, textOutput('dtdcl', container=pre)),
  column(3, textOutput('dthov', container=pre)),
  column(3, textOutput('dtbsh', container=pre))
  )
)

server <- function(input, output, session) {
  # set a intermediate variable to avoid page refresh of brush/hover, see http://blog.csdn.net/u014801157/article/details/48498233 for details.
  cords<-reactiveValues(xy=NULL) 
  observeEvent(input$pl_click,{
    if(!is.null(input$pl_click))
      cords$xy <- input$pl_click[c('x','y')]
  })
  output$pl <- renderPlot({
    plot(1:10)
    xy <- cords$xy
    if(!is.null(xy)) text(xy, labels=paste(as.list(xy),collapse=', '))
  })
  output$dtclk <- renderPrint({
    str(input$pl_click)
  })
  output$dtdcl <- renderPrint({
    str(input$pl_dclick)
  })
  output$dthov <- renderPrint({
    str(input$pl_hover)
  })
  output$dtbsh <- renderPrint({
    str(input$pl_brush)
  })
}

shinyApp(ui, server)