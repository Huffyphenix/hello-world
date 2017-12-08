library(DT)
library(shiny)
library(shinydashboard)
if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    DT::dataTableOutput('tmp')
    
  )
  
  server <- function(input, output) {
    output$tmp <- DT::renderDataTable({
      m = matrix(runif(1000 * 20), ncol = 20, dimnames = list(NULL, letters[1:20]))
      m = cbind(id = seq_len(nrow(m)), round(m, 2))
      datatable(m, extensions = c('Buttons','Scroller'), options = list(
        buttons = c('copy', 'excel'),
        dom = 'pBfrti',
        pageLength = -1,
        lengthMenu = c(5, 10, 15, 'ALL'),
        rownames= FALSE,
        scrollX = TRUE,
        fixedHeader = TRUE,
        fixedColumns = TRUE ,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE
      ))
    })
  }
  
  shinyApp(ui, server)
}