## learn from page: http://rstudio.github.io/shinydashboard/structure.html#boxes
## library package ----
library(shinydashboard)
library(shiny)

## dashborad header ----
header<-shinydashboard::dashboardHeader()


# dashboard sider bar -----------------------------------------------------
sidebar<-shinydashboard::dashboardSidebar()

# dashboard body ----------------------------------------------------------
body<-shinydashboard::dashboardBody(
  fluidRow(
    column(width=4,
           shinydashboard::box(title = "Box 1",
                               status = "primary",
                               checkboxGroupInput(inputId = "box1",label = "Box 1 Input",
                                                  inline = TRUE,
                                                  choices = list("Item 1"="1",
                                                                 "Item 2"="2")
                                                  )
                               ),
           shinydashboard::box(title = "Box 2",
                               status = "success",
                               solidHeader = TRUE,
                               footer = "Select over",
                               background = "teal",
                               width = 8,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               checkboxGroupInput(inputId = "box2",label = "Box 2 Input",
                                                  choices = list("Item 1"="1",
                                                                 "Item 2"="2")
                               )
           )
           ),
    column(width=8,
           shinydashboard::tabBox(title = "TabBox 1",
                                  id="tabbox1",
                                  height = "250px",
                                  tabPanel("Tab1","First tab content"),
                                  tabPanel("Tab2","Second tab content")
                                  ),
           shinydashboard::tabBox(title = "TabBox 2",
                                  id="tbbox2",
                                  height = "400px",
                                  side = "right",
                                  tabPanel("Tab1","First tab content"),
                                  tabPanel("Tab2","Second tab content"),
                                  tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                                  )
           ),
    column(width = 12,
           shinydashboard::infoBox(title = "Info box 1",
                                   value = "Info box 1",
                                   subtitle = "Info box 1-sub",
                                   icon = icon("credit-card"),
                                   color = "aqua",
                                   width = 4,
                                   href = NULL,
                                   fill = FALSE
                                  ),
           infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
           infoBoxOutput("progressBox2"), # use server to add a info box on the page.
           infoBoxOutput("approvalBox2")
           )
  )
)


## ui constructionn ----
shinyUI(dashboardPage( 
  title = "GSCA - Gene Set Cancer Analysis",
  header = header,
  sidebar = sidebar,
  body = body )) -> ui

## server -----
server<-function(input,output,server){
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
}
## test ----
shinyApp(ui = ui, server = server)
