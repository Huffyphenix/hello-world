ui <- navbarPage(
    "11111?",
    tabPanel("One", icon=icon("home")),
    tabPanel("Two"),
    navbarMenu(
      "Three",
      tabPanel("Four"),
      tabPanel("Five")
    ),
    navlistPanel(
      "22222", widths = c(3, 9),
      tabPanel("One", icon=icon("home")),
      tabPanel("Two"),
      navbarMenu(
        "Three",
        tabPanel("Four"),
        tabPanel("Five")
      )
    ),
    h4("33333"),
    tabsetPanel(
      "pills",
      tabPanel("One", icon=icon("home")),
      tabPanel("Two"),
      navbarMenu(
        "Three",
        tabPanel("Four"),
        tabPanel("Five")
      )
    ),
    tags$head(
      tags$style(".tab-content .tab-content {border: 1px solid gray; min-height:200px;}")
    )
  )

server = function(session, input, output) {
  }
