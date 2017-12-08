ui <- navbarPage(
    "导航条风格",
    tabPanel("One", icon=icon("home")),
    tabPanel("Two"),
    navbarMenu(
      "Three",
      tabPanel("Four"),
      tabPanel("Five")
    ),
    navlistPanel(
      "导航面板风格", widths = c(3, 9),
      tabPanel("One", icon=icon("home")),
      tabPanel("Two"),
      navbarMenu(
        "Three",
        tabPanel("Four"),
        tabPanel("Five")
      )
    ),
    h4("标签页风格"),
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
