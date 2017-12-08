cancerInput <- function(id){
  ns<-NS(id)
  tagList(
    checkboxGroupInput(ns("cancer"),"Cancer type",
                       choices = list("LUAD" = "LUAD",
                                      "LUSC" = "LUSC",
                                      "KIRC" = "KIRC")),
    checkboxGroupInput(ns("cancer1"),"Cancer type 2",
                       choices = list("LUAD2" = "LUAD",
                                      "LUSC2" = "LUSC",
                                      "KIRC2" = "KIRC")),
    textOutput(ns("cancer"))
  )
}

cancer<-function(input,output,session){
  output$cancer<-renderText(
    c(input$cancer,input$cancer1)
  )
}