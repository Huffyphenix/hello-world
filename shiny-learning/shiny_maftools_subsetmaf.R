mc3_pass<-readr::read_rds("C:\\Users\\MyPC\\Desktop\\test_data.rds.gz")
mc3_pass_1<-readr::read_rds("C:\\Users\\MyPC\\Desktop\\test_data.rds.gz")
library(maftools)
getClinicalData(mc3_pass)

library(shiny)
Lung_choice <- list(
  "Lung Adenocarcinoma(LUAD)" = "LUAD",
  "Lung Squamous Cell Carcinoma(LUSC)" = "LUSC",
  "BRCA"="BRCA",
  "SKCM"="SKCM"
)

ui <- fluidPage(
  checkboxGroupInput(inputId = "select",label = "Select cancers",
                     choices = Lung_choice),
  actionButton("go",label = "Go"),
  shiny::textOutput(outputId = "table")
)

all_cancer_type<-c("LUAD","LUSC","BRCA","SKCM")
server <- function(input, output, session) {
  ct <- reactive({
    input$select
  })
  observeEvent(input$go,{
    print(ct())
    print(class(ct()))
    # ct_2 <- as.character(ct())
    # print(ct_2)
    # ct_3 <- intersect(all_cancer_type,ct_2)
    # print(ct_3)
    # query = as.expression("cancer_types %in% ct()") # Error in ct: could not find function "ct"
    # query = as.expression("cancer_types %in% ct_2") # Error in eval: object 'ct_2' not found
    InpSel <-  paste0(input$select, collapse = "','")
    print(InpSel)
    query =  as.expression(paste0("cancer_types %in% c('",InpSel,"')")) # Error in eval: object 'input' not found
    print(query)
    maftools::subsetMaf(mc3_pass,query=query, mafObj = T) -> gene_list_maf # subsetMaf filter
    print("maf done!")
    gene_list_maf %>%
      maftools::getClinicalData() %>%
      dplyr::select(cancer_types) %>%
      unique() %>% t() %>%
      as.vector()->.x# check the filter results
    print(".x done!")
    output$table <- renderText({
      return(.x)
    })
    print("out done!")
    print(gene_list_maf %>%
            maftools::getClinicalData())
  })
}
shinyApp(ui, server)
