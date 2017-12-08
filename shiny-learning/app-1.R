library(shiny)

source("cancer.R")
source("cancerType.R")
source("plot.R")
ui<-fluidPage(#tabName = "tcga_cnv", align = "center",
  shinyjs::useShinyjs(),
  
  ## SNV message ----
  fluidRow(style="width:80%;",
           HTML("<div class='section'>
                <div class='container'>
                <div class='row'>
                <div class='col-md-12'>
                <h1 class='text-success text-left'>
                <i class='fa fa-angle-double-right  fa-fw'></i>CNV
                <font color='#777777'>
                <span style='font-size: 23.3999996185303px; line-height: 23.3999996185303px;'>Copy Number variation</span>
                </font>
                </h1>
                <hr>
                <p class='lead'>TCGA CNV data will be used to give you a visualization of you gene set for seleted cancer types.
                <br>GSAC offers different types of graphic layout (CNV Pie distribution, Hete CNV, Homo CNV, CNV Bar distribution, Oncostrip, see details in <code>help page</code> below.) for you to visualize the CNV of your gene set for your seleted cancer types.</p>
                </div>
                </div>
                </div>
                </div>")
           ),
  ## Hlep message including in tcga_cnv_help.ui----
  # source(file.path(config$ui,"tcga_cnv_help.R"))[1],
  
  shiny::tags$br(),
  shiny::tags$hr(width="85%"),
  
  # cancer type selection and result output---------------------------------------------------
  # fluidRow(
  #   # cancer type selection----
  #   column(width = 10,
  #          offset = 1,
  #          shiny::tags$br(),
  #          # use function cancerInput in function_ui.R through namespace "cnv".
  #          cancerTypeInput("cnv")
  #   ),
  cancerTypeInput("cnv"),
  # Tabset Panel
  # output plot -------------------------------------------------------------
  fluidRow(
    column(width = 10,
           offset = 1,
           shiny::tags$br(),
           shinydashboard::tabBox(width = 12, title = "PLOT",
                                  PlotInput(id="cnv_pie",label = "CNV Pie distribution",width = "700px", height = "100%"),
                                  PlotInput(id="cnv_hete",label = "Hete CNV profile",width = "700px", height = "100%")
           )
    )
  )#,
  # load footer
  # source(file.path(config$ui, "footer.R"))[1]
  # 
  # 
  #         
           ) # close tab


server <- function(input,output){
  callModule(cancer,"test")
  callModule(cancerType,"cnv")
  callModule(Plot,"cnv_pie")
}
shinyApp(ui,server)
