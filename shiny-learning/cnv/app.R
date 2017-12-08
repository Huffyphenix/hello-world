# sourced by 'ui.R'
# save as 'tcga_cnv_ui.R'
# ui elements 'tcga_cnv' sub tab of 'tcga' tab
source("cancerType.R")
source("Plot.R")
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
                 shinydashboard::tabBox(id = "PLOT",title = "PLOT",width = 12,
                                        tabPanel(title="CNV Pie distribution",PlotInput(id="cnv_pie")),
                                        tabPanel(title= "Hete CNV profile",PlotInput(id="cnv_hete")),
                                        tabPanel(title="Homo CNV profile",PlotInput(id="cnv_homo")),
                                        tabPanel(title="CNV Bar distribution",PlotInput("cnv_bar")),
                                        tabPanel(title="CNV oncostrip",PlotInput("cnv_oncostrip")),
                                        tabPanel(title="Exclusive CNV",PlotInput("cnv_exclusive"))
                 )
          )
        )#,
        # load footer
        # source(file.path(config$ui, "footer.R"))[1]
        ) # close tab

server<- function(input,output,session){
  # data input --------------------------------------------------------------
  # ? global data can't load here.
  
  #  get cancer type --------------------------------------------------------
  callModule(cancerTypetest,"cnv")
  callModule(Plot,"cnv_pie")
  callModule(Plot,"cnv_hete")
  callModule(Plot,"cnv_homo")
  callModule(Plot,"cnv_bar")
  callModule(Plot,"cnv_oncostrip")
  callModule(Plot,"cnv_exclusive")

}

shinyApp(ui, server)
