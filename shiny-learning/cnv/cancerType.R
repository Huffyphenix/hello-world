cancerTypeInput<-function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # cancer type selection----
      column(width = 10,
             offset = 1,
             shiny::tags$br(),
    shiny::tags$h3("Cancer Type Selection",class="text-success"),
    shiny::tags$br(),
    
    shinydashboard::tabBox(width = 12, title = "Tissue",
                           tabPanel("Kidney",
                                    shiny::tags$h4("Kidney",class="text-success"),
                                    checkboxGroupInput(inputId = ns("Kidney"),label = NULL,inline = TRUE,
                                                       choices = list("Kidney Chromophobe(KICH)"="KICH",
                                                                      "Kidney Renal Clear Cell Carcinoma(KIRC)"="KIRC",
                                                                      "Kidney Renal Papillary Cell Carcinoma(KIRP)"="KIRP"))),
                           tabPanel("Adrenal Gland",
                                    checkboxGroupInput(inputId = ns("Adrenal_Gland"),label = NULL,inline = TRUE,
                                                       choices = list("Adrenocortical Carcinoma(ACC)"="ACC",
                                                                      "Pheochromocytoma and Paraganglioma(PCPG)"="PCPG"))),
                           tabPanel("Brain",
                                    checkboxGroupInput(inputId = ns("Brain"),label = NULL,inline = TRUE,
                                                       choices = list("Glioblastoma Multiforme(GBM)"="GBM",
                                                                      "Brain Lower Grade Glioma(LGG)"="LGG"))),
                           tabPanel("Colorectal",
                                    checkboxGroupInput(inputId = ns("Colorectal"),label = NULL,inline = TRUE,
                                                       choices = list("Colon Adenocarcinoma(COAD)"="COAD",
                                                                      "Rectum Adenocarcinoma(READ)"="READ"))),
                           tabPanel("Lung",
                                    checkboxGroupInput(inputId = ns("Lung"),label = NULL,inline = TRUE,
                                                       choices = list("Lung Adenocarcinoma(LUAD)"="LUAD",
                                                                      "Lung Squamous Cell Carcinoma(LUSC)"="LUSC"))),
                           tabPanel("Uterus",
                                    checkboxGroupInput(inputId = ns("Uterus"),label = NULL,inline = TRUE,
                                                       choices = list("Uterine Corpus Endometrial Carcinoma(UCEC)"="UCEC",
                                                                      "Uterine Carcinosarcoma(UCS)"="UCS"))),
                           tabPanel("Bile Duct",
                                    checkboxGroupInput(inputId = ns("Bile_Duct"),label = NULL,inline = TRUE,
                                                       choices = list("Bladder Urothelial Carcinoma(BLCA)"="BLCA"))),
                           tabPanel("Bone Marrow",
                                    checkboxGroupInput(inputId = ns("Bone_Marrow"),label = NULL,inline = TRUE,
                                                       choices = list("Acute Myeloid Leukemia(LAML)"="LAML"))),
                           tabPanel("Breast",
                                    checkboxGroupInput(inputId = ns("Breast"),label = NULL,inline = TRUE,
                                                       choices = list("Breast Invasive Carcinoma(BRCA)"="BRCA"))),
                           tabPanel("Cervix",
                                    checkboxGroupInput(inputId = ns("Cervix"),label = NULL,inline = TRUE,
                                                       choices = list("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma(CESC)"="CESC"))),
                           tabPanel("Other tissues",
                                    checkboxGroupInput(inputId = ns("other_tissue"),label = NULL,inline = TRUE,
                                                       choices = list("Lymphoid Neoplasm Diffuse Large B-cell Lymphoma(DLBC)"="DLBC",
                                                                      "Esophageal Carcinoma(ESCA)"="ESCA",
                                                                      "Stomach Adenocarcinoma(STAD)"="STAD",
                                                                      "Head and Neck Squamous Cell Carcinoma(HNSC)"="HNSC",
                                                                      "Liver Hepatocellular Carcinoma(LIHC)"="LIHC",
                                                                      "Mesothelioma(MESO)"="MESO",
                                                                      "Ovarian Serous Cystadenocarcinoma(OV)"="OV",
                                                                      "Pancreatic Adenocarcinoma(PAAD)"="PAAD",
                                                                      "Prostate Adenocarcinoma(PRAD)"="PRAD",
                                                                      "Sarcoma(SARC)"="SARC",
                                                                      "Skin Cutaneous Melanoma(SKCM)"="SKCM",
                                                                      "Testicular Germ Cell Tumors(TGCT)"="TGCT",
                                                                      "Thyroid Carcinoma(THCA)"="THCA",
                                                                      "Thymoma(THYM)"="THYM",
                                                                      "Uveal Melanoma(UVM)"="UVM")))
    )),
    shiny::tags$hr(width="85%"),
    # Selected cancer show ----
    shiny::tags$h3("Cancer Type Check",class="text-success"),
    shiny::tags$h4("The cancers you selected: ",
                   textOutput(ns("selected_cancer")),
                   " Confirm and start analysis by click Submit!"),
    # Confirm and submit ----
    column(width = 2,offset = 5,
           actionButton(ns("submit"), label ="Submit!",icon = icon("check"))
    )
    )
  )
}


cancerTypetest <- function(input,output, session){
  output$selected_cancer <- renderText(
    c(input$Kidney,input$Adrenal_Gland,input$Brain,input$Colorectal,
      input$Lung,input$Uterus,input$Bile_Duct,input$Bone_Marrow,input$Breast,
      input$Cervix,input$other_tissue)
  )
}