library(shiny)
library(shinythemes)
library(DT)

ui <- navbarPage(id = "navbar",
  theme = shinytheme("flatly"),
  title = 'ABioTrans',
  tabPanel('Home',
           # useShinyjs(),
           sidebarPanel(
             radioButtons('file_type',"Choose File Type",
                          c('Raw file (read count)'='raw','Normalised file'='norm')),
             conditionalPanel(
               condition = "input.file_type=='raw'",  # raw
               p("Example ",a("here", href="https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_raw.png")),  # ADD EXAMPLE
               fileInput('file1','Choose Raw Counts'),
               # radioButtons('norm_method',"Normalisation method",
               #              c('RPKM','FPKM','TPM')),
               p("Example ",a("here", href = "https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_gene_length.png")),  # ADD EXAMPLE
               fileInput('length1','Choose Gene Length'), #gene id + length
               p("Example ",a("here", href = "https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_negative_control_genes.png")),  # ADD EXAMPLE
               fileInput('spikes1','Choose Negative Control Genes')
               # helpText("* Format requirement: CSV file. The first column contains gene names; the read counts of each genotype (conditions: wildtype, mutants, replicates, etc.) are in the following columns.Each genotype column should have a column name. ")
             ),
             conditionalPanel(
               condition = "input.file_type=='norm'", # normalized
               p("Example ",a("here", href = "https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_normalised.png")),  # ADD EXAMPLE
               fileInput('file2','Choose Normalized Expression')
               # helpText("* Format requirement: CSV file. Gene names in rows and genotypes in columns, following the usual format of files deposited in the GEO database.")
             ),
             p("Example ",a("here", href="https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_metadata.png")),  # ADD EXAMPLE
             fileInput('metafile1','Choose Meta Data File'),
             actionButton("submit_input","Submit")
           ),
           mainPanel(
             h3('Welcome to ABioTrans --'),
             h3('A Biostatistical tool for Transcriptomics Analysis'),
             img(src="Abiotrans-logo.png",
                 width = 570,height = 370)
           )
  ),
  tabPanel('Preprocessing',
           sidebarPanel(
             h4("Filtering"),
             splitLayout(
               numericInput("min_val","Min. value", min=0.1,step=0.1,value=1.0),
               numericInput("min_col","Min. columns", min=1, value=2)
             ),
             conditionalPanel(
               condition = "input.file_type=='raw'",
               radioButtons('norm_method',"Normalisation method",
                            c("None (Black)"="None",
                              'RPKM (Blue)'='RPKM','FPKM (Dark cyan)'='FPKM',
                              'TPM (Dark green)'='TPM',
                              "RUV (Brown)"='RUV'))
             ),
             actionButton("submit_preprocessing","Submit"),
             conditionalPanel(
               condition = "input.preprocessing_tabs == 'Data table' ",
               br(),
               br(),
               downloadButton("download_norm_data", "Download table (csv)")
             )
           ),
           mainPanel(
             tabsetPanel(type = "tabs",id="preprocessing_tabs",
                         tabPanel("RLE plot",
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   div(img(src="load.gif",width=240,height=180),
                                                       h4("Processing ... Please wait"),style="text-align: center;")
                                  ), 
                                  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                   plotOutput("RLE.plot2")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.file_type=='raw'",
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     div(img(src="load.gif",width=240,height=180),
                                                         h4("Processing ... Please wait"),style="text-align: center;")
                                    ), 
                                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                     plotOutput("RLE.plot")
                                    )
                                  )
                         ),
                         tabPanel("Data table",
                                  h3("Normalized data"),
                                  DT::dataTableOutput("norm_table")
                         ),
                         tabPanel("Description table",
                                  h3("Data description"),
                                  DT::dataTableOutput("meta_table")
                         )
             )
           )
  )
)