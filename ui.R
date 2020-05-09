library(shiny)
library(shinythemes)
library(DT)
library(plotly)

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
  ),
  tabPanel('    Scatter    ',
           sidebarPanel(
             selectInput(inputId = 'scatter.x',label = 'X-axis',choices = ""),
             selectInput(inputId = 'scatter.y',label = 'Y-axis',choices = ""),
             radioButtons('trans',"Transformation:",
                          c('None','Natural log','log2','log10')),
             downloadButton("downloadscatter", "Download as PDF"),
             h6('Download all pairs of samples in one PDF (this may take some time to run) :'),
             downloadButton("downloadscatter_collage","Download collage")),
           mainPanel(
             h3('Heatscatter'),
             plotOutput('scatter.plot')
           )),
           tabPanel('Distribution Fit',
           sidebarPanel(
             conditionalPanel(
               condition= "input.dist_tabs=='Distribution Fit'",
               selectInput(inputId = 'dist.var',label = 'Choose a column',choices = colnames('dataset')),
               checkboxGroupInput("distributions", "Distributions:",
                                  choices = c("Log-normal","Log-logistic","Pareto","Burr","Weibull","Gamma"),selected = c("Log-normal","Pareto")),
               radioButtons('dist_zoom',"Zoom to see fit",c('slider','text input')),
               conditionalPanel(
                 condition = "input.dist_zoom=='slider'",
                 sliderInput("dist_range", "Range:",
                             min = 0.1, max = 1000,step=1,
                             value = c(0.1,1000))
               ),
               conditionalPanel(
                 condition = "input.dist_zoom=='text input'",
                 textOutput('dist_range_allowed'),
                 numericInput('dist_range_min',"min",value=0.1,min=0.1,max=1000),
                 numericInput('dist_range_max',"max",value=1000,min=0.1,max=1000)
               ),
               downloadButton("downloaddist", "Download as PDF")
             ),
             conditionalPanel(
               condition = "input.dist_tabs=='AIC table'",
               downloadButton("downloaddistaic", "Download as CSV")
             )
           ),
           mainPanel(
             tabsetPanel(type = "tabs",id="dist_tabs",
                         tabPanel("Distribution Fit", plotOutput("dist.plot")),
                         tabPanel("AIC table",
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   div(img(src="load.gif",width=240,height=180),
                                                       h4("Processing ... Please wait"),style="text-align: center;")
                                  ), 
                                  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                   div(tableOutput('dist.aic'), style = "font-size:80%")
                                  ))
             )
           )),
             tabPanel('  Correlation  ',
           sidebarPanel(
             radioButtons('cor_method',"Method:",
                          c('Pearson correlation','Spearman correlation')),
             conditionalPanel(
               condition = "input.cor_tabs == 'Correlation heatmap'",
               downloadButton("downloadcorrplot", "Download as PDF")
             ),
             conditionalPanel(
               condition = "input.cor_tabs == 'Correlation plot'",
               downloadButton("downloadcorrplot2", "Download as PDF")
             ),
             conditionalPanel(
               condition = "input.cor_tabs == 'Correlation matrix'",
               downloadButton("downloadcorrmat","Download as CSV")
             )
           ),
           mainPanel(
             conditionalPanel(
               condition = "input.cor_method=='Pearson correlation'",
               h3('Pearson correlation')
             ),
             conditionalPanel(
               condition = "input.cor_method=='Spearman correlation'",
               h3('Spearman correlation')
             ),
             tabsetPanel(type = "tabs",id="cor_tabs",
                         tabPanel("Correlation heatmap", plotOutput('corr.plot')),
                         tabPanel("Correlation plot", plotOutput('corr.plot2')),
                         tabPanel("Correlation matrix", div(tableOutput('corr.matrix'), style = "font-size:80%"))
             )
           )
  ),
  tabPanel('PCA',
           sidebarPanel(
             conditionalPanel(
               condition = "input.pca_tabs == 'PCA-2D plot'",
               selectInput(inputId = 'pca.x',label = 'X-axis',choices = ""),
               selectInput(inputId = 'pca.y',label = 'Y-axis',choices = "")
             ),
             selectInput(inputId = 'gene_size',label = 'Gene sample size',choices = ""),
             radioButtons('gene_order',"Gene sample order (wrt column 1)",
                          c('Descending (highest to lowest)'='Descending','Ascending (lowest to highest)'='Ascending','Random')),
             conditionalPanel(
               condition = "input.pca_tabs == 'PCA-2D plot' || input.pca_tabs == 'PCA-3D plot'",
               checkboxInput('pca_cluster',strong('Kmeans clustering on columns'),FALSE),
               conditionalPanel(
                 condition = "input.pca_cluster == true",
                 sliderInput("pca_cluster_num","Number of clusters:",value=1,min=1,max=1,step=1),
                 checkboxInput('pca_text',strong('Display sample name'),FALSE)
               )
             ),
             conditionalPanel(
               condition = "input.gene_order=='Random'",
               helpText('* Click multiple times to resample'),
               actionButton('pca_refresh',"Resample",style="background-color: #337ab7;border-color:#337ab7"),
               br(),br()
             ),
             conditionalPanel(
               condition = "input.pca_tabs == 'PCA variance'",
               downloadButton("downloadpcavar", "Download as PNG")
             ),
             conditionalPanel(
               condition = "input.pca_tabs == 'PCA-2D plot'",
               downloadButton("downloadpca2d","Download as PNG")
             ),
             conditionalPanel(
               condition = "input.pca_tabs == 'PCA-3D plot'",
               downloadButton("downloadpca3d","Download as PNG")
             )
           ),
           mainPanel(
             tabsetPanel(type = "tabs",id="pca_tabs",
                         tabPanel("PCA variance", plotlyOutput("pcavar.plot")),
                         tabPanel("PCA-2D plot", plotlyOutput("pca2d.plot")),
                         tabPanel("PCA-3D plot",plotlyOutput("pca3d.plot"))
             )
           )),
           tabPanel("DE Analysis",
           # useShinyjs(),
           sidebarPanel(
             radioButtons("n_rep","Replicates?",choices=c("Multiple"=1,"Single"=0)),
             conditionalPanel(
               condition="input.n_rep=='1'",
               radioButtons("de_method1","DE Method",choices=c("EdgeR","DESeq2","NOISeq"))
             ),
             conditionalPanel(
               condition="input.n_rep=='0'",
               radioButtons("de_method0","DE Method",choices=c("NOISeq"))
             ),
             h5("Choose 2 experiment conditions for DE analysis"),
             selectInput("f1","Condition 1",choices = ""),
             selectInput("f2","Condition 2",choices = ""),
             
             h5("DE criteria"),
             splitLayout(
               numericInput("p_val","FDR",min=0.01,max=1,value=0.05,step=0.01),
               numericInput("fc","Fold Change",min=1,value=2,step=0.1)
             ),
             fluidRow(
               column(4,
                      actionButton("submit_DE","Submit")
               ),
               column(6,
                      conditionalPanel(
                        condition = "input.DE_tabs=='DE genes' ",
                        downloadButton("download_de_table","Download table (csv)")
                      ),
                      conditionalPanel(
                        condition = "input.DE_tabs=='Volcano plot' ",
                        downloadButton("download_volcano","Download plot (PDF)")
                      ),
                      conditionalPanel(
                        condition = "input.DE_tabs=='Dispersion plot' ",
                        downloadButton("download_dispersion","Download plot (PDF)")
                      )
                      # conditionalPanel(
                      #   condition = "input.DE_tabs=='Heatmap plot' ",
                      #   downloadButton("download_heatmap","Download plot")
                      # )
               )
             )
             
           ),
           mainPanel(
             tabsetPanel(type = "tabs", id= "DE_tabs",
                         tabPanel("DE genes",
                                  # h3("Differential Expression Analysis"),
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   div(img(src="load.gif",width=240,height=180),
                                                       h4("Processing ... Please wait"),style="text-align: center;")
                                  ), 
                                  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                   DT::dataTableOutput("DE_table")
                                  )
                         ),
                         tabPanel("Volcano plot",    # for DESeq and edgeR
                                  h6("Volcano plot is only available for edgeR and DESeq2 methods"),
                                  conditionalPanel(
                                    condition = "input.n_rep=='1' && input.method1!='NOISeq'",
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     div(img(src="load.gif",width=240,height=180),
                                                         h4("Processing ... Please wait"),style="text-align: center;")
                                    ), 
                                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                     plotOutput("volcano_plot") 
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.method0=='NOISeq' || input.method1=='NOISeq'",
                                    h6("Volcano Plot is only applicable to DESeq2 and edgeR")
                                  )
                         ),
                         tabPanel("Dispersion plot", # for edgeR
                                  h6("Dispersion plot is only available for edgeR and DESeq2 methods"),
                                  conditionalPanel(
                                    condition = "input.n_rep=='1' && input.method1!='NOISeq'",
                                    # h3("Dispersion plot"),
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     div(img(src="load.gif",width=240,height=180),
                                                         h4("Processing ... Please wait"),style="text-align: center;")
                                    ), 
                                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                     plotOutput("dispersion_plot")
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.method0=='NOISeq' || input.method1=='NOISeq'",
                                    h6("Dispersion Plot is only applicable to DESeq2 and edgeR")
                                  )
                         )
             )
           )
  ),
  tabPanel('Heatmap',
           sidebarPanel(
             conditionalPanel(
               condition = "input.heatmap_tabs=='Heatmap'",
               
               radioButtons("heatmap_de_ind",label="Choose data",choices=c("Indenpendent"="ind","DE result"="de")),
               numericInput('numOfCluster',"Number of clusters on rows",value=2,min=2,max=30,step=1),
               conditionalPanel(
                 condition = "input.heatmap_de_ind == 'ind' ",
                 # selectInput('numOfGeno',"Number of genotypes (mutants)",choices=c(1)),
                 splitLayout(
                   numericInput('fold',"Fold change",value=2,min=1,step=1),
                   numericInput("fold_ncol", "min. column",value=2,min=1,step=1)
                 )
                 
                 # uiOutput("refGeno"),
                 # radioButtons('heatmap_value',"Values",
                 #              c('Fold change','Log fold change'))
               ),
               
               downloadButton("downloadheatmap","Download as PDF"),
               actionButton('heatmap_plot',"Plot",width='65px',style="color: #fff; background-color: #337ab7; border-color: #337ab7;float:right")
               
               # conditionalPanel(
               #   condition = "input.heatmap_de_ind == 'ind' ",
               #   h5('Specify names of the genotypes'),
               #   uiOutput("expand_genonames")
               # )
             ),
             
             conditionalPanel(
               condition = "input.heatmap_tabs=='Gene clusters'",
               uiOutput("heatmap_display"),
               conditionalPanel(
                 condition = "input.display_cluster=='ALL'",
                 downloadButton("downloadclusters","Download as CSV")
               )
             )
             
           ),
           mainPanel(
             tabsetPanel(type = "tabs",id="heatmap_tabs",
                         tabPanel("Heatmap", 
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   div(img(src="load.gif",width=240,height=180),
                                                       h4("Processing ... Please wait"),style="text-align: center;")
                                  ), 
                                  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                   plotOutput("heatmap.plot")
                                  )),
                         tabPanel("Gene clusters", dataTableOutput('cluster.info'))
             )
           ))
)