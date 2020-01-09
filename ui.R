
library(cluster);library(caret);library(shiny);library(e1071) 
library(stats);library(DT);library(stringr);library(rpivotTable)
library(ggplot2);library(dplyr); library(readxl)

set.seed(87)
ClustereData <- read.csv("try.this.ClustereData.csv",header = T, sep = ",")
ClustereData <- as.data.frame(ClustereData )
ClustedMean <- read_excel("ClustedMean.xlsx")
clustedmean1row <- read_excel("df.xlsx")

#files <- list.files(pattern = ".R")    # get all .R files
#file.edit(files)  


#secure_app(head_auth = tags$script(inactivity),

navbarPage(
  
  title = 'Benvenuto',
  tabPanel('read.me', column(2,
                             
                             h3("Welcome to Interactive User Interface"),
                             
                             tags$a( "Copy Rights belong to Unicredit.it", href="https://www.unicredit.it/it/privati.html"),
                             br(),  
                             tags$b("Please wait/refresh to connect with the server.", style="color:red"),
                             
                             h3("Cluster Tab"),
                             
                             h4("Summary tabs shows the summary statistics and Elbow tab shows cluster statistics base on the selected Cluster Number."),
                             
                             h3("Modeling Tab"),
                             h4("First selec a .csv file with header, secondly, select Y and Xs for the calculation and wait for the calculation to complete."),
                             
                             h3("Contact"), 
                             tags$a("Contact with the App Builder", href="http://www00.unibg.it/struttura/strutturasmst.asp?id_notizia=81930"),
                             br(),
                             tags$a("See other Github repositories", href="https://github.com/seymakalay"),br(),br(),
                             
                             h6("Built with ",
                                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                "by",
                                img(src = " https://rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", height = "30px"),
                                ".")
                             # img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                             #".")
                             
  )), # one of the )  is the close  column(5,
  
  
  
  
  
  tabPanel(
    
    title = "Cluster Tab",  "This panel is intentionally left blank",
    sidebarPanel( width = 2, 
                  selectInput(inputId = 'xcol', 'X Variable', names(ClustereData),selected = "INDICE" ),
                  selectInput(inputId = 'ycol', 'Y Variable', names(ClustereData), selected = "index.oninvestment"),
                  selectInput(inputId = 'zcol', 'Z Variable', names(ClustereData), selected = "index.on.prodection"),
                  #selected=names(ClustereData)[[3]]),
                  #numericInput('clusters', 'Cluster Number', 3, min = 1, max = 9),
                  
                  sliderInput(inputId = "clusters", 
                              label = "Cluster Number", 
                              min = 1, max = 10, 
                              value = 5),
                  
                  
                  
                  #radioButtons(inputId = "filetype",
                  #              label = "Select filetype:",
                  #              choices = c("csv", "tsv"),
                  #              selected = "csv"),
                  
                  #sliderInput(inputId = "alpha", 
                  #           label = "Alpha:", 
                  #           min = 0, max = 1, 
                  #           value = 0.3)   ,
                  
                  tags$hr(),
                  checkboxGroupInput("var",label="Select Variables",choices = names(ClustereData))
                  
    ), #close sidebarPanel
    
    
    
    mainPanel( width = 9,  
               # plotOutput(outputId = "plot1", height = 300)
               
               tabsetPanel(type = "tabs",
                           tabPanel(title = "Summary",  icon = icon("list-alt"),
                                    br(),h4("Pairwise Correlation"),
                                    #textOutput(outputId = "correlation1"),
                                    #textOutput(outputId = "correlation2"),
                                    #textOutput(outputId = "correlation3"),
                                    tags$hr(),
                                    
                                    h4("Descriptive Statistics"), 
                                    h5("Please Select Variables From the Checkbox on the Lefside"),
                                    verbatimTextOutput("sum2"),
                                    tags$hr(),
                                    
                                    h4("Histogram of Xs"),
                                    plotOutput(outputId = "histplot.x1", height = 150),
                                    h4("Histogram of Ys"), 
                                    plotOutput(outputId = "histplot.y1", height = 150),
                                    h4("Histogram of Zs"), 
                                    plotOutput(outputId = "histplot.z1", height = 150),
                                    
                                    br(),
                                    h4("Mean Values Based on Color by Cluster Number"),
                                    plotOutput(outputId = "plot1", height = 300)
                                    
                                    
                                    
                                    
                           ), #close tabPanel(title = "Plot" 
                           
                           
                           
                           # Tab 1: Plot
                           tabPanel(title = "Elbow", icon = icon("less-than"),
                                    br(),h4("Visualize Elbow"),
                                    h5("Find the right cluster number for this study",  
                                       tags$a("find out more about elbow-method.", href="https://en.wikipedia.org/wiki/Elbow_method_(clustering)")),
                                    
                                    plotOutput(outputId = "plot.elbow"),
                                    br(),
                                    h4("Aggregate Cluster"),
                                    h5("Select the desired Cluster Number from the side bar and see the summary statistics based on the selected Cluster Number."),
                                    
                                    verbatimTextOutput("elbow.table"),
                                    br(),
                                    h4("Visiulasize the datatable with its interactive Cluster Number."),
                                    h5("For this part, select a desire Cluster Number from the left side panel and visiulasize the asssign cluster.number from the data-set below. Column c_I1I2Indice 
                                           present the cluster number based on 10 cluster. User may check if the number are match. "),
                                    
                                    
                                    DT::dataTableOutput("cluster.number"),
                                    br(),
                                    # p("Notice that the 'rows_all' attribute grabs the row indices of the data."),
                                    # verbatimTextOutput("filtered_row"),
                                    HTML("Filter The Data Table Above and select filetype on the left, then click"),
                                    downloadButton(outputId = 'cluster.downLoadFilter',
                                                   label = "Download the filtered data")
                                    
                                    
                           ) #, #close tabPanel(title = "Plot"  
                           
                           
                           #    # Tab 2: Data
                           #    tabPanel(title = "K-Mean", 
                           #             br(),
                           #             h4("Mean Values Based on Color by option"),
                           #             plotOutput(outputId = "plot1", height = 300)
                           #             
                           #    ) #close tabPanel(title = "Summary"
                           
                           
                           
               )) #cloase mainPanel
  ), #close tabPanel 
  
  ########################################################
  ########################################################
  
  
  
  
  tabPanel(title = "Caret Models",  #icon = icon("carrot"),
           sidebarPanel( width = 2, 
                         tags$b("Note: To see the calculation in this section, first upload the data and select y and x values.", style = "color:red"),
                         br(),
                         #h5(p("Data Input")),
                         fileInput("file", "Upload input data (csv file with header)"),
                         tags$hr(),
                         
                         radioButtons("disp", "Display",
                                      choices = c(Head = "head"),
                                      # All = "all"),
                                      selected = FALSE),
                         #selected = "head"),
                         
                         h5(p("Data Selection")),
                         htmlOutput("yvarselect"),
                         htmlOutput("xvarselect"),
                         htmlOutput("fxvarselect")
                         #h6(paste("Note: To see the calculation in this section, first upload the data and select y and x values."))
                         
                         
                         
                         #h4("Preprocessing"),
                         #checkboxInput("centerscale", "Center and Scale Variables", TRUE),
                         #br(),
                         #sliderInput("trainpercent",
                         #             "Fraction of data that goes to training",
                         #             value = 0.75, step = 0.05, min = 0.50, max = 0.80),
                         # br(),
                         # numericInput("randomseed", "Random Seed", 87, min = 1, max=100),
                         # HTML("<hr>"),
                         # radioButtons("method", h4("Caret Model"),  # radioButtons        
                         #      choices = c("Linear Discriminant Analysis (lda) [3 sec]"        = "lda",
                         #                "Classification and Regression Trees (rpart) [6 sec]" = "rpart",
                         #                "Bagged CART (treebag) [90 sec]"                      = "treebag",
                         #                "Stochastic Gradient Boosting (gbm) [20 sec]"         = "gbm",
                         #                "Random Forest (rf) [100 sec]"                        = "ranger",
                         #                "Support Vector Machines with Polynomial Kernel (svmPoly) [100 sec]"  = "svmPoly"
                         #              )),
                         ##h6(paste("Note:  Approximate processing times above are for a 2.4 GHz Intel i7-3630QM CPU.",
                         ##          "All models work on a local server, but only lda and rpart work at shinyapps.io.")),
                         #HTML("<hr>"),
                         
                         #tags$hr()#,
                         ##checkboxGroupInput("yAttr",label="Select Ys Variables",
                         ##                   choices = names(ClustereData[,c(18,44,45)])),
                         ## checkboxGroupInput("xAttr",label="Select Xs Variables",choices = names(ClustereData))
                         
                         
           ), #close sidebarPanel
           
           
           
           mainPanel( width = 10, 
                      tabsetPanel(type = "tabs",
                                  # Tab 1: Plot
                                  tabPanel(title = "Fit",
                                           #verbatimTextOutput("text1"),
                                           h4("Function used for this study."), verbatimTextOutput("y.value"),
                                           
                                           h4("Show the Head of the dataset"),tableOutput("contents"),
                                           h4("Model Fit"), verbatimTextOutput("sum1"),
                                           h4("Visualize the Model Fit"), plotOutput("plot"),
                                           h4("Mean of the Model Fit"), tableOutput("obs")
                                           #
                                           #verbatimTextOutput("fit"),
                                           #tags$b("Please wait/refresh to connect with the server.", style="color:red")
                                           #h5("write some explanation"),   
                                           #tableOutput(outputId = "my.resample")
                                           #verbatimTextOutput("my.resample")
                                           #verbatimTextOutput("olssummary")
                                           
                                  ), #close tabPanel(title = "Plot"  
                                  
                                  # Tab 2: Data
                                  tabPanel(title = "Variable Importance" 
                                           #,  
                                           #verbatimTextOutput("confusion")
                                           
                                  ), #close tabPanel(title = "Summary"
                                  
                                  # Tab 3: Data
                                  #tabPanel(title = "DotPlot" ,
                                  #        plotOutput("dotplot")
                                  
                                  #), #close tabPanel(title = "Table"
                                  
                                  # Tab 4: Data
                                  tabPanel(title = "Help" ,            
                                           HTML(HELP.HTML) 
                                           
                                  ) #close tabPanel(title = "Data"
                                  
                                  
                      )) # close tabsetPanel
  ) #, # close mainPanel 
  
  
  
  
  #  i deleted this tab, tabPanel(title = "Modeling Tab", .....
  
  
  
  
)


######################################################
######################################################




#  tabPanel(title = "Modeling Tab", 
#           sidebarPanel( width = 2,   selectInput(inputId = "y", 
#                                                  label = "Y-axis:",
#                                                  choices = c("index.oninvestment", "index.on.prodection", "INDICE"), 
#                                                  selected = "INDICE"),
#                         
#                         # Select variable for x-axis
#                         selectInput(inputId = "x", 
#                                     label = "X-axis:",
#                                     choices = names(ClustereData[,-c(1:6,18:21,44:46)]),
#                                     selected = "RISERVA_MATEMATICA_201711"),
#                         
#                         # Select variable for color
#                         selectInput(inputId = "z", 
#                                     label = "Color by:",
#                                     choices = c("DES_REGIONE_ABI_GEST" = "DES_REGIONE_ABI_GEST", 
#                                                 "DES_PROVINCIA_ABI_GEST" = "DES_PROVINCIA_ABI_GEST", 
#                                                 #"FLAG_RECESSO_2017" = "FLAG_RECESSO_2017", 
#                                                 "c_I1I2Indice" = "c_I1I2Indice"),
#                                     selected = "c_I1I2Indice"),
#                         # Set alpha level
#                         sliderInput(inputId = "alpha", 
#                                     label = "Alpha:", 
#                                     min = 0, max = 1, 
#                                     value = 0.3),
#                         
#                         # Select filetype
#                         radioButtons(inputId = "filetype",
#                                      label = "Select filetype:",
#                                      choices = c("csv", "tsv"),
#                                      selected = "csv"),
#                         
#                         
#                         # Select variables to download
#                         checkboxGroupInput(inputId = "selected_var",
#                                            label = "Select variables:",
#                                            choices = names(ClustereData),
#                                            selected = c(2:6,18,43:46))
#                         
#           ), #close sidebarPanel
#           
#           
#           mainPanel( width = 9,
#                      tabsetPanel(type = "tabs",
#                                  # Tab 1: Plot
#                                  tabPanel(title = "Plot", 
#                                           br(),h4("Visualize Custom Plot"),
#                                           h5("Choose an option from Color by on the left and select points on the graph with left click to see the selected values under the graph with its correlation."),br(),
#                                           plotOutput(outputId = "scatterplot", brush = "plot_brush"), 
#                                           
#                                           textOutput(outputId = "correlation"),
#                                           br(),
#                                           h4("Visualize the Selected Points "), br(),
#                                           DT::dataTableOutput(outputId = "moviestable"),
#                                           h4("Histogram of Xs"), 
#                                           plotOutput(outputId = "histplot.x", height = 150),
#                                           h4("Histogram of Ys"), 
#                                           plotOutput(outputId = "histplot.y", height = 150)
#                                           
#                                  ), #close tabPanel(title = "Plot"  


# Tab 2: Data
#                                  tabPanel(title = "Summary", 
#                                           br(),
#                                           h4("Mean Values Based on Color by option"),
#                                           h5("Choose an option from Color by. Higher the Deviation From the Mean of the Values Dangerous the Cluster is"),
#                                           h5("In This Case Cluster 1 and 4 Seems the Most Dangerous Clusters -> INSPECT"),
#                                           
#                                           #DT::dataTableOutput("mytable"),
#                                           
#                                           tableOutput(outputId="mytable1" ),
#                                           #bunu ekledim
#                                           
#                                           tableOutput(outputId="df.print")
#                                  ), #close tabPanel(title = "Summary"
#                                  
#                                  
#                                  # Tab 3: Data
#                                  tabPanel(title = "Table", 
#                                           br(),
#                                           
#                                           h4("Interactive Table"), 
#                                           h5("Recomendend: Table with Sub Total Col Heat map (or Bar Chart) and Count"),
#                                           h5("Recomendend: Changing the Place of DES_Regione, DES_Proviancia, Regione and c_I1I2Indice")#,
#                                          # rpivotTableOutput("pivot")
#                                           
#                                  ), #close tabPanel(title = "Table"

#                                  # Tab 4: Data
#                                  tabPanel(title = "Data", 
#                                           br(),
#                                           
#                                           tags$a("Visualize Your Data Table", href="https://seymakalay87.shinyapps.io/app5/"),br(),
#                                           br(),
#                                           
#                                           # DT::dataTableOutput( "alldataset"),
#                                           #HTML("Select filetype and variables, then click"), downloadButton("download_data", "Download data"),
#                                           #br()
#                                           
#                                           
#                                           #verbatimTextOutput("Raw"),
#                                           #DT::dataTableOutput("all.dataset"),
#                                           br(),
#                                           HTML("Filter The Data Table Above and select filetype on the left, then click")#,
#                                          #downloadButton('downLoadFilter',"Download the filtered data")



# DT::dataTableOutput(data= ClustereData %>% select(c(2:6),19:21,43:46),
#              options = list(pageLength = 50)
#DT::dataTableOutput(outputId = "moviestable")

#                                  ) #close tabPanel(title = "Data"




#                                  
#                      ) # close tabsetPanel
#           ) # close mainPanel 
# ))   # close tabPanel APP4




#) #cloase secure page


















