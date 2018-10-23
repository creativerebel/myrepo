#install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('stringr', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('data.table', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('shinyjs', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('shinythemes', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('shinyFiles', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('corrplot', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(shiny)
library(shinythemes) 
library(DT) 
library(stringr) 
library(data.table) 
library(shinyjs) 
library(shinyFiles)
library(corrplot)

#my_df <- read.csv("Sample Data.csv")
ui <- navbarPage(title = div("MMM Automation",img(src = "https://fractalanalytics.com/wp-content/uploads/2018/03/Fractal-Logo-WithBL.png", height = "32px",
                                                  style = "position: relative;bottom: 5px; left: 900px")), theme = shinytheme("cerulean"),
                 tabPanel("Home",
                          #tags$head(tags$style(HTML('body, label, input, button, select { font-family: "Calibri";background-color: #fdfdfd;}'))),
                          textInput("caption", "Create Project Folder"),
                          shinyDirButton("dir", "Chose directory", "Upload")
                 ),
                 tabPanel("Data Input & Classification", 
                          tabsetPanel(type = "tabs",
                                      tabPanel("Data Input",
                                               mainPanel(
                                                 tags $hr(),
                                                 flowLayout(fileInput('file1', 'POS/Modelling Data', accept = c(".csv")), selectInput("Browse", "Time-Period", choices = c("","Weekly", "Monthly", "Quartely" , "Yearly"))),
                                                 flowLayout(fileInput('file2', 'Coverage', accept = c(".csv")),selectInput("Browse1", "Time-Period", choices = c("","Weekly", "Monthly", "Quartely" , "Yearly"))),
                                                 flowLayout(fileInput('file3', 'Spends', accept = c(".csv")),selectInput("Browse2", "Time-Period", choices = c("","Weekly", "Monthly", "Quartely" , "Yearly")))),
                                               mainPanel(tableOutput(outputId = 'table.output'))),
                                      tabPanel("Dimension Classification"),
                                      tabPanel("Variable Classification", 
                                               fluidPage(
                                                 fluidRow( 
                                                   column(12, 
                                                          wellPanel( 
                                                            div(DT::dataTableOutput(outputId = "x1"),  
                                                                style = "font-size : 80%")))))))),
                 tabPanel("Data Review"), 
                 tabPanel("Modelling",
                          tabsetPanel(type ="tabs",
                                      tabPanel("Derived Variables"),
                                      tabPanel("Correlation",
                                               splitLayout(
                                                 div( DT::dataTableOutput(outputId = "correlation"),
                                                      style = "font-size : 70%"),
                                                 div( plotOutput("corrplot",height= 500))
                                               )),
                                      tabPanel("Modelling")
                                      
                          )),
                 tabPanel("Output & Results",
                          tabsetPanel(type ="tabs",
                                      tabPanel("Model Results"),
                                      tabPanel("ROIs"),
                                      tabPanel("Due-Tos and Waterfall"))),
                 tags$style(HTML(".navbar .navbar-nav {float: left; font-size: 13px; margin:0% } 
                                 .navbar .navbar-header {float: left; font-size: 16px} 
                                 .navbar-default .navbar-brand {font-size: 23px; margin:0% }")))

server = function(input, output) { 
  shinyDirChoose(input, 'dir', roots = c(name=getwd()))
  # setwd("/home/fractaluser/Documents/Udemy_DS")
  #getwd()
  vals <- reactiveValues() 
  observe({
    my_df <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      tbl <- read.csv(inFile$datapath)
      return(tbl)
    })
    variables <- reactive(colnames(my_df()))
    #my_df <- read.csv("/home/fractaluser/Documents/Udemy_DS/Simple_Linear_Regression/Sample Data.csv")
    # create a character vector of shiny inputs
    m = matrix(
      (1:10), nrow = length(variables()), ncol = 10, byrow = TRUE,
      dimnames = list(variables(), c("Price", 
                                     "Distribution", "Media", "SF", "TP", "Comp_Price","Comp_Distr",
                                     "Comp_Media","Comp_Promo", "Others")))
    # 
    for (i in seq_len(nrow(m))) {
      m[i, ] = sprintf(
        ifelse(colnames(m) == "Others",
               '<input type="radio" name="%s" value="%s" checked = "checked"/>',
               '<input type="radio" name="%s" value="%s"/>'),
        variables()[i], m[i, ])
    }
    
    correlation = matrix(
      (1:4), nrow = length(variables()), ncol = 4, byrow = TRUE,
      dimnames = list(variables(), c("Lag", "Log","Adstock","Default")))
    for (i in seq_len(nrow(correlation))) {
      correlation[i, ] = sprintf(
        ifelse(colnames(correlation) == "Default",'<input type="radio" name="%s" value="%s" checked = "checked"/>',
               '<input type="text" name="%s" value="%s"/>'),
        variables()[i], correlation[i, ])
    }
    output$x1 = DT::renderDataTable(m, 
                                    escape = FALSE, selection = 'none', server = FALSE, class = 'cell-border stripe', 
                                    options = list( initComplete = JS( 
                                      "function(settings, json) {", 
                                      "$(this.api().table().header()).css({'background-color': '#0B3861', 'color': '#fff'});", 
                                      "}"),ordering = FALSE, scroller = TRUE, scrollX = TRUE, 
                                      autoWidth = TRUE, scrollY = "525px", bPaginate = FALSE,  
                                      searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), 
                                    callback = JS("table.rows().every(function(i, tab, row) { 
                                                  var $this = $(this.node()); 
                                                  $this.attr('id', this.data()[0]); 
                                                  $this.addClass('shiny-input-radiogroup'); 
  }); 
                                                  Shiny.unbindAll(table.table().node()); 
                                                  Shiny.bindAll(table.table().node()); 
                                                  $(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});") 
    ) 
    
    output$correlation = DT::renderDataTable(correlation, 
                                             escape = FALSE, selection = 'none', server = FALSE, class = 'cell-border stripe', 
                                             options = list( initComplete = JS( 
                                               "function(settings, json) {", 
                                               "$(this.api().table().header()).css({'background-color': '#0B3861', 'color': '#fff'});", 
                                               "}"),ordering = FALSE, scroller = TRUE, scrollX = TRUE, 
                                               autoWidth = TRUE, scrollY = "525px", bPaginate = FALSE,  
                                               searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), 
                                             callback = JS("table.rows().every(function(i, tab, row) { 
                    var $this = $(this.node()); 
                    $this.attr('id', this.data()[0]); 
                    $this.addClass('shiny-input-radiogroup'); 
  }); 
                    Shiny.unbindAll(table.table().node()); 
                    Shiny.bindAll(table.table().node()); 
                    $(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});") 
    ) 
    my_test <- read.csv("/home/fractaluser/Documents/Udemy_DS/Simple_Linear_Regression/Sample Data.csv")
    my_test2<-my_test[c(-1)]
    input<-scale(my_test2)
    
    output$corrplot=renderPlot({
      corrplot(as.matrix(my_test2),is.corr = FALSE, method = "square")
    })
})
  }
# Run the app ---- 
shinyApp(ui = ui, server = server) 