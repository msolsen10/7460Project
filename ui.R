

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Utah Lake Algal Bloom Water Quality Data Application"),
  
  # Sidebar with user input controls
  sidebarLayout( 
    sidebarPanel(("Please Use the URL Link Below for Inputting New Data from Utah DWQ Buoy Stations"),
      uiOutput("tab"),
                 selectInput(inputId='site', 
                             label='Site', 
                             choices=unique(newbuoydata$SiteName), 
                             selected = NULL, 
                             multiple = FALSE,
                             selectize = TRUE, 
                             width = NULL, 
                             size = NULL),
                 selectInput(inputId='param',
                             label="Select Parameter to Plot and Create Stats Table:",
                             choices=list('Temperature'='Temperature',
                                          'pH'='pH','Specific Conductance'='Sp.Cond', 'Turbidity'='Turbidity',
                                          'ODO'='ODO','Phycocyanin' = 'Phycocyanin-RFU','Chlorophyll' = 'Chlorophyll'),
                             selected=NULL,
                             multiple=FALSE,
                             selectize=TRUE,
                             width= NULL,
                             size= NULL),
                selectInput(inputId='param2',
                  label="Select Parameter to Plot on Multiple Regression with Chlorophyll and Above Parameter:",
                  choices=list('Temperature'='Temperature',
                               'pH'='pH','Specific Conductance'='Sp.Cond', 'Turbidity'='Turbidity',
                               'ODO'='ODO','Phycocyanin' = 'Phycocyanin-RFU','Chlorophyll' = 'Chlorophyll'),
                  selected=NULL,
                  multiple=FALSE,
                  selectize=TRUE,
                  width= NULL,
                  size= NULL),
                 dateRangeInput("dates", label = h3("Date range 8/26/2016 to 11/1/2018"), 
                                start="2018-05-26",
                                end="2018-10-31"),
                  radioButtons('format', 'Document Format', c('PDF','HTML','Word')),
                  downloadButton('downloadReport.pdf')
                 
    ),
    
    # Show outputs, text, etc. in the main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Plot",textOutput("selected_var"), plotOutput("plot2")),
        tabPanel("Chlorophyll Plot", plotOutput("plot1"), textOutput("modelresults")),
        tabPanel("Multiple Regression Analysis",plotOutput("plot3"),textOutput("multresults")),
        tabPanel("Table",tableOutput("table")),
        tabPanel("Site Location",leafletOutput("mymap")))
    )
  )
))