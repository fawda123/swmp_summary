library(shiny)
library(shinyBS)

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # Application title
  h2("Monthly and annual summary of SWMP parameters"),
  
  h4('Created by Marcus W. Beck,', a('mbeck@tbep.org', href = 'mailto:mbeck@tbep.org'), "Todd O'Brien,", a('todd.obrien@noaa.gov', href = 'mailto:todd.obrien@noaa.gov')),
  
  p('This interactive widget provides graphical summaries of water quality, weather, and nutrient station data from the System Wide Monitoring Program of the National Estuarine Research Reserve System ', a('(NERRS).', href = 'http://www.nerrs.noaa.gov/', target = '_blank'), 'The drop down menus can be used to select the station, date range, and parameter for plotting. The raw data used for plotting include all SWMP records from the earliest date at each station after processing to remove QAQC flags.  The data include observations through December 2021 and are current as of January 25, 2022.  Plots are based on daily averages for each parameter. Missing values can be filled using the long-term average across years for each month (select "monthly averages") or as a linear interpolation between missing values (select "linear interpolation").  The monthly average works well for long gaps, but may not be an accurate representation of long-term trends, i.e., real averages may differ early vs late in the time series if a trend exists. The linear interpolation option is preferred for small gaps.  Cumulative precipitation data are based on the daily maximum. See the', a('GitHub repository', href='https://github.com/fawda123/swmp_summary', target = '_blank'), 'for source code.'),
  
  # buttons on top
  fluidRow(
    
    column(3, 
      h4('Select station'),
  
      uiOutput("stat")
      
    ),
    
    column(3,
      h4('Select date range'), 
    
      uiOutput("years")
      
    ),
  
    column(3,
      h4('Select variable'), 
    
      uiOutput("parms")
      
    ),
    
    column(3,
      h4('Fill missing values?'), 
           
        selectInput('fill', label = '', choices = list('none' = 'none', 'monthly average' = 'monoclim', 'linear interpolation' = 'interp'))
           
    )
    
  ),
  
  fluidRow(
   
    column(12, 
             
      tabsetPanel(
        
        tabPanel('Plot',
          
          # Show the plot,
          plotOutput("outplot", width = "100%"),
          
          tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }")
          
        ), 
        
        tabPanel('Monthly summary',
          
          column(12, 
            p('Tabular summary of monthly aggregations showing minimum, first quartile, median, mean, third quartile, maximum, missing values, and variance.'),
            DT::dataTableOutput('outtab_sum_mo')
          )
          
        ), 
        
        tabPanel('Monthly, annual summary',
          
          column(12, 
            p('Average values for monthly aggregations by year.  The trend indicates the average value across all years for the same month and the anomaly is the difference between the mean and trend.'),
            DT::dataTableOutput('outtab_sum_moyr')
            )
            
          ), 
        
        tabPanel('Annual summary',
          
          column(12, 
            p('Average values for annual aggregations.  The anomalies are the differences between the annual averages and overall average.'),
            DT::dataTableOutput('outtab_sum_yr')
            )
          ), 
        
        tabPanel('Downloads',
          
            column(7, 
              h4('Figures'), 
              column(6, 
                numericInput('height', 'Plot height (in)', value = 8, min = 0, step = 1),
                numericInput('width', 'Plot width (in)', value = 13, min = 0, step = 1)
                ),
              column(6, 
                HTML('<p></p>'),
                downloadButton('downloadplot', 'Complete plot'),
                HTML('<p></p>'),
                downloadButton('downloadplot1', 'Monthly means'),
                HTML('<p></p>'),
                downloadButton('downloadplot2', 'Monthly boxplots'),
                HTML('<p></p>'),
                downloadButton('downloadplot3', 'Monthly histograms'),
                HTML('<p></p>'),
                downloadButton('downloadplot4', 'Monthly, annual means'),
                HTML('<p></p>'),
                downloadButton('downloadplot5', 'Monthly, annual anomalies'),
                HTML('<p></p>'),
                downloadButton('downloadplot6', 'Annual anomalies')
                )
              ),
            column(4, h4('Tables'),
              column(4, 
                p(), 
                downloadButton('tab_mo', 'Monthly summary'),
                p(),
                downloadButton('tab_moyr', 'Monthly, annual summary'),
                p(),
                downloadButton('tab_yr', 'Annual summary')
              )
            )
          
        )
        
    ))
    
  )
    
))