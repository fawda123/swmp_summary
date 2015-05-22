# packages to use
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(httr)
library(XML)
library(shinyBS)

# names of files on server
files_s3 <- httr::GET('https://s3.amazonaws.com/swmpagg/')$content
files_s3 <- rawToChar(files_s3)
files_s3 <- htmlTreeParse(files_s3, useInternalNodes = T)
files_s3 <- xpathSApply(files_s3, '//contents//key', xmlValue)

source('R/funcs.R')

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  ## retrieve from AmazonS3, uses httr GET
  dat <- reactive({
    
    stat <- input$stat
    raw_content <- paste0('https://s3.amazonaws.com/swmpagg/', stat, '.RData')
    raw_content <- httr::GET(raw_content)$content
    connect <- rawConnection(raw_content)
    load(connect)
    dat <- get(stat)
    rm(list = stat)
    close(connect) 
    
    dat$year <- as.numeric(strftime(dat$datetimestamp, '%Y'))
    
    return(dat)
    
  })

  output$years <- renderUI({
  
    yrs <- as.numeric(as.character(unique(dat()$year)))
   
    sliderInput("years", label = '',  
      min = min(yrs), max = max(yrs), 
      value = range(yrs),
      sep = '', ticks = FALSE
    )
   
  })

  output$parms <- renderUI({
  
    type <- substring(input$stat, 6)
  
    vars <- list(
    
      wq = list(
        'Temperature (C)' = 'temp',
        'Specific conductivity (mS/cm)' = 'spcond',
        'Salinity (psu)' = 'sal',
        'Dissolved oxyxgen (%)' = 'do_pct',
        'Dissolved oxygen (mg/L)' = 'do_mgl',
        'Depth (m)' = 'depth',
        'Depth (nonvented, m)' = 'cdepth',
        'Referenced depth (m)' = 'level',
        'Referenced depth (nonvented, m)' = 'clevel',
        'pH' = 'ph',
        'Turbidity (NTU)' = 'turb',
        'Chl fluorescence (ug/L)' = 'chlfluor'
        ),
      
      met = list(
        'Air temperature (C)' = 'atemp',
        'Relative humidity (%)' = 'rh',
        'Barometric pressure (mb)' = 'bp',
        'Wind speed (m/s)' = 'wspd',
        'Max wind speed (m/s)' = 'maxwspd',
        'Wind direction (degrees)' = 'wdir',
        'Wind direction (sd, degrees)' = 'sdwdir',
        'Total PAR (mmol/m2)' = 'totpar',
        'Total precipitation (mm)' = 'totprcp',
        'Cumulative precipitation (mm)' = 'cumprcp',
        'Total solar radiation (watts/m2)' = 'totsorad'
        ), 
      
      nut = list(
        'Orthophosphate (mg/L)' = 'po4f', 
        'Ammonium (mg/L)' = 'nh4f', 
        'Nitrite (mg/L)' = 'no2f', 
        'Nitrate (mg/L)' = 'no3f', 
        'Nitrite + Nitrate (mg/L)' = 'no23f', 
        'Chlorophyll (ug/L)' = 'chla_n'
        )
        
      )
  
    # select appropriate type, then remove those w/ no data
    var_sub <- vars[[type]]
    nonempties <- colSums(is.na(dat())) == nrow(dat())
    var_sub <- var_sub[var_sub %in% names(dat())[!nonempties]]

    selectInput(inputId = "var", label = '',  
      choices = var_sub,
      selected = var_sub[[1]]
    )
   
  })
    
  plotInput <- reactive({
      
    # input from ui
    stat <- input$stat
    var <- input$var
    years <- input$years
    
    # output
    plot_summary(dat(), var, years)
    
    })

  output$outplot <- renderPlot({
  
    plotInput()
    
    }, height = 600, width = 1100)
  
  output$downloadplot <- downloadHandler(
    filename = function() { paste(input$stat, '.pdf', sep='') },
    content = function(file) {
      
      # input from ui
      stat <- input$stat
      var <- input$var
      years <- input$years
    
      pdf(file, width = 13, height = 7.5)
      plot_summary(dat(), var, years)
      dev.off()
      
   }
  ) 
  
})