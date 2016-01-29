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
stats <- gsub('\\.RData', '', files_s3)

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

  ## dynamic UI
  output$stat <- renderUI({
        
    selectInput("stat", label = '', 
    choices = stats,
    selected = 'apaeswq')  
    
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
  
  # whole plot  
  plotInput <- function(){
      
    # input from ui
    stat <- input$stat
    var <- input$var
    years <- input$years
    
    # output
    plot_summary(dat(), var, years)
    
    }

  # separate plots
  plotInput_sep <- function(){
      
    # input from ui
    stat <- input$stat
    var <- input$var
    years <- input$years
    
    # output
    plot_summary(dat(), var, years, plt_sep = TRUE)
    
    }
  
  # tabular data
  tabInput <- function(){
      
    # input from ui
    stat <- input$stat
    var <- input$var
    years <- input$years
    
    # output
    plot_summary(dat(), var, years, sum_out = TRUE)
    
    }
  
  output$outplot <- renderPlot({
  
    plotInput()
    
    }, height = 600, width = 1100)
  
  # table for monthly summary
  output$outtab_sum_mo <- renderDataTable({
    tabInput()$sum_mo
    })
  
  # table for monthly, yearly summary
  output$outtab_sum_moyr <- renderDataTable({
    tabInput()$sum_moyr
    })
  
  # table for yearly summary
  output$outtab_sum_yr <- renderDataTable({
    tabInput()$sum_yr
    })
  
  
  ## downloads
  
  # whole plot
  output$downloadplot <- downloadHandler(
    filename = function() { paste(input$stat, '.pdf', sep='') },
    content = function(file) {
    
      pdf(file, width = input$width, height =input$height, family = 'serif')
      plotInput()
      dev.off()
      
   }
  )
  
  # monthly means
  output$downloadplot1 <- downloadHandler(
    filename = function() { paste(input$stat, '1.pdf', sep='') },
    content = function(file) {
      
      pdf(file, width = input$width, height =input$height, family = 'serif')
      print(plotInput_sep()[[1]])
      dev.off()
      
   }
  )
  
  # monthly boxplots
  output$downloadplot2 <- downloadHandler(
    filename = function() { paste(input$stat, '2.pdf', sep='') },
    content = function(file) {
      
      pdf(file, width = input$width, height =input$height, family = 'serif')
      print(plotInput_sep()[[2]])
      dev.off()
      
   }
  )
  
  # monthly histograms
  output$downloadplot3 <- downloadHandler(
    filename = function() { paste(input$stat, '3.pdf', sep='') },
    content = function(file) {
      
      pdf(file, width = input$width, height =input$height, family = 'serif')
      print(plotInput_sep()[[3]])
      dev.off()
      
   }
  )
  
  # monthly, annual means
  output$downloadplot4 <- downloadHandler(
    filename = function() { paste(input$stat, '4.pdf', sep='') },
    content = function(file) {
      
      pdf(file, width = input$width, height =input$height, family = 'serif')
      print(plotInput_sep()[[4]])
      dev.off()
      
   }
  )
  
  # monthly, annual anomalies
  output$downloadplot5 <- downloadHandler(
      filename = function() { paste(input$stat, '5.pdf', sep='') },
      content = function(file) {
        
        pdf(file, width = input$width, height =input$height, family = 'serif')
        print(plotInput_sep()[[5]])
        dev.off()
        
     }
    )
  
  # annual anomalies
  output$downloadplot6 <- downloadHandler(
      filename = function() { paste(input$stat, '6.pdf', sep='') },
      content = function(file) {
        
        pdf(file, width = input$width, height =input$height, family = 'serif')
        print(plotInput_sep()[[6]])
        dev.off()
        
     }
    )
  
  # table of monthly summaries
  output$tab_mo <- downloadHandler(
    filename = function() { paste(input$stat, '_tab1.csv', sep='') },
    content = function(file) {
  
      write.csv(tabInput()$sum_mo, file, quote = F, row.names = F)
      
   }
  )
  
  # table of monthly, yearly summaries
  output$tab_moyr <- downloadHandler(
    filename = function() { paste(input$stat, '_tab2.csv', sep='') },
    content = function(file) {
    
      write.csv(tabInput()$sum_moyr, file, quote = F, row.names = F)
      
   }
  )
  
  # table of yearly summaries
  output$tab_yr <- downloadHandler(
    filename = function() { paste(input$stat, '_tab3.csv', sep='') },
    content = function(file) {
    
      write.csv(tabInput()$sum_yr, file, quote = F, row.names = F)
      
   }
  )

})