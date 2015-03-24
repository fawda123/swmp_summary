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

mo_labs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
mo_levs <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

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
    
    # year, month categories
    dat$year <- strftime(dat$datetimestamp, '%Y')
    dat$month <- strftime(dat$datetimestamp, '%m')
    dat$month <- factor(dat$month, labels = mo_levs, levels = mo_levs)
    
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
    
    ##
    # preprocessing

    # subset by variable to plot
    dat_plo <- dat()[dat()$year %in% seq(years[1], years[2]), ]
    dat_plo <- subset(dat_plo, select = c('datetimestamp', 'year', 'month', var))
    
    # label lookups
    lab_look <- list(
      temp = 'Temperature (C)', 
      spcond = 'Specific conductivity (mS/cm)',
      sal = 'Salinity (psu)',
      do_pct = 'Dissolved oxyxgen (%)',
      do_mgl = 'Dissolved oxygen (mg/L)',
      depth = 'Depth (m)',
      cdepth = 'Depth (nonvented, m)',
      level = 'Referenced depth (m)',
      clevel = 'Referenced depth (nonvented, m)',
      ph = 'pH',
      turb = 'Turbidity (NTU)',
      chlfluor = 'Chl fluorescence (ug/L)',
      atemp = 'Air temperature (C)',
      rh = 'Relative humidity (%)',
      bp = 'Barometric pressure (mb)',
      wspd = 'Wind speed (m/s)',
      maxwspd = 'Max wind speed (m/s)',
      wdir = 'Wind direction (degrees)',
      sdwdir = 'Wind direction (sd, degrees)',
      totpar = 'Total PAR (mmol/m2)',
      totprcp = 'Total precipitation (mm)',
      cumprcp = 'Cumulative precipitation (mm)',
      totsorad = 'Total solar radiation (watts/m2)',
      po4f = 'Orthophosphate (mg/L)', 
      nh4f = 'Ammonium (mg/L)',
      no2f = 'Nitrite (mg/L)',
      no3f = 'Nitrate (mg/L)',
      no23f = 'Nitrite + Nitrate (mg/L)',
      chla_n = 'Chlorophyll (ug/L)'
    )
    ylab <- lab_look[[var]]
    
    # monthly, annual aggs
    agg_fun <- function(x) mean(x, na.rm = T)
    form_in <- formula(paste0(var, ' ~ month'))
    mo_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = agg_fun)
    mo_agg_med <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = function(x) median(x, na.rm = T))
    form_in <- formula(paste0(var, ' ~ year'))
    yr_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'month')], FUN = agg_fun, na.action = na.pass)
    
    ##
    # plots
    
    # universal plot setting
    my_theme <- theme(axis.text = element_text(size = 8))
    
    # plot 1 - means and obs
    cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg))
    cols <- cols[rank(mo_agg[, var])]
    p1 <- ggplot(dat_plo, aes_string(x = 'month', y = var)) +
      geom_point(size = 2, alpha = 0.5, 
        position=position_jitter(width=0.1)
        ) +
      theme_classic() +
      ylab(ylab) + 
      xlab('Monthly distributions and means') +
      geom_point(data = mo_agg, aes_string(x = 'month', y = var), 
        colour = 'darkgreen', fill = cols, size = 7, pch = 21) + 
      my_theme
    
    # box aggs, colored by median
    cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg_med))
    cols <- cols[rank(mo_agg_med[, var])]
    p2 <- ggplot(dat_plo, aes_string(x = 'month', y = var)) + 
      geom_boxplot(fill = cols) +
      theme_classic() +
      ylab(ylab) + 
      xlab('Monthly distributions and medians') +
      my_theme
    
    # month histograms
    to_plo <- dat_plo
    to_plo$month <- factor(to_plo$month, levels = rev(mo_levs), labels = rev(mo_labs))
    p3 <- ggplot(to_plo, aes_string(x = var)) + 
      geom_histogram(aes(y = ..density..), colour = 'lightblue', binwidth = diff(range(to_plo[, var], na.rm = T))/30) + 
      facet_grid(month ~ .) + 
      xlab(ylab) +
      theme_bw(base_family = 'Times') + 
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        strip.text.y = element_text(size = 8, angle = 90),
        strip.background = element_rect(size = 0, fill = 'lightblue')) +
      my_theme
    
    # monthly means by year
    to_plo <- dat_plo[, names(dat_plo) %in% c('month', 'year', var)]
    form_in <- as.formula(paste(var, '~ .'))
    to_plo <- aggregate(form_in, to_plo, function(x) mean(x, na.rm = T),
      na.action = na.pass)
    
    to_plo$month <- factor(to_plo$month, labels = mo_labs, levels = mo_levs)
    names(to_plo)[names(to_plo) %in% var] <- 'V1'
    midpt <- mean(to_plo$V1, na.rm = T)
    p4 <- ggplot(subset(to_plo, !is.na(V1)), 
        aes(x = year, y = month, fill = V1)) +
      geom_tile() +
      geom_tile(data = subset(to_plo, is.na(V1)), 
        aes(x = year, y = month), fill = NA
        )  +
      scale_fill_gradient2(name = ylab,
        low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = midpt) +
      theme_classic() +
      ylab('Monthly means') +
      xlab('') +
      theme(legend.position = 'top', legend.title = element_blank()) +
      guides(fill = guide_colorbar(barheight = 0.5)) +
      my_theme
    
    # monthly anomalies
    mo_agg$month <- factor(mo_agg$month, labels = mo_labs, levels = mo_levs)
    to_plo <- merge(to_plo, mo_agg, by = 'month', all.x = T)
    names(to_plo)[names(to_plo) %in% var] <- 'trend'
    to_plo$anom <- with(to_plo, V1 - trend)
    rngs <- max(abs(range(to_plo$anom, na.rm = T)))
    p5 <- ggplot(subset(to_plo, !is.na(anom)), 
        aes(x = year, y = month, fill = anom)) +
      geom_tile() +
      geom_tile(data = subset(to_plo, is.na(anom)), 
        aes(x = year, y = month), fill = NA
        ) +
      scale_fill_gradient2(name = ylab,
        low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
        limits = c(-1 * rngs, rngs)) +
      theme_classic() +
      ylab('Monthly anomalies') +
      xlab('') +
      theme(legend.position = 'top', legend.title = element_blank()) +
      guides(fill = guide_colorbar(barheight= 0.5)) +
      my_theme
    
    # annual anomalies
    yr_avg <- mean(yr_agg[, var], na.rm = T)
    yr_agg$anom <- yr_agg[, var] - yr_avg
    p6 <- ggplot(yr_agg, aes(x = year, y = anom, group = 1, fill = anom)) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient2(name = ylab,
        low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0
        ) +
      stat_smooth(method = 'lm', se = F, linetype = 'dashed', size = 1) +
      theme_classic() +
      ylab('Annual anomalies') +
      xlab('') +
      theme(legend.position = 'none') +
      my_theme

    ##
    # combine plots
    arrangeGrob(
      arrangeGrob(p1, p2, ncol = 1), 
      p3, 
      arrangeGrob(p4, p5, p6, ncol = 1, heights = c(1, 1, 0.8)), 
      ncol = 3, widths = c(1, 0.5, 1)
    )
    
    })
  
  output$outplot <- renderPlot({
    print(plotInput())
    }, height = 600, width = 1100)
  
  output$downloadplot <- downloadHandler(
    filename = function() { paste(input$stat, '.pdf', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::pdf(..., width = 13, height = 7.5)
        ggsave(file, plot = plotInput(), device = device)
    }) 
  
})