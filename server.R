# packages to use
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(httr)
library(XML)

# names of files on server
files_s3 <- httr::GET('https://s3.amazonaws.com/swmpagg/')$content
files_s3 <- rawToChar(files_s3)
files_s3 <- htmlTreeParse(files_s3, useInternalNodes = T)
files_s3 <- xpathSApply(files_s3, '//contents//key', xmlValue)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {

  output$outplot <- renderPlot({
    
    # input from ui
    stat <- input$stat
    var <- input$var
    
    ##
    # retrieve from AmazonS3, uses httr GET
    raw_content <- paste0('https://s3.amazonaws.com/swmpagg/', stat, '.RData')
    raw_content <- httr::GET(raw_content)$content
    connect <- rawConnection(raw_content)
    load(connect)
    dat <- get(stat)
    rm(list = stat)
    close(connect)
    
    ##
    # preprocessing
    
#     browser()
    
    # subset by variable to plot
    dat <- subset(dat, select = c('datetimestamp', var))
    
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
      totsorad = 'Total solar radiation (watts/m2)'
    )
    ylab <- lab_look[[var]]
    
    # month category
    mo_labs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
    mo_levs <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
    dat$month <- strftime(dat$datetimestamp, '%m')
    dat$month <- factor(dat$month, labels = mo_levs, levels = mo_levs)
    
    # year category
    dat$year <- strftime(dat$datetimestamp, '%Y')
    
    # monthly, annual aggs
    agg_fun <- function(x) mean(x, na.rm = T)
    form_in <- formula(paste0(var, ' ~ month'))
    mo_agg <- aggregate(form_in, data = dat[, !names(dat) %in% c('datetimestamp', 'year')], FUN = agg_fun)
    form_in <- formula(paste0(var, ' ~ year'))
    yr_agg <- aggregate(form_in, data = dat[, !names(dat) %in% c('datetimestamp', 'month')], FUN = agg_fun)
    
    ##
    # plots
    
    # universal plot setting
    my_theme <- theme(axis.text = element_text(size = 8))
    
    # plot 1 - means and obs
    p1 <- ggplot(dat, aes_string(x = 'month', y = var)) +
      geom_point(size = 2, alpha = 0.5, 
        position=position_jitter(width=0.1)
        ) +
      theme_classic() +
      ylab(ylab) + 
      xlab('Monthly distributions and means') +
      geom_point(data = mo_agg, aes_string(x = 'month', y = var), 
        colour = 'darkgreen', fill = 'lightgreen', size = 7, pch = 21) + 
      my_theme
    
    # box aggs, colored by median
    cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg))
    cols <- cols[rank(mo_agg[, var])]
    p2 <- ggplot(dat, aes_string(x = 'month', y = var)) + 
      geom_boxplot(fill = cols) +
      theme_classic() +
      ylab(ylab) + 
      xlab('Monthly distributions and medians') +
      my_theme
    
    # month histograms
    to_plo <- dat
    to_plo$month <- factor(to_plo$month, levels = rev(mo_levs), labels = rev(mo_labs))
    p3 <- ggplot(to_plo, aes_string(x = var)) + 
      geom_histogram(aes(y = ..density..), colour = 'lightblue') + 
      facet_grid(month ~ .) + 
      xlab(ylab) +
      theme_bw(base_family = 'Times') + 
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        strip.text.y = element_text(size = 8, angle = 90),
        strip.background = element_rect(size = 0, fill = 'lightblue')) +
      my_theme
    
    # monthly means by year
    to_plo <- ddply(dat, 
      .variables = c('month', 'year'), 
      .fun = function(x) mean(x[, var],  na.rm = T)
      )
    to_plo$month <- factor(to_plo$month, labels = mo_labs, level = mo_levs)
    midpt <- mean(to_plo$V1, na.rm = T)
    p4 <- ggplot(to_plo, aes(x = year, y = month, fill = V1)) +
      geom_tile() +
      scale_fill_gradient2(name = ylab,
        low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = midpt) +
      theme_classic() +
      ylab('Monthly means') +
      xlab('') +
      theme(legend.position = 'none') +
      my_theme
    
    #   scale_fill_gradientn(name = 'Correlation', 
    #     colours = brewer.pal(11, 'GnBu')#, limits = c(0, 1)
    #     ) 
    
    # monthly anomalies
    mo_agg$month <- factor(mo_agg$month, labels = mo_labs, levels = mo_levs)
    to_plo <- merge(to_plo, mo_agg, by = 'month')
    names(to_plo)[names(to_plo) %in% var] <- 'trend'
    to_plo$anom <- with(to_plo, V1 - trend)
    rngs <- max(abs(range(to_plo$anom, na.rm = T)))
    p5 <- ggplot(to_plo, aes(x = year, y = month, fill = anom)) +
      geom_tile() +
      scale_fill_gradient2(name = ylab,
        low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
        limits = c(-1 * rngs, rngs)) +
      theme_classic() +
      ylab('Monthly anomalies') +
      xlab('') +
      theme(legend.position = 'none') +
      my_theme
    
    # annual anomalies
    yr_avg <- mean(yr_agg[, var], na.rm = T)
    yr_agg$anom <- yr_agg[, var] - yr_avg
    p6 <- ggplot(yr_agg, aes(x = year, y = anom, group = 1, fill = anom)) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient2(name = ylab,
        low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
        limits = c(-1, 1)) + 
      ylim(c(-1, 1)) +
      stat_smooth(method = 'lm', se = F, linetype = 'dashed', size = 1) +
      theme_classic() +
      ylab('Annual anomalies') +
      xlab('') +
      theme(legend.position = 'none') +
      my_theme
    
    ##
    # combine plots
    grid.arrange(
      arrangeGrob(p1, p2, ncol = 1), 
      p3, 
      arrangeGrob(p4, p5, p6, ncol = 1), 
      ncol = 3, widths = c(1, 0.5, 1))
    
    },height = 600, width = 1100)
  
})