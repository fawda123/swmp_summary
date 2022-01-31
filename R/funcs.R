#' Plot graphical summaries of SWMP data
#' 
#' Plot graphical summaries of SWMP data for individual parameters, including seasonal/annual trends and anomalies
#' 
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param years numeric vector of starting and ending years to plot, default all
#' @param plt_sep logical if a list is returned with separate plot elements
#' @param sum_out logical if summary data for the plots is returned
#' @param fill logical indicating if missing monthly values are replaced by long term monthly averages
#' @param base_size numeric for text size
#' @param ... additional arguments passed to other methods, currently not used
#' 
#' @import ggplot2 gridExtra
#' 
#' @importFrom stats aggregate as.formula formula median na.pass
#' @importFrom grDevices colorRampPalette
#' 
#' @export
#' 
#' @concept analyze
#' 
#' @details This function creates several graphics showing seasonal and annual trends for a given swmp parameter.  Plots include monthly distributions, monthly anomalies, and annual anomalies in multiple formats.  Anomalies are defined as the difference between the monthly or annual average from the grand mean.  Monthly anomalies are in relation to the grand mean for the same month across all years.  All data are aggregated for quicker plotting.  Nutrient data are based on monthly averages, wheras weather and water quality data are based on daily averages.  Cumulative precipitation data are based on the daily maximum.  An interactive Shiny widget is available: \url{https://beckmw.shinyapps.io/swmp_summary/}
#' 
#' Individual plots can be obtained if \code{plt_sep = TRUE}.  Individual plots for elements one through six in the list correspond to those from top left to bottom right in the combined plot.
#' 
#' Summary data for the plots can be obtained if \code{sum_out = TRUE}.  This returns a list with three data frames with names \code{sum_mo}, \code{sum_moyr}, and \code{sum_mo}.  The data frames match the plots as follows: \code{sum_mo} for the top left, bottom left, and center plots, \code{sum_moyr} for the top right and middle right plots, and \code{sum_yr} for the bottom right plot. 
#' 
#' @return A graphics object (Grob) of multiple \code{\link[ggplot2]{ggplot}} objects, otherwise a list of  individual \code{\link[ggplot2]{ggplot}} objects if \code{plt_sep = TRUE} or a list with data frames of the summarized data if \code{sum_out = TRUE}.
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}
#' 
#' @examples
#' ## import data
#' data(apacpnut)
#' dat <- qaqc(apacpnut)
#' 
#' ## plot
#' plot_summary(dat, param = 'chla_n', years = c(2007, 2013))
#' 
#' ## get individaul plots
#' plots <- plot_summary(dat, param = 'chla_n', years = c(2007, 2013), plt_sep = TRUE)
#' 
#' plots[[1]] # top left
#' plots[[3]] # middle
#' plots[[6]] # bottom right
#' 
#' ## get summary data
#' plot_summary(dat, param = 'chla_n', year = c(2007, 2013), sum_out = TRUE)
#' 
plot_summary <- function(swmpr_in, ...) UseMethod('plot_summary') 

#' @rdname plot_summary
#' 
#' @export
#' 
#' @concept analyze
#' 
#' @method plot_summary swmpr
plot_summary.swmpr <- function(swmpr_in, param, years = NULL, plt_sep = FALSE, sum_out = FALSE, fill = c('none', 'monoclim', 'interp'), 
                               base_size = 11, ...){
  
  fill <- match.arg(fill)
  
  stat <- attr(swmpr_in, 'station')
  parameters <- attr(swmpr_in, 'parameters')
  date_rng <- attr(swmpr_in, 'date_rng')
  
  mo_labs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  mo_levs <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  
  # sanity checks
  if(is.null(years)){
    years <- as.numeric(as.character(strftime(date_rng, '%Y')))
  } else {
    if(length(years) > 2) stop('One or two element year vector is required.')
    if(length(years) == 1) years <- c(years, years)
  }
  if(!param %in% parameters) stop('param must be included in the data')
  
  ##
  # preprocessing
  
  # fill na
  if(fill == 'monoclim'){
    
    names(swmpr_in)[names(swmpr_in) %in% param] <- 'toest'
    
    swmpr_in$year <- strftime(swmpr_in$datetimestamp, '%Y')
    swmpr_in$month <- strftime(swmpr_in$datetimestamp, '%m')
    swmpr_in$month <- factor(swmpr_in$month, labels = mo_levs, levels = mo_levs)
    
    swmpr_in <- tidyr::complete(swmpr_in, year, month, fill = list(toest = NA))
    swmpr_in <- dplyr::group_by(swmpr_in, month)
    swmpr_in <- dplyr::mutate(swmpr_in, 
                             toest = dplyr::case_when(
                               is.na(toest) ~ mean(toest, na.rm = T), 
                               T ~ toest
                             ))
    swmpr_in <- dplyr::ungroup(swmpr_in)
    swmpr_in$datetimestamp <- as.Date(ifelse(is.na(swmpr_in$datetimestamp), 
                                             paste(swmpr_in$year, swmpr_in$month, '01', sep = '-'), 
                                             as.character(as.Date(swmpr_in$datetimestamp))))
    swmpr_in <- as.data.frame(swmpr_in, stringsAsFactors = F)
    
    
    names(swmpr_in)[names(swmpr_in) %in% 'toest'] <- param
    
    swmpr_in <- swmpr(swmpr_in, stat)

  }
  
  if(fill == 'interp'){
    
    names(swmpr_in)[names(swmpr_in) %in% param] <- 'toest'
    
    swmpr_in$year <- strftime(swmpr_in$datetimestamp, '%Y')
    swmpr_in$month <- strftime(swmpr_in$datetimestamp, '%m')
    swmpr_in$month <- factor(swmpr_in$month, labels = mo_levs, levels = mo_levs)
    
    swmpr_in <- tidyr::complete(swmpr_in, year, month, fill = list(toest = NA))
    swmpr_in$datetimestamp <- as.Date(ifelse(is.na(swmpr_in$datetimestamp), 
                                             paste(swmpr_in$year, swmpr_in$month, '01', sep = '-'), 
                                             as.character(as.Date(swmpr_in$datetimestamp))))
    swmpr_in <- as.data.frame(swmpr_in, stringsAsFactors = F)
    
    names(swmpr_in)[names(swmpr_in) %in% 'toest'] <- param
    
    swmpr_in <- swmpr(swmpr_in, stat)
    
    swmpr_in <- na.approx.swmpr(swmpr_in, maxgap = 1e10)
  
  }
  
  ## aggregate by averages for quicker plots
  # nuts are monthly
  if(grepl('nut$', stat)){
    dat <- aggreswmp(swmpr_in, by = 'months', params = param)
  }
  
  # wq is monthly
  if(grepl('wq$', stat)){
    dat <- aggreswmp(swmpr_in, by = 'days', params = param)
  }
  
  # met is monthly, except cumprcp which is daily max
  if(grepl('met$', stat)){
    dat <- aggreswmp(swmpr_in, by = 'days', params = param)
    
    # summarize cumprcp as max if present
    if('cumprcp' %in% attr(swmpr_in, 'parameters')){
      cumprcp <- aggreswmp(swmpr_in, by = 'days', FUN = function(x) max(x, na.rm = TRUE), 
        params = 'cumprcp')
      dat$cumprcp <- cumprcp$cumprcp
    }
    
  }
  

  dat$year <- strftime(dat$datetimestamp, '%Y')
  dat$month <- strftime(dat$datetimestamp, '%m')
  dat$month <- factor(dat$month, labels = mo_levs, levels = mo_levs)
  
  # select years to plot
  dat_plo <- data.frame(dat[dat$year %in% seq(years[1], years[2]), ])

  # remove datetimestamp
  dat_plo <- dat_plo[, !names(dat_plo) %in% 'datetimestamp']

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
  ylab <- lab_look[[param]]

  # monthly, annual aggs
  agg_fun <- function(x) mean(x, na.rm = T)
  form_in <- formula(paste0(param, ' ~ month'))
  mo_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('year')], FUN = agg_fun)
  mo_agg_med <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('year')], FUN = function(x) median(x, na.rm = T))
  form_in <- formula(paste0(param, ' ~ year'))
  yr_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('month')], FUN = agg_fun, na.action = na.pass)
  
  ##
  # plots
  
  # universal plot setting
  my_theme <- theme()#axis.text = element_text(size = 10))
  
  # plot 1 - means and obs
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg))
  cols <- cols[rank(mo_agg[, param])]
  p1 <- suppressWarnings({ggplot(dat_plo, aes_string(x = 'month', y = param)) +
    geom_point(size = 2, alpha = 0.5, 
      position=position_jitter(width=0.1)
      ) +
    theme_classic(base_size = base_size) +
    ylab(ylab) + 
    xlab('Monthly distributions and means') +
    geom_point(data = mo_agg, aes_string(x = 'month', y = param), 
      colour = 'darkgreen', fill = cols, size = 7, pch = 21) + 
    my_theme
  })
  
  # box aggs, colored by median
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg_med))
  cols <- cols[rank(mo_agg_med[, param])]
  p2 <- suppressWarnings({ggplot(dat_plo, aes_string(x = 'month', y = param)) + 
    geom_boxplot(fill = cols) +
    theme_classic(base_size = base_size) +
    ylab(ylab) + 
    xlab('Monthly distributions and medians') +
    my_theme
  })
  
  # month histograms
  to_plo <- dat_plo
  to_plo$month <- factor(to_plo$month, levels = rev(mo_levs), labels = rev(mo_labs))
  p3 <- suppressWarnings({ggplot(to_plo, aes_string(x = param)) + 
    geom_histogram(aes_string(y = '..density..'), colour = 'lightblue', binwidth = diff(range(to_plo[, param], na.rm = T))/30) + 
    facet_grid(month ~ .) + 
    xlab(ylab) +
    theme_bw(base_family = 'Times', base_size = base_size) + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      strip.text.y = element_text(angle = 90),
      strip.background = element_rect(size = 0, fill = 'lightblue')) +
    my_theme
  })
  
  # monthly means by year
  to_plo <- dat_plo[, names(dat_plo) %in% c('month', 'year', param)]
  form_in <- as.formula(paste(param, '~ .'))
  to_plo <- aggregate(form_in, to_plo, function(x) mean(x, na.rm = T),
    na.action = na.pass)
  
  to_plo$month <- factor(to_plo$month, labels = mo_labs, levels = mo_levs)
  names(to_plo)[names(to_plo) %in% param] <- 'V1'
  midpt <- mean(to_plo$V1, na.rm = T)
  p4 <- suppressWarnings({ggplot(subset(to_plo, !is.na(to_plo$V1)), 
      aes_string(x = 'year', y = 'month', fill = 'V1')) +
    geom_tile() +
    geom_tile(data = subset(to_plo, is.na(to_plo$V1)), 
      aes(x = year, y = month), fill = NA
      )  +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = midpt) +
    theme_classic(base_size = base_size) +
    ylab('Monthly means') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight = 0.5)) +
    my_theme
  })
  
  # monthly anomalies
  mo_agg$month <- factor(mo_agg$month, labels = mo_labs, levels = mo_levs)
  to_plo <- merge(to_plo, mo_agg, by = 'month', all.x = T)
  names(to_plo)[names(to_plo) %in% param] <- 'trend'
  to_plo$anom <- with(to_plo, V1 - trend)
  rngs <- max(abs(range(to_plo$anom, na.rm = T)))
  p5 <- suppressWarnings({ggplot(subset(to_plo, !is.na(to_plo$anom)), 
      aes_string(x = 'year', y = 'month', fill = 'anom')) +
    geom_tile() +
    geom_tile(data = subset(to_plo, is.na(to_plo$anom)), 
      aes(x = year, y = month), fill = NA
      ) +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
      limits = c(-1 * rngs, rngs)) +
    theme_classic(base_size = base_size) +
    ylab('Monthly anomalies') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight= 0.5)) +
    my_theme
  })

  # annual anomalies
  yr_avg <- mean(yr_agg[, param], na.rm = T)
  yr_agg$anom <- yr_agg[, param] - yr_avg
  p6 <- suppressWarnings({ggplot(yr_agg, 
      aes_string(x = 'year', y = 'anom', group = '1', fill = 'anom')) +
    geom_bar(stat = 'identity') +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0
      ) +
    stat_smooth(method = 'lm', se = F, linetype = 'dashed', size = 1) +
    theme_classic(base_size = base_size) +
    ylab('Annual anomalies') +
    xlab('') +
    theme(legend.position = 'none') +
    my_theme
  })

  # return plot list if TRUE
  if(plt_sep) return(list(p1, p2, p3, p4, p5, p6))

  # return summary list if TRUE
  if(sum_out){

    # month summaries
    sum_mo <- split(dat_plo, dat_plo$month)
    sum_mo <- lapply(sum_mo, function(x){
        vr <- var(x[, param], na.rm = TRUE)
        summ <- my_summary(x[, param])
        names(summ)[1:6] <- c('min', 'firstq', 'med', 'mean', 'thirdq', 'max')
        c(summ, var = vr)
      })
    sum_mo <- do.call('rbind', sum_mo)
    sum_mo <- data.frame(month = rownames(sum_mo), sum_mo)
    sum_mo$month <- factor(sum_mo$month, levels = mo_levs, labels = mo_labs)
    row.names(sum_mo) <- 1:nrow(sum_mo)

    # month, yr summaries
    sum_moyr <- to_plo
    names(sum_moyr)[names(sum_moyr) %in% 'V1'] <- 'mean'
    sum_moyr <- sum_moyr[with(sum_moyr, order(year, month)), ]
    row.names(sum_moyr) <- 1:nrow(sum_moyr)
      
    # annual summaries
    sum_yr <- yr_agg
    names(sum_yr)[names(sum_yr) %in% param] <- 'mean'
      
    return(list(sum_mo = sum_mo, sum_moyr = sum_moyr, sum_yr = sum_yr))
    
  }
      
  ##
  # combine plots
  suppressWarnings(gridExtra::grid.arrange(
    arrangeGrob(p1, p2, ncol = 1), 
    p3, 
    arrangeGrob(p4, p5, p6, ncol = 1, heights = c(1, 1, 0.8)), 
    ncol = 3, widths = c(0.8, 0.5, 1.2)
  ))

}

#' Aggregate swmpr data
#' 
#' Aggregate swmpr data by specified time period and method
#' 
#' @param swmpr_in input swmpr object
#' @param by chr string of time period for aggregation one of \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'}
#' @param FUN aggregation function, default \code{mean} with \code{na.rm = TRUE}
#' @param params names of parameters to aggregate, default all
#' @param aggs_out logical indicating if \code{\link[base]{data.frame}} is returned of raw data with datetimestamp formatted as aggregation period, default \code{FALSE}
#' @param plot logical to return a plot of the summarized data, default \code{FALSE}
#' @param na.action function for treating missing data, default \code{na.pass}.  See the documentation for \code{\link[stats]{aggregate}} for options.
#' @param ... additional arguments passed to other methods
#' 
#' @concept analyze
#' 
#' @import data.table ggplot2
#' 
#' @importFrom stats aggregate formula na.pass
#' 
#' @export
#' 
#' @details The function aggregates parameter data for a swmpr object by set periods of observation and a user-supplied function. It is most useful for aggregating noisy data to evaluate trends on longer time scales, or to simply reduce the size of a dataset. Data can be aggregated by \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'} for the supplied function, which defaults to the \code{\link[base]{mean}}. A swmpr object is returned for the aggregated data, although the datetimestamp vector will be converted to a date object if the aggregation period is a day or longer. Days are assigned to the date vector if the aggregation period is a week or longer based on the round method for \code{\link[data.table]{IDate}} objects. This approach was used to facilitate plotting using predefined methods for Date and POSIX objects.
#' 
#' The method of treating NA values for the user-supplied function should be noted since this may greatly affect the quantity of data that are returned (see the examples). Finally, the default argument for \code{na.action} is set to \code{na.pass} for swmpr objects to preserve the time series of the input data.
#' 
#' @return Returns an aggregated swmpr object. QAQC columns are removed if included with input object.  If \code{aggs_out = TRUE}, the original \code{swmpr} object is returned with the \code{datetimestamp} column formatted for the first day of the aggregation period from \code{by}.  A \code{\link[ggplot2]{ggplot}} object of boxplot summaries is returned if \code{plot = TRUE}.
#' 
#' @seealso \code{\link[stats]{aggregate}}
#' 
#' @examples
#' \dontrun{
#' ## get data, prep
#' data(apacpwq)
#' dat <- apacpwq
#' swmpr_in <- subset(qaqc(dat), rem_cols = TRUE)
#'
#' ## get mean DO by quarters
#' aggreswmp(swmpr_in, 'quarters', params = c('do_mgl'))
#'
#' ## get a plot instead
#' aggreswmp(swmpr_in, 'quarters', params = c('do_mgl'), plot = T)
#' 
#' ## plots with other variables
#' p <- aggreswmp(swmpr_in, 'months', params = c('do_mgl', 'temp', 'sal'), plot = T)
#' p
#' library(ggplot2)
#' p + geom_boxplot(aes(fill = var)) + theme(legend.position = 'none')
#'
#' ## get variance of DO by years, remove NA when calculating variance
#' ## omit NA data in output
#' fun_in <- function(x)  var(x, na.rm = TRUE)
#' aggreswmp(swmpr_in, FUN = fun_in, 'years') 
#' }
aggreswmp <- function(swmpr_in, ...) UseMethod('aggreswmp')

#' @rdname aggreswmp
#' 
#' @concept analyze
#' 
#' @export
#'
#' @method aggreswmp swmpr
aggreswmp.swmpr <- function(swmpr_in, by, FUN = function(x) mean(x, na.rm = TRUE), params = NULL, aggs_out = FALSE, plot = FALSE, na.action = na.pass, ...){
  
  # data
  to_agg <- swmpr_in

  # attributes
  timezone <- attr(swmpr_in, 'timezone')
  parameters <- attr(swmpr_in, 'parameters')
  station  <- attr(swmpr_in, 'station')
  
  # sanity checks
  if(any(!params %in% parameters))
    stop('Aggregation parameters must be present in data')
  if(attr(swmpr_in, 'qaqc_cols'))
    warning('QAQC columns present, removed in output')
  if(!by %in% c('years', 'quarters', 'months', 'weeks', 'days', 'hours'))
    stop('Unknown value for by, see help documentation')
    
  # create agg values from datetimestamp
  # as posix if hours, as date if other
  if(by == 'hours'){
    
    to_agg$datetimestamp <- as.POSIXct(
      strftime(to_agg$datetimestamp, '%Y-%m-%d %H', 
        tz = timezone), format = '%Y-%m-%d %H',
      tz = timezone)

  } else {
    
    if(by == 'days'){
      
      to_agg$datetimestamp <- base::as.Date(to_agg$datetimestamp,
        tz = timezone)
      
    } else {
      
      to_agg$datetimestamp <- round(
        data.table::as.IDate(to_agg$datetimestamp, tz = timezone),
        digits = by
      )
      
      to_agg$datetimestamp <- base::as.Date(to_agg$datetimestamp, tz = timezone)
      
    }
   
  }
  
  # subset by parameters
  if(!is.null(params)) parameters <- parameters[parameters %in% params] 
  to_agg <- to_agg[, c('datetimestamp', parameters)]
  
  # return raw aggregations if true
  if(aggs_out) return(to_agg)
  
  # return plot if true
  if(plot){
    
    toplo <- tidyr::gather(to_agg, 'var', 'val', -datetimestamp)
    
    p <- ggplot(toplo, aes(x = factor(datetimestamp), y = val)) +
      geom_boxplot() +
      facet_wrap(~ var, scales = 'free_y', ncol = 1) + 
      theme_bw() +
      theme(axis.title.y = element_blank()) + 
      scale_x_discrete(by)
    
    return(p)
     
  }
  
  # aggregate
  form_in <- formula(. ~ datetimestamp)
  out <- suppressWarnings(aggregate(form_in, data.frame(to_agg), FUN = FUN, 
    na.action = na.action, simplify = TRUE, ...))

  # convert columns to numeric, missing converted to NA
  datetimestamp <- out[, 1]
  nr <- nrow(out)
  nc <- ncol(out) -1
  out <- c(as.matrix(out[, -1]))
  out[is.nan(out)] <- NA
  out[out %in%  c(-Inf, Inf)] <- NA
  out <- matrix(out, nrow = nr, ncol = nc) 
  out <- data.frame(
    datetimestamp = datetimestamp,
    out
    )
  names(out) <- c('datetimestamp', parameters)
  
  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

######
#' Create a swmpr object
#' 
#' Wrapper for creating a swmpr object
#' 
#' @param  stat_in \code{data.frame} of swmp data
#' @param  meta_in chr string for station code (7 or 8 characters), can be multiple stations if data are combined
#' 
#' @export swmpr
#' 
#' @return Returns a swmpr object to be used with S3 methods
#' 
#' @details 
#' This function is a simple wrapper to \code{\link[base]{structure}} that is used internally within other functions to create a swmpr object.  The function does not have to be used explicitly.  Attributes of a swmpr object include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, \code{stamp_class}, \code{metabolism} (if present), and \code{metab_units} (if present). 
#' 
swmpr <- function(stat_in, meta_in){
    
  if(!is.data.frame(stat_in)) 
    stop('stat_in must be data.frame')
  
  # qaqc attribute
  qaqc_cols <- FALSE
  if(any(grepl('^f_', names(stat_in)))) qaqc_cols <- TRUE
  
  # parameters attribute
  parameters <- grep('datetimestamp|^f_', names(stat_in), invert = TRUE, value = TRUE)
  
  # get stations, param_types attribtues
  param_types <- param_names()
  param_types <- unlist(lapply(param_types, function(x) any(x %in% parameters)))
  param_types <- names(param_names())[param_types]
  station <- grep(paste0(param_types, collapse = '|'), meta_in, value = TRUE)
  
  # timezone using time_vec function
  timezone <- time_vec(station_code = station, tz_only = TRUE)

  # create class, with multiple attributes
  structure(
    .Data = stat_in, 
    class = c('swmpr', 'data.frame'), 
    station = station,
    parameters = parameters, 
    qaqc_cols = qaqc_cols,
    date_rng = range(stat_in$datetimestamp),
    timezone = timezone, 
    stamp_class = class(stat_in$datetimestamp),
    metabolism = NULL, 
    metab_units = NULL
    )
  
}

######
#' Get parameters of a given type
#'
#' Get parameter column names for each parameter type
#' 
#' @param  param_type chr string specifying \code{'nut'}, \code{'wq'}, or \code{'met'}.  Input can be one to three types.
#' 
#' @export
#' 
#' @return Returns a named list of parameters for the \code{param_type}.  The parameter names are lower-case strings of SWMP parameters and corresponding qaqc names (\code{'f_'} prefix)
#' 
#' @details
#' This function is used internally within several functions to return a list of the expected parameters for a given parameter type: nutrients, water quality, or meteorological.  It does not need to be called explicitly. 
param_names <- function(param_type = c('nut', 'wq', 'met')){
  
  # sanity check
  if(any(!param_type %in% c('nut', 'wq', 'met')))
    stop('param_type must chr string of nut, wq, or met')
  
  nut_nms <- c('po4f', 'chla_n', 'no3f', 'no2f', 'nh4f', 'no23f', 'ke_n',
    'urea')
  nut_nms <- paste0(c('', 'f_'), rep(nut_nms, each = 2))
  
  wq_nms <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl', 'depth', 
    'cdepth', 'level', 'clevel', 'ph', 'turb', 'chlfluor')
  wq_nms <- paste0(c('', 'f_'), rep(wq_nms, each = 2))
  
  met_nms <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd', 'wdir', 'sdwdir',
    'totpar', 'totprcp', 'cumprcp', 'totsorad')
  met_nms <- paste0(c('', 'f_'), rep(met_nms, each = 2))
  
  # get names for a given parameter type
  out <- sapply(param_type, function(x) get(paste0(x, '_nms')), simplify = FALSE)
  
  return(out)
  
}

######
#' Format SWMP datetimestamp
#'
#' Format the datetimestamp column of SWMP data
#' 
#' @param  chr_in chr string of datetimestamp vector
#' @param  station_code is chr string for station (three or more characters)
#' @param  tz_only logical that returns only the timezone, default \code{FALSE}
#' 
#' @export
#' 
#' @return  Returns a POSIX vector if \code{tz_only} is true, otherwise the timezone for a station is returned as a chr string
#' 
#' @details 
#' This function formats the datetimestamp column of SWMP data to the \code{\link[base]{POSIXct}} format and the correct timezone for a station.  Note that SWMP data do not include daylight savings and the appropriate location based on GMT offsets is used for formatting.  This function is used internally within data retrieval functions and does not need to be called explicitly.
time_vec <- function(chr_in = NULL, station_code, tz_only = FALSE){
  
  # lookup table for time zones based on gmt offset - no DST!
  gmt_tab <- data.frame(
    gmt_off=c(-4,-5,-6,-8,-9),
    tz = c('America/Virgin', 'America/Jamaica', 'America/Regina',
      'Pacific/Pitcairn', 'Pacific/Gambier'),
    stringsAsFactors = FALSE
    )
  
  # hard-coded gmt offset for each site, from metadata direct from CDMO
  sites <- c('ace', 'apa', 'cbm', 'cbv', 'del', 'elk', 
    'gnd', 'grb', 'gtm', 'hud', 'jac', 'job', 'kac', 
    'lks', 'mar', 'nar', 'niw', 'noc', 'owc', 'pdb', 
    'rkb', 'sap', 'sfb', 'sos', 'tjr', 'wel', 'wkb',
    'wqb')
  gmt_offsets <- c(-5, -5, -5, -5, -5, -8, -6, -5, -5, -5, -5, -4, 
    -9, -6, -6, -5, -5, -5, -5, -8, -5, -5, -8, -8, -8,
    -5, -6, -5)  
  
  # get timezone from above information
  gmt_offset <- gmt_offsets[which(sites %in% substr(station_code, 1, 3))]
  tzone <- gmt_tab[gmt_tab$gmt_off %in% gmt_offset, 'tz']

  # timezone only if T
  if(tz_only) return(tzone)
  
  # format datetimestamp
  out <- as.POSIXct(chr_in, tz = tzone, format = '%m/%d/%Y %H:%M')
  
  # return output
  return(out)
  
}

# custom summary function to always include NA
my_summary <- function(v){
  if(!any(is.na(v))){
    res <- c(summary(v),"NA's"=0)
  } else{
    res <- summary(v)
  }
  return(res)
}

#' Linearly interpolate gaps
#' 
#' Linearly interpolate gaps in swmpr data within a maximum size 
#' 
#' @param object input swmpr object
#' @param params is chr string of swmpr parameters to smooth, default all
#' @param maxgap numeric vector indicating maximum gap size to interpolate where size is numer of records, must be explicit
#' @param ... additional arguments passed to other methods
#' 
#' @import zoo
#'
#' @export
#' 
#' @method na.approx swmpr
#' 
#' @concept analyze
#' 
#' @details A common approach for handling missing data in time series analysis is linear interpolation.  A simple curve fitting method is used to create a continuous set of records between observations separated by missing data.  A required argument for the function is \code{maxgap} which defines the maximum gap size  for interpolation. The ability of the interpolated data to approximate actual, unobserved trends is a function of the gap size.  Interpolation between larger gaps are less likely to resemble patterns of an actual parameter, whereas interpolation between smaller gaps may be more likely to resemble actual patterns.  An appropriate gap size limit depends on the unique characteristics of specific datasets or parameters.  
#' 
#' @seealso \code{\link[zoo]{na.approx}}
#' 
#' @return Returns a swmpr object. QAQC columns are removed if included with input object.
#' 
#' @examples
#' data(apadbwq)
#' dat <- qaqc(apadbwq)
#' dat <- subset(dat, select = 'do_mgl', 
#'  subset = c('2013-01-22 00:00', '2013-01-26 00:00'))
#' 
#' # interpolate, maxgap of 10 records
#' fill1 <- na.approx(dat, params = 'do_mgl', maxgap = 10)
#' 
#' # interpolate maxgap of 30 records
#' fill2 <- na.approx(dat, params = 'do_mgl', maxgap = 30)
#' 
#' # plot for comparison
#' par(mfrow = c(3, 1))
#' plot(fill1, col = 'red', main = 'Interpolation - maximum gap of 10 records')
#' lines(dat)
#' plot(fill2, col = 'red', main = 'Interpolation - maximum gap of 30 records')
#' lines(dat)
na.approx.swmpr <- function(object, params = NULL, maxgap, ...){
  
  swmpr_in <- object
  
  # attributes
  parameters <- attr(swmpr_in, 'parameters')
  station <- attr(swmpr_in, 'station')
  
  # sanity checks
  if(!any(params %in% parameters) & !is.null(params))
    stop('Params argument must name input columns')
  if(attr(swmpr_in, 'qaqc_cols'))
    warning('QAQC columns present, removed in output')
  
  # prep for interpolate
  if(!is.null(params)) parameters <- parameters[parameters %in% params]
  to_interp <- swmpr_in[, c('datetimestamp', parameters), 
                        drop = FALSE]
  datetimestamp <- to_interp$datetimestamp
  to_interp$datetimestamp <- NULL
  
  # interpolate column-wise
  out <- lapply(c(to_interp),
                FUN = function(in_col){
                  
                  interp <- try(zoo::na.approx(in_col, maxgap = maxgap, 
                                               na.rm = FALSE), silent = TRUE, ...)
                  
                  if('try-error' %in% class(interp)) interp  <- in_col
                  
                  return(interp)
                  
                })
  
  # format output as data frame
  out <- do.call('cbind', out)
  out <- data.frame(datetimestamp, out)
  names(out) <- c('datetimestamp', parameters)
  
  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}