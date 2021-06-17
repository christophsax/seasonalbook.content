#' Daily Seasonal Adjustment, Using dsa
#'
#' Daily Seasonal Adjustment, using the *dsa* package from Bundesbank.
#' The functions uses the same syntax as `seas_daily()` but does not support multiple time series.
#'
#' https://cran.r-project.org/web/packages/dsa/vignettes/dsa-vignette.html
#'
#' @param x ts-boxable time series, an object of class ts, xts, zoo, data.frame, data.table, tbl, tbl_ts, tbl_time, tis, irts or timeSeries.
#' @param h forecast horizon.
#'
#' @author XCSAX
#'
#' @export
#' @examples
#' if (interactive()) {
#'   data(casualties)
#'   # takes several minutes
#'   ans <- seas_dsa(casualties)
#'   seas_components(ans)
#' }
seas_dsa <- function(x, h = 40) {

    validate_seas_input(x)

    res <- dsa::dsa(ts_xts(x))

    effects <- dsa_seas_effects(res)


    z_wide <-
      res$output %>%
      ts_tbl() %>%
      ts_span(end = ts_summary(x)$end + h) %>%
      ts_c(x) %>%
      ts_c(effects) %>%
      ts_wide() %>%
      transmute(
        time,
        orig = x,
        fct = original,
        adj = seas_adj,
        trend,
        # irreg = fct - trend - sc_fac,
        seas = sc_fac,
        seas_x = 0,
        seas_w,
        seas_m,
        seas_y
      ) %>%
      # mutate(fct = if_else(is.na(orig), NA_real_, fct)) %>%
      # mutate(adj = if_else(is.na(orig), NA_real_, adj)) %>%
      # mutate(seas = if_else(is.na(orig), NA_real_, seas)) %>%
      # mutate(trend = if_else(is.na(orig), NA_real_, trend)) %>%
      select(time, !! .exp_cols_comp)

    z <- ts_na_omit(ts_long(z_wide))

    validate_seas_output(z)
}


# copy pasted from dsa::output
dsa_seas_effects <- function(daily.object, short=FALSE, SI=TRUE, SI365.seed=3, spec=TRUE, outlier=TRUE, Factor="auto", everyDay=TRUE, seasonals=FALSE, spectrum_linesize=0.5, progressBar=TRUE) {

  path=tempdir() # write to NIRVANA

  # if (progressBar) {
  #   total <- 1
  #   pb <- utils::txtProgressBar(title = "Output for dsa", min = 0, max = total, width = NA, label="Getting started", style=3)
  #   utils::setTxtProgressBar(pb, 1/21, title="Output for dsa", label="Getting started")}

  `%>%` <- dygraphs::`%>%`

  if (short) {
  SI <- spec <- outlier <- week <- FALSE
  }

  path <- gsub("//", "/", path)

  name <- as.character(substitute(daily.object))

  if (!dir.exists(paste(path))) {
    dir.create(paste(path))  }
  if (!dir.exists(paste(path, "Graphics", sep="/"))) {
    dir.create(paste(path, "Graphics", sep="/"))  }
  if (!dir.exists(paste(path, "Graphics", name, sep="/"))) {
    dir.create(paste(path, "Graphics", name, sep="/"))  }
if (!dir.exists(paste(path, "Graphics", name, "Graphics", sep="/"))) {
dir.create(paste(path, "Graphics", name, "Graphics", sep="/"))  }


  if (Factor=="auto") {factor <- ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e11, 1e9, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e10, 1e8, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e8, 1e6, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e5, 1e5, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e4, 1e4, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e3, 1e3, 1))))))}


  scaling_factor <- ifelse(factor==1e6, ", in millions", ifelse(factor==1e9, ", in billions", ifelse(factor==1e3, ", in thousands", ifelse(factor==1e4, ", in ten thousands", ifelse(factor==1e5, ", in hundred thousands", ifelse(factor==1e8, ", in hundred millions", ""))))))



# Objects to be used for output
  # if (progressBar) {utils::setTxtProgressBar(pb, 4/21, label="Creating objects for output")}

  days = ifelse(is.null(daily.object$info[3]), 365, daily.object$info[3])
  b <- daily.object$output[,c(2,4,1)] %>% dygraphs::dygraph(main="Extended Original Series and Final Seasonally Adjusted Series") %>% dygraphs::dyRangeSelector() %>% dygraphs::dyShading(from=(stats::end(daily.object$output[,2])-as.numeric(days)), to=(stats::end(daily.object$output[,2])), color="#E0ECF8") %>% dygraphs::dyOptions(colors = c("black","#006474", "#DD6B00"), strokeWidth=1.2) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")

  # if (progressBar) {utils::setTxtProgressBar(pb, 4.4/21, label="Creating objects for output")}

  # htmlwidgets::saveWidget(b, paste(path, "Graphics", name, "Graphics", "finalplot.html", sep="/"))

  # if (progressBar) {utils::setTxtProgressBar(pb, 4.8/21, label="Creating objects for output")}

  a <- daily.object$output[,3] %>% dygraphs::dygraph(main="Final Seasonal and Calendar Factor") %>% dygraphs::dyRangeSelector() %>% dygraphs::dyShading(from=(stats::end(daily.object$output[,3])-365), to=(stats::end(daily.object$output[,3])), color="#E0ECF8") %>% dygraphs::dyOptions(colors = c("#6E6E6E")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")

  # if (progressBar) {utils::setTxtProgressBar(pb, 5.4/21, label="Creating objects for output")}

  # htmlwidgets::saveWidget(a, paste(path, "Graphics", name, "Graphics", "seasonal_factor.html", sep="/"))

  if(everyDay) {
    # if (progressBar) {utils::setTxtProgressBar(pb, 6/21, label="Main Results Table")}

    if(short) {
 a<- daily.object$output[paste("/", zoo::index(xts::last(daily.object$output$seas_adj[!is.na(daily.object$output$seas_adj)])), sep="")]} else {
   a <- daily.object$output[paste("/",xts::last(zoo::index(daily.object$output))-365, sep="")]
 }
    if (daily.object$info[1]=="Log") {
    b <- data.frame(round(a[,c(2,1)]/factor,1), round(a[,c(3)],2))} else {
      b <- data.frame(round(a[,c(2,1)]/factor,1), round(a[,c(3)]/factor,2))
    }

    # if (progressBar) {utils::setTxtProgressBar(pb, 7/21, label="Creating objects for output")}

    b[,c(1,2)] <- apply(b[,c(1,2)], 2, function(x) sprintf("%.1f",x))
    b[,c(3)] <- sprintf("%.2f",b[,c(3)])
    # df2HTML(b[rev(row.names(b)),], file=paste(path, "Graphics", name, "Graphics", "EveryDay.html", sep="/"))
    # R2HTML::HTMLCSS(paste(path, "Graphics", name, "Graphics", "EveryDay.html", sep="/"))

  }





    # if (progressBar) {utils::setTxtProgressBar(pb, 8/21, label="Final Seasonal factors graphs")}
    if (short) { endpoint <- base::as.Date(zoo::index(xts::last(daily.object$output$seas_adj[!is.na(daily.object$output$seas_adj)])))  } else {
      endpoint <- base::as.Date(xts::last(zoo::index(daily.object$output)))-365
    }

    if (daily.object$info[1]=="Log") {
    quasi_sfac1 <- daily.object$output[paste("/",endpoint, sep="")][,2]/daily.object$sa_result[paste("/",xts::last(zoo::index(daily.object$output))-365, sep="")][,1]*100} else{
      quasi_sfac1 <- daily.object$output[paste("/",endpoint, sep="")][,2] -daily.object$sa_result[paste("/",xts::last(zoo::index(daily.object$output))-365, sep="")][,1]}
    quasi_sfac1[format(zoo::index(quasi_sfac1), "%m-%d")=="02-29"] <- NA ### LÃ¶schen des 29.2, Langfristig Bessere LÃ¶sung notwendig

    monday <- quasi_sfac1[format(zoo::index(quasi_sfac1), "%w")=="1"]
    weeked <- xts::merge.xts(quasi_sfac1, monday) ; colnames(weeked) <- c("Implicit Sfac 1", "Monday")


    if (daily.object$info[1]=="Log") {
    quasi_sfac2 <- daily.object$sa_result[paste("/",endpoint, sep="")][,2]/daily.object$sa_result[paste("/",endpoint, sep="")][,3]*100} else {
      quasi_sfac2 <- daily.object$sa_result[paste("/",endpoint, sep="")][,2] - daily.object$sa_result[paste("/",endpoint, sep="")][,3]}
    startpoint <- xts::last(zoo::index(daily.object$output)) - 365 - (62+as.numeric(format(xts::last(zoo::index(daily.object$output)), "%d")))
    quasi_sfac2[format(zoo::index(quasi_sfac2), "%m-%d")=="02-29"] <- NA
    first <- quasi_sfac2[format(zoo::index(quasi_sfac2), "%d")=="01"]
    monthed <- xts::merge.xts(quasi_sfac2, first) ; colnames(monthed) <- c("Implicit Sfac 2", "FirstofMonth")


    if (daily.object$info[1]=="Log") {
    quasi_sfac3 <- daily.object$sa_result[paste("/",endpoint, sep="")][,3]/daily.object$sa_result[paste("/",endpoint, sep="")][,4]*100} else {
      quasi_sfac3 <- daily.object$sa_result[paste("/",endpoint, sep="")][,3] - daily.object$sa_result[paste("/",endpoint, sep="")][,4]}

    startpoint <- xts::last(zoo::index(daily.object$output)) - 365 - (365+as.numeric(format(xts::last(zoo::index(daily.object$output)), "%j")))
    colnames(quasi_sfac3) <- c("Implicit Sfac 3")

    # if(TRUE) {

    # ##  Graph for weekly seasonal
    #    weeked_graph <- weeked %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(endpoint-15, endpoint)) %>% dygraphs::dyOptions(drawPoints=TRUE, pointSize=3, colors=c("green", "#8E0C0C")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")

    # ##  Graph for monthly seasonal
    # if (short) {startpoint <- xts::last(zoo::index(daily.object$output)) - 365 - (62+as.numeric(format(base::as.Date(zoo::index(xts::last(monthed))), "%d")))
    # monthed_graph <- monthed %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(base::as.Date(zoo::index(xts::last(monthed)))-62, base::as.Date(zoo::index(xts::last(monthed))))) %>% dygraphs::dyOptions(drawPoints=TRUE, pointSize=3, colors=c("green", "#8E0C0C")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")
    # } else {
    #   monthed_graph <- monthed %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(startpoint, endpoint)) %>% dygraphs::dyOptions(drawPoints=TRUE, pointSize=3, colors=c("green", "#8E0C0C")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")}

    # ## Graph for annual seasonal
    #  if (short) {yeared_graph <- quasi_sfac3 %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(base::as.Date(zoo::index(xts::last(quasi_sfac3)))-366, base::as.Date(zoo::index(xts::last(quasi_sfac3))))) %>% dygraphs::dyOptions(strokeWidth=3, colors="green") %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")} else {
    # yeared_graph <- quasi_sfac3 %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(startpoint, endpoint)) %>% dygraphs::dyOptions(strokeWidth=3, colors="green") %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")}


    # htmlwidgets::saveWidget(weeked_graph, paste(path, "Graphics", name, "Graphics", "SeasonalS1.html", sep="/"))
    # htmlwidgets::saveWidget(monthed_graph, paste(path, "Graphics", name, "Graphics", "SeasonalS2.html", sep="/"))
    # htmlwidgets::saveWidget(yeared_graph, paste(path, "Graphics", name, "Graphics", "SeasonalS3.html", sep="/"))
    # }

    ts_c(
      seas_w = select(ts_pick(ts_tbl(weeked), 1), -id),
      seas_m = select(ts_pick(ts_tbl(monthed), 1), -id),
      seas_y = ts_tbl(quasi_sfac3)
    )
  }





