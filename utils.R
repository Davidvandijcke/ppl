
prettyPlot <- function(disco, discot, t_list, ylim=c(-100,50), ylab="Change in Tenure (Days)", xlab = "Quantile", lw=1) {
  # plot nicer figure
  # reconstruct some parameters
  df <- disco$params$df
  t_max <- max(df$time_col)
  t_min <- min(df$time_col)
  t0 <- disco$params$t0
  T0 <- unique(df[time_col == t0]$time_col)  - t_min
  T_max <- max(df$time_col) - t_min + 1
  CI <- disco$params$CI
  cl <- disco$params$cl
  evgrid = seq(from=0,to=1,length.out=disco$params$G+1)
  qmethod <- disco$params$qmethod
  q_min <- disco$params$q_min
  q_max <- disco$params$q_max

  t_start <- t_min
  T_start <- 1

  cdf_centered = discot$treats
  grid_cdf <- discot$grid
  ci_lower <- discot$ci_lower
  ci_upper <- discot$ci_upper
  xlim <- NULL
  cdf=FALSE
  obsLine = NULL
  savePlots=FALSE
  plotName=NULL
  lty=1
  lty_obs=1
  t_plot = NULL


  # format as "Jan 2022" and add "(Start of Return to Office)" to September 2022
  t_list <- format(t_list, "%b %Y")
  t_list[length(t_list)] <- paste0("<span style='color:red;'>", t_list[length(t_list)], " (RTO)", "</span>")

  df <- data.frame(time = rep(t_list, each = length(grid_cdf)),
                   x = rep(grid_cdf, times = length(cdf_centered)),
                   y = unlist(cdf_centered),
                   ci_lower = if (CI) unlist(ci_lower) else NA,
                   ci_upper = if (CI) unlist(ci_upper) else NA,
                   obsLine = if (!is.null(obsLine)) unlist(obsLine) else NA)
  df$time <- factor(df$time, levels = t_list)

  if (!is.null(t_plot)) df <- df[df$time %in% t_plot,]

  # Create a ggplot
  p <- ggplot(df, aes(x = unlist(df$x))) +
    geom_line(aes(y = unlist(df$y)), colour = "dodgerblue3", linetype = lty, linewidth=lw)

  if (!is.null(obsLine)) p <- p + geom_line(aes(y = obsLine), colour = "dodgerblue3", linetype = lty_obs, show.legend = !is.null(obsLine))

  p <- p +
    geom_hline(yintercept = 0, colour = "black", linetype="dashed") +
    labs(title = "", x = xlab, y = ylab) +
    theme_minimal() +
    coord_cartesian(xlim = xlim, ylim = ylim)
  # # turn grid offs
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black"))

  clr <- "lightblue"
  if (CI) p <- p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "lightblue", alpha = .5)

  if (cdf) {
    p <- p + geom_hline(yintercept = 1, colour = "grey")
  }

  n_row <- data.table::uniqueN(df$time)
  clr <- "darkgrey"
  p <- p + facet_wrap(~time, ncol = 2, nrow = n_row) + theme(strip.text = ggtext::element_markdown(size=8),
                                                             panel.grid.major.x = element_blank(),
                                                             panel.grid.minor.x = element_blank(),
                                                             text = element_text(family = "serif"))
  return(p)
}




prettyHist <- function(disco, discot, t_list, ylim=c(-100,50), ylab="Change in Tenure (Days)",
                       xlab = "Quantile", lw=1, ncol=2, factor=FALSE) {
  # plot nicer figure
  # reconstruct some parameters
  df <- disco$params$df
  t_max <- max(df$time_col)
  t_min <- min(df$time_col)
  t0 <- disco$params$t0
  T0 <- unique(df[time_col == t0]$time_col)  - t_min
  T_max <- max(df$time_col) - t_min + 1
  CI <- disco$params$CI
  cl <- disco$params$cl
  evgrid = seq(from=0,to=1,length.out=disco$params$G+1)
  qmethod <- disco$params$qmethod
  q_min <- disco$params$q_min
  q_max <- disco$params$q_max

  t_start <- t_min
  T_start <- 1

  cdf_centered = discot$treats
  grid_cdf <- discot$grid
  ci_lower <- discot$ci_lower
  ci_upper <- discot$ci_upper
  xlim <- NULL
  cdf=FALSE
  obsLine = NULL
  savePlots=FALSE
  plotName=NULL
  lty=1
  lty_obs=1
  t_plot = NULL


  # format as "Jan 2022" and add "(Start of Return to Office)" to September 2022
  t_list <- format(t_list, "%b %Y")
  t_list[length(t_list)] <- paste0("<span style='color:red;'>", t_list[length(t_list)], " (RTO)", "</span>")

  df <- data.frame(time = rep(t_list, each = length(grid_cdf)),
                   x = rep(grid_cdf, times = length(cdf_centered)),
                   y = unlist(cdf_centered),
                   ci_lower = if (CI) unlist(ci_lower) else NA,
                   ci_upper = if (CI) unlist(ci_upper) else NA,
                   obsLine = if (!is.null(obsLine)) unlist(obsLine) else NA)
  df$time <- factor(df$time, levels = t_list)

  if (!is.null(t_plot)) df <- df[df$time %in% t_plot,]

  if (factor) {
    p <-ggplot(df, aes(x = as.factor(x), y = y))
  } else {
    p <- ggplot(df, aes(x = x, y = y))
  }
  p <- p +
    geom_histogram(stat="identity", fill = "lightblue", alpha=0.7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, color = "dodgerblue3", alpha=0.7, linewidth=0.6) +
    facet_wrap(~time, ncol = ncol) +  # Adjust ncol as needed
    labs(title = "", x = xlab, y = ylab) +
    theme_minimal() +
    theme(strip.text = ggtext::element_markdown(size=9),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(family = "serif"))
  return(p)
}




## print weights
printWeights <- function(disco) {
  # print weights
  df <- data.table(weights = disco$Weights_DiSCo_avg, id_col = disco$control_ids)
  temp <- unique(ppl, by=c("COMPANY_NAME", "id_col"))
  df <- df[temp[,c("COMPANY_NAME", "id_col")], on = "id_col"]

  # sort by weights
  df <- df[,-c("id_col")]
  df <- df[order(weights, decreasing = TRUE)]
  colnames(df) <- c("Weight", "Company")
  df <- df[,c("Company", "Weight")]
  df[, Company := stringr::str_to_title(Company)]
  return(df)
}




robustness <- function(rtocomp) {


  fn <- file.path(dataOut, paste0('disco_prepped_', rtocomp, '.csv.gz'))
  if (!file.exists(fn)) {
    print("Problematic layoff company")
    return()
  }
  ppl <- fread(fn)
  ppl <- as.data.table(ppl)

  # check that company has enough data points
  check <- ppl[COMPANY_NAME == rtocomp, .(cnt = .N), by = c("COMPANY_NAME", "month")]
  mincheck <- min(check$cnt)

  if (mincheck < 100) {
    return("Less than 100 workers per month")
  }
  # # double check condition on number of employees relative to target
  # check <-  ppl[, .(cnt = .N), by = c("COMPANY_NAME", "month")]
  # dropcomps <- unique(check[cnt < 0.05*mincheck]$COMPANY_NAME)
  # ppl <- ppl[!COMPANY_NAME %in% dropcomps]

  rtodate <- ppl %>% filter(COMPANY_NAME == rtocomp) %>% select(RTO_date_manual) %>%
    distinct()
  rtodate <- as.Date(rtodate$RTO_date_manual[1], format="%b %d %Y")

  # assign startDate as 6 months before rtodate and endDate as 6 months after
  firstDateOfMonth <- floor_date(rtodate, "month")
  startDate <- firstDateOfMonth %m-% months(8)
  endDate <- firstDateOfMonth %m+% months(6)


  # only keep companies that are never-treated or treated 3 months after sample ends
  ppl <- ppl[, date := as.Date(RTO_date_manual, format = "%b %d %Y")]
  ppl <- ppl[date >= endDate | is.na(date) | COMPANY_NAME == rtocomp]

  ppl <- ppl[, nobs_time := .N, by = c("id_col", "time_col")]
  ppl <- ppl[, nobs := min(nobs_time), by = "id_col"]

  test <- ppl

  id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
  t0 <- unique(test[month == firstDateOfMonth]$time_col)

  if ((min(test$time_col, na.rm=TRUE) > t0-6) | (max(test$time_col, na.rm=TRUE) < t0+2)) {
    print("Not enough time periods")
    return()
  }

  #-----

  t_min <- 6
  t_int <- c(t0-6, t0-3, t0, t0+3)

  grouped <- test
  grouped[, y_col := tenure]
  grouped <- grouped[time_col %between% c(t0-t_min,t0+2)]
  grouped[,time_col := findInterval(time_col, t_int)]
  grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

  grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]

  if (t_min == 3) {
    t0 <- 2
  } else {
    t0 <- 3
  }

  M <- max(c(uniqueN(grouped$id_col) * 3, 1000))
  #-----

  # check that there are not more controls than data points
  check <- grouped[id_col == id_col.target, .(cnt = .N), by = c("id_col", "time_col")]
  if (uniqueN(grouped$id_col) > min(check$cnt)) {
    id_sample <- unique(grouped[id_col != id_col.target]$id_col)
    id_sample <- sample(id_sample, round(0.09*min(check$cnt)))
    grouped <- grouped[id_col %in% c(id_sample, id_col.target)]
  }

  ## full distribution
  disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0, q_max=0.9, G=25, M=M, num.cores = 20,
                         permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE, seed=31, qmethod=NULL) # seed 5
  summary(disco$perm)
  discot <-  DiSCoTEA(disco,agg="quantileDiff", graph=TRUE)
  sign <- (disco$perm$p_overall < 0.1)
  save(disco, file = file.path(mods, paste0(rtocomp, "_model.RData")))

  #discot <- load(file.path(mods, paste0(rtocomp, "_model.RData")))
  discot <-  DiSCoTEA(disco,agg="quantileDiff", graph=TRUE)


  weightsdf <- printWeights(disco)
  tab <- xtable(weightsdf[1:5], digits=4)
  print.xtable(tab,  include.rownames=FALSE, floating=FALSE,file=file.path(tabs, paste0("weights_", rtocomp, ".tex")))


  ## plot figure
  ttemp <- unique(grouped$month)
  t_list <- seq(min(ttemp), max(ttemp), by = "quarter")
  p <- prettyPlot(disco, discot, t_list=t_list)
  p
  ggsave(file.path(figs, paste0(rtocomp, "_main_dist_", sign, ".pdf")), p, width = 5, height = 5,  dpi = 300)


  ## mixture of distributions
  #-----
  # try grouping by 4 months before and after
  t0 <- unique(test[month == firstDateOfMonth]$time_col)
  # grouped <- test[(END_DATE >= (as.Date(month) %m+% months(1))) | (is.na(END_DATE))] # end date not in month
  grouped <- test
  grouped[, y_col := title_categorical]
  grouped <- grouped[time_col %between% c(t0-6,t0+2)]
  grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
  grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

  grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]
  grouped <- grouped[(grouped$y_col %between% c(1,6)),]
  t0 <- 3


  # check that there are not more controls than data points
  check <- grouped[id_col == id_col.target, .(cnt = .N), by = c("id_col", "time_col")]
  mincheck <- min(check$cnt)
  if (uniqueN(grouped$id_col) > mincheck) {
    id_sample <- unique(grouped[id_col != id_col.target]$id_col)
    id_sample <- sample(id_sample, round(0.9*min(check$cnt)))
    grouped <- grouped[id_col %in% c(id_sample, id_col.target)]
  }
  # remove some controls that have really low N in some months
  check <-  grouped[, .(cnt = .N), by = c("id_col", "time_col")]
  dropcomps <- unique(check[cnt < 10]$id_col)
  grouped <- grouped[!id_col %in% dropcomps]

  M <- 2000 # max(c(uniqueN(ppl$id_col) * 1.5, 1000))
  #-----

  # full distribution
  disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
                 G = 1000, M=M, num.cores = 20, mixture=TRUE, grid.cat = seq(1,6),
                 permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                 seed=31, qmethod=NULL)
  save(disco, file = file.path(mods, paste0(rtocomp, "_model_titles.RData")))

  summary(disco$perm)
  discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE)

  sign <- (disco$perm$p_overall < 0.1)

  # restricted permutation test
  grouped <- grouped[(grouped$y_col %between% c(2,5)),]
  disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
                 G = 1000, M=M, num.cores = 1, mixture=TRUE, grid.cat = seq(2,5),
                 permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE,
                 seed=31, qmethod=NULL)
  save(disco, file = file.path(mods, paste0(rtocomp, "_model_titles_restricted.RData")))

  summary(disco$perm)


  # print weights
  disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
  weightsdf <- printWeights(disco)
  tab <- xtable(weightsdf[1:5], digits=4)
  print.xtable(tab,  include.rownames=FALSE, floating=FALSE,file=file.path(tabs, paste0("weights_", rtocomp, "_titles.tex")))


  ttemp <- unique(grouped$month)
  t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
  p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="Title Level", ylab="Change in CDF", ncol=1)
  p <- p + scale_x_continuous(breaks=seq(1,6))
  ggsave(file.path(figs, paste0(rtocomp, "title_dist", sign, ".pdf")), p, width = 4, height = 6,  dpi = 300)

}

