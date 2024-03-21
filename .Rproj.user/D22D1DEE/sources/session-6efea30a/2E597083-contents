
# setwd("/Users/davidvandijcke/Dropbox (University of Michigan)/Flo_GSRA/repo/DiSCos")
# devtools::load_all()


### Load data ####
conf <- list()
Sys.setenv("SPARK_MEM" = "70g")
conf$`sparklyr.shell.driver-memory` <- "50G"
conf$`spark.executor.memory` <- "10GB"
conf$`spark.driver.maxResultSize` <- "10GB"

spark_install(version=3.5)
sc <- spark_connect(master = "local", version=3.5, config=conf)



## load data and filter in spark before passing to data table
sparkdf <- spark_read_parquet(sc, name = "people_tenure_prepped",
                              path = file.path(dataIn, "people_tenure_prepped/*"),
                              memory = FALSE)

# change RTO_date_manual to May 1 2023 for COMPANY_NAME amazon
sparkdf <- sparkdf %>% mutate(RTO_date_manual = ifelse(COMPANY_NAME == "amazon", "May 1 2023", RTO_date_manual))
sparkdf <- sparkdf %>% mutate(naics_code = ifelse(COMPANY_NAME == "microsoft", 54151, naics_code))

# sparkdf <- sparkdf %>% mutate(RTO_date_manual = ifelse(COMPANY_NAME == "apple", "Apr 11 2022", RTO_date_manual))

sparkdf <- sparkdf %>% mutate(naics_2 = substr(naics_code, 1, 2))
sparkdf <- sparkdf %>% filter(naics_2 %in% c("51", "54", "33"))

## filter out layoffs
layoffs <- fread(file.path(dataOut, 'layoffs_matched_manual_v2.csv'))
layoffs <- layoffs %>% select(V1, Date)

rtocomp <- "microsoft"

rtodate <- sparkdf %>% filter(COMPANY_NAME == rtocomp) %>% select(RTO_date_manual) %>%
  distinct() %>% collect()
layoffcomp <- layoffs[V1 == rtocomp]

rtodate <- as.Date(rtodate$RTO_date_manual[1], format="%b %d %Y")

# assign startDate as 6 months before rtodate and endDate as 6 months after
firstDateOfMonth <- floor_date(rtodate, "month")
startDate <- firstDateOfMonth %m-% months(8)
endDate <- firstDateOfMonth %m+% months(6)

if (nrow(layoffcomp) > 0) {
  # if there are layoffs in this time window, restrict the sample period further
  layoffs_sub <- layoffcomp[as.Date(Date) %between% c(startDate, endDate)]
  # layoffs_sub <- layoffs[V1 == "netflix"][as.Date(Date) %between% c(startDate, endDate)]
  layoffs_sub <- layoffs_sub[, Date := floor_date(Date, "month")]
  if (nrow(layoffs_sub) > 0) {
    layoffs_sub[, startDiff := as.numeric(difftime(Date, firstDateOfMonth, units="days"))]
    if (any(layoffs_sub$startDiff %between% c(-70,50))) stop("Problematic layoffs in sample period") # skip this company if not at least 2 months
    if (!all(layoffs_sub$startDiff >= 0)) {
      startDate <- layoffs_sub[startDiff == max(layoffs_sub[startDiff < 0]$startDiff)]$Date %m+% months(1)
    }
    if (!all(layoffs_sub$startDiff < 0)) {
      endDate <- layoffs_sub[startDiff == min(layoffs_sub[startDiff > 0]$startDiff)]$Date %m-% months(1)
    }
  }
}

# now get companies with a layoff in that period
layoffs_filter <- unique(layoffs[Date %between% c(startDate, endDate)]$V1)

sparkdf <- sparkdf %>% filter(!COMPANY_NAME %in% layoffs_filter)

sparkdf <- sparkdf %>% filter((month >= startDate) & (month <= endDate))


# naics <- sparkdf %>% filter(COMPANY_NAME == "apple") %>% select(NAICS) %>% distinct()

# sparkdf <- sparkdf %>% filter(COMPANY_INDUSTRY == "computer software")

sparkdf <- sparkdf %>% select(ID, id_col, month, time_col, tenure, edu_days, START_DATE, END_DATE,
                              title_categorical, COMPANY_NAME, RTO_date_manual, naics_code)

# filter companies with at least 5% of RTO company's workforce
temp <- sparkdf %>%
  group_by(time_col, COMPANY_NAME) %>% summarise(nobs = n()) %>%
  group_by(COMPANY_NAME) %>% summarise(nobs = min(nobs)) %>% collect() %>% setDT()


ids <- temp[nobs > 0.02 * temp[COMPANY_NAME==rtocomp]$nobs]$COMPANY_NAME

# if (length(ids) < 20) next

sparkdf <- sparkdf %>% filter(COMPANY_NAME %in% ids)


ppl <- collect(sparkdf)

fwrite(ppl, file.path(dataOut, 'disco_prepped_microsoft.csv.gz'))



#----------------
# MICROSOFT
#----------------


# TENURE
#--------------

ppl <- fread(file.path(dataOut, 'disco_prepped_microsoft.csv.gz'))
ppl <- as.data.table(ppl)


ppl <- ppl[, date := as.Date(RTO_date_manual, format = "%b %d %Y")]
ppl <- ppl[date >= endDate | is.na(date) | COMPANY_NAME == rtocomp]


ppl[, y_col := tenure]


ppl <- ppl[, nobs_time := .N, by = c("id_col", "time_col")]
ppl <- ppl[, nobs := min(nobs_time), by = "id_col"]

test <- ppl

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

#-----
# try grouping by 4 months before and after
grouped <- test
grouped <- grouped[time_col %between% c(t0-8,t0+3)]
grouped[,time_col := findInterval(time_col, c(t0-8, t0-4, t0, t0+3))]
grouped <- grouped %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col))

# grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 2000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----

disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_max=0.9, G = 100, M=M, num.cores = 20,
                       permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE, seed=5, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="quantileDiff", graph=TRUE)


# TITLES
#------------


ppl <- fread(file.path(dataOut, 'disco_prepped_microsoft.csv.gz'))
ppl <- as.data.table(ppl)


ppl <- ppl[, date := as.Date(RTO_date_manual, format = "%b %d %Y")]
ppl <- ppl[date >= endDate | is.na(date) | COMPANY_NAME == rtocomp]

ppl[, y_col := title_categorical]

ppl <- ppl[, nobs_time := .N, by = c("id_col", "time_col")]
ppl <- ppl[, nobs := min(nobs_time), by = "id_col"]

test <- ppl

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

#-----
# try grouping by 4 months before and after
grouped <- test
grouped <- grouped[time_col %between% c(t0-8,t0+3)]
grouped[,time_col := findInterval(time_col, c(t0-8, t0-4, t0, t0+3))]
grouped <- grouped %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col))

# grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 2000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----

disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_max=0.9, G = 1000, M=M, num.cores = 20,
                       permutation = FALSE, CI = FALSE, boots = 1000, simplex=TRUE, seed=5, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="quantile", graph=TRUE)





printWeights <- function(disco) {
  # print weights
  df <- data.table(weights = disco$Weights_DiSCo_avg, id_col = disco$control_ids)
  temp <- unique(ppl, by=c("id_col", "COMPANY_NAME"))
  df <- df[temp[,c("id_col", "COMPANY_NAME")], on = "id_col"]

  # sort by weights
  df <- df[order(weights, decreasing = TRUE)]
  return(df)
}



prettyPlot <- s

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
ylim <- c(-200,100)
xlim <- NULL
cdf=FALSE
xlab <- "Quantile"
ylab="Change in Tenure (Days)"
obsLine = NULL
savePlots=FALSE
plotName=NULL
lty=1
lty_obs=1
t_plot = NULL

# Create a data frame for all data points
t_list <- rep(t_start:t_max, each = length(grid_cdf))

# instead of t_list, start at june 2022 and create list of months until may 2023
t_list <- seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "month")
# format as "Jan 2022" and add "(Start of Return to Office)" to September 2022
t_list <- format(t_list, "%b %Y")
t_list[4] <- paste0("<span style='color:red;'>", t_list[4], " (Start of Return to Office)", "</span>")

df <- data.frame(time = rep(t_list, each = length(grid_cdf)),
                 x = rep(grid_cdf, times = length(cdf_centered)),
                 y = unlist(cdf_centered),
                 ci_lower = if (CI) unlist(ci_lower) else NA,
                 ci_upper = if (CI) unlist(ci_upper) else NA,
                 obsLine = if (!is.null(obsLine)) unlist(obsLine) else NA)
df$time <- factor(df$time, levels = t_list)

if (!is.null(t_plot)) df <- df[df$time %in% t_plot,]

# Create a ggplot
p <- ggplot(df, aes(x = x)) +
  geom_line(aes(y = y), colour = "dodgerblue3", linetype = lty)

if (!is.null(obsLine)) p <- p + geom_line(aes(y = obsLine), colour = "dodgerblue3", linetype = lty_obs, show.legend = !is.null(obsLine))

p <- p +
  geom_hline(yintercept = 0, colour = "red") +
  labs(title = "", x = xlab, y = ylab) +
  theme_minimal() +
  coord_cartesian(xlim = xlim, ylim = ylim)
# # turn grid off
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#       panel.background = element_blank(), axis.line = element_line(colour = "black"))

clr <- "lightblue"
if (CI) p <- p + geom_line(aes(y = ci_lower), colour = clr, linetype = lty, show.legend = CI) +
  geom_line(aes(y = ci_upper), colour = clr, linetype = lty, show.legend = CI)

if (cdf) {
  p <- p + geom_hline(yintercept = 1, colour = "grey")
}

n_row <- data.table::uniqueN(df$time)
p <- p + facet_wrap(~time, ncol = 2, nrow = n_row) + theme(strip.text = ggtext::element_markdown())

ggsave(file.path(figs, "microsoft_main_dist.pdf"), p, width = 5, height = 7,  dpi = 300)






## mixture of distributions
#-----
# try grouping by 4 months before and after
t0 <- unique(test[month == firstDateOfMonth]$time_col)
grouped <- test
grouped <- grouped[time_col %between% c(t0-8,t0+3)]
grouped[,time_col := findInterval(time_col, c(t0-8, t0-4, t0, t0+3))]
grouped <- grouped %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col))

grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]
grouped <- grouped[(grouped$y_col <= 6),] # for tenure only!
t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----

disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_max=1,
               G = 1000, M=M, num.cores = 10,
               permutation = FALSE, CI = FALSE, boots = 500, simplex=TRUE,
               seed=5, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE)


df <- data.frame(time = numeric(), diff = numeric(), x=numeric())

for (t in seq(1,3)) {
  grid <- seq(0,6) # disco$results.periods[[t]]$target$grid
  targetq <- stats::ecdf(disco$results.periods[[t]]$target$quantiles)(grid)


  controls <- disco$results.periods[[t]]$controls$quantiles
  # Estimating the empirical CDFs
  CDF.control <- apply(controls, 2, stats::ecdf)

  # Evaluating the CDF on the random grid
  CDF.matrix <- matrix(0,nrow=length(grid), ncol = (ncol(controls)))
  for (ii in 1:ncol(controls)){
    CDF.matrix[,(ii)] <- CDF.control[[ii]](grid)
  }

  mixq <- CDF.matrix %*% disco$Weights_mixture_avg
  diff <- targetq - mixq


  mixlist <- data.frame(time = t, diff = diff, target=targetq, control=mixq, x = grid)
  df <- rbind(df, mixlist)
}
df <- setDT(df)

df[, categorical := floor(x)]
poop <- df[, .(diff = mean(diff)), by = c("time", "categorical")]

ggplot(df, aes(x=x, y=diff)) + geom_line() +
  #geom_line(aes(x=x, y=control,linetype="dashed")) +
  facet_wrap(~time, ncol = 1, nrow = 3)

ggplot(df, aes(x=x, y=diff)) + geom_line(aes(x=x, y=target)) +
  geom_line(aes(x=x, y=control), linetype="dashed") +facet_wrap(~time, ncol = 1, nrow = 3)


title_ranking_inverted <- c(
  CXO = 10,
  VP = 9,
  Director = 8,
  Partner = 7,
  Owner = 6,
  Senior = 5,
  Manager = 4,
  Entry = 3,
  Training = 2,
  Unpaid = 1
)

