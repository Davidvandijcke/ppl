#### Prepare paths and package ####

packages_load <- c("data.table", "DiSCos", "here", "ggtext")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = packages_load, character.only = TRUE)

codeDir <- here::here()

setwd(codeDir) # sets cd to program directory

dir <- sub("/[^/]+$", "", codeDir)# get main directory
dataIn <- file.path(dir, "data", "in")
dataOut <- file.path(dir, "data", "out")
tabs <- file.path(dir, "results", "tabs")
figs <- "/Users/davidvandijcke/Dropbox (University of Michigan)/Apps/Overleaf/RTO/figs"



### Load data ####

ppl <-fread(file.path(dataIn, "people_tenure_prepped.csv.gz"))

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

setwd("/Users/davidvandijcke/Dropbox (University of Michigan)/Flo_GSRA/repo/DiSCos")

# ppl <- ppl[, c("id_col", "time_col", "tenure")]
ppl[, y_col := tenure]


### specification that works
ppl <- ppl[time_col %between% c(6, 16)]

ppl <- ppl[COMPANY_NAME != "google"]
ppl <- ppl[id_col <= 24] # other companies don't have many employees

disco <- DiSCo(ppl, id_col.target = 3, t0 = 9, q_max=0.95, G = 1000, num.cores = 5,
               permutation = TRUE, CI = TRUE, boots = 500, simplex=FALSE, seed=5)
discot <-  DiSCoTEA(disco,  agg="quantileDiff", graph=TRUE, ylim=c(-150, 50))
ppl[, quantile := data.table::frank(y_col, ties.method = "average") / .N, by = id_col]


# print weights
df <- data.table(weights = disco$Weights_DiSCo_avg, id_col = disco$control_ids)

temp <- unique(ppl, by=c("id_col", "COMPANY_NAME"))
df <- df[temp[,c("id_col", "COMPANY_NAME")], on = "id_col"]



# sort by weights
df <- df[order(weights, decreasing = TRUE)]






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
t_list <- seq(as.Date("2022-06-01"), as.Date("2023-04-01"), by = "month")
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

ggsave(file.path(figs, "apple_main_dist.pdf"), p, width = 5, height = 7,  dpi = 300)




### same with simplex
ppl <- ppl[time_col %between% c(6, 16)]

ppl <- ppl[COMPANY_NAME != "google"]
ppl <- ppl[id_col <= 24] # other companies don't have many employees

disco <- DiSCo(ppl, id_col.target = 3, t0 = 9, q_max=0.95, G = 1000, num.cores = 5,
               permutation = TRUE, CI = TRUE, boots = 500, simplex=FALSE, seed=5)
discot <-  DiSCoTEA(disco,  agg="quantileDiff", graph=TRUE, ylim=c(-150, 50))
ppl[, quantile := data.table::frank(y_col, ties.method = "average") / .N, by = id_col]


# unrestricted
disco <- DiSCo(ppl, id_col.target = 3, t0 = 9, q_max=1, G = 1000, num.cores = 5,
               permutation = TRUE, CI = TRUE, boots = 500, simplex=FALSE, seed=5)
discot <-  DiSCoTEA(disco,  agg="quantileDiff", graph=TRUE, ylim=c(-150, 50))



