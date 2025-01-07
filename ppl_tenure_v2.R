#----------------------------
### Cross-company descriptives
#----------------------------

fn <- file.path(dataOut, paste0('mainfirms.csv.gz'))
# List all files in the directory
files <- list.files(fn, full.names = TRUE, pattern=".csv")

mainfirms <- fread(files)

## merge in layoffs
layoffs <- fread(file.path(dataOut, 'layoffs_matched_manual_v2.csv'))
layoffs <- layoffs %>% select(V1, Date)
layoffs[, month := floor_date(Date, "month")]
layoffs[, layoff := 1]
setnames(layoffs, c("V1", "Date"), c("COMPANY_NAME", "layoffdate"))
mainfirms <- layoffs[mainfirms, on = c("month", "COMPANY_NAME")]
mainfirms[is.na(layoff), layoff := 0]

# Convert your dataframe to a data.table if it's not already
df <- data.table(mainfirms)

# Convert date columns to Date class
df[, layoffdate := as.Date(layoffdate)]
df[, month := as.Date(month)]
df[, rtodate := as.Date(rtodate)]

# Updated function to process each company's data separately
critical_layoff <- function(layoffdates, rtodate, dates) {
  # If there are no layoffs or they are all NA, return NA
  if (all(is.na(layoffdates))) return(NA)

  # Get only the valid layoff dates
  valid_layoffdates <- na.omit(layoffdates)


  predate <- max(valid_layoffdates[valid_layoffdates < rtodate], na.rm=TRUE)
  postdate <- min(valid_layoffdates[valid_layoffdates >= rtodate], na.rm=TRUE)
  return(((dates < predate) & (dates < rtodate)) |
           ((dates > postdate) & (dates >= rtodate))
  )
}

# Apply function by group and create a new column for critical date
df[, dropdate := critical_layoff(layoffdate, rtodate, month), by = COMPANY_NAME]
df[is.na(dropdate), dropdate := FALSE]
df <- df[dropdate == FALSE]

mainfirms <- df
mainfirms[, pre := month < rtodate]
mainfirms[, pre_and_post := uniqueN(pre), by=c("COMPANY_NAME")]
mainfirms <- mainfirms[pre_and_post == 2]
mainfirms <- mainfirms[, cnt := .N, by=c("COMPANY_NAME", "pre")]
mainfirms <- mainfirms[cnt > 2000]
mainfirms[, max_tenure := max(floor(tenure_full / 365)), by=c("COMPANY_NAME")]

aggfirms <- mainfirms[,.(tenure_full = max(tenure_full), title_categorical=max(title_categorical)),
                  by=c("COMPANY_NAME", "ID", "pre")]
aggfirms[, tenure_year := floor(tenure_full / (365))]
# aggfirms <- aggfirms[tenure_full < quantile(tenure_full, 0.9)]

evgrid <- seq(0,1,0.1)

df <- data.table(quantiles=NA, comp=NA, q_diff=NA)
df_cdf <- data.table(quantiles=NA, comp=NA, q_diff=NA)

for (comp in unique(aggfirms$COMPANY_NAME)) {
  # tenure
  temp <- aggfirms[COMPANY_NAME == comp]
  temp_tenure <- temp[tenure_full < quantile(tenure_full, 0.9)]
  q_pre <- stats::quantile(temp_tenure[pre == TRUE]$tenure_full, evgrid)
  q_post <- stats::quantile(temp_tenure[pre == FALSE]$tenure_full, evgrid)
  q_diff <- q_post - q_pre
  dftemp <- data.table(quantiles = evgrid, q_diff = q_diff, comp=comp)
  df <- rbind(df, dftemp)

  # # tenure
  # temp <- aggfirms[COMPANY_NAME == comp]
  # ten_grid <- seq(0,10)
  # temp_tenure <- temp[tenure_year <= 15]
  # q_pre <- stats::ecdf(temp_tenure[pre == TRUE]$tenure_year)(ten_grid)
  # q_post <- stats::ecdf(temp_tenure[pre == FALSE]$tenure_year)(ten_grid)
  # q_diff <- q_post - q_pre
  # dftemp <- data.table(quantiles = ten_grid, q_diff = q_diff, comp=comp)
  # df <- rbind(df, dftemp)

  # title
  grid <- seq(1,10)
  q_pre <- stats::ecdf(temp[pre == TRUE]$title_categorical)(grid)
  q_post <- stats::ecdf(temp[pre == FALSE]$title_categorical)(grid)
  q_diff <- q_post - q_pre
  dftemp <- data.table(quantiles = grid, q_diff = q_diff, comp=comp)
  df_cdf <- rbind(df_cdf, dftemp)
}
cdf_grouped <- df_cdf[,.(q_diff = mean(q_diff)), by=c("quantiles")]

ggplot(df_cdf, aes(x=quantiles, y=q_diff, color=comp, fill=comp)) + geom_col(position="dodge") +
  scale_x_continuous(breaks=seq(1,10),
                     labels=list("1" = "Unpaid", "2"="Training",
                                 "3" = "Entry", "4"="Manager",
                                 "5"="Senior", "6"="Owner", "7"="Partner",
                                 "8"="Director", "9"="VP", "10"="CXO")) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, size=28, vjust=0.5))


# ggplot(df, aes(x=quantiles, y=q_diff, color=comp, fill=comp)) + geom_col(position="dodge") +
#   theme_minimal() +
#   theme(legend.title=element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.text.x = element_text(angle = 90, size=28, vjust=0.5))

ggplot(df, aes(x=quantiles, y=q_diff, color=comp, fill=comp)) + geom_line() +
  geom_hline(yintercept=0, linetype="dashed")




#----------------------------
### Company-by-company analysis ####
#----------------------------


#----------------
# MICROSOFT
#----------------

rtocomp <- "microsoft"
fn <- file.path(dataOut, paste0('disco_prepped_next_', rtocomp, '.csv.gz'))
# List all files in the directory
files <- list.files(fn, full.names = TRUE, pattern=".csv")

ppl <- fread(files)

rtodate <- ppl %>% filter(COMPANY_NAME == rtocomp) %>% select(RTO_date_manual) %>%
  distinct()
rtodate <- as.Date(rtodate$RTO_date_manual[1], format="%b %d %Y")

# assign startDate as 6 months before rtodate and endDate as 6 months after
firstDateOfMonth <- floor_date(rtodate, "month")
startDate <- firstDateOfMonth %m-% months(6)
endDate <- firstDateOfMonth %m+% months(3)


ppl <- ppl[, date := as.Date(RTO_date_manual, format = "%b %d %Y")]
ppl <- ppl[date >= endDate | is.na(date) | COMPANY_NAME == rtocomp]


ppl <- ppl[, nobs_time := .N, by = c("id_col", "time_col")]
ppl <- ppl[, nobs := min(nobs_time), by = "id_col"]
ppl[, leaver := (as.Date(END_DATE) > as.Date(month)) & (as.Date(END_DATE) <= as.Date(month) %m+% months(1))]
ppl[is.na(leaver), leaver:=FALSE]
ppl[, leaver_true := leaver & (COMPANY_NAME != COMPANY_NAME_next | is.na(COMPANY_NAME_next))]

test <- ppl



#### SUMMARY STATISTICS (for quarter before RTO) ####
#--------------
grouped <- test
t0 <- unique(test[month == firstDateOfMonth]$time_col)
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[time_col == 2]
grouped[, stayer := as.numeric(COMPANY_NAME == COMPANY_NAME_next)]
grouped[is.na(leaver), leaver:=FALSE]
grouped[, leaver := max(leaver), by="ID"]
grouped[, actual_leaver := leaver*(1-stayer)]
grouped[, small := EMPLOYEE_COUNT_next <= 50]
grouped <- grouped[!duplicated(ID)]
sum <- grouped[, .(
  `Median Tenure` = median(as.numeric(tenure_full), na.rm=TRUE),
  `Median Title` = median(as.numeric(title_categorical), na.rm=TRUE),
  `Number of Resumes` = .N,
  `Quarterly Turnover` = mean(actual_leaver,na.rm=TRUE),
  `Share Female` = mean(gender=="female",na.rm=TRUE),
  `Share Leaving to Startups` = mean(small, na.rm=TRUE)),
  by=c("id_col")
]
stargazer(sum[,-c("id_col")], out = file.path(tabs, 'sumstats.tex'),
          float=FALSE, summary.stat=c("mean", "min", "p25", "median", "p75", "max"))


#### SUMMARY STATISTICS on COVERAGE (for quarter before RTO) ####

grouped <- test
grouped$year <- year(grouped$month)
grouped <- grouped[year == 2022]
grouped <- grouped[, .(emp_count = uniqueN(ID)), by="COMPANY_NAME"]

truth <- fread(file.path(dataIn, "msft_sample_true_headcount_estimates.csv"))
colnames(truth) <- c("COMPANY_NAME", "emp_truth")
truth[, COMPANY_NAME := tolower(COMPANY_NAME)]

grouped <- truth[grouped, on = "COMPANY_NAME"]
grouped[, emp_truth := as.numeric(gsub(",", "", emp_truth))]

grouped[, `Employee Coverage` := emp_count / emp_truth]

cat("Employee Coverage in Microsoft Sample:")
summary(grouped$`Employee Coverage`)

# now specifically for microsoft, apple and spacex
fn <- file.path(dataOut, paste0('mainfirms.csv.gz'))
files <- list.files(fn, full.names = TRUE, pattern=".csv")
grouped <- fread(files)
grouped$year <- year(grouped$month)
grouped <- grouped[year == 2022]
grouped <- grouped[, .(emp_count = uniqueN(ID)), by="COMPANY_NAME"]
grouped <- truth[grouped, on = "COMPANY_NAME"]
grouped[, emp_truth := as.numeric(gsub(",", "", emp_truth))]
grouped[, `Employee Coverage` := emp_count / emp_truth]


sprintf("Employee coverage: Microsoft %f, Apple %f, SpaceX %f",
              grouped[COMPANY_NAME == "microsoft"]$`Employee Coverage`,
              grouped[COMPANY_NAME == "apple"]$`Employee Coverage`,
              grouped[COMPANY_NAME == "spacex"]$`Employee Coverage`)



#### NETWORK GRAPH (for quarter before RTO) ####
#-----------
grouped <- test[leaver==TRUE]
grouped <- grouped[COMPANY_NAME != COMPANY_NAME_next]
t0 <- unique(test[month == firstDateOfMonth]$time_col)
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
grouped <- grouped[!duplicated(ID, time_col, id_col)]
grouped <- grouped[COMPANY_NAME_next != ""]
grouped <- grouped[time_col == 3 & id_col == id_col.target]
dt <- grouped[, .(cnt = median(tenure_full)), by=c("COMPANY_NAME", "COMPANY_NAME_next")]
dt[, cnt := cnt/365]
#dt <- dt[cnt >1]

dt[, COMPANY_NAME_next := str_to_title(COMPANY_NAME_next)]
dt[COMPANY_NAME_next == "At A Tech Start-Up", COMPANY_NAME_next := "Tech Startup"]

fwrite(dt, file.path(dataOut, 'BFI_network_data_post.csv'))

dt <- fread(file.path(dataOut, 'BFI_network_data.csv'))
edge_list <- data.frame(from = dt$COMPANY_NAME, to = dt$COMPANY_NAME_next, weight = dt$cnt)



# Create a graph object from the edge list
graph <- graph_from_data_frame(edge_list, directed = TRUE)

# Calculate vertex sizes based on 'cnt' values (you may need to adjust this)
vertex_sizes<- c(0, dt$cnt)
names(vertex_sizes) <- V(graph)$name


# Generate an initial layout
layout <- layout_with_fr(graph, area=vcount(graph)^1)

# Identify the central vertex
central_vertex_name <- "microsoft" # replace with the actual name of your central vertex
central_vertex_id <- which(V(graph)$name == central_vertex_name)

# Center the layout on the central vertex
layout <- layout - layout[central_vertex_id, ]
layout[central_vertex_id, ] <- c(0, 0) # Fix the central vertex at (0,0)

# Scale vertices based on their size
for(i in seq_len(vcount(graph))) {
  if(i != central_vertex_id) { # Avoid moving the central vertex
    distance_factor <- sqrt(vertex_sizes[i]) # Scale factor based on size (sqrt for less dramatic effect)
    layout[i, ] <- layout[i, ] * distance_factor
  }
}
V(graph)$frame.color <- "white"
V(graph)$color <-  adjustcolor("dodgerblue", alpha.f = 0.5)
#V(graph)$label <- ""
size_threshold <- quantile(vertex_sizes, 0.85)
# Apply labels to vertices that are larger than the threshold
V(graph)$label[V(graph)$name %in% names(vertex_sizes[vertex_sizes > size_threshold])] <- V(graph)$name[V(graph)$name %in% names(vertex_sizes[vertex_sizes > size_threshold])]
E(graph)$arrow.mode <- 0

# Plot the graph
par(mar = c(0, 0, 0, 0)+0.1)
pdf(file=file.path(figs, "network_pre.pdf"))
plot(graph, layout=layout, vertex.size=vertex_sizes,
     vertex.label.color="black",
     vertex.label.dist=1.5, margin=0)
dev.off()



#### TENURE ####
#--------------


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

#-----
# try grouping by 4 months before and after
grouped <- test
grouped[, leaver := (as.Date(END_DATE) >= as.Date(month)) & (as.Date(END_DATE) < as.Date(month) %m+% months(1))]
grouped[is.na(leaver), leaver:=FALSE]
grouped[, leaver_true := leaver & (COMPANY_NAME != COMPANY_NAME_next | is.na(COMPANY_NAME_next))]
grouped[, y_col := tenure_full]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month),
                       leaver = max(leaver_true)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))
#grouped <- grouped[leaver == FALSE]
grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
print(paste0("N=", nrow(grouped)))
print(paste0("J=", uniqueN(grouped$id_col)))

check <- grouped[, .(cnt =.N), by=c("id_col", "time_col")]

## full distribution
G <- 10
disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_max=0.9, G = G, M=M, num.cores=20,
               cl=0.95,uniform=TRUE, permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE, seed=30, qtype=7,
               qmethod=NULL) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="quantileDiff", graph=TRUE)



weightsdf <- printWeights(disco)
tab <- xtable(weightsdf[1:10], digits=4)
print.xtable(tab,  include.rownames=FALSE, floating=FALSE,file=file.path(tabs, "weights_microsoft.tex"))


## plot figure
ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp), max(ttemp), by = "quarter")
p <- prettyPlot(disco, discot, t_list=t_list, ylim = c(-150, 100), ncol=1)
ggsave(file.path(figs, "microsoft_main_dist.pdf"), p, width = 3, height = 6,  dpi = 300)



## pre and post observed pdf
db <- '#003E4B'
lb <-  '#70ADC9'

dfplot <- grouped[time_col %between% c(2,3) & id_col == id_col.target]
dfplot[, quantile := data.table::frank(y_col, ties.method = "average") / .N, by = c("id_col", "time_col")]
dfplot <- dfplot[(quantile <= 0.9)]
dfplot[, time_col := factor(ifelse(time_col == 2, "Pre", "Post"), levels=c("Pre", "Post"))]

p<-ggplot(dfplot, aes(x=y_col, fill=time_col, color=time_col, group=time_col)) +
  geom_density(alpha=0.2, bw=90, kernel="epanechnikov", position="identity") + xlim(c(0,4000)) + theme_minimal() +
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size=26),
        text = element_text(family = "serif", size=28)) +
  scale_fill_manual(values = c( "Post" = db, "Pre" = lb)) +
  scale_color_manual(values = c( "Post" = db, "Pre" = lb)) +
  xlab("Tenure (Days)") +
  ylab("Density")
p
ggsave(file.path(figs, "microsoft_desc_pdf.pdf"), p, width = 12, height = 10,  dpi = 300)


## pre and post observed quantile
discoplot <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_max=0.9, G = 100, M=M, num.cores = 20,
               permutation = FALSE, CI = FALSE, boots = 1000, simplex=TRUE, seed=31, qmethod=NULL)
dfplot <- data.table("quantile" = unlist(lapply(2:3, function(x) discoplot$results.periods[[x]]$target$quantiles)),
                     "grid" = rep(discoplot$evgrid, 2),
                     "time" = rep(2:3, each=discoplot$params$G+1))
dfplot <- dfplot %>%
  mutate(Change = c(FALSE, diff(quantile) != 0)) %>%
  filter(Change)
dfplot[, time := factor(ifelse(time == 2, "Pre", "Post"), levels=c("Pre", "Post"))]


p <- ggplot(dfplot, aes(y=quantile, x=grid, color=as.factor(time))) + geom_line(linewidth=1.5) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size=26),
        text = element_text(family = "serif", size=28)) + xlab("Quantile") +
  ylab("Tenure (Days)") +
  scale_color_manual(values = c( "Post" = db, "Pre" = lb)) # '#003E4B', '#B6D8E4'
p
ggsave(file.path(figs, "microsoft_desc_quantiles.pdf"), p, width = 12, height = 10,  dpi = 300)





#### TENURE IN YEARS ####
#--------------

#-----
# try grouping by 4 months before and after
id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

# need to ignore the leavers for this
grouped <- test
grouped[, y_col := tenure_full]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))
grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
grouped <- grouped[, y_col := 1 * floor(y_col / (1 * 365))] # for tenure only!
grouped[y_col >= 10, y_col := 10]

t0 <- 3
M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
print(paste0("N=", nrow(grouped)))
print(paste0("J=", uniqueN(grouped$id_col)))

# full distribution
disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
               G = 1000, M=M, num.cores = 20, mixture=TRUE, grid.cat = unique(grouped$y_col),
               permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,uniform=FALSE,
               seed=31, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdf", graph=TRUE, samples=c(0))

ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="Tenure (Years)", ylab="Change in CDF",
                ncol=1)
p <- p + scale_x_continuous(breaks=sort(unique(grouped$y_col)))
ggsave(file.path(figs, paste0("tenure_years_", rtocomp, ".pdf")), p, width = 3, height = 6,  dpi = 300)



#### TITLES ####
#--------------

#-----
# try grouping by 4 months before and after
id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

# need to ignore the leavers for this
grouped <- test
grouped[, y_col := title_categorical]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]
grouped <- grouped[(grouped$y_col %between% c(1,10)),] # for tenure only!

t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
print(paste0("N=", nrow(grouped)))
print(paste0("J=", uniqueN(grouped$id_col)))

# full distribution
disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
                       G = 1000, M=M, num.cores = 20, mixture=TRUE, grid.cat = unique(grouped$y_col),
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE, uniform=TRUE,
                       seed=31, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, ylim=c(-0.01, 0.01), samples=c(0))


# print weights
weightsdf <- printWeights(disco)
tab <- xtable(weightsdf[1:10], digits=4)
print.xtable(tab,  include.rownames=FALSE, floating=FALSE,file=file.path(tabs, "weights_microsoft_titles.tex"))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="Title", ylab="Change in CDF")
p <- p + scale_x_continuous(breaks=sort(unique(grouped$y_col)),
                            labels=list("1" = "Unpaid", "2"="Training",
                                        "3" = "Entry", "4"="Manager",
                                        "5"="Senior", "6"="Owner", "7"="Partner",
                                        "8"="Director", "9"="VP", "10"="CXO"))
p<- p + theme(axis.text.x = element_text(angle = 90, hjust=1))
p
ggsave(file.path(figs, paste0(rtocomp, "_title_dist.pdf")), p, width = 6, height = 5,  dpi = 300)




## pre and post observed pdf
db <- '#003E4B'
lb <-  '#70ADC9'

dfplot <- grouped[time_col %between% c(2,3) & id_col == id_col.target]
dfplot[, time_col := factor(ifelse(time_col == 2, "Pre", "Post"), levels=c("Pre", "Post"))]

p<-ggplot(dfplot, aes(x=y_col, y=after_stat(density), fill=time_col, color=time_col, group=time_col)) +
  geom_histogram(position=  position_dodge(width = 0.5)) + theme_minimal() +
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size=26),
        text = element_text(family = "serif", size=28),
        axis.text.x = element_text(angle = 90, size=28, vjust=0.5)) +
  scale_x_continuous(breaks=sort(unique(grouped$y_col)),
                     labels=list("1" = "Unpaid", "2"="Training",
                                 "3" = "Entry", "4"="Manager",
                                 "5"="Senior", "6"="Owner", "7"="Partner",
                                 "8"="Director", "9"="VP", "10"="CXO")) +
  scale_fill_manual(values = c( "Post" = db, "Pre" = lb)) +
  scale_color_manual(values = c( "Post" = db, "Pre" = lb)) +
  xlab("Title") +
  ylab("Density")
p
ggsave(file.path(figs, "microsoft_desc_pdf_title.pdf"), p, width = 12, height = 10,  dpi = 300)


## pre and post observed cdf
discoplot <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_max=0.9, G = 100, M=M, num.cores = 20,
                   permutation = FALSE, CI = FALSE, boots = 1000, simplex=TRUE, seed=31, qmethod=NULL, mixture=TRUE,
                   grid.cat=sort(unique(grouped$y_col)))
dfplot <- data.table("quantile" = unlist(lapply(2:3, function(x) discoplot$results.periods[[x]]$target$cdf)),
                     "grid" = unlist(lapply(2:3, function(x) discoplot$results.periods[[x]]$target$grid)),
                     "time" = rep(2:3, each=10))
# dfplot <- dfplot %>%
#   mutate(Change = c(FALSE, diff(quantile) != 0)) %>%
#   filter(Change)
dfplot[, time := factor(ifelse(time == 2, "Pre", "Post"), levels=c("Pre", "Post"))]

p <- ggplot(dfplot, aes(y=quantile, x=grid, color=time)) + geom_line(linewidth=1) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size=26),
        text = element_text(family = "serif", size=28)) + xlab("Title Level") +
  ylab("Cumulative Probability") + scale_x_continuous(breaks=sort(unique(grouped$y_col))) +
  scale_color_manual(values = c( "Post" = db, "Pre" = lb)) # '#003E4B', '#B6D8E4'
p
ggsave(file.path(figs, "microsoft_desc_cdf.pdf"), p, width = 12, height = 10,  dpi = 300)



#----------------
#### Repeat for all other companies ####
#----------------

for (comp in c("spacex", "apple")) {

  robustness(comp)

}

# check permutation tests
mdl <- load(file.path(mods, paste0(rtocomp, "_model_titles.RData")))
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE)
sign <- (disco$perm$p_overall < 0.1)

ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="Title Level", ylab="Change in CDF", ncol=1)
p <- p + scale_x_continuous(breaks=seq(1,6))
ggsave(file.path(figs, paste0(rtocomp, "title_dist", sign, ".pdf")), p, width = 4, height = 8,  dpi = 300)






#---------
#### EMPLOYEE COUNT ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
grouped <- grouped[leaver == TRUE]
grouped <- grouped[COMPANY_NAME != COMPANY_NAME_next]


grouped[, y_col := ifelse(EMPLOYEE_COUNT_next %between% c(0,50), 0,
                          ifelse(EMPLOYEE_COUNT_next %between% c(50,250), 1, 2))]
grouped[, y_col := as.numeric(EMPLOYEE_COUNT_next < 50)]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3




M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
print(paste0("N=", nrow(grouped)))
print(paste0("J=", uniqueN(grouped$id_col)))

#----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE, grid.cat = unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01),
                ylab="Change in CDF", factor=TRUE, xlab="No. Employees")
p <- p + scale_x_discrete(labels=c(">=50 Emp.","<50 Emp."))
ggsave(file.path(figs, "microsoft_startups.pdf"), p, width = 5, height = 5,  dpi = 300)







#### MALE VS FEMALE ####
#--------------

#-----
# try grouping by 4 months before and after
id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)

t0 <- unique(test[month == firstDateOfMonth]$time_col)

# need to ignore the leavers for this
grouped <- test[(END_DATE > month %m+% months(3))| (is.na(END_DATE)) | (COMPANY_NAME == COMPANY_NAME_next) ] # [(END_DATE > month) & (END_DATE <= (month %m+% months(1)))] # leavers
grouped <- grouped[gender != "" & !is.na(gender)]
grouped[, y_col := as.numeric(gender=="female")]
# grouped[gender == "" | is.na(gender), y_col := 2]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=max(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 2000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----

disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
               G = 1000, M=M, num.cores = 20, mixture=TRUE, grid.cat = sort(unique(grouped$y_col)),
               permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
               seed=31, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE,
                    samples=c(0) # this is needed to avoid error
)

ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="Employee Gender",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Male","Female"))
ggsave(file.path(figs, "microsoft_gender.pdf"), p, width = 5, height = 5,  dpi = 300)










#---------
#### SEARCH TIME ####
# NB appears to be no effect either for 0-1 (some vs no search) or when splitting into monthly periods


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
# try grouping by 4 months before and after
grouped <- test
grouped <- grouped[leaver == TRUE]
grouped <- grouped[COMPANY_NAME != COMPANY_NAME_next]
grouped[, search_days := as.numeric(as.Date(START_DATE_next) - as.Date(START_DATE))]
# grouped <- grouped[search_days > -31]
grouped[search_days < 0 | search_days==1, search_days := 0]
# grouped <- grouped[!is.na(search_days)]
# grouped[, y_col := as.numeric( cut(search_days,
#                                    breaks = c(-Inf, 31, 120, Inf),
#                                    labels = seq(1,3),
#                                    right = FALSE))]
grouped[, y_col := as.numeric(search_days <= 31)]
#grouped[, y_col := search_days]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))

#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0, q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE, seed=31,
                       qmethod=NULL, mixture=TRUE, grid.cat=c(0,1)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))

ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Employed","Unemployed"))
ggsave(file.path(figs, "microsoft_search.pdf"), p, width = 5, height = 5,  dpi = 300)







#---------
#### INDUSTRY (changed or not) ####


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
grouped <- grouped[COMPANY_NAME != COMPANY_NAME_next]
grouped[, y_col := as.numeric(substr(naics_code,1,2) != substr(naics_code_next,1,2))] # not changing naics code


grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3

check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
mincnt <- min(check[id_col == id_col.target]$cnt)
dropcomps <- unique(check[check$cnt < 10]$id_col)
grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=seq(0,1)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Employment Sector",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Same","Changed"))
ggsave(file.path(figs, "microsoft_naics.pdf"), p, width = 5, height = 5,  dpi = 300)

disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
weightsdf <- printWeights(disco)






#---------
#### FUNDING STAGE -- nothing really here ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
grouped <- grouped[leaver == TRUE]
grouped <- grouped[COMPANY_NAME != COMPANY_NAME_next]


# Assuming df is your DataFrame and 'funding_stage' is the column you want to recode

# Define the mapping from specific stages to broader categories
category_mapping <- list(
  "0" = c("pre_seed", "seed", "angel", "convertible_note", "equity_crowdfunding"), # early stage
  "1" = c("series_a", "series_b", "series_c", "series_d", "series_e", "series_f", "series_g", "series_h", "series_i"), # growth stage
  "2" = c("debt_financing", "private_equity", "secondary_market", "corporate_round"), # late-stage
  "3" = c("post_ipo_equity", "post_ipo_debt", "post_ipo_secondary"), # post-public
  "4" = c("non_equity_assistance", "undisclosed", "grant", "series_unknown")
)

# Define the mapping for the binary categorization
category_mapping <-list(
  "0" = c("pre_seed", "seed", "angel", "series_a", "series_b", "series_c", "series_d", "series_e", "series_f", "series_g", "series_h", "series_i"),
  "1" = c( "debt_financing", "private_equity", "post_ipo_equity", "post_ipo_debt",
           "post_ipo_secondary", "secondary_market", "corporate_round")
)
# Flatten the mapping to make each specific stage point to its new category
flattened_mapping <- unlist(lapply(names(category_mapping), function(cat) setNames(rep(cat, length(category_mapping[[cat]])), category_mapping[[cat]])))

# Recode the 'funding_stage' column
grouped$funding_grouped <- sapply(grouped$LATEST_FUNDING_STAGE_next, function(x) flattened_mapping[x])
# grouped[funding_grouped == '4', funding_grouped := NA]
grouped[, y_col := as.numeric(funding_grouped)]



grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3

check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
mincnt <- min(check[id_col == id_col.target]$cnt)
dropcomps <- unique(check[check$cnt < 20]$id_col)
grouped <- grouped[!id_col %in% dropcomps]



M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE, grid.cat = unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Employment Sector",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Same","Changed"))
ggsave(file.path(figs, "microsoft_naics.pdf"), p, width = 5, height = 5,  dpi = 300)

disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
weightsdf <- printWeights(disco)



#---------
#### AGE ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
grouped <- grouped[leaver == TRUE]
grouped <- grouped[COMPANY_NAME != COMPANY_NAME_next]
grouped[, age := (2023-FOUNDED_next)]
grouped[, y_col := 1-as.numeric(age <= 5 & EMPLOYEE_COUNT_next < 100)] # not changing naics code



grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3

check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 20]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE, grid.cat=unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Employment Sector",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Same","Changed"))
ggsave(file.path(figs, "microsoft_naics.pdf"), p, width = 5, height = 5,  dpi = 300)

disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
weightsdf <- printWeights(disco)




#---------
#### INDUSTRY (grouping) ####


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
grouped <- grouped[leaver == TRUE]
grouped <- grouped[COMPANY_NAME != COMPANY_NAME_next]
grouped$naics_2 <- substr(grouped$naics_code_next, 1,2)
naics_groups <- table(substr(grouped$naics_code_next,1,2))
naics_groups[order(naics_groups)]
grouped <- grouped %>%
  mutate(naics_2 = as.character(naics_2), # Ensure naics_2 is character to mix numbers and "other"
         y_col = as.numeric(ifelse(naics_2 %in% c("54"), "1", "0")))


grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3

check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
mincnt <- min(check[id_col == id_col.target]$cnt)
dropcomps <- unique(check[check$cnt < 10]$id_col)
grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
                       q_min=0, q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = FALSE, boots = 500,
                       simplex=TRUE, seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Employment Sector",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Same","Changed"))
ggsave(file.path(figs, "microsoft_naics.pdf"), p, width = 5, height = 5,  dpi = 300)

disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
weightsdf <- printWeights(disco)






#---------
#### PROMOTIONS ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----s
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
grouped <- grouped[(COMPANY_NAME == COMPANY_NAME_next) | (is.na(COMPANY_NAME_next))]
#grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next)]
#grouped <- grouped[leaver==TRUE]
grouped[, thismonth := as.numeric( (as.Date(START_DATE_next) - as.Date(month)) <= 31)]
grouped[is.na(COMPANY_NAME_next), thismonth := 0]
grouped[, promoted := as.numeric(title_categorical_next >= title_categorical)] # not changing naics code
grouped[, y_col := promoted ]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = min(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3
#
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 5]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]
grouped[, y_col := 1-y_col]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=seq(0,1)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Employment Sector",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Same","Changed"))
ggsave(file.path(figs, "microsoft_naics.pdf"), p, width = 5, height = 5,  dpi = 300)

disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
weightsdf <- printWeights(disco)







#---------
#### DEMOTIONS FOR LEAVERS ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----s
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next) ]
#grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next)]
grouped <- grouped[leaver==TRUE]
grouped[, thismonth := as.numeric( (as.Date(START_DATE_next) - as.Date(month)) <= 31)]
grouped[is.na(COMPANY_NAME_next), thismonth := 0]
grouped[, promoted := as.numeric(title_categorical_next >= title_categorical)] # not changing naics code
grouped[, y_col := promoted ]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3
#
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 5]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]
grouped[, y_col := y_col]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=seq(0,1)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Title Level",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Same or Promoted","Demoted"))
ggsave(file.path(figs, "microsoft_demotions.pdf"), p, width = 5, height = 5,  dpi = 300)









#---------
#### ROLE CHANGES ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----s
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next) ]
#grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next)]
grouped <- grouped[leaver==TRUE]
grouped <- grouped[TITLE_ROLE != "" & !is.na(TITLE_ROLE_next)]
grouped[, y_col := as.numeric(TITLE_ROLE_next == TITLE_ROLE)]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3
#
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 5]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=seq(0,1)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Role",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Same Role","Changed Role"))
ggsave(file.path(figs, "microsoft_rolechange.pdf"), p, width = 5, height = 5,  dpi = 300)






#---------
#### ROLE DISTRIBUTION ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----s
grouped <- test

grouped <- grouped[TITLE_ROLE == "", TITLE_ROLE := "unknown"]
grouped <- grouped[leaver == FALSE]
grouped <- grouped[TITLE_ROLE != "unknown"]

# Create a named list with categories and corresponding roles
roles <- list(
  "Business Administration" = c("sales", "operations", "finance", "real_estate"),
  "Technical and Engineering" = c("engineering", "trades"),
  "Health and Education" = c("health", "education"),
  "Creative and Media" = c("design", "media"),
  "Customer and Client Services" = c("customer_service", "public_relations"),
  "Marketing and Communications" = c("marketing"),
  "Legal and Compliance" = c("legal"),
  "Human Resources" = c("human_resources"),
  "Unknown" = c("unknown")
)
# roles <- list(
#   "high office presence" = c("operations", "customer_service", "trades", "health", "legal", "human_resources"),
#   "moderate office presence" = c("engineering", "finance", "real_estate", "public_relations"),
#   "low office presence" = c("sales", "marketing", "design", "media", "education")
# )

# Print the list to view the mapping
print(role_categories)

# Flatten the mapping to make each specific stage point to its new category
flattened_mapping <- unlist(lapply(names(roles), function(cat) setNames(rep(cat, length(roles[[cat]])), roles[[cat]])))

# Recode the 'funding_stage' column
grouped$y_col <- sapply(grouped$TITLE_ROLE, function(x) flattened_mapping[x])

mapping <- setNames(1:uniqueN(grouped$TITLE_ROLE), unique(grouped$TITLE_ROLE))
#grouped <- grouped[ !is.na(grouped$y_col) ,]
grouped[, y_col := sapply(TITLE_ROLE, function(x) mapping[[x]])]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

t0 <- 3
#
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 5]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="New Title Level",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(breaks=sort(unique(grouped$y_col)),
                            labels=setNames(names(mapping), mapping))
p<- p + theme(axis.text.x = element_text(angle = 90, hjust=1))
p
ggsave(file.path(figs, "microsoft_demotions.pdf"), p, width = 5, height = 5,  dpi = 300)






#---------
#### MARKET SHARE ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----s
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next) ]
#grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next)]
grouped <- grouped[leaver==TRUE]
mdn <- quantile(grouped$EMP_SHARE_next, 0.05, na.rm=TRUE)
grouped[, y_col := EMP_SHARE_next < mdn]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3
#
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 5]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=seq(0,1)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="Labor Share",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("> 5th Quantile","< 5th Quantile"))
ggsave(file.path(figs, "microsoft_laborshare.pdf"), p, width = 5, height = 5,  dpi = 300)








#---------
# EDUCATION

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----s
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
grouped <- grouped[leaver == FALSE]
# grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next)]
#grouped <- grouped[leaver==FALSE | COMPANY_NAME == COMPANY_NAME_next]
grouped <- grouped[edu_days >= 0]
grouped[, y_col := as.numeric(edu_days <= 1600)]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3
#
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 5]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE, grid.cat=seq(0,1)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="Labor Share",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("> 5th Quantile","< 5th Quantile"))
ggsave(file.path(figs, "microsoft_laborshare.pdf"), p, width = 5, height = 5,  dpi = 300)




#---------
#### Tenure by market share, classical sc

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next) ]
#grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next)]
grouped <- grouped[leaver==TRUE]
mdn <- quantile(grouped$EMP_SHARE_next, 0.75, na.rm=TRUE)
grouped[, y_col := tenure_full]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped <- grouped[!is.na(y_col)]
#grouped <- grouped[EMP_SHARE_next > mdn]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))
synth <- grouped[,.(y_col = median(y_col,na.rm=TRUE)), by=c("time_col", "id_col")]
check <- synth[, cnt := .N, by="id_col"]
dropcomps <- unique(check[cnt<3]$id_col)
synth <- synth[!id_col %in% dropcomps]
t0 <- 3
synth_out <-
  synth %>%
  # initial the synthetic control object
  synthetic_control(outcome = y_col, # outcome
                    unit = id_col, # unit index in the panel data
                    time = time_col, # time index in the panel data
                    i_unit = id_col.target, # unit where the intervention occurred
                    i_time = t0-1, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window=t0-1, y_col_pre=y_col) %>%
  generate_predictor(time_window=t0-2, y_col_pre=y_col) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = min(synth$time_col):(t0-1) # time to use in the optimization task
  ) %>%
  # Generate the synthetic control
  generate_control()

synth_out %>% plot_trends()
synth_out %>% plot_differences()
synth_out %>% plot_placebos()
synth_out %>% grab_significance()







#---------
#### LEAVERS' TITLES -- not enough power, huge CIs ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
# grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next) | (COMPANY_NAME_next == "") ]
grouped <- grouped[leaver==TRUE]
mdn <- quantile(grouped$EMP_SHARE_next, 0.05, na.rm=TRUE)
grouped[, y_col := title_categorical]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))


grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3


M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="Labor Share",
                ylab="Change in CDF", factor=TRUE)
p
p <- p + scale_x_discrete(labels=c("> 5th Quantile","< 5th Quantile"))
ggsave(file.path(figs, "microsoft_laborshare.pdf"), p, width = 5, height = 5,  dpi = 300)





#---------
#### LEAVERS' TENURE IN YEARS -- not enough power, huge CIs ####

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
grouped[is.na(leaver), leaver := FALSE] # account for those staying at the company indefinitely
#grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next) | (COMPANY_NAME_next == "") ]
grouped <- grouped[leaver==TRUE]
grouped[, y_col := tenure_full]

grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))
grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
grouped <- grouped[, y_col := 5 * floor(y_col / (5 * 365))] # for tenure only!
grouped[y_col >= 10, y_col := 10]

grouped <- grouped[ !is.na(grouped$y_col) ,]
t0 <- 3


M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0,
                       q_max=1, G = 1000, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
                       seed=31, qmethod=NULL, mixture=TRUE,
                       grid.cat=unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.05, 0.01), xlab="Labor Share",
                ylab="Change in CDF", factor=TRUE)
p
p <- p + scale_x_discrete(labels=c("> 5th Quantile","< 5th Quantile"))
ggsave(file.path(figs, "microsoft_laborshare.pdf"), p, width = 5, height = 5,  dpi = 300)




#### HIGH PERFORMERS ####
#--------------


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

#-----
# try grouping by 4 months before and after
grouped <- test[title_categorical > 4]
grouped[, leaver := (as.Date(END_DATE) >= as.Date(month)) & (as.Date(END_DATE) < as.Date(month) %m+% months(1))]
grouped[is.na(leaver), leaver:=FALSE]
grouped[, leaver_true := leaver & (COMPANY_NAME != COMPANY_NAME_next | is.na(COMPANY_NAME_next))]
grouped[, y_col := days_between_relative]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month),
                       leaver = max(leaver_true)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))
#grouped <- grouped[leaver == FALSE]
grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
print(paste0("N=", nrow(grouped)))
print(paste0("J=", uniqueN(grouped$id_col)))

check <- grouped[, .(cnt =.N), by=c("id_col", "time_col")]

## full distribution
G <- 1000
disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_max=0.9, G = G, M=M, num.cores=20,
               cl=0.95,uniform=TRUE, permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE, seed=30,
               qmethod=NULL) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="quantileDiff", graph=TRUE, ylim=c(-0.05,0.05))



weightsdf <- printWeights(disco)
tab <- xtable(weightsdf[1:10], digits=4)


## plot figure
ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp), max(ttemp), by = "quarter")
p <- prettyPlot(disco, discot, t_list=t_list, ncol=1, ylim = c(-0.03, 0.03))
p
#ggsave(file.path(figs, "microsoft_main_dist.pdf"), p, width = 3, height = 6,  dpi = 300)







#### HIGH PERFORMERS, discrete ####
# TODO: take speed of promotion at their PREVIOUS title role instead of their current one
# cause leavers might take longer to get promoted
#--------------


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

#-----
# try grouping by 4 months before and after
grouped <- test# [title_categorical > 4]
grouped[, leaver := (as.Date(END_DATE) >= as.Date(month)) & (as.Date(END_DATE) < as.Date(month) %m+% months(1))]
grouped[is.na(leaver), leaver:=FALSE]
grouped[, leaver_true := leaver & (COMPANY_NAME != COMPANY_NAME_next | is.na(COMPANY_NAME_next))]
grouped[, y_col := as.numeric(days_between_relative <= 0)]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month),
                       leaver = max(leaver_true)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))
#grouped <- grouped[leaver == FALSE]
grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
print(paste0("N=", nrow(grouped)))
print(paste0("J=", uniqueN(grouped$id_col)))

check <- grouped[, .(cnt =.N), by=c("id_col", "time_col")]

## full distribution
G <- 1000
disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0, G = G, M=M, num.cores=20,
               cl=0.95,uniform=TRUE, permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE, seed=30,
               qmethod=NULL, mixture=TRUE, grid.cat=unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))

weightsdf <- printWeights(disco)
tab <- xtable(weightsdf[1:10], digits=4)


## plot figure
ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ncol=1)
p







#### degree type, discrete ####
# TODO: take speed of promotion at their PREVIOUS title role instead of their current one
# cause leavers might take longer to get promoted
#--------------


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

#-----
# try grouping by 4 months before and after
grouped <- test#[title_categorical > 4]
grouped[, leaver := (as.Date(END_DATE) >= as.Date(month)) & (as.Date(END_DATE) < as.Date(month) %m+% months(1))]
grouped[is.na(leaver), leaver:=FALSE]
grouped[, leaver_true := leaver & (COMPANY_NAME != COMPANY_NAME_next | is.na(COMPANY_NAME_next))]
grouped[, y_col := degree_type]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month),
                       leaver = max(leaver_true)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))
grouped <- grouped[leaver == TRUE]
grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
print(paste0("N=", nrow(grouped)))
print(paste0("J=", uniqueN(grouped$id_col)))

check <- grouped[, .(cnt =.N), by=c("id_col", "time_col")]

## full distribution
G <- 1000
disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0, G = G, M=M, num.cores=20,
               cl=0.95,uniform=TRUE, permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE, seed=30,
               qmethod=NULL, mixture=TRUE, grid.cat=unique(grouped$y_col)) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, samples=c(0))

weightsdf <- printWeights(disco)
tab <- xtable(weightsdf[1:10], digits=4)


## plot figure
ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ncol=1)
p
