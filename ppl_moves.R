




#----------------------------
### Company-by-company analysis ####
# TODO: need to check that START_DATE_next is always after END_DATE because rn it's not (AWS)
#----------------------------


#----------------
# MICROSOFT
#----------------
rtocomp <- "microsoft"
fn <- file.path(dataOut, paste0('disco_prepped_next_', rtocomp, '.csv.gz'))
# List all files in the directory
files <- list.files(fn, full.names = TRUE, pattern=".csv")

ppl <- fread(files)
ppl <- ppl[!duplicated(ppl)]
ppl <- as.data.table(ppl)

rtodate <- ppl %>% filter(COMPANY_NAME == rtocomp) %>% select(RTO_date_manual) %>%
  distinct()
rtodate <- as.Date(rtodate$RTO_date_manual[1], format="%b %d %Y")

# assign startDate as 6 months before rtodate and endDate as 6 months after
firstDateOfMonth <- floor_date(rtodate, "month")
startDate <- firstDateOfMonth %m-% months(12)
endDate <- firstDateOfMonth %m+% months(6)


ppl <- ppl[, date := as.Date(RTO_date_manual, format = "%b %d %Y")]
ppl <- ppl[date >= endDate | is.na(date) | COMPANY_NAME == rtocomp]


ppl <- ppl[, nobs_time := .N, by = c("id_col", "time_col")]
ppl <- ppl[, nobs := min(nobs_time), by = "id_col"]

ppl[, leaver := (as.Date(END_DATE) > as.Date(month)) & as.Date(END_DATE) <= as.Date(month) %m+% months(1)]


test <- ppl




# MALE VS FEMALE
#--------------

#-----
# try grouping by 4 months before and after
id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)

t0 <- unique(test[month == firstDateOfMonth]$time_col)

# need to ignore the leavers for this
grouped <- test[(END_DATE >= (month %m+% months(3))) | (is.na(END_DATE)) | (COMPANY_NAME == COMPANY_NAME_next) ] # [(END_DATE > month) & (END_DATE <= (month %m+% months(1)))] # leavers
# grouped <- test[leaver == TRUE]
#grouped <- grouped[(COMPANY_NAME != COMPANY_NAME_next) | (is.na(COMPANY_NAME_next))]
grouped <- grouped[gender != "" & !is.na(gender)]
grouped[, y_col := as.numeric(gender=="male")]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 2000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 500]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]

disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
               G = 1000, M=M, num.cores = 20, mixture=TRUE, grid.cat = seq(0,1),
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
# EMPLOYEE COUNT

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

disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
weightsdf <- printWeights(disco)








#---------
# SEARCH TIME
# NB appears to be no effect either for 0-1 (some vs no search) or when splitting into monthly periods


id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
# try grouping by 4 months before and after
grouped <- test
#grouped <- grouped[leaver == TRUE]
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
# check <- grouped[, cnt := .N, by=c("id_col", "time_col")]
# mincnt <- min(check[id_col == id_col.target]$cnt)
# dropcomps <- unique(check[check$cnt < 5]$id_col)
# grouped <- grouped[!id_col %in% dropcomps]

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))

#-----
## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0, q_max=1, G = 1000, M=M, num.cores = 15,
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
# INDUSTRY (changed or not) -- can phrase as "moving out of software engineering"


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
# FUNDING STAGE -- nothing really here

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)
#-----
grouped <- test
# grouped <- grouped[leaver == TRUE]
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
                       permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE,
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
# AGE

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
# INDUSTRY (grouping)


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
# PROMOTIONS


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





