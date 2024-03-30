
# setwd("/Users/davidvandijcke/Dropbox (University of Michigan)/Flo_GSRA/repo/DiSCos")
# devtools::load_all()

#----------------------------
### Load full data ####
#----------------------------

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

sparkdf <- sparkdf %>% mutate(RTO_date_manual = ifelse(COMPANY_NAME == "apple", "Apr 11 2022", RTO_date_manual))

sparkdf <- sparkdf %>% mutate(naics_2 = substr(naics_code, 1, 2))
sparkdf <- sparkdf %>% filter(naics_2 %in% c("51", "54", "33"))

# retrieve list of large companies that implement RTO
comp_list <-  sparkdf %>% filter((RTO_date_manual != "never") & (!is.na(RTO_date_manual))) %>%
  select("COMPANY_NAME") %>% distinct() %>% collect()



#----------------------------
### Create Analysis Ready Datasets for Each Company ####
#----------------------------

redo_data_parsing <- FALSE

if (redo_data_parsing) {
  ## filter out layoffs
  layoffs <- fread(file.path(dataOut, 'layoffs_matched_manual_v2.csv'))
  layoffs <- layoffs %>% select(V1, Date)

  ## loop over all RTO companies and save their prepped datasets
  for (rtocomp in c("microsoft", "spacex", "apple")) { # all other companies have layoffs within sample period

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
        if (any(layoffs_sub$startDiff %between% c(-70,50))) {
          print("Problematic layoffs in sample period")
          next
        }# skip this company if not at least 2 months
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

    sparkout <- sparkdf %>% filter(!COMPANY_NAME %in% layoffs_filter)

    sparkout <- sparkout %>% filter((month >= startDate) & (month <= endDate))


    # naics <- sparkdf %>% filter(COMPANY_NAME == "apple") %>% select(NAICS) %>% distinct()

    # sparkdf <- sparkdf %>% filter(COMPANY_INDUSTRY == "computer software")

    sparkout <- sparkout %>% select(ID, id_col, month, time_col, tenure, edu_days, START_DATE, END_DATE,
                                    title_categorical, COMPANY_NAME, RTO_date_manual, naics_code, gender)

    # filter companies with at least 5% of RTO company's workforce
    temp <- sparkout %>%
      group_by(time_col, COMPANY_NAME) %>% summarise(nobs = n()) %>%
      group_by(COMPANY_NAME) %>% summarise(nobs = min(nobs)) %>% collect() %>% setDT()


    ids <- temp[nobs > 0.05 * temp[COMPANY_NAME==rtocomp]$nobs]$COMPANY_NAME

    # if (length(ids) < 20) next

    sparkout <- sparkout %>% filter(COMPANY_NAME %in% ids)


    ppl <- collect(sparkout)

    fwrite(ppl, file.path(dataOut, paste0('disco_prepped_', rtocomp, '.csv.gz')))

  }

}

#----------------------------
### Company-by-company analysis ####
#----------------------------


#----------------
# MICROSOFT !!! TODO: filter out SpaceX!!!
#----------------


# TENURE
#--------------

rtocomp <- "microsoft"


ppl <- fread(file.path(dataOut, 'disco_prepped_microsoft.csv.gz'))
ppl <- as.data.table(ppl)

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

test <- ppl

id_col.target <- unique(test[COMPANY_NAME == rtocomp]$id_col)
t0 <- unique(test[month == firstDateOfMonth]$time_col)

#-----
# try grouping by 4 months before and after
grouped <- test
grouped[, y_col := tenure]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 1000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----


## full distribution
disco <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0, q_max=0.9, G = 25, M=M, num.cores = 20,
                       permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE, seed=31, qmethod=NULL) # seed 5
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="quantileDiff", graph=TRUE)

## restricted distribution to check permutation test
disco_part <- DiSCos::DiSCo(grouped, id_col.target = id_col.target, t0 = t0, q_min=0.5, q_max=0.9, G = 25, M=M, num.cores = 20,
                            permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE, seed=31, qmethod=NULL) # seed 5
summary(disco_part$perm)


weightsdf <- printWeights(disco)
tab <- xtable(weightsdf[1:5], digits=4)
print.xtable(tab,  include.rownames=FALSE, floating=FALSE,file=file.path(tabs, "weights_microsoft.tex"))


## plot figure
ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp), max(ttemp), by = "quarter")
p <- prettyPlot(disco, discot, t_list=t_list)
p
ggsave(file.path(figs, "microsoft_main_dist.pdf"), p, width = 5, height = 5,  dpi = 300)




# TITLES
#--------------

#-----
# try grouping by 4 months before and after
t0 <- unique(test[month == firstDateOfMonth]$time_col)

# need to ignore the leavers for this
grouped <- test
grouped[, y_col := title_categorical]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[(grouped$y_col > 0) & !is.na(grouped$y_col) ,]
grouped <- grouped[(grouped$y_col %between% c(1,6)),] # for tenure only!
t0 <- 3

M <- 2000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----

# full distribution
disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
               G = 1000, M=M, num.cores = 3, mixture=TRUE, grid.cat = seq(1,6),
               permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
               seed=31, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, ylim=c(-0.01, 0.01))


# restricted distribution to check permutation test
temp <- grouped[(grouped$y_col %between% c(2,5)),] # for tenure only!
disco_part <- DiSCo(temp, id_col.target = id_col.target, t0 = t0,
                    G = 1000, M=M, num.cores = 20, mixture=TRUE, grid.cat = seq(2,5),
                    permutation = TRUE, CI = FALSE, boots = 1000, simplex=TRUE,
                    seed=31, qmethod=NULL)
summary(disco_part$perm)


# print weights
disco$Weights_DiSCo_avg <- as.vector(disco$Weights_mixture_avg)
weightsdf <- printWeights(disco)
tab <- xtable(weightsdf[1:5], digits=4)
print.xtable(tab,  include.rownames=FALSE, floating=FALSE,file=file.path(tabs, "weights_microsoft_titles.tex"))


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="Title Level", ylab="Change in CDF")
p <- p + scale_x_continuous(breaks=seq(1,6))
ggsave(file.path(figs, "microsoft_title_dist.pdf"), p, width = 6, height = 5,  dpi = 300)






# MALE VS FEMALE
#--------------

#-----
# try grouping by 4 months before and after
t0 <- unique(test[month == firstDateOfMonth]$time_col)

# need to ignore the leavers for this
grouped <- test[END_DATE > month %m+% months(1)] # [(END_DATE > month) & (END_DATE <= (month %m+% months(1)))] # leavers
grouped[, y_col := as.numeric(gender == "female")]
grouped <- grouped[time_col %between% c(t0-6,t0+2)]
grouped[,time_col := findInterval(time_col, c(t0-6, t0-3, t0, t0+3))]
grouped <- grouped[, .(y_col = max(y_col), month=min(month)), by=c("time_col", "ID", "id_col")]  # %>% group_by(time_col, ID, id_col) %>% summarize(y_col = max(y_col), month=min(month))

grouped <- grouped[(grouped$y_col >= 0) & !is.na(grouped$y_col) ,]
t0 <- 3

M <- 2000 #  max(list(uniqueN(ppl$id_col) * 1.5, 1000))
#-----

disco <- DiSCo(grouped, id_col.target = id_col.target, t0 = t0,
               G = 1000, M=M, num.cores = 3, mixture=TRUE, grid.cat = seq(0,1),
               permutation = TRUE, CI = TRUE, boots = 1000, simplex=TRUE,
               seed=31, qmethod=NULL)
summary(disco$perm)
discot <-  DiSCoTEA(disco,agg="cdfDiff", graph=TRUE, ylim=c(-0.01, 0.01), samples=c(0) # this is needed to avoid error
)


ttemp <- unique(grouped$month)
t_list <- seq(min(ttemp, na.rm=TRUE), max(ttemp), by = "quarter")
p <- prettyHist(disco, discot, t_list=t_list, ylim=c(-0.01, 0.01), xlab="Employee Gender",
                ylab="Change in CDF", factor=TRUE)
p <- p + scale_x_discrete(labels=c("Male","Female"))
ggsave(file.path(figs, "microsoft_gender.pdf"), p, width = 5, height = 5,  dpi = 300)



#----------------
# Repeat for all other companies
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


