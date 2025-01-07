
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
sparkdf <- spark_read_parquet(sc, name = "people_tenure_prepped_next",
                              path = file.path(dataIn, "people_tenure_prepped_next_spells_performers_edu/*"),
                              memory = FALSE)


# change RTO_date_manual to May 1 2023 for COMPANY_NAME amazon
sparkdf <- sparkdf %>% mutate(RTO_date_manual = ifelse(COMPANY_NAME == "amazon", "May 1 2023", RTO_date_manual))
sparkdf <- sparkdf %>% mutate(naics_code = ifelse(COMPANY_NAME == "microsoft", 54151, naics_code))

sparkdf <- sparkdf %>% mutate(RTO_date_manual = ifelse(COMPANY_NAME == "apple", "Apr 11 2022", RTO_date_manual))

sparkdf <- sparkdf %>% mutate(naics_2 = substr(naics_code, 1, 2))
# comp_list <-  sparkdf %>% filter((RTO_date_manual != "never") & (!is.na(RTO_date_manual))) %>%
#   select("COMPANY_NAME") %>% distinct() %>% collect()
# comps_filter <- comp_list$COMPANY_NAME

## filter main firms sample
mainfirms <- sparkdf %>% filter(COMPANY_NAME %in% comps_filter) # %>% collect()
mainfirms <- mainfirms %>% mutate(rtodate=trunc(to_date(RTO_date_manual, "MMM d yyyy"), "month"))
mainfirms <- mainfirms %>% filter(sql("month >= add_months(rtodate, -3)
                                 AND month <= add_months(rtodate, 2)"))
mainfirms <- mainfirms %>% select(RTO_date_manual, month, COMPANY_NAME, ID, tenure_full,
                                  title_categorical, rtodate)
mainfirms <- mainfirms %>% sdf_repartition(partitions = 1)



# Write the repartitioned dataframe to a CSV file
spark_write_csv(mainfirms, path =
                  file.path(dataOut, paste0('mainfirms.csv.gz')),
                header = TRUE, mode="overwrite")




sparkdf <- sparkdf %>% filter(naics_2 %in% c("51", "54", "33"))

# # retrieve list of large companies that implement RTO
# comp_list <-  sparkdf %>% filter((RTO_date_manual != "never") & (!is.na(RTO_date_manual))) %>%
#   select("COMPANY_NAME") %>% distinct() %>% collect()

sparkdf <- sparkdf %>% select(ID, id_col, month, time_col, tenure, tenure_full, edu_days, START_DATE, END_DATE,
                              title_categorical, COMPANY_NAME, RTO_date_manual, naics_code, gender,
                              COMPANY_NAME_next, TITLE_LEVELS_next, title_categorical_next,
                              START_DATE_next, END_DATE_next, TOTAL_FUNDING_RAISED_next,
                              LATEST_FUNDING_STAGE_next, EMPLOYEE_COUNT_next, FOUNDED_next, TYPE_next,
                              INFERRED_REVENUE_next, naics_code_next, TITLE_ROLE, TITLE_ROLE_next,
                              EMP_SHARE_next, tenure_role_relative, tenure_role_relative_comp,
                              days_between_relative, degree_type
)


#----------------------------
### Create Analysis Ready Datasets for Each Company ####
#----------------------------

redo_data_parsing <- TRUE

if (redo_data_parsing) {
  ## filter out layoffs
  layoffs <- fread(file.path(dataOut, 'layoffs_matched_manual_v2.csv'))
  layoffs <- layoffs %>% select(V1, Date)

  ## loop over all RTO companies and save their prepped datasets
  for (rtocomp in c("microsoft")) {# , "spacex", "apple")) { # all other companies have layoffs within sample period

    rtodate <- sparkdf %>% filter(COMPANY_NAME == rtocomp) %>% select(RTO_date_manual) %>%
      distinct() %>% collect()
    layoffcomp <- layoffs[V1 == rtocomp]

    rtodate <- as.Date(rtodate$RTO_date_manual[1], format="%b %d %Y")

    # assign startDate as 6 months before rtodate and endDate as 6 months after
    firstDateOfMonth <- floor_date(rtodate, "month")
    startDate <- firstDateOfMonth %m-% months(12)
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

    # filter companies with at least 5% of RTO company's workforce
    temp <- sparkout %>%
      group_by(time_col, COMPANY_NAME) %>% summarise(nobs = n()) %>%
      group_by(COMPANY_NAME) %>% summarise(nobs = min(nobs)) %>% collect() %>% setDT()


    ids <- temp[nobs > 0.05 * temp[COMPANY_NAME==rtocomp]$nobs]$COMPANY_NAME

    # if (length(ids) < 20) next

    sparkout <- sparkout %>% filter(COMPANY_NAME %in% ids)

    sparkout_single_partition <- sparkout %>% sdf_repartition(partitions = 1)

    # Specify the path for the output file. Adjust the path as necessary.

    # Write the repartitioned dataframe to a CSV file
    spark_write_csv(sparkout_single_partition, path =
                      file.path(dataOut, paste0('disco_prepped_next_', rtocomp, '.csv.gz')),
                    header = TRUE, mode="overwrite")

  }

}
