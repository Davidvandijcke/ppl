
# merge in tech layoffs data
layoffs <- fread(file.path(dataIn, 'layoffs_data(1).csv'))
# layoffs <- layoffs[Country == "United States"]
layoffs[, COMPANY_NAME := tolower(Company)]

tech <- fread(file.path(dataOut, 'names_to_match.csv'))

test <- bind_rows(lapply(unique(tolower(tech$COMPANY_NAME)),
                         function(x) tidystringdist::tidy_comb(layoffs$COMPANY_NAME, x))
)


temp <- test %>%
  tidy_stringdist(method = c("jw", "lv", "cosine"))

temp <- temp %>% setDT()

# only retain the minimum cosine, jw, or lv
temp <- temp[, min_cosine := min(cosine), by = V1]
temp <- temp[, min_jw := min(jw), by = V1]
temp <- temp[, min_lv := min(lv), by = V1]

temp <- temp[cosine == min_cosine | jw == min_jw | lv == min_lv,]

fwrite(temp, file.path(dataOut, 'layoffs_matching.csv'))




## get the manually corrected file and construct a wide version

matched <- fread(file.path(dataOut, 'layoffs_matching_manual_v2.csv'))

matched <- matched[!duplicated(matched)]

# inner join with layoffs
matched <- matched[layoffs, on = c("V2" = "COMPANY_NAME"), nomatch = NULL]
matched <- matched[Country == "United States"]

matched[, both_na := is.na(Percentage) & is.na(Laid_Off_Count)]
matched <- matched[(Percentage > 0.01 | Laid_Off_Count > 100) | both_na]

fwrite(matched, file.path(dataOut, 'layoffs_matched_manual_v2.csv'))

# matched <- matched[, .(date = list(Date)), by=V2]
# test <- setkey(matched, V2, Date)
# test[, count := 1:.N, by = V2]
# woop <- dcast(test, V2 ~ count, value.var="Date")


# TODO: add to spark code: find layoff dates within sample period and restrict it so
# it doesnt include them


# good alternatives to apple: amazon, ibm,google

