
df <- fread(file.path(dataIn, "advan_changepoint.csv"))

# remove any aggregate_key that has NAs in raw_visitor_counts_sum
temp <- df[, .(missing = max(is.na(raw_visitor_counts_sum))), by = aggregate_key]
temp <- temp[missing == 0]
df <- df[aggregate_key %in% temp$aggregate_key]

compsamp <- c("cisco.com", "alphabet.com", "microsoft.com", "target.com", "tesla.com" , "hp.com", "nike.com")

dfplot <- df[aggregate_key %in% compsamp]

# plot time series in facet_wrap for aggregate_key
dfplot[, date := as.Date(date_range_start)]

ggplot(dfplot, aes(x = date, y = raw_visit_counts_sum)) +
  geom_line() +
  facet_wrap(~aggregate_key, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Time series of raw_visitor_counts_sum for selected companies",
       x = "Date",
       y = "raw_visitor_counts_sum")
