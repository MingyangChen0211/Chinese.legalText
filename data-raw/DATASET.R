freq_dta <- read.csv(
  "data-raw/charfreq.csv",
  stringsAsFactors = FALSE
)

usethis::use_data(freq_dta, internal = TRUE, overwrite = TRUE)
