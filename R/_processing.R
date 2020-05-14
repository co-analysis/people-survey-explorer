library(tidyverse)

# process data

csps_median_raw <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/867297/Civil-Service-People-Survey-2009-to-2019-Median-Benchmark-Scores-CSV.csv")

csps_median_dt <- csps_median_raw %>%
  mutate_at(vars(-Measure, -Label), as.numeric) %>%
  mutate(Measure = case_when(
    str_detect(Label, "E(05|06).*\\(% yes\\)") ~ paste0(Measure, "_YES"),
    str_detect(Label, "E(05|06).*\\(% no\\)") ~ paste0(Measure, "_NO"),
    str_detect(Label, "E(05|06).*\\(% pref") ~ paste0(Measure, "_PNS"),
    TRUE ~ Measure),
    sortorder = as.numeric(rownames(.))
    ) %>%
  pivot_longer(cols = c(-Measure, -Label, -sortorder), 
               names_to = "year",
               values_to = "value") %>%
  janitor::clean_names() %>%
  mutate(
    measure = toupper(str_remove_all(measure, "_ts")),
    label = str_trim(str_remove_all(label, "^\\w+\\.|\\((B\\d|%).*$")),
    label = case_when(
      measure == "E05_Yes" ~ paste0(label, ": Any yes response"),
      measure == "E05_Yes" ~ paste0(label, ": Any no response"),
      measure == "E05_Yes" ~ paste0(label, ": Prefer not to say"),
      str_detect(measure, "^E") ~ str_replace(label, "_", ": "),
      TRUE ~ label),
    theme = case_when(
      str_detect(measure, "^(EES|MW|OP|LM|MT|LD|IF|RW|PB|LC)$") ~ "SUMMARY",
      str_detect(measure, "B01|B02|B03|B04|B05") ~ "MW",
      str_detect(measure, "B06|B07") ~ "OP",
      str_detect(measure, "B08|B09|B10|B11|B12|B13|B14|B15|B16|B17") ~ "LM",
      str_detect(measure, "B18|B19|B20") ~ "MT",
      str_detect(measure, "B21|B22|B23|B24") ~ "LD",
      str_detect(measure, "B25|B26|B27|B28") ~ "IF",
      str_detect(measure, "B29|B30|B31|B32|B33|B34") ~ "RW",
      str_detect(measure, "B35|B36|B37") ~ "MW",
      str_detect(measure, "B38|B39|B40|B41|B42|B44|B45|B46") ~ "MW",
      str_detect(measure, "B47|B48|B49|B50|B51") ~ "EE",
      str_detect(measure, "B52|B53") ~ "TA",
      str_detect(measure, "B54|B55|B56|B57|B58") ~ "OC",
      str_detect(measure, "B59|B60") ~ "VL",
      str_detect(measure, "C01") ~ "FI",
      str_detect(measure, "^D") ~ "CS",
      str_detect(measure, "E01|E02") ~ "DS",
      str_detect(measure, "E03|E04|E05|E06") ~ "BH",
      str_detect(measure, "^W|HSE|PERMA") ~ "WB")) %>%
  drop_na(value) %>%
  arrange(sortorder, year) %>%
  select(theme, measure, label, year, value, sortorder)

write_csv(csps_median_dt, "data/csps_median.csv")
