library(tidyverse)

# process data

csps2018_demog_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/797063/Civil-Service-People-Survey-2018-results-by-all-demographic-groups.xlsx"

# download file, not however this is not tracked by git
download.file(csps2018_demog_url, "data/csps2018_demographics.xlsx", mode = "wb")

readxl::excel_sheets("data/csps2018_demographics.xlsx")

csps2018_demog_raw <- tidyxl::xlsx_cells("data/csps2018_demographics.xlsx")

csps2018_demog_proc <- csps2018_demog_raw %>%
  mutate(character = case_when(
    address == "A1" ~ "label",
    address == "A2" ~ NA_character_,
    TRUE ~ character)) %>%
  filter(!is_blank) %>%
  unpivotr::behead("up-left", "demographic") %>%
  unpivotr::behead("up", "category") %>%
  mutate(demographic = if_else(col == 3, "All Civil Servants", demographic)) %>%
  select(row, col, numeric, character, demographic, category)

csps2018_demog_measures <- csps2018_demog_proc %>%
  filter(demographic == "label") %>%
  mutate(
    character = case_when(
      row == 109 ~ paste("C01_1.", character),
      row == 110 ~ paste("C01_2.", character),
      row == 111 ~ paste("C01_3.", character),
      row == 112 ~ paste("C01_4.", character),
      TRUE ~ character),
    question = str_detect(character, "^[A-Z]\\d\\d(\\.|_\\d\\.)"),
    measure = case_when(
      question ~ str_remove(str_extract(character, "^\\w{3,5}\\."), "\\."),
      character == "Number of responses" ~ "NR",
      character == "Employee engagement index 2" ~ "EES",
      character == "My work " ~ "MW",
      character == "Organisational objectives and purpose " ~ "OP",
      character == "My manager " ~ "LM",
      character == "My team " ~ "MT",
      character == "Learning and development " ~ "LD",
      character == "Inclusion and fair treatment " ~ "IF",
      character == "Resources and workload " ~ "RW",
      character == "Pay and benefits " ~ "PB",
      character == "Leadership and managing change " ~ "LC"),
    label = str_trim(case_when(
      str_detect(measure, "^W0") ~ 
        str_remove(str_remove(character, "^\\w{3,5}\\. "), "\\(0.*$"),
      str_detect(measure, "^E0") ~ 
        paste(str_remove(character, "^\\w{3,5}\\. "), "(% yes)"),
      question ~ str_remove(character, "^\\w{3,5}\\. "),
      measure == "EES" ~ str_remove_all(character, "\\d"),
      measure != "EES" ~ paste(character, "theme score")
    ))) %>%
  select(row, measure, label)

csps2018_demog_master <- csps2018_demog_proc %>%
  filter(demographic != "label") %>%
  left_join(csps2018_demog_measures, by = c("row"))  %>%
  select(demographic, category, measure, label, value = numeric)

csps2018_demog_responses <- csps2018_demog_master %>%
  filter(measure == "NR") %>%
  rename(r = value) %>%
  select(demographic, category, r)

csps2018_demog_master_out <- csps2018_demog_master %>%
  filter(measure != "NR") %>%
  left_join(csps2018_demog_responses) %>%
  mutate(
    demographic = str_remove(demographic, " \\(cont\\.\\)"),
    theme = case_when(
      str_detect(measure, "^(EES|MW|OP|LM|MT|LD|IF|RW|PB|LC)$") ~ "SUMMARY",
      str_detect(measure, "B01|B02|B03|B04|B05") ~ "MW",
      str_detect(measure, "B06|B07") ~ "OP",
      str_detect(measure, "B08|B09|B10|B11|B12|B13|B14|B15|B16|B17") ~ "LM",
      str_detect(measure, "B18|B19|B20") ~ "MT",
      str_detect(measure, "B21|B22|B23|B24") ~ "LD",
      str_detect(measure, "B25|B26|B27|B28") ~ "IF",
      str_detect(measure, "B29|B30|B31|B32|B33|B34") ~ "RW",
      str_detect(measure, "B35|B36|B37") ~ "PB",
      str_detect(measure, "B38|B39|B40|B41|B42|B43|B44|B45|B46") ~ "LC",
      str_detect(measure, "B47|B48|B49|B50|B51") ~ "EE",
      str_detect(measure, "B52|B53") ~ "TA",
      str_detect(measure, "B54|B55|B56|B57|B58") ~ "OC",
      str_detect(measure, "B59|B60|B61|B62") ~ "VL",
      str_detect(measure, "C01") ~ "FI",
      str_detect(measure, "^D") ~ "CS",
      str_detect(measure, "E01|E02") ~ "DS",
      str_detect(measure, "E03|E04|E05|E06") ~ "BH",
      str_detect(measure, "^W|HSE|PERMA") ~ "WB"))

write_excel_csv(csps2018_demog_master_out, "data/csps2018_demographics.csv")
