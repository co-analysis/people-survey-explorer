library(tidyverse)

# process data

csps2019_orgscores_raw <- read_lines("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/867300/Civil-Service-People-Survey-2019-All-Organisation-Scores-CSV.csv")

csps2019_orgscores_proc <- csps2019_orgscores_raw %>% 
  str_remove_all("\"|,,") %>% 
  paste0(collapse = "\n") %>% 
  read_tsv()

csps2019_qref <- read_csv("data/csps_median.csv") %>%
  select(theme, measure, label) %>%
  distinct() %>%
  mutate(qsort = as.numeric(rownames(.))) %>%
  bind_rows(
    tibble(theme = "SUMMARY",
           measure = "RR",
           label = "Response rate",
           qsort = 0)
  ) %>% 
  arrange(qsort)

csps2019_orgsort <- csps2019_orgscores_proc %>% 
  janitor::clean_names() %>%
  distinct(organisation_code) %>%
  mutate(orgsort = as.numeric(rownames(.)))

csps2019_orgscores_dt <- csps2019_orgscores_proc %>%
  pivot_longer(cols = c(-`Departmental Group`, -`Organisation`, -`Organisation code`)) %>%
  janitor::clean_names() %>%
  mutate(
    measure = case_when(
      str_detect(name, "^Response Rate$") ~ "RR",
      str_detect(name, "^Employee Engagement Index$") ~ "EES",
      str_detect(name, "^My work$") ~ "MW",
      str_detect(name, "^Organisational objectives and purpose$") ~ "OP",
      str_detect(name, "^My manager$") ~ "LM",
      str_detect(name, "^My team$") ~ "MT",
      str_detect(name, "^Learning and development$") ~ "LD",
      str_detect(name, "^Inclusion and fair treatment$") ~ "IF",
      str_detect(name, "^Resources and workload$") ~ "RW",
      str_detect(name, "^Pay and benefits$") ~ "PB",
      str_detect(name, "^Leadership and managing change$") ~ "LC",
      str_detect(name, "^PERMA Index$") ~ "PERMA_INDEX",
      str_detect(name, "^Proxy Stress Index$") ~ "HSE_INDEX",
      str_detect(name, "^C01_leaveASAP$") ~ "C01_1",
      str_detect(name, "^C01_leave12mths$") ~ "C01_2",
      str_detect(name, "^C01_stay1year$") ~ "C01_3",
      str_detect(name, "^C01_stay3years$") ~ "C01_4",
      str_detect(name, "^E05 \\(Grouped\\)$") ~ "E05_YES",
      str_detect(name, "^E0\\w+$") ~ paste0(name, "_YES"),
      TRUE ~ name),
    year = 2019,
    value = as.numeric(str_remove(value, "\\%"))/100) %>%
  left_join(csps2019_orgsort, by = "organisation_code") %>%
  left_join(csps2019_qref, by = "measure") %>%
  arrange(orgsort, qsort) %>%
  select(year, organisation_code, organisation, departmental_group, orgsort, 
         qsort, theme, measure, label, value)

write_csv(csps2019_orgscores_dt, "data/csps2019_orgscores.csv")
