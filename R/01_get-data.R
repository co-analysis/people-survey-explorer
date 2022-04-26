# extract and process published data
library(tidyverse)

csps2020_median_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/977276/Civil_Service_People_Survey_2009_to_2020_Median_Benchmark_Scores.csv"

download.file(
  csps2020_median_url,
  file.path("R", "data", "csps2020_median_timeseries.csv")
)

csps2020_mean_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/977277/Civil_Service_People_Survey_2009_to_2020_Mean_Benchmark_Scores.csv"

download.file(
  csps2020_mean_url,
  file.path("R", "data", "csps2020_mean_timeseries.csv")
)

csps2020_orgscores_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/983195/Civil_Service_People_Survey_2020-_All_organisation_scores.ods"

download.file(
  csps2020_orgscores_url,
  file.path("R", "data", "csps2020_org_scores.ods")
)

orgscores_t1 <- readODS::read_ods("R/data/csps2020_org_scores.ods",
                                  "Table_1") %>%
  as_tibble() %>%
  janitor::clean_names()

orgscores_t2 <- readODS::read_ods("R/data/csps2020_org_scores.ods",
                                  "Table_2") %>%
  as_tibble() %>%
  janitor::clean_names()

org_meta <- orgscores_t1 %>%
  select(organisation_code, organisation, department_group)

ost1_long <- orgscores_t1 %>%
  select(organisation_code,
         rr = response_rate,
         ees = engagement_index,
         mw_ts = my_work,
         op_ts = organisational_objectives_and_purpose,
         lm_ts = my_manager,
         mt_ts = my_team,
         ld_ts = learning_and_development,
         if_ts = inclusion_and_fair_treatment,
         rw_ts = resources_and_workload,
         pb_ts = pay_and_benefits,
         lc_ts = leadership_and_managing_change,
         hse_index = proxy_stress_index,
         perma_index = perma_index) %>%
  pivot_longer(cols = -organisation_code, names_to = "measure")

ost2_long <- orgscores_t2 %>%
  select(-organisation, -department_group) %>%
  pivot_longer(cols = -organisation_code, names_to = "measure") %>%
  mutate(
    measure = case_when(
      measure == "E01" ~ "E01_yes",
      measure == "E03" ~ "E03_yes",
      TRUE ~ measure
    )
  )

orgscores_full <- bind_rows(ost1_long, ost2_long) %>%
  mutate(measure = toupper(measure))

orgscores_t31 <- readODS::read_ods("R/data/csps2020_org_scores.ods",
                                   "Table_3_1") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(
    measure = c("rr", "ees", "mw_ts", "op_ts", "mt_ts", "lm_ts", "ld_ts",
                "if_ts", "rw_ts", "pb_ts", "lc_ts",
                "hse_index", "perma_index"),
    section = c("Metadata", rep( "Theme scores", 10), rep("Wellbeing", 2))
  ) %>%
  select(measure, section, label = item_name, description)

orgscores_t32 <- readODS::read_ods("R/data/csps2020_org_scores.ods",
                                   "Table_3_2") %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  transmute(
    measure = case_when(
      question_number == "E01" ~ "E01_yes",
      question_number == "E03" ~ "E03_yes",
      TRUE ~ question_number
    ),
    section = theme,
    label = paste(measure, question_text, sep = ". "),
    description = score_shown
    )

measure_meta <- bind_rows(orgscores_t31, orgscores_t32) %>%
  mutate(measure = toupper(measure))

write_csv(
  orgscores_full,
  file.path("R", "data", "csps2020_orgscores_long.csv")
)

write_csv(
  measure_meta,
  file.path("R", "data", "csps2020_orgscores_measures.csv")
)

write_csv(
  org_meta,
  file.path("R", "data", "csps2020_orgscores_meta.csv")
)
