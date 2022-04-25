# extract and process

csps2020_median_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/977276/Civil_Service_People_Survey_2009_to_2020_Median_Benchmark_Scores.csv"

download.file(
  csps2020_median_url,
  file.path("R", "data", "csps2020_median_timeseries.csv")
)
