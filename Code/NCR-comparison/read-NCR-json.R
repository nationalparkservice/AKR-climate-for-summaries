library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)

SiteID = 

# Define the URL
url <- "https://earthmaps.io/indicators/base/area/NPS17"

# Fetch the data from the website
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  json_data <- content(response, "text", encoding = "UTF-8")
  parsed_data <- fromJSON(json_data, flatten = TRUE)
  
  # Recursively flatten nested lists into a dataframe
  df <- tibble::tibble()
  
  for (indicator in names(parsed_data)) {
    for (era in names(parsed_data[[indicator]])) {
      for (model in names(parsed_data[[indicator]][[era]])) {
        for (scenario in names(parsed_data[[indicator]][[era]][[model]])) {
          temp <- parsed_data[[indicator]][[era]][[model]][[scenario]] %>%
            as_tibble() %>%
            mutate(
              indicator = indicator,
              era = era,
              model = model,
              scenario = scenario
            )
          df <- bind_rows(df, temp)
        }
      }
    }
  }
  
  # Ensure consistent column names and types
  df <- df %>%
    relocate(indicator, era, model, scenario, .before = everything()) %>%
    mutate(across(c(min, mean, max), as.numeric))
  
  # Print dataframe
  print(df)
} else {
  print(paste("Failed to fetch data. Status code:", status_code(response)))
}
