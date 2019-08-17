# functions -------------------------------------------------------------------------------------------------------

add_age_bucket <- function(data, ...){
  data %>% mutate(
    age_bucket = cut(time_on_book, ...)
  )
}

add_default_indicator <- function(data){
  data %>% mutate(
    default_indicator = case_when(
      stage == '1' ~ FALSE,
      stage == '2' ~ FALSE,
      stage == '3' ~ TRUE,
      TRUE ~ NA
    )
  )
}

default_rates <- function(data, ..., emergence_period = 12L) {
  .dots = c(...) # convert the '...' to a vector
  data %>% 
    group_by(account_id) %>% 
    mutate(
      lead_default_indicator = (lead(cumsum(default_indicator), n = emergence_period) - cumsum(default_indicator)) > 0
    ) %>% 
    group_by_at(.vars = .dots) %>% 
    group_by(default_indicator, lead_default_indicator, add = TRUE) %>% 
    summarise(total = sum(account_balance)) %>% 
    mutate(default_rate = total / sum(total)) %>% 
    filter(default_indicator == FALSE & lead_default_indicator == TRUE) %>% 
    ungroup() %>% 
    select(-total, -default_indicator, -lead_default_indicator)
}

# pd modelling ----------------------------------------------------------------------------------------------------


data %>%
  add_default_indicator() %>% 
  default_rates('period', 'stage') %>% 
  plotly::plot_ly(x = ~period, y=~default_rate, color=~stage, type = 'scatter', mode = 'line')

data %>%
  add_default_indicator() %>% 
  default_rates('time_on_book', emergence_period = 1L) %>% 
  plotly::plot_ly(x = ~time_on_book, y=~default_rate, type = 'scatter', mode = 'line')


data %>%
  add_age_bucket(breaks = seq(0, 120, 12), include.lowest = TRUE) %>% 
  add_default_indicator() %>% 
  default_rates('age_bucket', emergence_period = 1L) %>% 
  plotly::plot_ly(x = ~age_bucket, y=~default_rate, type = 'scatter', mode = 'line')


data %>%
  add_age_bucket(breaks = seq(0, 120, 12), include.lowest = TRUE) %>% 
  add_default_indicator() %>% 
  default_rates('stage', 'age_bucket', emergence_period = 12L) %>% 
  plotly::plot_ly(x = ~age_bucket, y=~default_rate, color = ~stage, type = 'scatter', mode = 'line')


data %>%
  add_default_indicator() %>% 
  mutate(origination_date = as.character(format(origination_date, format = '%Y-%m'))) %>% 
  default_rates('origination_date', 'time_on_book', emergence_period = 1L) %>% 
  plotly::plot_ly(x = ~time_on_book, y=~default_rate, color = ~origination_date, type = 'scatter')

