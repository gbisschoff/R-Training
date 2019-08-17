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
#' In this section we will see how to calculate default probabilities for portfolios that have sufficient data.
#' We first calculate the average 12 month default rate for each period to see what the trends are over time.
#' If the trend is stable over time this allows us to continue without having to segment on origination cohort (out
#' of scope for this training).
#' 
#' From the first analysis we see that the stage 2 default rates are increasing and that the default rates are very
#' volatile. This means that we need to do further investigation (out of scope), but is likely to low data volumes.

data %>%
  add_default_indicator() %>% 
  default_rates('period', 'stage', emergence_period = 12L) %>% 
  plotly::plot_ly(x = ~period, y=~default_rate, color=~stage, type = 'scatter', mode = 'line')

#' Assuming the trends were stable and not volatile we would build a term structure by segmenting on time on book.
#' From the analysis below we see that the term structure is very volatile due to low data volumes and becomes
#' more volatile as time on book increases. This makes sense because the loans get paid off, thus there is less
#' accounts past the end of the contract term.
data %>%
  add_default_indicator() %>% 
  default_rates('time_on_book', emergence_period = 1L) %>% 
  plotly::plot_ly(x = ~time_on_book, y=~default_rate, type = 'scatter', mode = 'line')


#' we could group time on book into age buckets to increase the data volumes in each segment. We could also try to
#' smooth the term structure using some form of averaging, smoothing or regression (out of scope).
#' We can also increase or decrease the bucket size to become more accurate or less volatile.
data %>%
  add_age_bucket(breaks = seq(0, 120, 12), include.lowest = TRUE) %>% 
  add_default_indicator() %>% 
  default_rates('age_bucket', emergence_period = 1L) %>% 
  plotly::plot_ly(x = ~age_bucket, y=~default_rate, type = 'scatter', mode = 'line')


#' we could also split the term structure by age to increase accuracy. Remember the relationship between 
#' accuracy, volatility and data volumes.
data %>%
  add_age_bucket(breaks = seq(0, 120, 12), include.lowest = TRUE) %>% 
  add_default_indicator() %>% 
  default_rates('stage', 'age_bucket', emergence_period = 12L) %>% 
  plotly::plot_ly(x = ~age_bucket, y=~default_rate, color = ~stage, type = 'scatter', mode = 'line')

#' If we found that the there is a trend in the default rates over time we have to segment by origination date
#' to allow for these trends. This can easily be done by just passing the origination segment along with time on book.
#' Once again we could group origination date and time on book if data volumes dont allow for the granular segmentation
#' or we could use some form of triangle completion on the results to increase data volumnes before doing smooting,
#' averaging or regression.
data %>%
  add_default_indicator() %>% 
  mutate(origination_date = as.character(format(origination_date, format = '%Y-%m'))) %>% 
  default_rates('origination_date', 'time_on_book', emergence_period = 1L) %>% 
  plotly::plot_ly(x = ~time_on_book, y=~default_rate, color = ~origination_date, type = 'scatter')

