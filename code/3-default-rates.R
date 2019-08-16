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


default_rates <- function(data, ..., emergence_period = 1L) {
  .dots = c(...) # convert the '...' to a vector
  data %>% 
    group_by(account_id) %>% 
    mutate(
      lead_default_indicator = (lead(cumsum(default_indicator), n = emergence_period) - cumsum(default_indicator)) > 0
    ) %>% 
    filter(
        !is.na(default_indicator),
        !is.na(lead_default_indicator),
        !is.na(account_balance),
        account_balance > 0
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

#' one month default rates. We should see that there are not many defaults from stage 1, because those are 
#' usually fraud cases. 
#' 
#' We see that the default rates are quite volatile around 8% - 9%, maybe with a slight increasing trend.
#' Volatility means we dont have enough data in that segment. This can be verified by creating a graph of number 
#' of accounts per period and stage.
#' We see there is a large spike in Jan 2017, this will require further investigation to identify the root cause.
#' 
#' The default rates however aren't IFRS 9 compliant because it does not contain a term structure. For us to add a
#' term structure we have to segment by time on book.

data %>%
  add_default_indicator() %>% 
  default_rates('period', 'stage') %>% 
  plotly::plot_ly(x = ~period, y=~default_rate, color=~stage, type = 'scatter', mode = 'line')

#' To create a term structure, we segment by time on book. 
#' We see large amounts of volatility which means that we dont have enough data in each segment.
#' The volatility increases as time on book increases because there are less accounts in those
#' segments - see the time-on-book distibution
#' 

data %>%
  add_default_indicator() %>% 
  default_rates('stage', 'time_on_book') %>% 
  plotly::plot_ly(x = ~time_on_book, y=~default_rate, color = ~stage, type = 'scatter', mode = 'line')

#' we can create less granular segmentation using bucketing. We will bucket the time on book into age
#' buckets using the cut function, we specify that each bucket should be 12 months. From the time on 
#' book distribution we see that we dont have enough data past 60 months so we will drop the remaining
#' times on book

data_with_age_bucket <- data %>%
  add_age_bucket(breaks = seq(0, 60, 12), include.lowest = TRUE)

data_with_age_bucket %>% 
  group_by(product_type, age_bucket) %>% 
  summarise(total = sum(account_balance)) %>% # change sum(account_balance) to n() so see how many accounts there are in each bucket
  plotly::plot_ly(type = 'bar', x = ~age_bucket, color = ~product_type, y = ~total) %>% 
  plotly::layout(barmode = 'stack')

#' To build a term structure on our new age buckets we we just group by age bucket and stage.
#' To build a 12 month PD for each age bucket we just increase the emergence period. 
#' IMPORTANT: Remember as you increase the emergence period, more accounts will be filtered out by the
#' !is.na(lead_default_indicator) filter. Because there are less accounts that are on book for the full 
#' duration of the emergence period. Thus this code cant directly be used to calculate a lifetime PD.

# One month default rates
data_with_age_bucket %>% 
  add_default_indicator() %>% 
  default_rates('stage', 'age_bucket', emergence_period = 1L) %>% 
  plotly::plot_ly(x = ~age_bucket, y=~default_rate, color = ~stage, type = 'scatter', mode = 'line')

# 12 month default rates
data_with_age_bucket %>% 
  add_default_indicator() %>% 
  default_rates('stage', 'age_bucket', emergence_period = 12L) %>% 
  plotly::plot_ly(x = ~age_bucket, y=~default_rate, color = ~stage, type = 'scatter', mode = 'line')




