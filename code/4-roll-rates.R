
# functions -------------------------------------------------------------------------------------------------------

matrix_power <- function(matrix, n) {
  if(n == 1) return(matrix)
  m = matrix
  for(i in 1:(n-1)) {
    m = m %*% matrix
  }
  return(m)
}

create_term_structure <- function(start, rr_matrix, n) {
  term_structure = array(dim = c(n + 1, 3))
  term_structure[1,] = start
  for(t in 2:(n + 1)) {
    term_structure[t,] = term_structure[t-1,] %*% rr_matrix
  }
  colnames(term_structure) <- c('S1', 'S2', 'S3')
  tibble::as_tibble(term_structure) %>% 
    mutate(
      forecast_horizon = 1:n() - 1,
      marginal_default_rate = S3 - lag(S3, default = 0)
    ) %>% 
  return()
}

roll_rates <- function(data, ...) {
  .dots = c(...) # convert the '...' to a vector
  data %>% 
    group_by(account_id) %>% 
    mutate(
      to_stage = lead(stage)
    ) %>% 
    filter(
      !is.na(stage),
      !is.na(to_stage),
      !is.na(account_balance),
      account_balance > 0
    ) %>% 
    group_by_at(.vars = .dots) %>% 
    group_by(stage, to_stage, add = TRUE) %>% 
    summarise(total = sum(account_balance)) %>% 
    mutate(roll_rate = total / sum(total)) %>% 
    ungroup() %>% 
    select(-total)
}

# roll rates ------------------------------------------------------------------------------------------------------
#' When working with insufficient data volumes, eg. corporate or low volume retail portfolios, to create a
#' default rate term structure it is usually better to look at a roll rate approach. In this methodology 
#' you calculate the roll rate matrix and then impute a term structure from it. This term structure is not
#' technically IFRS 9 compliant because it does not take into account the age of a loan. This could be done 
#' by calculating different roll rate matixes for each age on book and multiplying them out starting from the
#' accounts age. This is out of scope for this training.
#' 
#' You have to have the default state as an absorbing state otherwise you wouldn't be calculating the 
#' probability of default, but rather the proportion that would be in default. We do this by overwritting 
#' the stage three migrations to be zero.
#' 
#' We convert the roll rates into a term structure by multiplying the matrix by itself for each forecast horizon.
#' This gives us the cumulative default probabilities, we convert this into a marginal probability by 
#' taking the first differences.
#' 
#' Once you have the marginal probabilities you can use it so calculate survival probabilities, default 
#' probabilities including the life-time default probability.

rr = data %>% 
  roll_rates()

rr_matrix = rr %>% 
  mutate(
    roll_rate = if_else(stage == 3, 0, roll_rate),
    roll_rate = if_else(stage == 3 & to_stage == 3, 1, roll_rate)
  ) %>% 
  tidyr::spread(key=to_stage, value=roll_rate) %>% 
  select(-stage) %>% 
  as.matrix()

#' by multiplying the roll rate matrix by itself will give you the 12 month migration matrix. from this
#' matrix we can just read off the correct probability.
rr_12 <- matrix_power(rr_matrix, 12)

#' we can repeat the above exercise to calculate the probability for each forecast horizon. 
#' You have to specify the start state as c(1, 0, 0) to calculate the probabilities from stage 1, and c(0, 1, 0)
#' to calculate the probabilities from stage 2.
ts <- create_term_structure(start = c(1, 0, 0), rr_matrix = rr_matrix, n = 120)

ts %>% 
  plotly::plot_ly(x = ~forecast_horizon, y = ~marginal_default_rate, type = 'scatter')

#' to calcuate the 12 month default probability we first have to calculate the marginal survival 
#' probabilities and then multiply them to calculate the total surival probability over the forecast 
#' horizon. 1 minus this survival probability gives us the default probability over the same horizon. 
PD12 <- 1 - prod((1 - ts$marginal_default_rate)[1:13])

#' To calculate the life time probability of default we have to assume the what 'life-time' means. Usually you
#' would first calculate the behavioural life. This is out of scope for now, so lets assume that the behavioural 
#' life is 60 months because that is where the time on books distribution ends. 
#' 
#' Note that if an account is already 30 months on book, the remaining behavioural life is another 30 months.
#' 
#' So the life-time default probability for an account that is 0 months on book would be:
PD_life <- 1 - prod((1 - ts$marginal_default_rate[1:61]))

#' And the life-time default probability for an account that is 30 months on book would be:
PD_life <- 1 - prod((1 - ts$marginal_default_rate[1:31]))
