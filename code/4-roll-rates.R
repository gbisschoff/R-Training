
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
  return(term_structure)
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

rr_12 <- matrix_power(rr_matrix, 2)

ts <- create_term_structure(start = c(1, 0, 0), rr_matrix = rr_matrix, n = 120)
plot(ts[,3])
plot(ts[,3]- lag(ts[,3]))

