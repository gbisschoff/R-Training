source('./code/0-setup.R')

# data import -----------------------------------------------------------------------------------------------------
#' Write a function that imports the clean data template with the additional columns.
#' This makes it easy so seperate the data cleaning from using the data in modelling
#' or analysis

import_data = function(file_path){
  data = rio::import(file_path, setclass = 'tbl') %>% 
    dplyr::mutate(
      account_id = as.factor(account_id),
      period = as.Date(period),
      origination_date = as.Date(origination_date),
      maturity_date = as.Date(maturity_date),
      interest_rate = as.double(interest_rate),
      product_type = factor(product_type, levels = c("unauthorised overdraft", "overdraft", "loan", "leasing")),
      arrears_status = factor(arrears_status, levels = c('P', 'SM', 'D', 'L', 'SS'), ordered = TRUE),
      stage = factor(stage, levels = c(1, 2, 3), ordered = TRUE),
      account_balance = as.double(account_balance),
      contract_term = as.integer(contract_term),
      time_on_book = as.integer(time_on_book),
      remaining_term = as.integer(remaining_term)
    ) %>%
    dplyr::arrange(account_id, period)
  return(data)
}

data = import_data('./data/master-data-clean.csv.gz')

# basic data analysis ---------------------------------------------------------------------------------------------

data %>% 
  group_by(period, product_type) %>% 
  summarise(total = sum(account_balance)) %>% 
  plotly::plot_ly(x = ~period, y = ~total, color = ~product_type, type = 'scatter', mode = 'line')


data %>% 
  group_by(period, stage) %>% 
  summarise(total = sum(account_balance)) %>% 
  mutate(total = total / sum(total)) %>% 
  plotly::plot_ly(x = ~period, y = ~total, color = ~stage, type = 'bar') %>% 
  plotly::layout(barmode='stack')


data %>% 
  filter(product_type == 'loan') %>% 
  group_by(period, stage) %>% 
  summarise(total = sum(account_balance)) %>% 
  mutate(total = total / sum(total)) %>% 
  plotly::plot_ly(x = ~period, y = ~total, color = ~stage, type = 'bar') %>% 
  plotly::layout(barmode='stack')


data %>% 
  filter(period == max(period)) %>% 
  group_by(product_type, time_on_book) %>% 
  summarise(total = sum(account_balance)) %>% 
  plotly::plot_ly(alpha = 0.7, type = 'bar', x = ~time_on_book, y = ~total, color = ~product_type) %>% 
  plotly::layout(barmode='overlay')
