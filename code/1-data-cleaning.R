source('./code/0-setup.R')

# data import -----------------------------------------------------------------------------------------------------
#' https://rdrr.io/cran/rio/man/import.html
#' 
#' Create a function that takes the data and maps the column types correctly,
#' creates extra columns, and cleans the data.
#' It requires that the data that is being read in be in an appropriate data template.
#' A data template increases code re-use because the cleaning code does not have to change each
#' time you read in new data.
#' Export the cleaned data into a cleaned dataset so that other team members can use it
#' or at least develop models that use the cleaned data template before they have the cleaned data.


# Helper functions ------------------------------------------------------------------------------------------------


period_to_date <- function(period){
  as.Date(paste0(period, '01'), format='%Y%m%d', optional = TRUE)
}

as_month_end_date <- function(date) {
  lubridate::ceiling_date(date, "month") - lubridate::days(1)
}

clean_data <- function(data){
    data %>% dplyr::mutate(
      account_id = as.factor(account_id),
      period = as_month_end_date(period_to_date(period)),
      origination_date = period_to_date(origination_date),
      maturity_date = period_to_date(maturity_date),
      interest_rate = as.double(interest_rate),
      product_type = factor(product_type, levels = c("unauthorised overdraft", "overdraft", "loan", "leasing")),
      arrears_status = factor(arrears_status, levels = c(1, 2, 3, 4, 5), labels = c('P', 'SM', 'D', 'L', 'SS')),
      stage = factor(stage, levels = c(1, 2, 3)),
      account_balance = as.double(account_balance)
    ) %>%
      dplyr::arrange(account_id, period) %>% 
      dplyr::mutate(
        contract_term = 12 * (zoo::as.yearmon(maturity_date) - zoo::as.yearmon(origination_date)),
        time_on_book = 12 * (zoo::as.yearmon(period) - zoo::as.yearmon(origination_date)),
        remaining_term = 12 * (zoo::as.yearmon(maturity_date) - zoo::as.yearmon(period)),
      ) %>% 
    return()
}


# Import and clean data -------------------------------------------------------------------------------------------
#' Directly import the data and clean it before saving the data. You dont want to save multiple copies of the
#' data in memory.
#' 
#' Export the cleaned data so that you dont have to re-clean the data again. 

data = rio::import('.data/master-data.csv.gz', setclass = 'tbl') %>% 
  clean_data()

rio::export(data, file = '.data/master-data-clean.csv.gz')


