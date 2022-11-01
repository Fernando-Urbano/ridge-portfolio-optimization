# RIDGE Regularization for Efficient Frontier Portfolio Optimization in R
### Introduction to RIDGE
Often, efficient frontier leads to considerably extreme weights and even negative ones. Neverthless, when trying to optimize an efficient frontier, portfolio managers choose the stocks they want in the portfolio prior to running the optimization algorithm and hope to have all the stocks with a resonable weight. When portfolio managers find that their portfolio optimization with a conventional efficient frontier method leads to negative or null weights, they often make "manual" adjustment or create constraints for min and max weight per stock.

The "manual" adjustment or constraint can solve the problem, nonethless, they are not the more efficient and correct way to get the best portfolio allocation.

In this repository, we show how RIDGE, a method for regularization often used in regression, can also be applied in efficient frontier.

### RIDGE in Regression
RIDGE is used in regression to reduce the effect of features in order to avoid overfitting.
Steady of minimizing a function of OLS (Ordinary Least Squares), the RIDGE algorith proposes the minimization of OLS + penalization factor. The penalization factor is composed by a ```lambda``` multiplied by the sum of the squares of ```betas```. The formulas are described ahead:

```math
Y = \beta_{0} + \beta_{1} X_{1} + \beta_{2} X_{2} + \beta_{3} X_{3} + ... + \beta_{n} X_{n}
```

```math
minimize(OLS + \lambda * (\sum_{i=1} ^ {n} \beta_{i}^{2}))
```

Therefore, parameters that minimize the OLS are not the sane parameters that minimize the new function.
The beta parameters have a tendencdy to be smaller, since there is a penalization for bigger betas.
The lambda is a hyper parameter chosen by test and error using training and testing to understand which level of regularization is best to predict new values.

### RIDGE in Efficient Frontier
RIDGE has a similar purpose in efficient frontier. Steady of having the betas in the penalizer function, it is presented having the sum of squares of the weights. Therefore, when trying to minimize the variance of a portfolio by choosing the weights, we do not only consider the variance but also the penalizer.

```math
minimize(f(w_{1}, w_{2}, ..., w_{n}) = \sigma_{portfolio} + \lambda * (\sum_{i=1} ^ {n} w_{i}^{2}))
```
```math
constraint: (\sum_{i=1} ^ {n} w_{i}) = 1
```

Considering that the sum of weights must always be equal to 1, the closer the weights are to the average of weights, the smaller the penalizer will be. Therefore, the bigger the lambda (hyper parameter of penalization), the closer the optimal portfolio weights will be to 1 / (nº of stocks). The lambda is defined in the same way as it would be defined in a regression using RIDGE.

### Methodology
We now describe the steps taken to obtain the optimal portfolio according to RIDGE:
_Goal_: Find the best weights for the portfolio without extreme allocations with stocks selected apriori.
1) *Stocks choice*: ten stocks are chosen for the portfolio. The Yahoo Finance API is used to obtain the stocks.
2) *Division of Training and Testing*: stock's prices are transformed in returns. The returns are divided in training and testing. The training period is composed by 2 years and the testing period is the most recent 1 year.
3) *Creation of Possible Portfolio Weights*: `expand.grid` function is used to create many different combinations of portfolio weights. The function creates a dataframe in which each row represents a different portfolio weight combination. For instance, the first row in column `w1` represents the weight that the first stock has in the first combination. Because the sum of weights is not always equal to 1, the weights are divided by the sum of weights (the sum of the values in the row).
4) *Calculation of Variance for Portfolios in the Training Sample*: variance is calculated for each row in the dataframe created by expand.grid. The calculation is done by the multiplication of matrixes.
5) *Calculation of Penalizer for Portfolios in Training Sample*: penalizer is calculated considering the sum of the squares of weights. The penalizer is them multiplied by many different lambdas. After that, for each value of lambda, the portfolio that has the smallest function (variance + penalizer) is chosen. For instance, if 30 different lambdas as tested, there will be 30 different portfolios that are the ones that minimize the function.
6) *Best Lambda Search in Test Sample*: the portfolios chosen by the lambdas are than tested in the testing sample (last year). The testing includes the calculation of average returns, volatility, and Sharpe ratio.
7) *Lambda Choice*: the lambda is chosen by selecting the lambda that leads to the portfolio with best Sharpe ratio. Alternatively, one can choose the lambda by selectiing the lambda that leads to the portfolio with the smallest volatility.
8) *Reusing the Selected Lambda in the In-Sample*: with the selected lambda, we now calculate the volatility of each portfolio (changes are due to weights) for the in-sample (training sample + testing sample). The volatility is than added to the penalizer (sum of squares of weights multiplied by the lambda with the best performance in the testing sample).
9) *Choice of Portfolio*: the selected weights are the ones that lead to the portfolio with the smallest function (`variance + penalizer * selected lambda`).

```
"RIDGE for Portfolio Optimization."
library(tidyverse)
library(lubridate)
library(quantmod)
library(readxl)
try(library(Rblpapi), silent = TRUE) # For Bloomberg

# Opt only necessary if using Bloomberg
opt <- c("periodicitySelection" = "DAILY")

# Stocks used in the portfolio
selected_stocks = tribble(
  ~ ticker, ~ sector,
  "BBAS3", "Banks", 
  "BBSE3", "Insurers",
  "BBDC4", "Banks",
  "CSMG3", "Sanitation",
  "ENBR3", "Electrical",
  "GGBR4", "Steel Production",
  "ODPV3", "Health Care",
  'TAEE11', "Electrical",
  "TRPL4", "Electrical",
  "VALE3", "Mining"
)

# Stock tickers
stock_tickers = selected_stocks$ticker

# get_data_opt: TRUE is default
get_data_opt = TRUE 
# us_stock_opt: TRUE if stocks are from the US (FALSE if they are from Brazil)
us_stock_opt = FALSE 
# bloomberg_opt: TRUE if a Bloomberg terminal and Rblpapi are available
bloomberg_opt = FALSE 

# By default, we use training sample of 2 years and testing sample of 1 year
begin_training = today() - years(3)
end_training = today() - years(1) - 1
begin_testing = today() - years(1)
end_testing = today()

stocks_db = tibble(date = as.numeric()) %>% 
  mutate(date = ymd(date))

if (get_data_opt == TRUE){
  
  try(blpConnect(), silent = TRUE)
  
  get_stock_data = function (
    stock_name,  start_date = today() - years(5),
    bloomberg_data = FALSE, us_stock = FALSE
  ){
    
    if (bloomberg_data == FALSE){
      
      if (us_stock == FALSE){
        
        stock_name = paste0(stock_name %>% str_remove("[.]SA"), ".SA")
        
      } else {
        
        stock_name = stock_name %>% str_remove("[.]SA")
        
      }
      
      new_stock = quantmod::getSymbols(
        stock_name,
        from = as.Date(start_date),
        src='yahoo',
        auto.assign = FALSE
      ) %>% 
        data.frame() %>%
        select(contains('Adjusted')) %>% 
        purrr::set_names(stock_name %>% str_remove("[.]SA")) %>% 
        rownames_to_column("date") %>% 
        mutate(date = ymd(date))
      
    } else {
      
      opt = c(
        "periodicitySelection"="DAILY", 'CshAdjAbnormal'=TRUE,
        "CshAdjNormal"=TRUE, "CapChg"=TRUE
      )
      
      stock_name = paste0(stock_name %>% str_remove("[.]SA"), " US Equity")
      
      new_stock = bdh(
        securities = stock_name,fields = c("PX_LAST"),
        start.date = start_date, options = opt
      ) %>% 
        purrr::set_names("date", stock_name %>% str_remove(" US Equity"))
    }
    
    stocks_db <<- stocks_db %>% 
      full_join(new_stock, by = 'date')
    
    print(paste(stock_name %>% str_remove("[.]SA"), "added."))
  }
  
  purrr::map(
    stock_tickers, ~ get_stock_data(
      ., bloomberg_data = bloomberg_opt, us_stock = us_stock_opt
    )
  )
  
  stocks_db %>%
    write_csv("ridge_stocks.csv") # Write prices to current working directory
  
}

stocks_db = read_csv("ridge_stocks.csv", col_types = c("date" = "D"))
```


