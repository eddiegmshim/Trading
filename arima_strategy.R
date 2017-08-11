#Simple strategy playing with ARIMA forecasting
#If price forecasted at t+1 is greater than at t, then buy/hold stock
#If price forecasted at t+1 is less than at t, then sell stock/ do nothing

{install.packages('quantmod')
install.packages('ggplot2')
install.packages('forecast')

require(quantmod)
require(ggplot2)
require(forecast)
}

arima_strat = function(stock_ticker, date, days_timeframe){
  start_date = date - days_timeframe
  end_date = date
  getSymbols(stock_ticker, src = 'google', from = date - days_timeframe, to = date) 
  
  price = as.numeric(Cl((get(paste(stock_ticker)))))

  log_price = log(price)
  log_returns = diff(log_price)
  
  # all_days = seq(as.Date(start_date), as.Date(end_date), by = 'days')
  # day_index = length(all_days) - days_timeframe+1
  # days = all_days[day_index: length(all_days)]
  # qplot(days,price) + geom_line() #plot prices
  # qplot(days[2:length(days)], log_returns) + geom_line() #plot log returns
  
  arima_model = auto.arima(log_price)
  # summary(arima_model)
  
  pred_log_return = forecast(arima_model,1)
  pred_price = exp(as.numeric(pred_log_return$mean))
  
  pred_profit = pred_price - price[length(price)]
  return(pred_profit)
}

perform_strat = function(stock_ticker, start_date, end_date, days_timeframe){
  getSymbols(stock_ticker, src = 'google', from = start_date, to = end_date) 
  dates = index(get(paste(stock_ticker)))
  length_days = length(dates)
  
  array_pred_profit = c()
  for(i in 0:(length_days-1)){
    date = start_date + i
    
    pred_profit = arima_strat(stock_ticker, date, days_timeframe)
    array_pred_profit = c(array_pred_profit, pred_profit)
  }
  
  return(array_pred_profit)
}

stock_ticker = 'SPY'
start_date = as.Date('2016-01-01')
# end_date = Sys.Date() #grabs todays date
end_date = as.Date('2017-01-01')
days_timeframe = 100

model_predict = perform_strat(stock_ticker, start_date, end_date, days_timeframe)
buy_sell = sign(sign(model_predict)+1) #converts price predictions into binary actions. 1 = buy/hold, 0 = sell/do nothing


getSymbols(stock_ticker, src = 'google', from = start_date, to = end_date)
true_returns = as.numeric(dailyReturn(get(paste(stock_ticker))))

strategy_returns = buy_sell*true_returns
strategy_returns = sum(buy_sell * true_returns)







