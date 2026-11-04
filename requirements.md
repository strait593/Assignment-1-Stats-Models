# Requirements for the assignemnt

1. Download the precipitation data for the Betania basin and read it in
2. Divide the data into a random training and test set with a ratio of 80/20
3. Predict caudal (water inflow) with the rest of the columns as covariates (exclude the Fecha/Date column)
4. Use multiple linear regression, ridge regression, LASSO and elastic net
5. Find the best model according to RMSE
6. Build your own function to calculate the Nash-Sutcliffe Efficiency
### Choose the best model according to the Nash-Sutcliffe Efficiency (NSE)