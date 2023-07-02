library(ggplot2) 
library(tidyverse)  
library(forcats) 
library(scales) 
library(readxl)
library("pastecs")
library("dplyr")
library("xts")
library("zoo")

data = read_excel('K204141908.xlsx')

data

data_before <- data[1:37,]
data_after <- data[38:50,]
print(data_after)

str(data_before)






min_ta <- min(data_before$Total_Assets)
max_ta <- max(data_before$Total_Assets)
mean_ta <- mean(data_before$Total_Assets)
med_ta <- median(data_before$Total_Assets)
std_ta <- sd(data_before$Total_Assets)

cat(paste("Min:", min_ta,
          "Max:", max_ta,
          "Mean:", mean_ta,
          "Median:", med_ta,
          "Standard deviation:", std_ta))

min_lev <- min(data_before$LEV)
max_lev <- max(data_before$LEV)
mean_lev <- mean(data_before$LEV)
med_lev <- median(data_before$LEV)
std_lev <- sd(data_before$LEV)

cat(paste("Min:", min_lev,
          "Max:", max_lev,
          "Mean:", mean_lev,
          "Median:", med_lev,
          "Standard deviation:", std_lev))

min_invest <- min(data_before$investment)
max_invest <- max(data_before$investment)
mean_invest <- mean(data_before$investment)
med_invest <- median(data_before$investment)
std_invest <- sd(data_before$investment)

cat(paste("Min:", min_invest,
          "Max:", max_invest,
          "Mean:", mean_invest,
          "Median:", med_invest,
          "Standard deviation:", std_invest))

min_nfa <- min(data_before$Net_Fixed_Assets)
max_nfa <- max(data_before$Net_Fixed_Assets)
mean_nfa <- mean(data_before$Net_Fixed_Assets)
med_nfa <- median(data_before$Net_Fixed_Assets)
std_nfa <- sd(data_before$Net_Fixed_Assets)

cat(paste("Min:", min_nfa,
          "Max:", max_nfa,
          "Mean:", mean_nfa,
          "Median:", med_nfa,
          "Standard deviation:", std_nfa))

min_ta <- min(data_after$Total_Assets)
max_ta <- max(data_after$Total_Assets)
mean_ta <- mean(data_after$Total_Assets)
med_ta <- median(data_after$Total_Assets)
std_ta <- sd(data_after$Total_Assets)

cat(paste("Min:", min_ta,
          "Max:", max_ta,
          "Mean:", mean_ta,
          "Median:", med_ta,
          "Standard deviation:", std_ta))

min_lev <- min(data_after$LEV)
max_lev <- max(data_after$LEV)
mean_lev <- mean(data_after$LEV)
med_lev <- median(data_after$LEV)
std_lev <- sd(data_after$LEV)

cat(paste("Min:", min_lev,
          "Max:", max_lev,
          "Mean:", mean_lev,
          "Median:", med_lev,
          "Standard deviation:", std_lev))

min_invest <- min(data_after$investment)
max_invest <- max(data_after$investment)
mean_invest <- mean(data_after$investment)
med_invest <- median(data_after$investment)
std_invest <- sd(data_after$investment)

cat(paste("Min:", min_invest,
          "Max:", max_invest,
          "Mean:", mean_invest,
          "Median:", med_invest,
          "Standard deviation:", std_invest))

min_nfa <- min(data_after$Net_Fixed_Assets)
max_nfa <- max(data_after$Net_Fixed_Assets)
mean_nfa <- mean(data_after$Net_Fixed_Assets)
med_nfa <- median(data_after$Net_Fixed_Assets)
std_nfa <- sd(data_after$Net_Fixed_Assets)

cat(paste("Min:", min_nfa,
          "Max:", max_nfa,
          "Mean:", mean_nfa,
          "Median:", med_nfa,
          "Standard deviation:", std_nfa))

boxplot(data_before$Total_Assets, data_before$Net_Fixed_Assets,
main = "boxplots 2 variables before covid",
at = c(1,2),
names = c("TA", "NFA"),
las = 2,
col = c("orange","red"),
border = "brown",
horizontal = TRUE,
notch = TRUE
)

boxplot(data_before$LEV, data_before$investment,
main = "boxplots 2 variables before covid",
at = c(1,2),
names = c("LEV", "Invest"),
las = 2,
col = c("orange","red"),
border = "brown",
horizontal = TRUE,
notch = TRUE
)

hist(data_before$Total_Assets)

hist(data_before$Net_Fixed_Assets)

hist(data_before$LEV)

hist(data_before$investment)

boxplot(data_after$Total_Assets, data_after$Net_Fixed_Assets,
main = "boxplots 2 variables after covid",
at = c(1,2),
names = c("TA", "NFA"),
las = 2,
col = c("orange","red"),
border = "brown",
horizontal = TRUE,
notch = TRUE
)

boxplot(data_after$LEV, data_after$investment,
main = "boxplots 2 variables after covid",
at = c(1,2),
names = c("LEV", "Invest"),
las = 2,
col = c("orange","red"),
border = "brown",
horizontal = TRUE,
notch = TRUE
)

hist(data_after$Total_Assets)

hist(data_after$Net_Fixed_Assets)

hist(data_after$LEV)

hist(data_after$investment)

model1 <- lm(investment ~LEV + Net_Fixed_Assets + Total_Assets, data = data)
summary(model1)

data <- data %>% 
  mutate(covid=0)

data$covid[37:50] = 1


Model2 <- lm(investment ~LEV + Net_Fixed_Assets + Total_Assets + 
             LEV*covid + Net_Fixed_Assets*covid + Total_Assets*covid, data = data)
summary(Model2)

library(caret) 
set.seed(908)
testing.samples = data$investment %>%
  createDataPartition(p = 1, list = FALSE)
test.data  = data[testing.samples, ]

# Make predictions
predictions = model1 %>% predict(test.data)
# Model performance

# R-square
R2(predictions, test.data$investment)

data$investment_predict = predictions
View(select(data, investment, investment_predict))

class(data) 
data$time = as.Date(data$time)

data_ts = xts(data$investment, data$time) 
data_ts
class(data_ts)

library(forecast)
library(tseries)

plot(data_ts, xlab='Time', ylab = 'Investment')
acf(data_ts)
pacf(data_ts)
adf.test(data_ts)

#Convert the non-stationary data into stationary
invest_model = auto.arima(data_ts, ic = "aic", trace = TRUE)
invest_model
#Countinue to check stationary
acf(ts(invest_model$residuals))
pacf(ts(invest_model$residuals))
#Prediction
forec_invest = forecast(invest_model, level = c(95), h=4)
forec_invest

forecast_values <- forec_invest$mean
forecast_dates <- seq(as.Date("2022-01-01"), by = "quarter", length.out = 4)

# Extract the actual (realized) values and their corresponding time periods
actual_values <- data$investment[(nrow(data) - 3):nrow(data)]
actual_dates <- seq(as.Date("2022-01-01"), by = "quarter", length.out = 4)

plot(forecast_dates, forecast_values, type = "b", pch = 16, col = "blue",
     ylim = range(c(forecast_values, actual_values)),
     xlab = "Time", ylab = "Investment")
lines(actual_dates, actual_values, type = "b", pch = 16, col = "red")
legend("topright", legend = c("Forecasted", "Realized"),
       col = c("blue", "red"), pch = 16)


