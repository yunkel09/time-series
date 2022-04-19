
forecast::tsCV()

dat <- fpp2::goog
vol <- ts(volcan_raw, start = c(1500))


far1 <- function(x, h){forecast::forecast(forecast::Arima(x, order(1, 0, 0), include.constant = TRUE))}

e <- tsCV(dat, forecastfunction = naive, h = 8, initial = 800)
f <- tsCV(vol, forecastfunction = naive, h = 8, initial = 800)


mse_tscv <- colMeans(e^2, na.rm = TRUE)
tl <- data.frame(MSE_TSCV = mse_tscv)
data.frame(h = 1:8, MSE = mse_tscv) %>%
	ggplot(aes(x = h, y = MSE)) +
	geom_point()