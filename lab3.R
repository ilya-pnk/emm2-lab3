set.seed(123)
n =2100
theta =-0.3
theta2=0.4
A <- c(1, 0.2, 0.1, 0.2)

# Инициализация
x = numeric(n)
sigma = numeric(n)
eps = rnorm(n)

# AR(2)ARCH(3)
for (t in 4:n) {
  sigma[t] <- A[1] + A[2] * x[t-1]^2 + A[3] * x[t-2]^2 + A[4] * x[t-3]^2
  x[t] <- theta * x[t-1] + theta2 * x[t-2] + sqrt(sigma[t]) * eps[t]
}

# Построение графика
plot(x, type = "l", main = "AR(2)ARCH(3) процесс", xlab = "Время", ylab = "Значение")

## разделим выборку на обуч-ся и тестовую 
study_size = floor(20/21 * n)
test_size = n - study_size
x_study = x[1:study_size]
x_test = x[(study_size + 1):n]


library(tseries)
#оценка параметров
ar2 = arima(x_study, order = c(2, 0, 0), include.mean = FALSE)
theta_new = coef(ar2)
theta_new;

residuals=residuals(ar2)
## оценка А процесса ARCH(3) при помощи garch() 
ARCH3 = garch(residuals, order=c(0,3))
summary(ARCH3)

A_new = coef(ARCH3)
A_new
x_forecast = rep(NA, test_size)
sigma_forecast = rep(NA, test_size)

# Прогнозируем на один шаг вперед по формуле
for (i in 1:test_size) {
  turn <- study_size + i  # Индекс текущего шага для тестовой выборки
  
  # Прогноз среднего значения 
  Xn <- c(x[turn-1], x[turn-2])  # Вектор значений Xn для AR(2)
  x_forecast[i] <- sum(theta_new * Xn)  # Прогноз x_n+1|n
  
  # Прогноз дисперсии 
  sigma_forecast[i] <- sqrt(A_new[1] + A_new[2] * x[turn-1]^2 + A_new[3] * x[turn-2]^2 + A_new[4] * x[turn-3]^2)
}
# Границы прогноза
up_end = x_forecast + sigma_forecast
down_end = x_forecast - sigma_forecast

#  график фактических значений процесса 
plot(x_test, type = "l", col = "blue", main = "Прогноз на 1 шаг вперед", 
     xlab = "Наблюдения", ylab = "Значение процесса", ylim = range(c(down_end, up_end, x_test)))

#  прогнозы 
points(x_forecast, col = "black", pch = 1)

#  границы прогноза волатильности 
lines(up_end, col = "red", lty = 2)
lines(down_end, col = "red", lty = 2)

# подписи для графиков
legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("blue", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))




data= read.csv2("C:/Users/User/Downloads/GAZP.csv", sep= ";")
str(data)
print(data)


# График динамики актива
plot( data$X.CLOSE,
     type = "l", main = "Газпром акции(каждые 5 мин/сут.)" , xlab =  "время(мин)", ylab = "Цена")

log_data = diff(log(as.numeric(data$X.CLOSE)))
log_data
log_returns = log(as.numeric(data$X.CLOSE.[-1]) / as.numeric(data$X.CLOSE.[-nrow(data)]))
log_returns
plot(log_data, type = "l", , main = "График доходности актива",
     xlab = "Наблюдения", ylab = "Доходность")



study_z = floor(20/21 * length(log_data))  #  размер обучающей выборки
test_z = length(log_data) - study_z  # Размер тестовой выборки


study_data_z =log_data[1:study_z]  # Обучающая выборка
test_data_z = log_data[(study_z + 1):length(log_data)]  # Тестовая выборка



# Оценка модели AR(2)
ar_Z = arima(study_data_z, order = c(2, 0, 0), include.mean = FALSE)
theta_z = coef(ar_Z)[1:2]  # Оценка параметров theta
theta_z

residuals_z <- residuals(ar_Z)

# Оценка ARCH(3)
arch_Z = garch(residuals_z, order = c(0, 3))
summary(arch_Z)

# параметры
arch_param_Z = coef(arch_Z)
arch_param_Z
# Прогноз AR(2)
ar_Z_predict = predict(ar_Z, n.ahead = 10)


# Прогнозирование на один шаг вперед 
x_predict_z =rep(NA, test_z)
sigma_predict_z = rep(NA, test_z)

for (i in 1:test_z) {
  turn_z <- study_z + i  # Индекс текущего шага для 
  
  # Прогноз среднего значения 
  Xn_z <- c(log_data[ turn_z-1], log_data[ turn_z-2])  # Вектор значений Xn для AR(2)
  x_predict_z[i] <- sum(theta_z * Xn_z)  # Прогноз z_n+1|n
  
  # Прогноз дисперсии 
  sigma_predict_z[i] <- sqrt(arch_param_Z[1] + arch_param_Z[2] * log_data[turn_z-1]^2 + arch_param_Z[3] * log_data[turn_z-2]^2 + arch_param_Z[4] * log_data[study_z-3]^2)
}

# Границы прогноза 
upend_z = x_predict_z + sigma_predict_z
downend_z = x_predict_z - sigma_predict_z

# Проверка на наличие NA в данных
sum_na_high <- sum(is.na(data$X.CLOSE.))
cat("Количество NA в наивысших ценах (HIGH):", sum_na_high, "\n")
c(downend_z, upend_z, test_data_z)
#  график  процесса Z
plot(test_data_z, type = "l", col = "darkblue", main = "Прогноз на 1 шаг", 
     xlab = "Наблюдения", ylab = "Доходность", ylim = range(c(downend_z, upend_z, test_data_z)))

# Наносим прогнозы для {z_n}
points(x_predict_z, col = "black", pch = 1)

# Наносим  границы прогноза волатильности 
lines(upend_z, col = "red", lty = 2)
lines(downend_z, col = "red", lty = 2)

# Легенда для графика {z_n}
legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("darkblue", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))


