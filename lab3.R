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
x_study = x[1:study_size]
x_test = x[(study_size + 1):n]


library(tseries)
#оценка параметров
ar2 <- arima(x_study, order = c(2, 0, 0), include.mean = FALSE)
theta_new <- coef(ar2)
theta_new;
## оценка А процесса ARCH(3) при помощи garch() 
ARCH3 = garch(x_study, order=c(0,3))
summary(ARCH3)

A_new = coef(ARCH3)
A_new

# Прогноз AR(2)
ar_predict = predict(ar2, h = length(x_test))
ar_predict


data <- read.table("Desktop\GAZP_211130_241130.txt", header = FALSE, sep = " ")
print(data)


