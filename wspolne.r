# Wczytanie niezbędnych bibliotek
library(fitdistrplus)
library(ggplot2)
library(ggExtra)
library(MASS)

# Wczytanie danych dla dwóch akcji
akcja1 <- read.csv("C:/Users/macie/Desktop/modelowanie/Wojtek/wwl_d.csv", header=TRUE, sep=",")
akcja2 <- read.csv("C:/Users/macie/Desktop/modelowanie/Kajtek/compute/vgo_d.csv", header=TRUE, sep=",")

merged_data <- merge(akcja1, akcja2, by = "Data", suffixes = c("_akcja1", "_akcja2"))

# Extract closing prices after merging
kurs1 <- merged_data$Zamkniecie_akcja1
kurs2 <- merged_data$Zamkniecie_akcja2

log_returns1 <- diff(log(kurs1))
log_returns2 <- diff(log(kurs2))

# Łączenie log-zwrotów w ramkę danych
log_returns <- data.frame(akcja1 = log_returns1, akcja2 = log_returns2)

# Estymacja wektora średnich
mean_vector <- colMeans(log_returns)
print(mean_vector)

# Estymacja macierzy kowariancji
cov_matrix <- cov(log_returns)
print(cov_matrix)

# Estymacja macierzy korelacji
cor_matrix <- cor(log_returns)
print(cor_matrix)

# Estymacja współczynnika korelacji
rho <- cor_matrix[1, 2]
print(rho)

# Liczność próby
n <- nrow(log_returns)




# Wykres rozrzutu z histogramami
p <- ggplot(log_returns, aes(x = akcja1, y = akcja2)) + geom_point()
p_marginal <- ggMarginal(p, type = "histogram")
print(p_marginal)




# Tworzenie siatki punktów do obliczenia gęstości
x_seq <- seq(min(log_returns$akcja1), max(log_returns$akcja1), length.out = 100)
y_seq <- seq(min(log_returns$akcja2), max(log_returns$akcja2), length.out = 100)
grid <- expand.grid(x = x_seq, y = y_seq)

# Obliczanie gęstości w każdym punkcie siatki
library(mvtnorm)
densities <- dmvnorm(grid, mean = mean_vector, sigma = cov_matrix)

# Dodawanie gęstości do siatki
grid$density <- densities

# Wykres gęstości łącznej
ggplot(grid, aes(x = x, y = y, z = density)) + 
  geom_tile(aes(fill = density)) + 
  stat_contour(color = "white") +
  labs(title = "Estymowana Gęstość Łączna", x = "Log-zwroty Akcji 1", y = "Log-zwroty Akcji 2")



# Dla Akcji 1
ggplot(log_returns, aes(x = akcja1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean_vector[1], sd = sqrt(cov_matrix[1,1])), color = "red", size = 1) +
  labs(title = "Gęstość Brzegowa Akcji 1", x = "Log-zwroty Akcji 1", y = "Gęstość")

# Dla Akcji 2
ggplot(log_returns, aes(x = akcja2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean_vector[2], sd = sqrt(cov_matrix[2,2])), color = "red", size = 1) +
  labs(title = "Gęstość Brzegowa Akcji 2", x = "Log-zwroty Akcji 2", y = "Gęstość")





# Generowanie próby o liczności n z rozkładu N(μ̂, Σ̂)
set.seed(123)
generated_sample <- mvrnorm(n, mu = mean_vector, Sigma = cov_matrix)
generated_sample <- data.frame(akcja1 = generated_sample[,1], akcja2 = generated_sample[,2])

# Porównanie wykresów rozrzutu
par(mfrow = c(1, 2))
plot(log_returns$akcja1, log_returns$akcja2, main = "Dane Empiryczne", xlab = "Log-zwroty Akcji 1", ylab = "Log-zwroty Akcji 2")
plot(generated_sample$akcja1, generated_sample$akcja2, main = "Wygenerowana Próba", xlab = "Log-zwroty Akcji 1", ylab = "Log-zwroty Akcji 2")

