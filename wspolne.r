# Updated wspolne (1).r based on GaussWielowymiarowy_wyklad_2024.R
# Integrated and improved sections
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


# Additions from GaussWielowymiarowy_wyklad_2024.R
#Rozkład normalny wielowymiarowy
library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)

#Przyklad 1 a)
#==========
#Wykresy gestosci: N(0,0,1,1,-0.7),N(0,0,1,1,0),N(0,0,1,1,0.7),N(0,0,1,1,0.9)

#wartosc oczekiwana
mu    <- c(0, 0)  

#macierz kowariancji
Sigma_list <- list(
  S1 <- matrix(c(1, -0.7, 
                -0.7, 1), nrow = 2),
  S2 <- matrix(c(1, 0, 
                 0, 1), nrow = 2),
  S3 <- matrix(c(1, 0.7, 
                 0.7, 1), nrow = 2),
  S4 <- matrix(c(1, 0.9, 
                 0.9, 1), nrow = 2)
)

par(mfrow=c(2,2))

for(Sigma in Sigma_list){

#siatka punktow (korzystamy z reguly trzech sigm, dla rozkladu normalnego)
s1 <- s2 <-  1  #odchylenia standardowe 
x     <- seq(-3*s1, 3*s1, 0.25) 
y     <- seq(-3*s2, 3*s2, 0.25)

#gestosc i wartosci gestosci na siatce
f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)  

#wykres gestosci
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")
}

#b) Geenerowanie z rozkladu w bibliotece 'mnormt'
set.seed(100)

par(mfrow=c(2,2))

# Scatter Plot Addition

# Scatter plot based on GaussWielowymiarowy_wyklad_2024.R
library(ggplot2)
library(ggExtra)

# Example data for scatter plot
set.seed(100)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
Z <- MASS::mvrnorm(1000, mu = mu, Sigma = Sigma)

Z <- as.data.frame(Z)
colnames(Z) <- c('x', 'y')

# Scatter plot with marginal histograms
p <- ggplot(Z, aes(x = x, y = y)) + geom_point()
p2 <- ggMarginal(p, type = "histogram")

# Save the plot
ggsave("scatter_plot.png", plot = p2, width = 8, height = 6)
