# Wczytanie niezbędnych bibliotek
library(fitdistrplus)
library(ggplot2)
library(ggExtra)
library(MASS)
library(mvtnorm)

# Wczytanie danych dla trzech akcji
akcja1 <- read.csv("C:/Users/milew/Downloads/wwl_d.csv", header=TRUE, sep=",") # akcja WWL
akcja2 <- read.csv("C:/Users/milew/Downloads/vgo_d.csv", header=TRUE, sep=",") # akcja VGO
akcja3 <- read.csv("C:/Users/milew/Downloads/dtr_d.csv", header=TRUE, sep=";") # akcja DTR  

# Łączenie danych dla trzech akcji
merged_data <- merge(akcja1, akcja2, by = "Data", suffixes = c("_akcja1", "_akcja2"))
merged_data <- merge(merged_data, akcja3, by = "Data")

# Extract closing prices after merging
kurs1 <- merged_data$Zamkniecie_akcja1
kurs2 <- merged_data$Zamkniecie_akcja2
kurs3 <- merged_data$Zamkniecie

log_returns1 <- diff(log(kurs1))
log_returns2 <- diff(log(kurs2))
log_returns3 <- diff(log(kurs3))

# Łączenie log-zwrotów w ramkę danych
log_returns <- data.frame(akcja1 = log_returns1, akcja2 = log_returns2, akcja3 = log_returns3)

# Teoretyczne wzory estymatorów dla par akcji:
# Wektor średnich: μ̂ = (1/n)∑(xi, yi)
# Macierz kowariancji: Σ̂ = (1/(n-1))∑((xi-μ̂x)(yi-μ̂y))
# Współczynnik korelacji: ρ̂ = Σ̂xy/(sqrt(Σ̂xx*Σ̂yy))

# Funkcja do analizy pary akcji
analyze_pair <- function(x, y, pair_name) {
  n <- length(x)
  mean_vec <- c(mean(x), mean(y))
  cov_mat <- cov(cbind(x, y))
  cor_coef <- cor(x, y)
  
  cat("\nAnaliza pary:", pair_name, "\n")
  cat("Wektor średnich:\n")
  print(mean_vec)
  cat("Macierz kowariancji:\n")
  print(cov_mat)
  cat("Współczynnik korelacji:\n")
  print(cor_coef)
  
  return(list(mean_vec=mean_vec, cov_mat=cov_mat, cor_coef=cor_coef))
}

# Analiza dla wszystkich par akcji
pair1 <- analyze_pair(log_returns$akcja1, log_returns$akcja2, "WWL-VGO")
pair2 <- analyze_pair(log_returns$akcja1, log_returns$akcja3, "WWL-DTR")
pair3 <- analyze_pair(log_returns$akcja2, log_returns$akcja3, "VGO-DTR")

# Estymacja wektora średnich
mean_vector <- colMeans(log_returns)
print(mean_vector)

# Estymacja macierzy kowariancji i korelacji
cov_matrix <- cov(log_returns)
cor_matrix <- cor(log_returns)
print(cov_matrix)
print(cor_matrix)

# Liczność próby
n <- nrow(log_returns)

# Generate sample from N(μ̂,Σ̂) with the same size as original data
set.seed(123)
generated_sample <- MASS::mvrnorm(n, mu=mean_vector, Sigma=cov_matrix)
generated_sample <- as.data.frame(generated_sample)
colnames(generated_sample) <- c("akcja1", "akcja2", "akcja3")

# Compare original and generated data using scatter plots
par(mfrow=c(2,3))
plot(log_returns$akcja1, log_returns$akcja2, 
     main="Dane empiryczne WWL-VGO",
     xlim=c(-0.15,0.15), ylim=c(-0.15,0.15))
grid()

plot(log_returns$akcja1, log_returns$akcja3,
     main="Dane empiryczne WWL-DTR",
     xlim=c(-0.15,0.15), ylim=c(-0.15,0.15))
grid()

plot(log_returns$akcja2, log_returns$akcja3,
     main="Dane empiryczne VGO-DTR",
     xlim=c(-0.15,0.15), ylim=c(-0.15,0.15))
grid()

plot(generated_sample$akcja1, generated_sample$akcja2,
     main="Próba wygenerowana WWL-VGO",
     xlim=c(-0.15,0.15), ylim=c(-0.15,0.15))
grid()

plot(generated_sample$akcja1, generated_sample$akcja3,
     main="Próba wygenerowana WWL-DTR",
     xlim=c(-0.15,0.15), ylim=c(-0.15,0.15))
grid()

plot(generated_sample$akcja2, generated_sample$akcja3,
     main="Próba wygenerowana VGO-DTR",
     xlim=c(-0.15,0.15), ylim=c(-0.15,0.15))
grid()

# Additional visualization with marginal distributions
for (pair in list(
  c("akcja1", "akcja2", "WWL-VGO"),
  c("akcja1", "akcja3", "WWL-DTR"),
  c("akcja2", "akcja3", "VGO-DTR")
)) {
  # Original data
  p1 <- ggplot(log_returns, aes_string(x=pair[1], y=pair[2])) + 
    geom_point() +
    ggtitle(paste("Dane empiryczne:", pair[3]))
  p1 <- ggMarginal(p1, type="histogram")
  print(p1)
  
  # Generated data
  p2 <- ggplot(generated_sample, aes_string(x=pair[1], y=pair[2])) + 
    geom_point() +
    ggtitle(paste("Próba wygenerowana:", pair[3]))
  p2 <- ggMarginal(p2, type="histogram")
  print(p2)
}

# Wykresy rozrzutu dla każdej pary akcji
pairs(log_returns, main="Wykresy rozrzutu dla wszystkich par akcji", 
      labels=c("WWL", "VGO", "DTR"))

# Histogramy dla każdej akcji
par(mfrow=c(1,3))
hist(log_returns$akcja1, main="Histogram WWL", prob=TRUE)
curve(dnorm(x, mean=mean_vector[1], sd=sqrt(cov_matrix[1,1])), add=TRUE, col="red")

hist(log_returns$akcja2, main="Histogram VGO", prob=TRUE)
curve(dnorm(x, mean=mean_vector[2], sd=sqrt(cov_matrix[2,2])), add=TRUE, col="red")

hist(log_returns$akcja3, main="Histogram DTR", prob=TRUE)
curve(dnorm(x, mean=mean_vector[3], sd=sqrt(cov_matrix[3,3])), add=TRUE, col="red")

# Generowanie próby z rozkładu normalnego trójwymiarowego
set.seed(123)
generated_sample <- mvrnorm(n, mu = mean_vector, Sigma = cov_matrix)
generated_sample <- data.frame(akcja1 = generated_sample[,1], 
                               akcja2 = generated_sample[,2],
                               akcja3 = generated_sample[,3])

# Wzór gęstości rozkładu normalnego wielowymiarowego:
# f(x) = (1/((2π)^(p/2)|Σ|^(1/2))) * exp(-1/2 * (x-μ)'Σ^(-1)(x-μ))
# Gęstości brzegowe: N(μᵢ, σᵢᵢ)

# Funkcja do obliczenia gęstości dla rozkładu normalnego wielowymiarowego
calculate_density <- function(x, y, mu, Sigma) {
  dmnorm(cbind(x, y), mu, Sigma)
}

# Siatka punktów do wykresu (używamy reguły 3 sigm)
s1 <- sqrt(cov_matrix[1,1])
s2 <- sqrt(cov_matrix[2,2])
x <- seq(-3*s1 + mean_vector[1], 3*s1 + mean_vector[1], length=50)
y <- seq(-3*s2 + mean_vector[2], 3*s2 + mean_vector[2], length=50)

# Obliczenie wartości gęstości na siatce dla wszystkich par akcji
density_values_wwl_vgo <- outer(x, y, function(x, y) 
  calculate_density(x, y, mean_vector[1:2], cov_matrix[1:2, 1:2]))

density_values_wwl_dtr <- outer(x, y, function(x, y) 
  calculate_density(x, y, c(mean_vector[1], mean_vector[3]), 
                    matrix(c(cov_matrix[1,1], cov_matrix[1,3], 
                             cov_matrix[3,1], cov_matrix[3,3]), 2, 2)))

density_values_vgo_dtr <- outer(x, y, function(x, y) 
  calculate_density(x, y, c(mean_vector[2], mean_vector[3]), 
                    matrix(c(cov_matrix[2,2], cov_matrix[2,3], 
                             cov_matrix[3,2], cov_matrix[3,3]), 2, 2)))

# Zamiana układu wykresów gęstości na pojedyncze wykresy
# WWL-VGO
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
persp(x, y, density_values_wwl_vgo, theta=-30, phi=25,
      main="Gęstość łączna rozkładu normalnego dla log-zwrotów WWL-VGO\nEstymowana na podstawie danych historycznych",
      shade=0.75, col="lightblue", expand=0.7, r=2,
      ticktype="detailed", cex.main=1.2)

# WWL-DTR
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
persp(x, y, density_values_wwl_dtr, theta=-30, phi=25,
      main="Gęstość łączna rozkładu normalnego dla log-zwrotów WWL-DTR\nEstymowana na podstawie danych historycznych",
      shade=0.75, col="lightgreen", expand=0.7, r=2,
      ticktype="detailed", cex.main=1.2)

# VGO-DTR
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
persp(x, y, density_values_vgo_dtr, theta=-30, phi=25,
      main="Gęstość łączna rozkładu normalnego dla log-zwrotów VGO-DTR\nEstymowana na podstawie danych historycznych",
      shade=0.75, col="lightpink", expand=0.7, r=2,
      ticktype="detailed", cex.main=1.2)

# Wykresy gęstości brzegowych (zachowujemy układ 3x1)
par(mfrow=c(3,1))

curve(dnorm(x, mean=mean_vector[1], sd=sqrt(cov_matrix[1,1])),
      xlim=c(min(x), max(x)), 
      main="Gęstość brzegowa rozkładu normalnego dla log-zwrotów WWL")
grid()

curve(dnorm(x, mean=mean_vector[2], sd=sqrt(cov_matrix[2,2])),
      xlim=c(min(y), max(y)),
      main="Gęstość brzegowa rozkładu normalnego dla log-zwrotów VGO")
grid()

curve(dnorm(x, mean=mean_vector[3], sd=sqrt(cov_matrix[3,3])),
      xlim=c(min(y), max(y)),
      main="Gęstość brzegowa rozkładu normalnego dla log-zwrotów DTR")
grid()

# Porównanie empirycznych i wygenerowanych danych
par(mfrow=c(2,3))
pairs(log_returns, main="Dane Empiryczne", labels=c("WWL", "VGO", "DTR"))
pairs(generated_sample, main="Wygenerowana Próba", labels=c("WWL", "VGO", "DTR"))