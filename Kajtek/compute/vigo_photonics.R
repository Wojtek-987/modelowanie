vgo_d <- read.csv("/home/LABPK/klach/Pulpit/modelowanie-matematyczne2024/zadanie_projektowe/compute/vgo_d.csv")
class(vgo_d)

# ceny akcji
kurs_zamk <- vgo_d$Zamkniecie
# daty
daty <- as.Date(vgo_d$Data)
# log-zwroty
s <- log(kurs_zamk)
log_zwrot <- (diff(s))

# wykresy kursow zamkniecia oraz log-zwrotow

par(mfrow=c(1,1))
plot(daty, kurs_zamk, type = "l", xlab = "Data", ylab = "Kurs zamknięcia")
plot(daty[2:501], log_zwrot, type = "l", xlab = "Data", ylab = "Zwroty")

# wykorzystaj wlasciwy estymator do wyliczenia sredniej u

u <- mean(log_zwrot); u

# estymacja wariancji estymatorem nieobciazonym

myVar <- function(x) {
  N <- length(x)
  m <- mean(x)
  sum((x-m)^2)/(N-1)
}
varnb <- myVar(log_zwrot); varnb

# odchylenie standardowe
sqrt(varnb);
sd(log_zwrot)

# wyestymuj kwantyl rzędu: 5%, 50%(mediana), 95%
kwantyle <- quantile(log_zwrot, c(0.05, 1/2, 0.95)); kwantyle

# dorysuj do histogramu srednia i kwantyle (czerwone/niebieskie punkty na wykresie)
hist(log_zwrot, prob=TRUE, main="", xlab="Dzienne Zwroty")
points(u, 0, col = "red", pch = 16, cex=2)
points(kwantyle, rep(0, length(kwantyle)), col="blue", pch=16, cex=1)

# dystrybuanta
par(mfrow=c(1,1))
plot(ecdf(log_zwrot), main="Dystrybuanta F")

# wyestymuj parametry rozkładu normalnego i t-Studenta, wykorzystaj estymator największej wiarygodności (MLE)
library(fitdistrplus)

fit_norm <- fitdist(log_zwrot, "norm")
m_param <- fit_norm$estimate[[1]]
s_param <- fit_norm$estimate[[2]]

fit_tstudent <- fitdist(log_zwrot,"t", start=list(df=12)) 
df_param <- fit_tstudent$estimate

# wykresy diagnostyczne 
par(mfrow=c(2,2))
plot.legend <- c("normal", "t")
denscomp(list(fit_norm, fit_tstudent), legendtext = plot.legend)
qqcomp(list(fit_norm, fit_tstudent), legendtext = plot.legend)
cdfcomp(list(fit_norm, fit_tstudent), legendtext = plot.legend)
ppcomp(list(fit_norm, fit_tstudent), legendtext = plot.legend)

# weryfikacja wyboru przy wykorzystaniu statystyk KS, CM, AD oraz kryteriow informayjnych AIC oraz BIC
criteria <- gofstat(list(fit_norm, fit_tstudent), fitnames = c("normal", "t"))

# dla wybranego rozkladu przetestuj metoda MC hipoteze o rownosci rozkladow wykorzystujac KS, CM lub AD
# H0: F = N(-0.0006787669, 0.0248544) przeciwko H1: F != N(-0.0006787669, 0.0248544)
# 1. Rozkład statystyki Dn
# Generujemy N = 10000 probek licznosci danych z rozkladu F0=N(-0.0006787669, 0.0248544) wybranego wczesniej
# i obliczamy wartosc statystyki KS 
N <- 10000
n <- length(log_zwrot)
D <- c()

for (i in 1:N) {
  Y <- rnorm(n, m_param, s_param)
  D[i] <- ks.test(Y, "pnorm", m_param, s_param, exact=TRUE)$statistic
}

# 2. Obliczamy dn wartosc statystyki dla naszej proby i F0
dn <- ks.test(log_zwrot, "pnorm", m_param, s_param, exact=TRUE)$statistic

# 3. Obliczamy p-value
p_value <- length(D[D > dn])/N; p_value