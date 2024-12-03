dtr_d <- read.csv("//NAS1/home/jmilewczyk/Downloads/dtr_d.csv", sep=";")

class(dtr_d)

#kursy zamkniecia
closings <- dtr_d$Zamkniecie
R <- diff(log(closings))

#wykresy
#średnia
m1 <- mean(closings); m1
m2 <- mean(R); m2
#wariancja
v1 <- var(closings)
v2 <- var(R)
print(v2)
#odchylenie
s1 <- sd(closings)
s2 <- sd(R)
print(s2)
#kwantyle
quantiles <- quantile(R, probs = c(0.05, 0.5, 0.95))
print(quantiles)

par(mfrow=c(2,2))
hist(closings,prob=TRUE,main="Histogram kursow zamkniecia")
grid()
plot(closings,main="Wykres kursow zamkniecia")
abline(h=m1, col="blue", lwd=2)
grid()

hist(R,prob=TRUE,main="Histogram log-zwrotow")
abline(v=m2, col="blue")
abline(v=quantiles[1], col="red")
abline(v=quantiles[2], col="green")
abline(v=quantiles[3], col="purple")
grid()
legend("topright", legend=c("Srednia", "q(5%)", "q(50%)", "q(95%)"), col=c("blue", "red", "green", "purple"), lwd=2, lty=c(1,1,1,1))
plot(R,main="Wykres log-zwrotow")
abline(h=m2, col="blue", lwd=2)
grid()

# Estymacja dystrybuanty empirycznej i wykres

emdy <- ecdf(R)
plot(emdy, main="Empiryczna dystrybuanta log-zwrotów")

# Dopasowanie rozkładu normalnego i t-studenta
library(fitdistrplus)

#Rozkład normalny
fit_normal <- fitdist(R, "norm")
summary(fit_normal)

#Rozkład t-studenta
fit_t <- fitdist(R, "t", start=list(df=12))
summary(fit_t)

# Wykresy diagnostyczne obu rozkladow
par(mfrow=c(2,2))
plot.legend <- c("Normalny", "t-Studenta")
denscomp(list(fit_normal, fit_t), legendtext = plot.legend, main="Porownanie dopasowania - gestosc")
qqcomp(list(fit_normal, fit_t), legendtext = plot.legend, main="Porownanie dopasowania - kwantyle")
cdfcomp(list(fit_normal, fit_t), legendtext = plot.legend, main="Porownanie dopasowania - dystrybuanta")
ppcomp(list(fit_normal, fit_t), legendtext = plot.legend, main="Porownanie dopasowania - prawdopodobienstwo")

# Testowanie dopasowania przy uzyciu KS, CM, AD oraz kryteriow AIC i BIC
ks_test_normal <- gofstat(fit_normal, fitnames="Normal")
ks_test_t <- gofstat(fit_t, fitnames="t-Studenta")

print(ks_test_normal)
print(ks_test_t)

#Metoda MC
N <- 10000
n <- length(R); n


Dln <- c()

for (i in 1:N) { 
  Yln <- rnorm(n,fit_normal$estimate[1],fit_norma++++++l$estimate[2])
  
  Dln[i] <-  ks.test(Yln,"pnorm", fit_normal$estimate[1],fit_normal$estimate[2],exact=TRUE)$statistic
}

dn_ln <-  ks.test(R,"pnorm",fit_normal$estimate[[1]],fit_normal$estimate[[2]],exact=TRUE)$statistic
dn_ln

par(mfrow=c(1,1))
hist(Dln,prob=T)
points(dn_ln,0,pch=19,col=2)

p_value_ln <- length(Dln[Dln>dn_ln])/N; p_value_ln

alpha <- 0.05
p_value_ln <= alpha

