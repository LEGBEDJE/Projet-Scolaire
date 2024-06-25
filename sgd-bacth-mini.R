set.seed(42)
n<-1000 # nbre de donnéé a augmenter pour voir plus de différence entre les algo
# génrération des donnéés à utiliser
x<-runif(n,0,10) # Variables explicatives
plot(x)
epsilon <- rnorm(n, 0, 4)  # Erreur gaussien
#beta <- c(3, 2)  # Coefficients de la vraie relation linéaire
beta1<-3
beta2<-2
y <- beta1 + beta2*x + epsilon  # droite exacte
plot(x, y, xlim = c(0, 10), ylim = c(-10, 30), xlab = "x", ylab = "y", main = "droite exacte",
     col = "black", pch = 20)
#traçage de la droite
abline(beta1, beta2, col = "red", lwd = 2, lty = 2)

#Algorithme du gradient stochastique

SGD_regression <- function(x, y, beta1_init,beta2_init, gamma, k) {
  n <- length(y)
  beta<-c(beta1_init,beta2_init)
  for (i in 1:(k - 1)) {
    idx <- sample(n, 1)  # Choix aléatoire d'un point
    y_pred <- beta[1] + beta[2]*x[idx]
    grad_1 <- (y_pred - y[idx])
    grad_2 <- grad_1 * x[idx]
    beta[1] <- beta[1] - gamma * grad_1
    beta[2] <- beta[2] - gamma * grad_2
  }
  return(beta)
}
# Définition de la fonction de coût (Erreur quadratique moyenne)
function_ct <- function(beta1,beta2,x, y) {
  p <- length(y)
  return(sum(( beta1 + beta2*x - y)^2) / (2 * p))
}



# Paramètres
gamma <- 0.0001  # Taux d'apprentissage à varier pour voir la différence entre les algo
k <- 10000  # Nombre d'itérations
beta1_init <- 0 # Initialisation des coefficients
beta2_init<-0


beta_sgd <- SGD_regression(x, y,beta1_init ,beta2_init, gamma, k)
lines(x, beta_sgd[1] + beta_sgd[2]*x, col = "blue", lwd = 2)  # Résultat du SGD
# Algorithme du gradient classique (BACTH) pour la régression linéaire
BACTH_regression <- function(x, y, beta1_init,beta2_init, gamma, k) {
  n <- length(y)
  beta<-c(beta1_init,beta2_init)
  for (i in 1:(k - 1)) {
    y_pred <- beta[1] + beta[2]*x
    grad_1 <- (y_pred - y)
    grad_2 <- grad_1 * x
    beta[1] <- beta[1] - gamma * mean(grad_1)
    beta[2] <- beta[2] - gamma * mean(grad_2)
  }
  return(beta)
}

beta_BACTH <- BACTH_regression(x, y,beta1_init ,beta2_init, gamma, k)#resultat du bacth



lines(x, beta_BACTH[1] + beta_BACTH[2]*x, col = "green", lwd = 2) # Résultat de la descente de gradient classique
#Algorithme du mini BACTH
MINI_BACTH_regression<-function(x, y, beta1_init,beta2_init, gamma, k,lot){
  n <- length(y)
  beta<-c(beta1_init,beta2_init)
  for (i in 1:(k - 1)) {
    # Sélection aléatoire d'un mini-lot
    idx <- sample(n, lot, replace = FALSE)
    y_pred <- beta[1] + beta[2]*x[idx]
    grad_1 <- (y_pred - y[idx])
    grad_2 <- grad_1 * x[idx]
    beta[1] <- beta[1] - gamma *mean(grad_1) 
    beta[2] <- beta[2] - gamma * mean(grad_2)
  }
  return(beta)
  
}
#taille du lot
lot<-100
beta_MINI_BACTH <- MINI_BACTH_regression(x, y,beta1_init ,beta2_init, gamma, k,lot)#Resultat du mini bash
lines(x, beta_MINI_BACTH[1] + beta_MINI_BACTH[2]*x, col = "yellow", lwd = 2)  # Résultat du SGD

