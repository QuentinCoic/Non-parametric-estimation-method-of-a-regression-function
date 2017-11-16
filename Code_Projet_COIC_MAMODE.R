#étape 1
n <- 100
x <- runif(n)

#étape 2
f <- function(x){ 
  return((((x^2)*2^(x-1)) - ((x-0.5)^3))*sin(10*x))
}

eps <- rnorm(100)
sig <- 0.2
y <- f(x) + (eps*sig)

#étape 3
plot (f, xlim=c(0,1), ylim=c(-1,1), col='red', main='Gr1 : Nuage de points et f', xlab='', ylab='')
par(new=TRUE)
plot(x, y, xlim=c(0,1), ylim=c(-1,1), type='p', col='blue', xlab='', ylab='')

#étape 4
phi <- function(j,x){ #Définition des éléments de la base trigonométrique
  if (j==1) {
    return(1+0*x)  
  }
  if (j%%2==1){
    return(sqrt(2)*cos((j+1)*pi*x))
  }
  return(sqrt(2)*sin(j*pi*x))
} 

vtild <- function(j){ #Définition de des estimateurs par la méthode des moments des paramètres v_j  
  return(sum(phi(j,x)*y)/n)
}

ftild <- function(x,N){ #Définition de l'estimateur ftild de f 
  res <- 0
  for (j in 1:N) {
    res <- res + (vtild(j)*phi(j,x))
  }
  return(res)
}

N <- 5 #On change ce N de 5 en 5 de 5 à 50

ftildb <- function(x) #Définition purement technique afin de tracer la courbe avec x en abscisse (N est fixé)
{
  return(ftild(x,N))
}

plot(ftildb, xlim=c(0,1), ylim=c(-1,1), main='Gr2 : Nuage de points, f et estimateur ftild (N=5)', xlab='', ylab='')
par(new=TRUE)
plot (f, xlim=c(0,1), ylim=c(-1,1), col='red', xlab='', ylab='')
par(new=TRUE)
plot(x, y, xlim=c(0,1), ylim=c(-1,1), type='p', col='blue', xlab='', ylab='')
legend(x="bottomright", legend=c("f", "ftild"),col=c("red", "black"), lty=1, cex=0.8)

#étape 5
sig2ch <- function(M) #Définition de l'estimateur de sigma^2 de la question h)
{
  return(sum((y-ftild(x,M))*(y-ftild(x,M)))/(n-M))
}

sig2ch(50)

valN <- seq(1, 50, by=1)

amin <- seq(0, 0, length=50) #On va placer ici les valeurs de la fonction dont on veut trouver l'argmin

for(j in 1:50){
  amin[j] <- ((n-valN[j])*sig2ch(valN[j]))-((n-2*valN[j])*sig2ch(50))
}

which.min(amin) 

#étape 6
N <- which.min(amin)

plot(ftildb, xlim=c(0,1), ylim=c(-1,1), main='Gr12 : f et estimateur ftild (N=Nch)', xlab='', ylab='')
par(new=TRUE)
plot(f,xlim=c(0,1), ylim=c(-1,1), col="red", xlab='', ylab='')
legend(x="bottomright", legend=c("f", "ftild"),col=c("red", "black"), lty=1, cex=0.8)

#étape7
Nch<-seq(0, 0, length=100) # On va placer ici les Nchap

for(l in 1:100){
  x <- runif(100)
  eps <- rnorm(100)
  y <- f(x) + (eps*sig)
  for(j in 1:50){
    amin[j] <- ((n-valN[j])*sig2ch(valN[j]))-((n-2*valN[j])*sig2ch(50))
  }
  Nch[l] <- which.min(amin)
}

Nch

bords <- seq(0.5, 11.5, by=1)
hist(Nch, breaks=bords, main="Répartition de Nch")


