# TO-FINISH...

N <- 1000
D <- 10

ntrials <- 10 
y_list <- list()

tau <- 1
alpha <- rep(1,D)
alpha[1:5] <- 1e6

for (i in 1:ntrials) {
  W <- sapply(1/sqrt(alpha), function(a) rnorm(1,sd=a))
  X <- matrix(rnorm(N*D), N, D)
  y <- c(X %*% W + rnorm(N, sd = (1/tau)))
}
  
