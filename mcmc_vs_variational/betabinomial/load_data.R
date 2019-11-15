N <- 100
s <- 10 #number of trials
theta <- .4
y <- rbinom(N, s, theta)
data <- list(y=y, N=N, s = s)