library(MASS)
set.seed(42)

# Specify dimensions
K <-3
D <- 10 
N <- 300

# Generate weight matrix
l1 <- c(0.99, 0.00, 0.25, 0.00, 0.80, 0.00, 0.50, 0.00, 0.00, 0.00)
l2 <- c(0.00, 0.90, 0.25, 0.40, 0.00, 0.50, 0.00, 0.00, -0.30, -0.30)
l3<-  c(0.00, 0.00, 0.85, 0.80, 0.00, 0.75, 0.75, 0.00, 0.80, 0.80)
W <-cbind(l1,l2,l3)

# Sample factors
mu_theta <-rep(0,K) # the mean of the factors
Phi <- diag(rep(1,K))
Z <- mvrnorm(N, mu_theta, Phi) # sample factor scores

# Sample errors
mu_epsilon<-rep(0,D) # the mean of the weights
Psi <- diag(c(0.2079, 0.19, 0.1525, 0.20, 0.36, 0.1875, 0.1875, 1.00, 0.27, 0.27))
Epsilon <-mvrnorm(N, mu_epsilon, Psi)

# Sample observations
Y <- Z %*% t(W) + Epsilon

