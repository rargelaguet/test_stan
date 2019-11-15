library(ISLR)
X <- Smarket[,1:8]
y <- Smarket[,9]

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
