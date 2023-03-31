mu <- 1
theta <- 2
M <- 100
n <- 999
res <- matrix(data = NA, nrow = M, ncol = 2)
for (k in 1:M) {
    x <- rlaplace(n, mu, theta)
    res[k, 1] <- median(x) / n
    res[k, 2] <- mean(abs(x - res[k, 1]))
}
boxplot(res)
