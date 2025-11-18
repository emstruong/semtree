devtools::load_all()
library(lavaan)
library(lobstr) # recall; lobstr::tree()

# Setup ------------------------------------------------------------------
set.seed(23)
N <- 1000
M <- 5
icept <- rnorm(N, 10, sd = 4)
slope <- rnorm(N, 3, sd = 1.2)
p1 <- sample(c(0, 1), size = N, replace = TRUE)
loadings <- 0:4
x <-
  (slope + p1 * 5) %*% t(loadings) + 
  matrix(rep(icept, each = M), byrow = TRUE, ncol = M) + 
  rnorm(N * M, sd = .08)
growth.data <- data.frame(x, factor(p1))
names(growth.data) <- c(paste0("T", 1:M), "P1")

model <- "
intercept =~ 1*T1 + 1*T2 + 1*T3 + 1*T4 + 1*T5
slope  =~ 0*T1 + 1*T2 + 2*T3 + 3*T4 + 4*T5
"
lav <- lavaan::growth(model = model, data = growth.data, do.fit = FALSE)
(mod <- semtree(lav, data = growth.data, predictors = "P1"))
plot(mod)


# Debug ------------------------------------------------------------------
# debugonce(semtree)
# semtree(lav, data = growth.data, predictors = "P1")

stop()
debugonce(growTree)
semtree(lav, data = growth.data, predictors = "P1")
