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
# (mod <- semtree(lav, data = growth.data, predictors = "P1"))
# plot(mod)


# Debug ------------------------------------------------------------------
# semtree(lav, data = growth.data, predictors = "P1")

# stop()
# debugonce(semtree)
# debugonce(growTree)
# debugonce(naiveSplit)
# (mod <- semtree(lav, data = growth.data, predictors = "P1",control = semtree.control(method = "naive")))




# Other Attempt -----------------------------------------------------------
# From https://brandmaier.github.io/semtree/articles/spi-semtree.html

library(psychTools)
library(psych)
library(OpenMx)
data(spi)
data(spi.keys)


# score all available scales and extract two broad dimensions
spi_scored <- scoreFast(keys = spi.keys, items = spi)


selected_traits <- c("Agree-A", "Extra-A")

trait_scores <- spi_scored[, selected_traits, drop = FALSE]

# potential predictors available in the dataset
candidate_covariates <- intersect(c("age", "education", "gender"), names(spi))
spi_analysis <- cbind(trait_scores, spi[, candidate_covariates, drop = FALSE])
spi_analysis <- na.omit(spi_analysis)

selected_traits <- c("Agreeableness", "Extraversion")
names(spi_analysis)[1:2] <- selected_traits

# add some noise
set.seed(104358)
spi_analysis$noise1 <- ordered(sample(c(0,1), nrow(spi_analysis), TRUE))
spi_analysis$noise2 <- ordered(sample(c(1:10), nrow(spi_analysis), TRUE))
spi_analysis$noise3 <- ordered(sample(c(1:10), nrow(spi_analysis), TRUE))
spi_analysis$noise4 <- ordered(sample(c(1:20), nrow(spi_analysis), TRUE))

spi_analysis$education <- ordered(spi_analysis$education)


two_dim_model <- mxModel(
  "TwoDimensions",
  type = "RAM",
  manifestVars = selected_traits,
  mxData(spi_analysis[, selected_traits, drop = FALSE], type = "raw"),
  # freely estimate the variances and their covariance
  mxPath(
    from = selected_traits,
    arrows = 2,
    connect = "unique.pairs",
    free = TRUE,
    values = c(1, 0.2, 1),
    labels = c("var_trait1", "cov_traits", "var_trait2")
  ),
  # estimate manifest means
  mxPath(
    from = "one",
    to = selected_traits,
    arrows = 1,
    free = TRUE,
    values = 0,
    labels = paste0("mean_", selected_traits)
  )
)

two_dim_fit <- mxRun(two_dim_model)



## Debugging ---------------------------------------------------------------
debug(semtree)
debug(growTree)
debug(naiveSplit)
debug(ScoreSplit)

(spi_tree_score <- semtree(
  model = two_dim_fit,
  data = spi_analysis,
  control = semtree_control(method="naive")
))

(spi_tree_score <- semtree(
  model = two_dim_fit,
  data = spi_analysis,
  control = semtree_control(method="score")
))