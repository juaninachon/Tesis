#############################################
#   FULL DIMENSION-REDUCTION COMPARISON
#   PCA • PLS • LASSO/Ridge/ElasticNet • FA
#############################################

library(pls)
library(glmnet)
library(psych)
library(tidyverse)

# -------------------------------------------
# LOAD DATA
# -------------------------------------------
# df should contain your predictors + "outcome"
# Example:
df <- read.csv("data/shiny-app_data.csv", check.names=FALSE) |>
  filter(Estudio == "jardines") |>
  select(c(4, 6:33)) %>%
  filter(complete.cases(.)) |>

y <- df[$outcome]
X <- df[, setdiff(names(df), "outcome")]
X_scaled <- scale(X)
X_mat <- as.matrix(X_scaled)

#############################################
# 1. PCA → REGRESSION ON PC SCORES
#############################################

pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# Choose #components to explain 80% variance
var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
k <- which(var_exp >= 0.80)[1]
cat("\n--- PCA: Using", k, "components (80% variance) ---\n")

PCs <- pca$x[, 1:k]

model_pca <- lm(y ~ PCs)
cat("\nPCA model summary:\n")
print(summary(model_pca))

#############################################
# 2. PLS REGRESSION (SUPERVISED)
#############################################

model_pls <- plsr(y ~ X_mat, scale = TRUE, validation = "CV")

best_comp <- which.min(model_pls$validation$PRESS)
cat("\n--- PLS: Best #components =", best_comp, "---\n")

pred_pls <- predict(model_pls, ncomp = best_comp)

cat("\nPLS validation summary:\n")
print(summary(model_pls))

#############################################
# 3. REGULARIZATION MODELS (GLMNET)
#############################################

# LASSO
cv_lasso <- cv.glmnet(X_mat, y, alpha = 1)
cat("\n--- LASSO: lambda.min =", cv_lasso$lambda.min, "---\n")
print(coef(cv_lasso, s = "lambda.min"))

# Ridge
cv_ridge <- cv.glmnet(X_mat, y, alpha = 0)
cat("\n--- Ridge: lambda.min =", cv_ridge$lambda.min, "---\n")
print(coef(cv_ridge, s = "lambda.min"))

# Elastic Net (50/50)
cv_elnet <- cv.glmnet(X_mat, y, alpha = 0.5)
cat("\n--- Elastic Net: lambda.min =", cv_elnet$lambda.min, "---\n")
print(coef(cv_elnet, s = "lambda.min"))

#############################################
# 4. FACTOR ANALYSIS (INTERPRETABLE DOMAINS)
#############################################

cat("\n--- Factor Analysis: Parallel Analysis ---\n")
fa.parallel(X_scaled, fm = "ml", fa = "fa")

# Choose number from the parallel analysis
nf <- 3   # set manually after checking parallel analysis output

fa_res <- fa(X_scaled, nfactors = nf, rotate = "oblimin")
fa_scores <- fa_res$scores

cat("\nFactor loadings:\n")
print(fa_res$loadings)

model_fa <- lm(y ~ fa_scores)
cat("\nFactor regression summary:\n")
print(summary(model_fa))