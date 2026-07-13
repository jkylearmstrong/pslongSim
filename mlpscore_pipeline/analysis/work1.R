library(MLPScore)



## Estimating PS of adherence #### 
# PS model: P(Compliant_it = 1 | X_it)
fit_ps <- glm(
  Compliant ~ Treatment + Age + GenderMale + VisitNumber,
  data = dat,
  family = binomial()
)

dat$ps_hat <- as.numeric(predict(fit_ps, type = "response"))

# Stabilized weights: numerator can be marginal P(C=1 | VisitNumber) for stability
marg <- aggregate(Compliant ~ VisitNumber, data = dat, FUN = mean)
dat <- merge(dat, setNames(marg, c("VisitNumber", "p_marg")), by = "VisitNumber", all.x = TRUE, sort = FALSE)

# For binary compliance, stabilized IPTW:
# w = [p_marg]^C * [1-p_marg]^(1-C) / [ps_hat]^C / [1-ps_hat]^(1-C)
dat$w <- with(dat, (p_marg^Compliant * (1 - p_marg)^(1 - Compliant)) /
                (pmax(ps_hat, 1e-6)^Compliant * pmax(1 - ps_hat, 1e-6)^(1 - Compliant)))

# Weighted mixed effects model (effect of Treatment accounting for adherence via IPTW)
# Outcome modeled as a function of Treatment + time, random intercept by patient, weights = IPTW
# NOTE: lme4::lmer ignores weights as frequency weights; for proper IPTW you may prefer geepack::geeglm
#       or survey-weighted methods. Here, we show lmer with weights as an approximation.
if (requireNamespace("lme4", quietly = TRUE)) {
  library(lme4)
  m_w <- lmer(Outcome ~ Treatment + VisitNumber + (1 | PatientID), data = dat, weights = w)
  summary(m_w)
}


## Example ML PS #### 
if (requireNamespace("xgboost", quietly = TRUE)) {
  library(xgboost)
  X <- model.matrix(~ Treatment + Age + GenderMale + VisitNumber, data = dat)[, -1]
  dtrain <- xgb.DMatrix(data = X, label = dat$Compliant)
  xgb_ps <- xgboost(
    data = dtrain,
    nrounds = 200,
    objective = "binary:logistic",
    max_depth = 4,
    eta = 0.05,
    subsample = 0.9,
    colsample_bytree = 0.8,
    verbose = 0
  )
  dat$ps_hat <- as.numeric(predict(xgb_ps, newdata = X))
}

if (requireNamespace("ranger", quietly = TRUE)) {
  library(ranger)
  rf_ps <- ranger::ranger(
    Compliant ~ Treatment + Age + GenderMale + VisitNumber,
    data = dat,
    probability = TRUE,
    num.trees = 1000,
    mtry = 3,
    min.node.size = 10,
    classification = TRUE,
    seed = 1
  )
  dat$ps_hat <- rf_ps$predictions[, "1"]
}
