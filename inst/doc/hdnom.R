## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  dev = "ragg_png",
  dpi = 72,
  fig.retina = 2,
  fig.align = "center",
  out.width = "100%",
  pngquant = "--speed=1 --quality=50"
)

## -----------------------------------------------------------------------------
library("hdnom")

## -----------------------------------------------------------------------------
data("smart")
x <- as.matrix(smart[, -c(1, 2)])
time <- smart$TEVENT
event <- smart$EVENT
y <- survival::Surv(time, event)

## ----eval = FALSE-------------------------------------------------------------
#  suppressMessages(library("doParallel"))
#  registerDoParallel(detectCores())
#  
#  fit <- fit_aenet(x, y, nfolds = 10, rule = "lambda.1se", seed = c(5, 7), parallel = TRUE)
#  names(fit)

## ----echo = FALSE-------------------------------------------------------------
fit <- readRDS("fit.rds")

## -----------------------------------------------------------------------------
model <- fit$model
alpha <- fit$alpha
lambda <- fit$lambda
adapen <- fit$pen_factor

## ----fig.width = 8, fig.height = 8, out.width = 600, out.height = 600---------
nom <- as_nomogram(
  fit, x, time, event,
  pred.at = 365 * 2,
  funlabel = "2-Year Overall Survival Probability"
)

plot(nom)

## -----------------------------------------------------------------------------
val_int <- validate(
  x, time, event,
  model.type = "aenet",
  alpha = alpha, lambda = lambda, pen.factor = adapen,
  method = "bootstrap", boot.times = 10,
  tauc.type = "UNO", tauc.time = seq(1, 5, 0.5) * 365,
  seed = 42, trace = FALSE
)

print(val_int)
summary(val_int)

## ----fig.width = 8, fig.height = 8, out.width = 500, out.height = 500---------
plot(val_int)

## ----fig.width = 8, fig.height = 8, out.width = 500, out.height = 500---------
x_new <- as.matrix(smart[, -c(1, 2)])[1001:2000, ]
time_new <- smart$TEVENT[1001:2000]
event_new <- smart$EVENT[1001:2000]

val_ext <- validate_external(
  fit, x, time, event,
  x_new, time_new, event_new,
  tauc.type = "UNO",
  tauc.time = seq(0.25, 2, 0.25) * 365
)

print(val_ext)
summary(val_ext)
plot(val_ext)

## -----------------------------------------------------------------------------
cal_int <- calibrate(
  x, time, event,
  model.type = "aenet",
  alpha = alpha, lambda = lambda, pen.factor = adapen,
  method = "bootstrap", boot.times = 10,
  pred.at = 365 * 5, ngroup = 3,
  seed = 42, trace = FALSE
)

print(cal_int)
summary(cal_int)

## ----fig.width = 8, fig.height = 8, out.width = 500, out.height = 500---------
plot(cal_int, xlim = c(0.5, 1), ylim = c(0.5, 1))

## ----fig.width = 8, fig.height = 8, out.width = 500, out.height = 500---------
cal_ext <- calibrate_external(
  fit, x, time, event,
  x_new, time_new, event_new,
  pred.at = 365 * 5, ngroup = 3
)

print(cal_ext)
summary(cal_ext)
plot(cal_ext, xlim = c(0.5, 1), ylim = c(0.5, 1))

## ----fig.width = 9, fig.height = 6, out.width = 600, out.height = 400---------
kmplot(
  cal_int,
  group.name = c("High risk", "Medium risk", "Low risk"),
  time.at = 1:6 * 365
)

kmplot(
  cal_ext,
  group.name = c("High risk", "Medium risk", "Low risk"),
  time.at = 1:6 * 365
)

## -----------------------------------------------------------------------------
cal_int_logrank <- logrank_test(cal_int)
cal_int_logrank
cal_int_logrank$pval

cal_ext_logrank <- logrank_test(cal_ext)
cal_ext_logrank
cal_ext_logrank$pval

## ----fig.width = 8, fig.height = 6.4, out.width = 600, out.height = 480-------
cmp_val <- compare_by_validate(
  x, time, event,
  model.type = c("lasso", "alasso"),
  method = "cv", nfolds = 5, tauc.type = "UNO",
  tauc.time = seq(0.25, 2, 0.25) * 365,
  seed = 42, trace = FALSE
)

print(cmp_val)
summary(cmp_val)
plot(cmp_val)
plot(cmp_val, interval = TRUE)

## ----fig.width = 8, fig.height = 6.4, out.width = 600, out.height = 480-------
cmp_cal <- compare_by_calibrate(
  x, time, event,
  model.type = c("lasso", "alasso"),
  method = "cv", nfolds = 5,
  pred.at = 365 * 9, ngroup = 5,
  seed = 42, trace = FALSE
)

print(cmp_cal)
summary(cmp_cal)
plot(cmp_cal, xlim = c(0.3, 1), ylim = c(0.3, 1))

## -----------------------------------------------------------------------------
predict(fit, x, y, newx = x[101:105, ], pred.at = 1:10 * 365)

