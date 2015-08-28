## ------------------------------------------------------------------------
library("hdnom")

## ------------------------------------------------------------------------
data("smart")
x = as.matrix(smart[, -c(1, 2)])
time = smart$TEVENT
event = smart$EVENT

library("survival")
y = Surv(time, event)

## ---- eval = FALSE-------------------------------------------------------
#  # Enable parallel parameter tuning
#  suppressMessages(library("doParallel"))
#  registerDoParallel(detectCores())
#  
#  aenetfit = hdcox.aenet(x, y, nfolds = 10, rule = "lambda.1se",
#                         seeds = c(5, 7), parallel = TRUE)
#  names(aenetfit)

## ---- echo = FALSE-------------------------------------------------------
aenetfit = readRDS("aenetfit.rds")

## ------------------------------------------------------------------------
fit    = aenetfit$aenet_model
alpha  = aenetfit$aenet_best_alpha
lambda = aenetfit$aenet_best_lambda
adapen = aenetfit$pen_factor

## ---- fig.width = 8, fig.height = 8, out.width = 600, out.height = 600----
suppressMessages(library("rms"))
x.df = as.data.frame(x)
dd = datadist(x.df)
options(datadist = "dd")

nom = hdnom.nomogram(fit, x, time, event, x.df,
                     lambda = lambda, pred.at = 365 * 2,
                     funlabel = "2-Year Overall Survival Probability")
plot(nom)

## ------------------------------------------------------------------------
set.seed(11)
val = hdnom.validate(x, time, event,
                     alpha = alpha, lambda = lambda, pen.factor = adapen,
                     method = "bootstrap", boot.times = 10,
                     tauc.type = "UNO", tauc.time = seq(1, 5, 0.5) * 365,
                     trace = FALSE)
val
summary(val)

## ---- fig.width = 8, fig.height = 8, out.width = 600, out.height = 600----
plot(val, ylim = c(0.6, 0.8), xaxt.label = seq(1, 5, 0.5))

## ------------------------------------------------------------------------
cal = hdnom.calibrate(x, time, event,
                      alpha = alpha, lambda = lambda, pen.factor = adapen,
                      method = "bootstrap", boot.times = 10,
                      pred.at = 365 * 5, ngroup = 5,
                      trace = FALSE)
cal
summary(cal)

## ---- fig.width = 8, fig.height = 8, out.width = 600, out.height = 600----
plot(cal, xlim = c(0.6, 1), ylim = c(0.6, 1))

