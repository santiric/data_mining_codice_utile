library(sm)
library(splines)

# ============================================================
# LOESS
# CV dello span con la stessa struttura cv_id del file principale
# ============================================================

# --- CV ---
griglia_span <- seq(0.05, 0.75, length = 50)
loess_cv_err <- matrix(0, ncol(cv_id), length(griglia_span))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id[, k]
  id_train <- as.vector(cv_id[, -k])

  for (i in seq_along(griglia_span)) {
    tmp  <- loess(y ~ x, data = sss[id_train, ], span = griglia_span[i])
    pred <- predict(tmp, newdata = sss[id_test, ])
    loess_cv_err[k, i] <- errore(pred, sss$y[id_test])
  }
}

cv_loess      <- colMeans(loess_cv_err)
best_span     <- griglia_span[which.min(cv_loess)]
cat("Span ottimale:", best_span, "\n")

plot(griglia_span, cv_loess, type = "l",
     xlab = "span", ylab = "CV MSE")
abline(v = best_span, col = 2, lty = 2)

# --- Modello finale su tutti i dati ---
m_loess  <- loess(y ~ x, data = sss, span = best_span)
loCV     <- loess.smooth(sss$x, sss$y, span = best_span)

plot(sss$x, sss$y, pch = 16, xlab = "x", ylab = "y")
lines(loCV, col = 2, lwd = 2)

# --- Errore CV ---
err_loess <- min(cv_loess)
cat("CV errore loess:", err_loess, "\n")


# --- SV (stima/verifica) ---
# Span già scelto dalla CV interna al training: rifai la CV solo su sss
m_loess_sv  <- loess(y ~ x, data = sss, span = best_span)
pred.loess  <- predict(m_loess_sv, newdata = vvv)
err_loess   <- errore(pred.loess, vvv$y)
cat("SV errore loess:", err_loess, "\n")

plot(sss$x, sss$y, pch = 16, xlab = "x", ylab = "y")
xgrid <- seq(min(sss$x), max(sss$x), length = 200)
lines(xgrid, predict(m_loess_sv, data.frame(x = xgrid)), col = 2, lwd = 2)


# ============================================================
# SPLINE DI REGRESSIONE  (bs)
# CV dei gradi di libertà (= nodi + 4 per cubica con intercetta)
# ============================================================

# --- CV ---
df_grid      <- 4:12          # da 0 nodi (df=4) a 8 nodi (df=12)
bspl_cv_err  <- matrix(0, ncol(cv_id), length(df_grid))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id[, k]
  id_train <- as.vector(cv_id[, -k])

  for (i in seq_along(df_grid)) {
    tmp  <- lm(y ~ bs(x, df = df_grid[i], degree = 3, intercept = TRUE) - 1,
               data = sss[id_train, ])
    pred <- predict(tmp, newdata = sss[id_test, ])
    bspl_cv_err[k, i] <- errore(pred, sss$y[id_test])
  }
}

cv_bspl   <- colMeans(bspl_cv_err)
best_df   <- df_grid[which.min(cv_bspl)]
cat("df ottimale spline regressione:", best_df, "\n")

plot(df_grid, cv_bspl, type = "b", pch = 16,
     xlab = "df", ylab = "CV MSE")
abline(v = best_df, col = 2, lty = 2)

# --- Modello finale su tutti i dati ---
m_bspl <- lm(y ~ bs(x, df = best_df, degree = 3, intercept = TRUE) - 1,
             data = sss)

xgrid   <- seq(min(sss$x), max(sss$x), length = 200)
splfit  <- predict(m_bspl, newdata = data.frame(x = xgrid))

plot(sss$x, sss$y, pch = 16, xlab = "x", ylab = "y")
lines(xgrid, splfit, col = 2, lwd = 2)
abline(v = attr(bs(sss$x, df = best_df, degree = 3, intercept = TRUE), "knots"),
       lty = 3)

# --- Errore CV ---
err_bspl <- min(cv_bspl)
cat("CV errore spline regressione:", err_bspl, "\n")


# --- SV ---
m_bspl_sv   <- lm(y ~ bs(x, df = best_df, degree = 3, intercept = TRUE) - 1,
                  data = sss)
pred.bspl   <- predict(m_bspl_sv, newdata = vvv)
err_bspl    <- errore(pred.bspl, vvv$y)
cat("SV errore spline regressione:", err_bspl, "\n")


# ============================================================
# SMOOTH SPLINE
# smooth.spline con CV interna (cv = TRUE → LOO-CV)
# ============================================================

# --- CV interna (LOO) ---
# smooth.spline sceglie lambda via LOO-CV internamente; nessun fold loop necessario.
# Usiamo la CV su sss per stimare l'errore di test.

sspl_cv_err <- numeric(ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id[, k]
  id_train <- as.vector(cv_id[, -k])

  tmp  <- smooth.spline(sss$x[id_train], sss$y[id_train], cv = TRUE)
  pred <- predict(tmp, x = sss$x[id_test])$y
  sspl_cv_err[k] <- errore(pred, sss$y[id_test])
}

err_sspl <- mean(sspl_cv_err)
cat("CV errore smooth spline:", err_sspl, "\n")

# --- Modello finale ---
m_sspl <- smooth.spline(sss$x, sss$y, cv = TRUE)
cat("df scelti smooth.spline:", m_sspl$df, "\n")

xgrid <- seq(min(sss$x), max(sss$x), length = 200)
plot(sss$x, sss$y, pch = 16, xlab = "x", ylab = "y")
lines(predict(m_sspl, x = xgrid), col = 2, lwd = 2)


# --- SV ---
m_sspl_sv  <- smooth.spline(sss$x, sss$y, cv = TRUE)
pred.sspl  <- predict(m_sspl_sv, x = vvv$x)$y
err_sspl   <- errore(pred.sspl, vvv$y)
cat("SV errore smooth spline:", err_sspl, "\n")


# ============================================================
# LISCIATORE BIVARIATO  (sm.regression)
# solo per pochi regressori quantitativi; h scelto via plug-in interno
# Non adatto a CV classica: usiamo stima/verifica
# ============================================================

library(sm)

# Indica le due variabili (modificare i nomi)
X_sm <- sss[, c("x1", "x2")]

# --- Stima/verifica ---
# sm.regression non ha predict() diretto; usiamo il valore sulla griglia interna
# Per predire su vvv serve eval.points
sm.regression(X_sm, sss$y,
              eval.points = as.matrix(vvv[, c("x1", "x2")]),
              display = "none") -> sm_fit

pred.sm  <- sm_fit$estimate
err_sm   <- errore(pred.sm, vvv$y)
cat("SV errore sm.regression bivariato:", err_sm, "\n")

# Grafico eslorativo (griglia interna, senza eval.points)
sm.regression(X_sm, sss$y, xlab = "x1", ylab = "x2", zlab = "y")


# ============================================================
# Aggiunta al confronto modelli (da incollare nel blocco ranking)
# ============================================================

# Nel blocco modelli_list del file principale, aggiungere:
#   "Loess"              = "err_loess",
#   "Spline regressione" = "err_bspl",
#   "Smooth spline"      = "err_sspl",
#   "SM bivariato"       = "err_sm",
