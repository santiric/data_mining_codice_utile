source("utils.R")
library(tidyverse)


# per evitare errori successivi:

dati <- dati %>%
  mutate(across(where(is.character), as.factor))

# CODIFICA EFFETTIVAMENTE COME FACTOR LE ESPLICATIVE QUALITATIVE
# ANCHE SE SONO 0/1, IN QUESTO MODO VA TUTTO FLUIDO DOPO E NON FAI ERRORI
# COME LISCIARE UN FACTOR


# MEGLIO NON AVERE VARIABILI CON TROPPE MODALITÀ, POI È UN CASINO SE ALCUNE
# VARIABILI NEL FOLD DI VERIFICA HANNO MODALITÀ DIVERSE DA QUELLE NELLA STIMA.

# Variabile risposta ------------------------------------------------------

dati <- dati %>%
  rename("y" = "risposta_nome")
# one-hot dei fattori a troppi livelli ------------------------------------------------

# Soglia per "troppe modalità"
soglia_modalita = 32

# Trova tutti i fattori con numero di livelli >= soglia
fattori_high = names(dati)[sapply(dati, function(x) {
  is.factor(x) && nlevels(x) >= soglia_modalita
})]

cat("Fattori con", soglia_modalita, "+ modalità trovati:\n")
print(fattori_high)

# Per ciascuno, crea le indicatrici con contr.sum e sostituisce la colonna
for (var in fattori_high) {

  # Formula dinamica
  formula_var = as.formula(paste("~", var))

  # Matrice delle indicatrici (contr.sum droppa l'ultimo livello)
  contrasts_list = setNames(list(contr.sum), var)
  ind_mat = model.matrix(formula_var, dati,
                         contrasts.arg = contrasts_list)[, -1, drop = FALSE]

  # Nomi colonne: var_livello (tutti tranne l'ultimo)
  livelli       = levels(dati[[var]])
  livelli_usati = livelli[-length(livelli)]
  colnames(ind_mat) = paste0(var, "_", livelli_usati)

  # Sostituisce il fattore originale con le indicatrici
  dati[[var]] = NULL
  dati = cbind(dati, ind_mat)
}

cat("\nDimensioni dataset dopo trasformazione:", dim(dati), "\n")
cat("Nuove colonne aggiunte per ogni fattore:\n")
for (var in fattori_high) {
  nuove_col = grep(paste0("^", var, "_"), names(dati), value = TRUE)
  cat(" -", var, "->", length(nuove_col), "indicatrici\n")
}


# variabili numeriche e factor --------------------------------------------
id_risposta <- which(names(dati) == "y")

id_num <- setdiff(which(sapply(dati, function(x) is.numeric(x) | is.integer(x))), id_risposta)
id_factor <- which(sapply(dati, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(dati)[id_risposta]]


# rinomina ----------------------------------------------------------------


# solo per riciclare codice:
sss <- dati
#rm(dati)


# Standardizzazione -------------------------------------------------------

cat("Variabili quantitative standardizzate:\n")
print(id_num)

sss[, id_num] = scale(sss[, id_num])


# ==============================================================================
# SETUP GENERALE
# ==============================================================================

# Funzione errore di classificazione
ce <- function(previsto, osservato) {
  tab <- table(previsto, osservato)
  1 - sum(diag(tab)) / sum(tab)
}

# y deve essere factor
sss$y <- factor(sss$y)
(livelli <- levels(sss$y))
(K <- nlevels(sss$y))

# Matrice indicatrici risposta (per i modelli lineari/lasso)
library(nnet)
Y_full <- class.ind(sss$y)   # n x K, tenuta fuori da X

# Contrasti a somma zero per tutti i fattori
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
cat_vars <- setdiff(cat_vars, "y")
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- lvls[-length(lvls)]
  C
}
contrasts_list <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)

# Matrice design sum-zero globale (senza intercetta, senza y)
# NON si usa lm() (crasha con p > n) né reformulate() con molte variabili
# (causa infinite recursion nell'evaluator di R).
# Soluzione: togliamo y dal dataframe e usiamo ~ . su sss_X.
sss_X  <- sss[, setdiff(names(sss), "y"), drop = FALSE]
X_sum0 <- model.matrix(~ ., data = sss_X,
                       contrasts.arg = contrasts_list)[, -1, drop = FALSE]
cat("Dimensioni X_sum0:", dim(X_sum0), "\n")


# ==============================================================================
# FOLD DI CONVALIDA INCROCIATA
# ==============================================================================

n_fold   <- 4
seed_cv  <- 1

set.seed(seed_cv)
n_trunc  <- floor(nrow(sss) / n_fold) * n_fold
cv_id_sb <- matrix(sample(1:nrow(sss), n_trunc), ncol = n_fold)
cv_id    <- cv_id_sb   # distribuzione naturale, nessun ribilanciamento

cat("Fold creati:", n_fold, "| dimensione fold verifica:", nrow(cv_id_sb), "\n")
for (k in 1:n_fold) {
  cat(sprintf("[Fold %d] distribuzione y:\n", k))
  print(table(sss$y[cv_id_sb[, k]]))
}


# ==============================================================================
# DATAFRAME PREVISIONI OUT-OF-FOLD
# ==============================================================================
# Una riga per osservazione, una colonna per modello.
# Salviamo la CLASSE predetta (factor), non una probabilità.

cv_preds <- data.frame(
  id = 1:nrow(sss),
  y  = sss$y
)

errori_cv <- data.frame(modello = character(), errore_cv = numeric(),
                        stringsAsFactors = FALSE)
# Dizionario per le tabelle di confusione cumulative
conf_mat_list <- list()

# Aggiorna la tabella di confusione cumulativa per un modello
aggiungi_conf <- function(nome_modello, previsto, osservato) {
  tab <- table(Previsto = previsto, Osservato = osservato)
  if (is.null(conf_mat_list[[nome_modello]])) {
    conf_mat_list[[nome_modello]] <<- tab
  } else {
    conf_mat_list[[nome_modello]] <<- conf_mat_list[[nome_modello]] + tab
  }
}

# ==============================================================================
# HELPER: matrice design sum-zero per un sottoinsieme di righe
# ==============================================================================
make_X <- function(idx, data = sss, contrasts = contrasts_list) {
  d <- data[idx, setdiff(names(data), "y"), drop = FALSE]
  model.matrix(~ ., data = d, contrasts.arg = contrasts)[, -1, drop = FALSE]
}

make_X_newdata <- function(newdata, contrasts = contrasts_list) {
  d <- newdata[, setdiff(names(newdata), "y"), drop = FALSE]
  model.matrix(~ ., data = d, contrasts.arg = contrasts)[, -1, drop = FALSE]
}


# ==============================================================================
# 1. LINEARE MULTIVARIATO (K lm separati + step)
# ==============================================================================
# Strategia: step su subsample, poi predice su test con formula selezionata.
# Per semplicità in CV: step su train, predict su test.

metrics_folds_lm_multi <- vector("list", n_fold)
if (!"lm_multi" %in% names(cv_preds)) cv_preds$lm_multi <- NA_character_

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  train <- sss[id_train, ]
  test  <- sss[id_test,  ]
  Y_tr  <- class.ind(train$y)

  # Stima K modelli lineari con step (forward) su train
  m_lin_k <- lapply(1:K, function(j)
    step(lm(Y_tr[, j] ~ . - y, data = train,
            contrasts = contrasts_list),
         direction = "both", trace = 0)
  )

  # Previsioni: matrice n_test x K
  pr_mat <- do.call(cbind, lapply(m_lin_k, predict, newdata = test))
  classe  <- livelli[apply(pr_mat, 1, which.max)]

  cv_preds$lm_multi[id_test] <- classe
  metrics_folds_lm_multi[[k]] <- ce(classe, test$y)
  aggiungi_conf("Lineare multivariato step", classe, test$y)
  cat("lm_multi fold", k, "\n")
}

err_lm_multi <- mean(unlist(metrics_folds_lm_multi))
errori_cv <- rbind(errori_cv, data.frame(modello = "Lineare multivariato step",
                                         errore_cv = round(err_lm_multi, 4)))

cat(sprintf("\n[Lineare multivariato step] Errore CV medio: %.4f\n\n", err_lm_multi))

# Modello finale su tutti i dati
m_lin_final <- lapply(1:K, function(j)
  step(lm(Y_full[, j] ~ . - y, data = sss, contrasts = contrasts_list),
       direction = "both", trace = 0)
)
# Riestimiamo con contrasti sum-zero (già impostati, ma esplicitiamo)
m_lin_sum0 <- lapply(1:K, function(j) {
  f <- formula(m_lin_final[[j]])
  lm(f, data = sss, contrasts = contrasts_list)
})
lapply(m_lin_sum0, summary)


# ==============================================================================
# 2. REGRESSIONE MULTINOMIALE (multinom + step) politopmico
# ==============================================================================
library(nnet)

metrics_folds_multi <- vector("list", n_fold)
if (!"multinomiale" %in% names(cv_preds)) cv_preds$multinomiale <- NA_character_

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  train <- sss[id_train, ]
  test  <- sss[id_test,  ]

  m_multi0_k <- multinom(y ~ ., data = train, maxit = 200, trace = FALSE)
  m_multi_k  <- step(m_multi0_k, trace = FALSE)

  classe <- predict(m_multi_k, newdata = test, type = "class")

  cv_preds$multinomiale[id_test] <- as.character(classe)
  metrics_folds_multi[[k]] <- ce(classe, test$y)
  aggiungi_conf("Multinomiale step", classe, test$y)
  cat("multinomiale fold", k, "\n")
}

err_multi <- mean(unlist(metrics_folds_multi))
errori_cv <- rbind(errori_cv, data.frame(modello = "Multinomiale step",
                                         errore_cv = round(err_multi, 4)))

cat(sprintf("\n[Multinomiale step] Errore CV medio: %.4f\n\n", err_multi))

# Modello finale
m_multi0_final <- multinom(y ~ ., data = sss, maxit = 200, trace = FALSE)
m_multi_step   <- step(m_multi0_final, trace = FALSE)
formula_multi  <- formula(m_multi_step)
m_multi        <- multinom(formula_multi, data = sss, maxit = 200, trace = FALSE)
m_multi_sum0   <- multinom(formula_multi, data = sss, maxit = 200,
                           contrasts = contrasts_list, trace = FALSE)
summary(m_multi_sum0)


# ==============================================================================
# 3. BASELINE CATEGORY LOGIT (vglm + step4vglm)
# ==============================================================================
library(VGAM)

metrics_folds_vglm <- vector("list", n_fold)
if (!"vglm_step" %in% names(cv_preds)) cv_preds$vglm_step <- NA_character_

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  train <- sss[id_train, ]
  test  <- sss[id_test,  ]

  m_vglm0_k  <- vglm(y ~ ., data = train, family = multinomial)
  m_vglm_k   <- step4vglm(m_vglm0_k)
  formula_k  <- formula(m_vglm_k)
  m_vglm_fit <- vglm(formula_k, data = train, family = multinomial)

  pr_mat <- predict(m_vglm_fit, newdata = test, type = "response")
  classe  <- livelli[apply(pr_mat, 1, which.max)]

  cv_preds$vglm_step[id_test] <- classe
  metrics_folds_vglm[[k]] <- ce(classe, test$y)
  aggiungi_conf("Baseline category logit step", classe, sss$y[id_test])
  cat("vglm fold", k, "\n")
}

err_vglm <- mean(unlist(metrics_folds_vglm))
errori_cv <- rbind(errori_cv, data.frame(modello = "Baseline category logit step",
                                         errore_cv = round(err_vglm, 4)))

cat(sprintf("\n[Baseline category logit] Errore CV medio: %.4f\n\n", err_vglm))

# Modello finale
m_vglm0_final <- vglm(y ~ ., data = sss, family = multinomial)
m_vglm_step   <- step4vglm(m_vglm0_final)
formula_vglm  <- formula(m_vglm_step)
m_vglm        <- vglm(formula_vglm, data = sss, family = multinomial)
m_vglm_sum0   <- vglm(formula_vglm, data = sss, family = multinomial,
                      contrasts.arg = contrasts_list)
coef(m_vglm_sum0, matrix = TRUE)


# ==============================================================================
# 4. LDA
# ==============================================================================
library(MASS)

metrics_folds_lda <- vector("list", n_fold)
if (!"lda" %in% names(cv_preds)) cv_preds$lda <- NA_character_

# Usiamo la formula selezionata dalla multinomiale come riduzione variabili
formula_lda <- formula_multi   # oppure y ~ . per usare tutto

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  train <- sss[id_train, ]
  test  <- sss[id_test,  ]

  m_lda_k <- lda(formula_lda, data = train)
  classe   <- predict(m_lda_k, newdata = test)$class

  cv_preds$lda[id_test] <- as.character(classe)
  metrics_folds_lda[[k]] <- ce(classe, test$y)
  aggiungi_conf("LDA", classe, test$y)
  cat("lda fold", k, "\n")
}

err_lda <- mean(unlist(metrics_folds_lda))
errori_cv <- rbind(errori_cv, data.frame(modello = "LDA",
                                         errore_cv = round(err_lda, 4)))

cat(sprintf("\n[LDA] Errore CV medio: %.4f\n\n", err_lda))

m_lda <- lda(formula_lda, data = sss)


# ==============================================================================
# 5. QDA
# ==============================================================================
# ATTENZIONE: QDA non stima se una variabile non varia dentro una classe.
# In caso di errore di rank deficiency, ridurre le variabili.

metrics_folds_qda <- vector("list", n_fold)
if (!"qda" %in% names(cv_preds)) cv_preds$qda <- NA_character_

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  train <- sss[id_train, ]
  test  <- sss[id_test,  ]

  m_qda_k <- tryCatch(
    qda(formula_lda, data = train),
    error = function(e) { cat("QDA errore fold", k, ":", e$message, "\n"); NULL }
  )
  if (is.null(m_qda_k)) next

  classe <- predict(m_qda_k, newdata = test)$class

  cv_preds$qda[id_test] <- as.character(classe)
  metrics_folds_qda[[k]] <- ce(classe, test$y)
  aggiungi_conf("QDA", classe, test$y)
  cat("qda fold", k, "\n")
}

err_qda <- mean(unlist(metrics_folds_qda))
errori_cv <- rbind(errori_cv, data.frame(modello = "QDA",
                                         errore_cv = round(err_qda, 4)))

cat(sprintf("\n[QDA] Errore CV medio: %.4f\n\n", err_qda))

m_qda <- tryCatch(qda(formula_lda, data = sss), error = function(e) NULL)


# ==============================================================================
# 6. LASSO LINEARE (K glmnet gaussiani separati)
# ==============================================================================
# - Griglia lambda comune a tutti i K modelli, fissata sull'intero dataset.
# - Per ogni fold: stima K modelli su train, valida su test (ce su cb interno),
#   seleziona lambda ottimo, salva la classe predetta.
library(glmnet)

# Griglia lambda globale (fissata una volta sola)
griglia_lasso_lin <- lapply(1:K, function(j)
  glmnet(X_sum0, Y_full[, j], alpha = 1,
         lambda.min.ratio = 1e-8)$lambda
)
# Griglia comune = intersezione (lambda dal max al min comune)
lambda_seq_lasso_lin <- Reduce(function(a, b) {
  lo <- max(min(a), min(b)); hi <- min(max(a), max(b))
  exp(seq(log(hi), log(lo), length.out = 100))
}, griglia_lasso_lin)
n_lam_ll <- length(lambda_seq_lasso_lin)

metrics_folds_lasso_lin <- vector("list", n_fold)
if (!"lasso_lin" %in% names(cv_preds)) cv_preds$lasso_lin <- NA_character_
lambda_per_fold_lasso_lin <- numeric(n_fold)

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  # Subsampling interno per selezione lambda (2/3 stima, 1/3 verifica)
  set.seed(k)
  cb1_k <- sample(id_train, floor(2/3 * length(id_train)))
  cb2_k <- setdiff(id_train, cb1_k)

  X_cb1 <- X_sum0[cb1_k, ]; Y_cb1 <- Y_full[cb1_k, ]
  X_cb2 <- X_sum0[cb2_k, ]; Y_cb2 <- Y_full[cb2_k, ]
  X_tst <- X_sum0[id_test, ]

  # Stima K modelli lasso su cb1
  fits_k <- lapply(1:K, function(j)
    glmnet(X_cb1, Y_cb1[, j], alpha = 1,
           lambda = lambda_seq_lasso_lin)
  )

  # Selezione lambda su cb2
  err_cb2 <- sapply(lambda_seq_lasso_lin, function(lam) {
    pr_mat <- do.call(cbind, lapply(fits_k, predict, newx = X_cb2, s = lam))
    classe  <- apply(pr_mat, 1, which.max)
    ce(classe, sss$y[cb2_k])
  })
  lambda_k <- lambda_seq_lasso_lin[which.min(err_cb2)]
  lambda_per_fold_lasso_lin[k] <- lambda_k

  # Predici su test
  pr_mat_tst <- do.call(cbind, lapply(fits_k, predict,
                                      newx = X_tst, s = lambda_k))
  classe <- livelli[apply(pr_mat_tst, 1, which.max)]

  cv_preds$lasso_lin[id_test] <- classe
  metrics_folds_lasso_lin[[k]] <- ce(classe, sss$y[id_test])
  aggiungi_conf("Lasso lineare", classe, sss$y[id_test])
  cat("lasso_lin fold", k, "| lambda:", round(lambda_k, 5), "\n")
}

err_lasso_lin <- mean(unlist(metrics_folds_lasso_lin))
errori_cv <- rbind(errori_cv, data.frame(modello = "Lasso lineare",
                                         errore_cv = round(err_lasso_lin, 4)))

cat(sprintf("\n[Lasso lineare] Errore CV medio: %.4f\n\n", err_lasso_lin))

# Modello finale con lambda medio da CV
lambda_ottimo_lasso_lin <- mean(lambda_per_fold_lasso_lin)
# oppure: median(lambda_per_fold_lasso_lin)

m_lasso_lin <- lapply(1:K, function(j)
  glmnet(X_sum0, Y_full[, j], alpha = 1, lambda = lambda_ottimo_lasso_lin)
)


# Numero di predittori selezionati per classe
cat("\n--- Predittori selezionati per classe (lambda ottimo) ---\n")
predittori_per_classe <- sapply(1:K, function(j) {
  coef_j <- coef(m_lasso_lin[[j]], s = lambda_ottimo_lasso_lin)
  sum(coef_j[-1] != 0)  # escludi intercetta
})
names(predittori_per_classe) <- livelli
print(predittori_per_classe)

# Estrai coefficienti per tutte le classi
coef_multi_lasso_lin <- extract_lasso_coefs_multi(
  lasso_models = m_lasso_lin,
  s            = lambda_ottimo_lasso_lin,
  class_names  = livelli,
  top_k        = 10
)

# Visualizza
plot_lasso_coefs_multi(coef_multi_lasso_lin)

# Variabili selezionate in almeno 2 classi
coef_multi_lasso_lin %>%
  group_by(variabile) %>%
  summarise(n_classi = n(), abs_medio = mean(abs_coefficiente)) %>%
  arrange(desc(n_classi), desc(abs_medio))


# ==============================================================================
# 7. LASSO LOGISTICO (K glmnet binomiali separati)
# ==============================================================================
griglia_lasso_logit <- lapply(1:K, function(j)
  glmnet(X_sum0, as.numeric(Y_full[, j]), alpha = 1, family = "binomial",
         lambda.min.ratio = 1e-8)$lambda
)
lambda_seq_lasso_logit <- Reduce(function(a, b) {
  lo <- max(min(a), min(b)); hi <- min(max(a), max(b))
  exp(seq(log(hi), log(lo), length.out = 100))
}, griglia_lasso_logit)

metrics_folds_lasso_logit  <- vector("list", n_fold)
lambda_per_fold_lasso_logit <- numeric(n_fold)          # <-- nuovo

if (!"lasso_logit" %in% names(cv_preds)) cv_preds$lasso_logit <- NA_character_

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  set.seed(k)
  cb1_k <- sample(id_train, floor(2/3 * length(id_train)))
  cb2_k <- setdiff(id_train, cb1_k)

  X_cb1 <- X_sum0[cb1_k, ]; Y_cb1 <- Y_full[cb1_k, ]
  X_cb2 <- X_sum0[cb2_k, ]
  X_tst <- X_sum0[id_test, ]

  fits_k <- lapply(1:K, function(j)
    glmnet(X_cb1, as.numeric(Y_cb1[, j]), alpha = 1, family = "binomial",
           lambda = lambda_seq_lasso_logit)
  )

  err_cb2 <- sapply(lambda_seq_lasso_logit, function(lam) {
    pr_mat <- do.call(cbind, lapply(fits_k, predict,
                                    newx = X_cb2, s = lam, type = "response"))
    ce(apply(pr_mat, 1, which.max), sss$y[cb2_k])
  })
  lambda_k <- lambda_seq_lasso_logit[which.min(err_cb2)]
  lambda_per_fold_lasso_logit[k] <- lambda_k             # <-- salva

  pr_mat_tst <- do.call(cbind, lapply(fits_k, predict,
                                      newx = X_tst, s = lambda_k,
                                      type = "response"))
  classe <- livelli[apply(pr_mat_tst, 1, which.max)]

  cv_preds$lasso_logit[id_test] <- classe
  metrics_folds_lasso_logit[[k]] <- ce(classe, sss$y[id_test])
  aggiungi_conf("Lasso logistico", classe, sss$y[id_test])
  cat("lasso_logit fold", k, "| lambda:", round(lambda_k, 5), "\n")
}

err_lasso_logit <- mean(unlist(metrics_folds_lasso_logit))
errori_cv <- rbind(errori_cv, data.frame(modello = "Lasso logistico",
                                         errore_cv = round(err_lasso_logit, 4)))
cat(sprintf("\n[Lasso logistico] Errore CV medio: %.4f\n\n", err_lasso_logit))

# Modello finale: lambda medio da CV
lambda_ottimo_lasso_logit <- mean(lambda_per_fold_lasso_logit)  # <-- nuovo
cat("Lambda ottimo lasso logistico (media CV):", lambda_ottimo_lasso_logit, "\n")

m_lasso_logit <- lapply(1:K, function(j)
  glmnet(X_sum0, as.numeric(Y_full[, j]), alpha = 1,
         family = "binomial", lambda = lambda_ottimo_lasso_logit)
)

cat("\n--- Predittori selezionati per classe (lambda ottimo) ---\n")
predittori_per_classe_logit <- sapply(1:K, function(j) {
  coef_j <- coef(m_lasso_logit[[j]], s = lambda_ottimo_lasso_logit)
  sum(coef_j[-1] != 0)
})
names(predittori_per_classe_logit) <- livelli
print(predittori_per_classe_logit)

coef_multi_lasso_logit <- extract_lasso_coefs_multi(
  lasso_models = m_lasso_logit,
  s            = lambda_ottimo_lasso_logit,
  class_names  = livelli,
  top_k        = 10
)
plot_lasso_coefs_multi(coef_multi_lasso_logit)

coef_multi_lasso_logit %>%
  group_by(variabile) %>%
  summarise(n_classi = n(), abs_medio = mean(abs_coefficiente)) %>%
  arrange(desc(n_classi), desc(abs_medio))

# ==============================================================================
# 8. MARS POLICLASSE (polyclass)
# ==============================================================================
library(polspline)
metrics_folds_mars <- vector("list", n_fold)
if (!"mars" %in% names(cv_preds)) cv_preds$mars <- NA_character_

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  set.seed(k)
  cb1_k <- sample(id_train, floor(2/3 * length(id_train)))
  cb2_k <- setdiff(id_train, cb1_k)

  m_mars_k <- polyclass(
    sss$y[cb1_k],
    cov   = X_sum0[cb1_k, ],
    tdata = sss$y[cb2_k],
    tcov  = X_sum0[cb2_k, ]
  )

  classe <- cpolyclass(m_mars_k, cov = X_sum0[id_test, ])
  cv_preds$mars[id_test] <- as.character(classe)
  metrics_folds_mars[[k]] <- ce(classe, sss$y[id_test])
  aggiungi_conf("MARS (polyclass)", classe, sss$y[id_test])
  cat("mars fold", k, "| basi selezionate:", m_mars_k$number, "\n")  # <-- info utile
}

err_mars <- mean(unlist(metrics_folds_mars))
errori_cv <- rbind(errori_cv, data.frame(modello = "MARS (polyclass)",
                                         errore_cv = round(err_mars, 4)))
cat(sprintf("\n[MARS polyclass] Errore CV medio: %.4f\n\n", err_mars))

# Modello finale (subsample necessario: polyclass richiede validation set separato)
set.seed(1)
cb1_f <- sample(1:nrow(sss), floor(2/3 * nrow(sss)))
cb2_f <- setdiff(1:nrow(sss), cb1_f)
m_mars <- polyclass(
  sss$y[cb1_f],
  cov   = X_sum0[cb1_f, ],
  tdata = sss$y[cb2_f],
  tcov  = X_sum0[cb2_f, ]
)

# Plot diagnostico
n_cb1_f <- length(cb1_f); n_cb2_f <- length(cb2_f)
err_tr <- m_mars$logl[, "loss-trn"] / n_cb1_f
err_vl <- m_mars$logl[, "loss-tst"] / n_cb2_f
plot(m_mars$logl[, "dim"], err_tr, type = "l", lwd = 2,
     xlab = "Numero basi spline", ylab = "Errore classificazione",
     main = "MARS / Polyclass")
lines(m_mars$logl[, "dim"], err_vl, col = 2, lwd = 2)
legend("topright", legend = c("Stima", "Verifica"), col = c(1, 2), lwd = 2, bty = "n")

# ==============================================================================
# SUMMARY - MARS polyclass
# ==============================================================================
cat("\n--- Summary modello MARS finale ---\n")
cat("Numero basi spline selezionate:", m_mars$number, "\n")
cat("Classi:", livelli, "\n")

# Predittori coinvolti in almeno una base
summary(m_mars)

library(ggplot2)
library(tidyr)

cov_idx  <- 3  # Scegli qui l'indice della covariata (es. 3)

# 1. Genera i dati per il grafico usando i range del modello
valori_x    <- seq(m_mars$ranges[1, cov_idx], m_mars$ranges[2, cov_idx], length.out = 200)
matrice_cov <- matrix(apply(X_sum0, 2, median), byrow = TRUE, nrow = 200, ncol = ncol(X_sum0))
matrice_cov[, cov_idx] <- valori_x

# 2. Calcola le probabilità e assegna in automatico i nomi di X e Y dai dati veri
prob_predette <- ppolyclass(matrice_cov, m_mars)
colnames(prob_predette) <- levels(as.factor(sss$y)) # Estrae i nomi veri delle categorie
nome_cov <- colnames(X_sum0)[cov_idx]               # Estrae il nome vero della covariata

# 3. Grafico istantaneo
df_long <- pivot_longer(data.frame(X = valori_x, prob_predette), cols = -X)

ggplot(df_long, aes(x = X, y = value, color = name)) +
  geom_line(size = 1.2) +
  labs(x = nome_cov, y = "Probabilità Predetta", color = "Categoria",
       title = paste("Probabilità rispetto a", nome_cov)) +
  theme_minimal() + theme(legend.position = "bottom")


# ==============================================================================
# 9. ADDITIVO (polyclass con additive = TRUE)
# ==============================================================================

metrics_folds_add <- vector("list", n_fold)
if (!"additivo" %in% names(cv_preds)) cv_preds$additivo <- NA_character_

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  set.seed(k)
  cb1_k <- sample(id_train, floor(2/3 * length(id_train)))
  cb2_k <- setdiff(id_train, cb1_k)

  m_add_k <- polyclass(
    sss$y[cb1_k],
    cov      = X_sum0[cb1_k, ],
    tdata    = sss$y[cb2_k],
    tcov     = X_sum0[cb2_k, ],
    additive = TRUE
  )

  classe <- cpolyclass(m_add_k, cov = X_sum0[id_test, ])

  cv_preds$additivo[id_test] <- as.character(classe)
  metrics_folds_add[[k]] <- ce(classe, sss$y[id_test])
  aggiungi_conf("Additivo (polyclass add)", classe, sss$y[id_test])
  cat("additivo fold", k, "\n")
}

err_add <- mean(unlist(metrics_folds_add))
errori_cv <- rbind(errori_cv, data.frame(modello = "Additivo (polyclass add)",
                                         errore_cv = round(err_add, 4)))

cat(sprintf("\n[Additivo] Errore CV medio: %.4f\n\n", err_add))

# Modello finale
m_add <- polyclass(
  sss$y[cb1_f],
  cov      = X_sum0[cb1_f, ],
  tdata    = sss$y[cb2_f],
  tcov     = X_sum0[cb2_f, ],
  additive = TRUE
)

library(ggplot2)
library(tidyr)

summary(m_add)

# 1. IMPOSTAZIONI
cov_idx  <- 3                  # Scegli l'indice della covariata da plottare
nome_cov <- colnames(X_sum0)[cov_idx]
# nome_cov <- "MioNomeAValore" # <--- Togli il '#' e scrivi qui il nome se X_sum0 non ha intestazioni

# 2. GENERAZIONE DATI E CALCOLO PROBABILITÀ
valori_x    <- seq(m_add$ranges[1, cov_idx], m_add$ranges[2, cov_idx], length.out = 200)
matrice_cov <- matrix(apply(X_sum0, 2, median), byrow = TRUE, nrow = 200, ncol = ncol(X_sum0))
matrice_cov[, cov_idx] <- valori_x

prob_predette <- ppolyclass(matrice_cov, m_add)
colnames(prob_predette) <- levels(as.factor(sss$y)) # Nomi reali delle categorie della risposta

# 3. GRAFICO GGPLOT2
df_long <- pivot_longer(data.frame(X = valori_x, prob_predette), cols = -X)

ggplot(df_long, aes(x = X, y = value, color = name)) +
  geom_line(size = 1.2) +
  labs(
    title = paste("Modello Additivo: Probabilità rispetto a", nome_cov),
    x = nome_cov,
    y = "Probabilità Predetta",
    color = "Categoria"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ==============================================================================
# 10. ALBERO DI CLASSIFICAZIONE (tree + prune.tree)
# ==============================================================================
library(tree)

dim_albero <- 2:15
n_dim      <- length(dim_albero)

# Matrice fold x dimensioni per raccogliere gli errori
err_albero_mat <- matrix(NA_real_, nrow = n_fold, ncol = n_dim,
                         dimnames = list(fold = NULL, dim = dim_albero))

metrics_folds_tree <- vector("list", n_fold)

for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  train <- sss[id_train, ]
  test  <- sss[id_test,  ]

  m_tree_k <- tree(y ~ ., data = train, split = "deviance",
                   control = tree.control(nobs    = nrow(train),
                                          minsize = 2,
                                          mindev  = 0.0001))

  # Errore per ogni dimensione di potatura
  err_albero_mat[k, ] <- sapply(dim_albero, function(l) {
    t_pot <- tryCatch(prune.tree(m_tree_k, best = l),
                      error = function(e) NULL)
    if (is.null(t_pot)) return(NA_real_)
    classe <- predict(t_pot, newdata = test, type = "class")
    ce(classe, test$y)
  })
  cat("tree fold", k, "\n")
}

# Dimensione ottima: minimizza l'errore medio sui fold
err_medio_dim <- colMeans(err_albero_mat, na.rm = TRUE)
B <- which.min(err_medio_dim)
cat(sprintf("\nDimensione ottima albero: %d foglie (errore CV: %.4f)\n\n",
            dim_albero[B], err_medio_dim[B]))

# Plot errore vs dimensione
plot(dim_albero, err_medio_dim, type = "b", pch = 16, lwd = 2,
     xlab = "Foglie", ylab = "Errore CV medio", main = "Selezione dimensione albero")
abline(v = dim_albero[B], col = 2, lwd = 2)

# Previsioni out-of-fold alla dimensione ottima
if (!"tree" %in% names(cv_preds)) cv_preds$tree <- NA_character_

# CICLO FOR SOLO PER SALVERE I RISULTATI DELL'OTTIMO, STIAMO SEMPRE
# USANDO LA STESSA CV SIA PER SCEGLIERE LA PROFONDITÁ CHE PER
# STIMARE L'ERRORE.
for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  m_tree_k <- tree(y ~ ., data = sss[id_train, ], split = "deviance",
                   control = tree.control(nobs    = length(id_train),
                                          minsize = 2,
                                          mindev  = 0.0001))
  m_tree_pot <- prune.tree(m_tree_k, best = dim_albero[B])
  classe <- predict(m_tree_pot, newdata = sss[id_test, ], type = "class")

  cv_preds$tree[id_test] <- as.character(classe)
  metrics_folds_tree[[k]] <- ce(classe, sss$y[id_test])
  aggiungi_conf("Albero", classe, sss$y[id_test])
}

err_tree <- mean(unlist(metrics_folds_tree))
errori_cv <- rbind(errori_cv, data.frame(modello = "Albero",
                                         errore_cv = round(err_tree, 4)))

cat(sprintf("\n[Albero] Errore CV medio: %.4f\n\n", err_tree))

# Modello finale
m_tree_full <- tree(y ~ ., data = sss, split = "deviance",
                    control = tree.control(nobs = nrow(sss),
                                           minsize = 2, mindev = 0.0001))
m_tree_best <- prune.tree(m_tree_full, best = dim_albero[B])
plot(m_tree_best)
text(m_tree_best, pretty = 0, cex = 0.7)


# ==============================================================================
# 11. RANDOM FOREST (ranger)
# ==============================================================================
library(ranger)

m_try_seq <- c(10,20)
n_mtry    <- length(m_try_seq)
n_trees   <- 500

# Errore out-of-fold per ogni mtry
perf_mat_rf <- matrix(NA_real_, nrow = nrow(sss), ncol = n_mtry)

metrics_folds_rf <- vector("list", n_fold)
if (!"rf" %in% names(cv_preds)) cv_preds$rf <- NA_character_

set.seed(1)
for (k in 1:n_fold) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]

  perf_fold      <- numeric(n_mtry)
  classe_fold_list <- matrix(NA_character_, nrow = length(id_test), ncol = n_mtry)

  for (m in seq_along(m_try_seq)) {
    fit_k <- ranger(y ~ ., data = sss[id_train, ],
                    mtry     = m_try_seq[m],
                    num.trees = n_trees,
                    importance = "none")
    classe_m <- predict(fit_k, data = sss[id_test, ])$predictions
    perf_fold[m] <- ce(classe_m, sss$y[id_test])
    classe_fold_list[, m] <- as.character(classe_m)
  }

  perf_mat_rf[id_test, ] <- matrix(rep(perf_fold, each = length(id_test)),
                                   nrow = length(id_test))

  # mtry ottimo per questo fold = minimizza errore
  best_m_idx <- which.min(perf_fold)
  classe_best <- classe_fold_list[, best_m_idx]

  cv_preds$rf[id_test] <- classe_best
  metrics_folds_rf[[k]] <- ce(classe_best, sss$y[id_test])
  aggiungi_conf("Random Forest", classe_best, sss$y[id_test])
  cat("rf fold", k, "| mtry ottimo:", m_try_seq[best_m_idx], "\n")
}

# mtry ottimo globale
perf_mean_rf <- colMeans(perf_mat_rf, na.rm = TRUE)
mtry_ottimo  <- m_try_seq[which.min(perf_mean_rf)]
cat("\nMtry ottimo globale:", mtry_ottimo, "\n")

# Barplot errore vs mtry
barplot(perf_mean_rf, names.arg = m_try_seq,
        xlab = "mtry", ylab = "Errore CV medio",
        main = "Selezione mtry - Random Forest", col = "steelblue")
abline(h = min(perf_mean_rf), col = 2, lty = 2)

err_rf <- mean(unlist(metrics_folds_rf))
errori_cv <- rbind(errori_cv, data.frame(modello = "Random Forest",
                                         errore_cv = round(err_rf, 4)))

cat(sprintf("\n[Random Forest] Errore CV medio: %.4f\n\n", err_rf))

# Modello finale con importance
set.seed(1)
m_rf <- ranger(y ~ ., data = sss,
               mtry      = mtry_ottimo,
               num.trees = n_trees,
               importance = "permutation")

# Importanza variabili
imp <- sort(m_rf$variable.importance, decreasing = TRUE)
barplot(imp[1:min(20, length(imp))],
        las = 2, cex.names = 0.7,
        main = "Importanza variabili (permutazione)",
        col = "steelblue")


## ==============================================================================
# CONFRONTO FINALE
# ==============================================================================

errori_cv$errore_cv <- round(as.numeric(errori_cv$errore_cv), 4)
errori_cv_ord <- errori_cv[order(errori_cv$errore_cv), ]

cat(paste(c(
  "",
  "================================================================",
  "   CONFRONTO FINALE — ERRORE DI CLASSIFICAZIONE (CV)",
  "================================================================",
  sprintf("  %-35s %s", "Modello", "Errore CV"),
  "  -----------------------------------------------",
  apply(errori_cv_ord, 1, function(r)
    sprintf("  %-35s %.4f", r["modello"], as.numeric(r["errore_cv"]))
  ),
  "================================================================",
  ""
), collapse = "\n"), "\n")

print(errori_cv_ord, row.names = FALSE)

# ------------------------------------------------------------------
# TABELLE DI CONFUSIONE CUMULATIVE (somma sui fold)
# ------------------------------------------------------------------
cat("\n================================================================\n")
cat("   TABELLE DI CONFUSIONE (somma fold CV)\n")
cat("================================================================\n\n")

for (nome in errori_cv_ord$modello) {
  if (!is.null(conf_mat_list[[nome]])) {
    cat(sprintf("--- %s ---\n", nome))

    tab <- conf_mat_list[[nome]]
    print(tab)

    # Metriche aggregate dalla tabella cumulativa
    n_tot     <- sum(tab)
    n_correct <- sum(diag(tab))
    acc       <- n_correct / n_tot

    # Precision, Recall per classe
    prec_per_classe   <- diag(tab) / colSums(tab)  # TP / (TP+FP) per colonna=osservato
    recall_per_classe <- diag(tab) / rowSums(tab)  # TP / (TP+FN) per riga=previsto
    # Nota: se Previsto=righe, Osservato=colonne:
    # precision = TP/(TP+FP) = diag/colSums  (quanti osservati di classe k
    #                                          sono stati predetti k)
    # recall    = TP/(TP+FN) = diag/rowSums  (dei predetti k, quanti erano k)

    cat(sprintf("  Accuratezza: %.4f\n", acc))
    cat("  Recall per classe (sensitività):\n")
    print(round(recall_per_classe, 4))
    cat("  Precision per classe:\n")
    print(round(prec_per_classe, 4))
    cat("\n")
  }
}
