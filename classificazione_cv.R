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


#### y deve eddere numerica 


# variabili numeriche e factor --------------------------------------------

id_num <- setdiff(which(sapply(dati, function(x) is.numeric(x) | is.integer(x))), id_risposta)
id_factor <- which(sapply(dati, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(dati)[id_risposta]]



# errore ------------------------------------------------------------------

table(sss$y)/nrow(sss)
classification_metrics <- function(probs, truth, threshold = 0.5) {
  
  # --- Validazione input ---
  stopifnot(
    "probs e truth devono avere la stessa lunghezza" = length(probs) == length(truth),
    "truth deve contenere solo 0/1 o FALSE/TRUE"    = all(truth %in% c(0, 1, TRUE, FALSE)),
    "threshold deve essere tra 0 e 1"               = threshold >= 0 & threshold <= 1
  )
  
  # Se le probabilità escono dall'intervallo [0, 1] (es. output non calibrati
  # di certi modelli), le clippiamo all'intervallo valido.
  # pmax(0, x) porta a 0 i valori negativi, pmin(1, x) porta a 1 quelli > 1.
  n_clipped <- sum(probs < 0 | probs > 1, na.rm = TRUE)
  if (n_clipped > 0)
    message(sprintf("Attenzione: %d valore/i fuori da [0,1] cappato/i.", n_clipped))
  probs <- pmax(0, pmin(1, probs))
  
  truth     <- as.integer(truth)
  predicted <- as.integer(probs >= threshold)  # classificazione dura alla soglia
  
  # --- Confusion Matrix ---
  # La confusion matrix è la base di tutte le metriche successive.
  # Mette in relazione i valori predetti con quelli reali in 4 celle:
  #
  #                     Predetto POSITIVO   Predetto NEGATIVO
  #   Reale POSITIVO  |       TP           |       FN         |
  #   Reale NEGATIVO  |       FP           |       TN         |
  
  TP <- sum(predicted == 1 & truth == 1)
  # True Positive: il modello predice positivo ed era davvero positivo.
  
  TN <- sum(predicted == 0 & truth == 0)
  # True Negative: il modello predice negativo ed era davvero negativo.
  
  FP <- sum(predicted == 1 & truth == 0)
  # False Positive (errore di tipo I / "falso allarme"):
  # il modello predice positivo ma era negativo.
  # Es. un test medico che segnala malattia in un paziente sano.
  
  FN <- sum(predicted == 0 & truth == 1)
  # False Negative (errore di tipo II / "mancata rilevazione"):
  # il modello predice negativo ma era positivo.
  # Es. un test medico che non rileva la malattia in un paziente malato.
  
  N <- length(truth)  # numero totale di osservazioni
  
  # --- Accuracy ---
  # Proporzione di predizioni corrette sul totale.
  # Formula: (TP + TN) / N
  # Limite: fuorviante con classi sbilanciate (es. 95% negativi ->
  #         un modello che predice sempre 0 ha accuracy 0.95 senza imparare nulla).
  accuracy <- (TP + TN) / N
  
  # --- Sensitivity (= Recall = True Positive Rate, TPR) ---
  # Tra tutti i casi realmente positivi, quanti ne rileva il modello?
  # Formula: TP / (TP + FN)
  # Alta sensitivity -> pochi falsi negativi. Fondamentale in diagnostica:
  # meglio fare un falso allarme che perdersi un caso reale.
  sensitivity <- if (TP + FN > 0) TP / (TP + FN) else NA
  
  # --- Specificity (= True Negative Rate, TNR) ---
  # Tra tutti i casi realmente negativi, quanti vengono classificati correttamente?
  # Formula: TN / (TN + FP)
  # Alta specificity -> pochi falsi positivi. Importante quando un falso allarme
  # ha costi elevati (es. trattamenti invasivi o screening di massa).
  specificity <- if (TN + FP > 0) TN / (TN + FP) else NA
  
  # --- Precision (necessaria per il calcolo di F1, non esposta direttamente) ---
  # Tra tutti i casi predetti positivi, quanti lo sono davvero?
  # Formula: TP / (TP + FP)
  precision <- if (TP + FP > 0) TP / (TP + FP) else NA
  
  # --- F1 Score ---
  # Media armonica di precision e sensitivity (recall).
  # Formula: 2 * (precision * sensitivity) / (precision + sensitivity)
  # Preferibile alla media aritmetica perché penalizza fortemente i casi in cui
  # una delle due misure è molto bassa. Massimo = 1, minimo = 0.
  # Utile con classi sbilanciate: sintetizza il trade-off precision/recall
  # in un unico numero.
  f1 <- if (!is.na(precision) && !is.na(sensitivity) &&
            (precision + sensitivity) > 0)
    2 * precision * sensitivity / (precision + sensitivity) else NA
  
  # --- FPR (False Positive Rate = Tasso di Falsi Positivi) ---
  # Tra tutti i negativi reali, quanti vengono erroneamente classificati positivi?
  # Formula: FP / (FP + TN)  =  1 - specificity
  # È l'asse X della curva ROC. Un FPR alto significa molti falsi allarmi.
  fpr <- if (TN + FP > 0) FP / (FP + TN) else NA
  
  # --- FNR (False Negative Rate = Tasso di Falsi Negativi) ---
  # Tra tutti i positivi reali, quanti vengono erroneamente classificati negativi?
  # Formula: FN / (FN + TP)  =  1 - sensitivity
  # Un FNR alto significa che il modello "si perde" molti positivi reali.
  fnr <- if (TP + FN > 0) FN / (TP + FN) else NA
  
  # --- Output strutturato ---
  result <- list(
    threshold    = threshold,
    n            = N,
    
    confusion_matrix = list(TP = TP, TN = TN, FP = FP, FN = FN),
    
    accuracy    = accuracy,
    sensitivity = sensitivity,  # = recall = TPR
    specificity = specificity,  # = TNR
    f1          = f1,
    fpr         = fpr,          # tasso di falsi positivi = 1 - specificity
    fnr         = fnr           # tasso di falsi negativi = 1 - sensitivity
  )
  
  class(result) <- "classification_metrics"
  return(result)
}



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


# rinomina ----------------------------------------------------------------

id_risposta <- which(names(dati) == "y")
# solo per riciclare codice:
sss <- dati
#rm(dati)

# Salvataggio dati non standardizzati e matrici design originali ----------
sss_raw <- sss
m0_sum0_raw  <- lm(y~., data = sss_raw, contrasts = contrasts_list)
X_sum0_raw   <- model.matrix(m0_sum0_raw)[, -1]

# Standardizzazione -------------------------------------------------------

cat("Variabili quantitative standardizzate:\n")
print(id_num)

sss[, id_num] = scale(sss[, id_num])




# fold convalida incrociata -----------------------------------------------

ribilancia       <- TRUE   # FALSE = cv_id == cv_id_sb
classe_minoranza <- "y"    # nome della variabile risposta
valore_minoranza <- 0      # valore della classe rara
prop_minoranza   <- 0.5    # proporzione classe rara nel fold di stima (es. 0.5 = 50%)
n_fold           <- 4
seed_cv          <- 1

# FOLD DI VERIFICA — sbilanciati, distribuzione naturale
set.seed(seed_cv)
n_trunc  <- floor(NROW(sss) / n_fold) * n_fold
cv_id_sb <- matrix(sample(1:NROW(sss), n_trunc), ncol = n_fold)

# FOLD DI STIMA
if (!ribilancia) {
  cv_id <- cv_id_sb
  cat("Nessun ribilanciamento: cv_id == cv_id_sb\n")
} else {
  
  # Inizializziamo le variabili per bloccare la dimensione sul primo fold
  n_rari_target  <- NULL
  n_magg_target  <- NULL
  n_total_target <- NULL
  
  # Creeremo una matrice pulita, senza NA, perché tutti i fold avranno la stessa dimensione
  cv_id_list <- list() 
  
  set.seed(seed_cv)
  
  for (k in 1:n_fold) {
    id_tmp  <- cv_id_sb[, k]
    d_tmp   <- sss[id_tmp, ]
    
    # Identifica gli indici dei rari e dei maggioritari disponibili in QUESTO fold
    tutti_rari  <- id_tmp[d_tmp[[classe_minoranza]] == valore_minoranza]
    tutti_magg  <- id_tmp[d_tmp[[classe_minoranza]] != valore_minoranza]
    
    # --- FOLD 1: Definisce la dimensione ancora per tutti ---
    if (k == 1) {
      n_rari_target  <- length(tutti_rari)
      n_magg_target  <- round(n_rari_target * (1 - prop_minoranza) / prop_minoranza)
      n_total_target <- n_rari_target + n_magg_target
    }
    
    # --- ESTRAZIONE BILANCIATA A DIMENSIONE FISSA ---
    # Per i rari: se i rari disponibili sono meno del target, campiona con reinserimento (replace = TRUE)
    mm <- sample(tutti_rari, size = n_rari_target, replace = (length(tutti_rari) < n_rari_target))
    
    # Per i maggioritari: se i maggioritari sono meno del target, usa replace = TRUE
    vv <- sample(tutti_magg, size = n_magg_target, replace = (length(tutti_magg) < n_magg_target))
    
    # Combina e salva nel database del fold k
    cv_id_list[[k]] <- sample(c(mm, vv)) # sample aggiuntivo per mischiare rari e maggioritari
  }
  
  # Trasforma la lista in una matrice perfetta senza NA
  cv_id <- do.call(cbind, cv_id_list)
  
  cat(sprintf(
    "Fold di stima OMOOGENEI: tutti i fold hanno dimensione %d (%.0f%% minoritaria)\n",
    n_total_target, prop_minoranza * 100
  ))
  cat("Numerosità per fold (righe x colonne):\n")
  print(dim(cv_id))
}

# --- CHECK DEI RISULTATI ---
cat("\n--- CHECK DISTRIBUZIONE CLASSI ---\n")
for(k in 1:n_fold){
  cat(sprintf("\n[Fold %d] STIMA:\n", k))
  print(table(sss[[classe_minoranza]][cv_id[, k]]))
  cat(sprintf("[Fold %d] VERIFICA:\n", k))
  print(table(sss[[classe_minoranza]][cv_id_sb[, k]]))
}

# Pulizia
rm(list = intersect(c("mm", "vv", "id_tmp", "d_tmp", "k", "tutti_rari", "tutti_magg",
                      "n_rari_target", "n_magg_target", "n_total_target", "cv_id_list"), ls()))
gc()


### se hai delle osservazioni dipendenti e non vuoi mischiarle:
set.seed(seed_cv)
soggetti      <- unique(sss$soggetto_id)
n_sogg        <- length(soggetti)
n_trunc_sogg  <- floor(n_sogg / n_fold) * n_fold
soggetti_camp <- sample(soggetti, n_trunc_sogg)
sogg_fold     <- matrix(soggetti_camp, ncol = n_fold)
cv_id_sb <- apply(sogg_fold, 2, function(ids) which(sss$soggetto_id %in% ids))
cv_id <- cv_id_sb

# Dataframe previsioni in convalida incrociata ----------------------------
# Una riga per ogni osservazione in sss (indice originale).
# Ogni modello aggiunge una colonna con le sue previsioni out-of-fold.
# Le osservazioni non assegnate ad alcun fold di verifica restano NA.

cv_preds <- data.frame(
  id  = 1:nrow(sss),
  y   = sss$y
)






# modello lineare passo-passo ----------------------------------------------------------

pred_names <- setdiff(names(sss), "y")

scope <- list(
  lower = ~ 1,
  upper = reformulate(pred_names)
)

# lista per raccogliere le metriche di ogni fold
metrics_folds <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]                          # <-- sbilanciato
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]             # <-- rimuove NA (necessario se ribilancia=TRUE)
  
  train <- sss[id_train, ]
  test  <- sss[id_test, ]
  
  m_null_k <- lm(y ~ 1, data = train)
  m_step_k <- step(m_null_k, scope = scope, direction = "forward", trace = 0)
  
  probs <- predict(m_step_k, newdata = test)
  probs <- pmax(0, pmin(1, probs))
  
  # salva le previsioni out-of-fold
  if (!"lm_step" %in% names(cv_preds))
    cv_preds$lm_step <- NA_real_
  cv_preds$lm_step[id_test] <- probs
  
  metrics_folds[[k]] <- classification_metrics(probs, truth = test$y)
  cat(k)
}

# Media di ogni metrica sui fold
err_step <- list(
  accuracy    = mean(sapply(metrics_folds, function(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds, function(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds, function(m) m$specificity)),
  f1          = mean(sapply(metrics_folds, function(m) m$f1)),
  fpr         = mean(sapply(metrics_folds, function(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds, function(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - MODELLO LINEARE STEP-WISE  ",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_step$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_step$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_step$specificity),
  sprintf("  %-12s %.4f", "F1",          err_step$f1),
  sprintf("  %-12s %.4f", "FPR",         err_step$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_step$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# Modello finale su tutti i dati
m_null_final <- lm(y ~ 1, data = sss)
m_step <- step(m_null_final, scope = scope, direction = "forward", trace = 1)
summary(m_step)


# param a somma 0
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- lvls[-length(lvls)]
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)
# Refitta lo stesso modello con parametrizzazione a somma 0
formula_step <- formula(m_step)
m_step_sum0  <- lm(formula_step, data = sss, contrasts = ctr)
summary(m_step_sum0)










# modello logistico passo-passo ----------------------------------------------------------
pred_names <- setdiff(names(sss), "y")
scope <- list(
  lower = ~ 1,
  upper = reformulate(pred_names)
)

# lista per raccogliere le metriche di ogni fold
metrics_folds_logit <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]                          # <-- sbilanciato
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]             # <-- rimuove NA (necessario se ribilancia=TRUE)
  
  train <- sss[id_train, ]
  test  <- sss[id_test, ]
  
  m_null_k <- glm(factor(y) ~ 1, data = train, family = "binomial")
  m_step_k <- step(m_null_k, scope = scope, direction = "forward", trace = 0)
  
  probs <- predict(m_step_k, newdata = test, type = "response")
  
  # salva le previsioni out-of-fold
  if (!"logit_step" %in% names(cv_preds))
    cv_preds$logit_step <- NA_real_
  cv_preds$logit_step[id_test] <- probs
  
  metrics_folds_logit[[k]] <- classification_metrics(probs, truth = test$y)
  cat(k)
}

# Media di ogni metrica sui fold
err_step_logit <- list(
  accuracy    = mean(sapply(metrics_folds_logit, function(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_logit, function(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_logit, function(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_logit, function(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_logit, function(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_logit, function(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - MODELLO LOGISTICO STEP-WISE",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_step_logit$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_step_logit$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_step_logit$specificity),
  sprintf("  %-12s %.4f", "F1",          err_step_logit$f1),
  sprintf("  %-12s %.4f", "FPR",         err_step_logit$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_step_logit$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# Modello finale su tutti i dati
m_null_final <- glm(factor(y) ~ 1, data = sss, family = "binomial")
m_step_logit <- step(m_null_final, scope = scope, direction = "forward", trace = 1)
summary(m_step_logit)

# param a somma 0
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- lvls[-length(lvls)]
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)

# Refitta lo stesso modello con parametrizzazione a somma 0
formula_step <- formula(m_step_logit)
m_step_logit_sum0 <- glm(formula_step, data = sss, family = "binomial", contrasts = ctr)
summary(m_step_logit_sum0)








# RIDGE BINOMIALE ---------------------------------------------------------
library(glmnet)

# 1. Parametrizzazione a somma 0
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- lvls[-length(lvls)]
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)

x_full <- model.matrix(~ ., data = sss[, -id_risposta], contrasts.arg = ctr)[, -1]
y_full  <- sss$y  # deve essere 0/1 oppure factor a due livelli

# 2. Griglia lambda comune fissata sull'intero dataset
griglia_lambda <- glmnet(x_full, y_full, alpha = 0, family = "binomial",
                         lambda.min.ratio = 1e-8)$lambda
n_lambda       <- length(griglia_lambda)

# 3. CV loop esplicito (gestisce cv_id != cv_id_sb per dati sbilanciati)
# perf_mat raccoglie la metrica out-of-fold per ogni osservazione e ogni lambda
# metrica usata per scegliere lambda: accuracy
# -- alternativa: sostituire $accuracy con $f1 nelle due apply sotto --
perf_mat <- matrix(NA_real_, nrow = nrow(sss), ncol = n_lambda)

metrics_folds_ridge <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  x_train <- x_full[id_train, ];  y_train <- y_full[id_train]
  x_test  <- x_full[id_test,  ];  y_test  <- y_full[id_test]
  
  # ridge su tutti i lambda per questo fold
  fit_k <- glmnet(x_train, y_train, alpha = 0, family = "binomial",
                  lambda = griglia_lambda)
  
  # probabilità predette: matrice n_test x n_lambda
  probs_mat <- predict(fit_k, newx = x_test, s = griglia_lambda, type = "response")
  
  # ametrica per ogni lambda su questo fold
  perf_fold <- apply(probs_mat, 2, function(probs)
    classification_metrics(probs, truth = y_test)$accuracy)
  # -- alternativa F1 --
  # perf_fold <- apply(probs_mat, 2, function(probs)
  #   classification_metrics(probs, truth = y_test)$f1)
  
  perf_mat[id_test, ] <- matrix(rep(perf_fold, each = length(id_test)),
                                nrow = length(id_test))
  
  # lambda ottimo di questo fold
  probs_best <- probs_mat[, which.max(perf_fold)]
  
  # salva previsioni out-of-fold con il lambda ottimo del fold
  if (!"ridge_bin" %in% names(cv_preds))
    cv_preds$ridge_bin <- NA_real_
  cv_preds$ridge_bin[id_test] <- probs_best
  
  metrics_folds_ridge[[k]] <- classification_metrics(probs_best, truth = y_test)
  cat(k)
}

# 4. Lambda ottimo globale: massimizza la metrica media sui fold
perf_mean     <- colMeans(perf_mat, na.rm = TRUE)
lambda_ottimo <- griglia_lambda[which.max(perf_mean)]
cat("\nLambda ottimo:", lambda_ottimo, "\n")

# 5. Stima dell'errore di generalizzazione
err_ridge_bin <- list(
  accuracy    = mean(sapply(metrics_folds_ridge, \(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_ridge, \(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_ridge, \(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_ridge, \(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_ridge, \(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_ridge, \(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - RIDGE BINOMIALE",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_ridge_bin$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_ridge_bin$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_ridge_bin$specificity),
  sprintf("  %-12s %.4f", "F1",          err_ridge_bin$f1),
  sprintf("  %-12s %.4f", "FPR",         err_ridge_bin$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_ridge_bin$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# 6. Modello finale su tutti i dati con lambda ottimo
m_ridge_bin <- glmnet(x_full, y_full, alpha = 0, family = "binomial",
                      lambda = lambda_ottimo)
coef_final <- coef(m_ridge_bin, s = lambda_ottimo)
print(coef_final)





# LASSO BINOMIALE ---------------------------------------------------------

# x_full, y_full, ctr, griglia già definiti sopra (riusali)
# se vuoi una griglia separata per il lasso:
griglia_lambda_lasso <- glmnet(x_full, y_full, alpha = 1, family = "binomial",
                               lambda.min.ratio = 1e-8)$lambda

perf_mat_lasso <- matrix(NA_real_, nrow = nrow(sss), ncol = length(griglia_lambda_lasso))

metrics_folds_lasso <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  x_train <- x_full[id_train, ];  y_train <- y_full[id_train]
  x_test  <- x_full[id_test,  ];  y_test  <- y_full[id_test]
  
  fit_k <- glmnet(x_train, y_train, alpha = 1, family = "binomial",
                  lambda = griglia_lambda_lasso)
  
  probs_mat <- predict(fit_k, newx = x_test, s = griglia_lambda_lasso, type = "response")
  
  perf_fold <- apply(probs_mat, 2, function(probs)
    classification_metrics(probs, truth = y_test)$accuracy)
  # -- alternativa F1 --
  # perf_fold <- apply(probs_mat, 2, function(probs)
  #   classification_metrics(probs, truth = y_test)$f1)
  
  perf_mat_lasso[id_test, ] <- matrix(rep(perf_fold, each = length(id_test)),
                                      nrow = length(id_test))
  
  probs_best <- probs_mat[, which.max(perf_fold)]
  
  if (!"lasso_bin" %in% names(cv_preds))
    cv_preds$lasso_bin <- NA_real_
  cv_preds$lasso_bin[id_test] <- probs_best
  
  metrics_folds_lasso[[k]] <- classification_metrics(probs_best, truth = y_test)
  cat(k)
}

perf_mean_lasso   <- colMeans(perf_mat_lasso, na.rm = TRUE)
lambda_ottimo_lasso <- griglia_lambda_lasso[which.max(perf_mean_lasso)]
cat("\nLambda ottimo lasso:", lambda_ottimo_lasso, "\n")

err_lasso_bin <- list(
  accuracy    = mean(sapply(metrics_folds_lasso, \(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_lasso, \(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_lasso, \(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_lasso, \(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_lasso, \(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_lasso, \(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - LASSO BINOMIALE",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_lasso_bin$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_lasso_bin$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_lasso_bin$specificity),
  sprintf("  %-12s %.4f", "F1",          err_lasso_bin$f1),
  sprintf("  %-12s %.4f", "FPR",         err_lasso_bin$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_lasso_bin$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

m_lasso_bin <- glmnet(x_full, y_full, alpha = 1, family = "binomial",
                      lambda = lambda_ottimo_lasso)
coef_final_lasso <- coef(m_lasso_bin, s = lambda_ottimo_lasso)
print(coef_final_lasso)

df <- extract_lasso_coefs(m.lasso.cv)
plot_lasso_coefs(df[1:20,])












# LDA ---------------------------------------------------------------------
library(MASS)

metrics_folds_lda <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  train <- sss[id_train, ]
  test  <- sss[id_test,  ]
  
  m_lda_k <- lda(y ~ ., data = train)
  probs    <- predict(m_lda_k, newdata = test)$posterior[, 2]
  
  if (!"lda" %in% names(cv_preds))
    cv_preds$lda <- NA_real_
  cv_preds$lda[id_test] <- probs
  
  metrics_folds_lda[[k]] <- classification_metrics(probs, truth = test$y)
  cat(k)
}

err_lda <- list(
  accuracy    = mean(sapply(metrics_folds_lda, \(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_lda, \(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_lda, \(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_lda, \(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_lda, \(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_lda, \(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - LDA",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_lda$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_lda$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_lda$specificity),
  sprintf("  %-12s %.4f", "F1",          err_lda$f1),
  sprintf("  %-12s %.4f", "FPR",         err_lda$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_lda$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# Modello finale
m_lda <- lda(y ~ ., data = sss)


# QDA ---------------------------------------------------------------------
# occhio che la qda non stima se c'è perfetta collinearità
# POSSIBILE ERROR: rank deficiency in group ... -> una variabile non varia
# all'interno di una categoria della risposta, quindi Sigma non è stimabile

metrics_folds_qda <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  train <- sss[id_train, ]
  test  <- sss[id_test,  ]
  
  m_qda_k <- qda(y ~ ., data = train)
  probs    <- predict(m_qda_k, newdata = test)$posterior[, 2]
  
  if (!"qda" %in% names(cv_preds))
    cv_preds$qda <- NA_real_
  cv_preds$qda[id_test] <- probs
  
  metrics_folds_qda[[k]] <- classification_metrics(probs, truth = test$y)
  cat(k)
}

err_qda <- list(
  accuracy    = mean(sapply(metrics_folds_qda, \(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_qda, \(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_qda, \(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_qda, \(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_qda, \(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_qda, \(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - QDA",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_qda$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_qda$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_qda$specificity),
  sprintf("  %-12s %.4f", "F1",          err_qda$f1),
  sprintf("  %-12s %.4f", "FPR",         err_qda$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_qda$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# Modello finale
m_qda <- qda(y ~ ., data = sss)







# GAM LOGISTICO -----------------------------------------------------------
library(gam)

metrics_folds_gam <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  train <- sss[id_train, ]
  test  <- sss[id_test,  ]
  
  gam_null_k <- gam(y ~ 1, family = binomial, data = train)
  gam_scope_k <- gam.scope(train, response = id_risposta,
                           smoother = "s", arg = c("df=2", "df=3", "df=4"))
  gam_sel_k <- step.Gam(gam_null_k, gam_scope_k, trace = TRUE)
  
  probs <- predict(gam_sel_k, newdata = test, type = "response")
  
  if (!"gam_logit" %in% names(cv_preds))
    cv_preds$gam_logit <- NA_real_
  cv_preds$gam_logit[id_test] <- probs
  
  metrics_folds_gam[[k]] <- classification_metrics(probs, truth = test$y)
  cat(k)
}

err_gam_logit <- list(
  accuracy    = mean(sapply(metrics_folds_gam, \(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_gam, \(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_gam, \(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_gam, \(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_gam, \(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_gam, \(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - GAM LOGISTICO",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_gam_logit$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_gam_logit$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_gam_logit$specificity),
  sprintf("  %-12s %.4f", "F1",          err_gam_logit$f1),
  sprintf("  %-12s %.4f", "FPR",         err_gam_logit$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_gam_logit$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# Modello finale su tutti i dati -------------------------------------------
gam_null_final  <- gam(y ~ 1, family = binomial, data = sss)
gam_scope_final <- gam.scope(sss, response = id_risposta,
                             smoother = "s", arg = c("df=2", "df=3", "df=4"))
gam_logit       <- step.Gam(gam_null_final, gam_scope_final, trace = TRUE)
summary(gam_logit)

# Numero variabili selezionate
term_labels        <- attr(terms(gam_logit), "term.labels")
n_scelti_vars      <- length(term_labels)
n_disponibili_vars <- length(names(sss)[-id_risposta])
cat("Variabili scelte:", n_scelti_vars, "su", n_disponibili_vars, "disponibili\n")

# Riestimiamo con contrasti a somma 0 (per leggere i coefficienti) ---------
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- lvls[-length(lvls)]
  C
}
ctr          <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)
formula_gam  <- formula(gam_logit)
gam_logit_sum0 <- gam(formula_gam, data = sss, family = binomial, contrasts = ctr)
summary(gam_logit_sum0)

# Visualizzazione effetti smooth -------------------------------------------
smooth_terms <- term_labels[grepl("^s\\(", term_labels)]
par(mfrow = c(1, length(smooth_terms)))
plot(gam_logit_sum0, terms = smooth_terms, se = TRUE)
par(mfrow = c(1, 1))












# ALBERO DI CLASSIFICAZIONE -----------------------------------------------
library(tree)

# Dimensioni albero da esplorare (numero foglie)
dim_albero <- 2:10
n_dim      <- length(dim_albero)

# Modello pieno su tutti i dati (serve per la potatura finale)
m_tree_full <- tree(factor(y) ~ ., data = sss,
                    split = "deviance",
                    control = tree.control(nobs   = nrow(sss),
                                           minsize = 2,
                                           mindev  = 0.001))

plot(m_tree_full)
text(m_tree_full,
     pretty = 0,
     splits = TRUE,
     label  = "yprob",   # mostra le probabilità invece della classe
     cex    = 0.6)

# Array fold x metriche x dimensioni albero
metr_m <- array(NA, dim = c(ncol(cv_id), 6, n_dim),
                dimnames = list(fold      = NULL,
                                metriche  = c("accuracy","sensitivity",
                                              "specificity","f1","fpr","fnr"),
                                dim_albero = dim_albero))

metrics_folds_tree <- vector("list", ncol(cv_id))  # per il lambda ottimo scelto

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  train <- sss[id_train, ]
  test  <- sss[id_test,  ]
  
  m_tree_k <- tree(factor(y) ~ ., data = train,
                   split = "deviance",
                   control = tree.control(nobs    = nrow(train),
                                          minsize = 2,
                                          mindev  = 0.001))
  
  # tutti i livelli di potatura
  tree_list <- lapply(dim_albero, function(l) prune.tree(m_tree_k, best = l))
  pred_list <- lapply(tree_list,  function(t) predict(t, newdata = test)[, 2])
  metr_list <- lapply(pred_list,  function(p) classification_metrics(p, truth = test$y))
  
  # salva le 6 metriche per ogni dimensione
  metr_m[k, , ] <- sapply(metr_list, function(m)
    c(m$accuracy, m$sensitivity, m$specificity, m$f1, m$fpr, m$fnr))
  
  cat(k)
}

# Media sui fold per ogni metrica e dimensione
metr_albero <- apply(metr_m, c(2, 3), mean)   # 6 x n_dim

# Dimensione ottima: massimizza accuracy media
# -- alternativa F1: which.max(metr_albero["f1", ]) --
B <- which.max(metr_albero["accuracy", ])
cat("\nDimensione ottima albero:", dim_albero[B], "foglie\n")

# Curva accuracy al variare della dimensione
plot(dim_albero, metr_albero["accuracy", ], type = "l", lwd = 2,
     xlab = "Dimensione albero (foglie)", ylab = "Accuracy")
abline(v = dim_albero[B], col = 2, lwd = 2)

# Stima dell'errore di generalizzazione alla dimensione ottima
err_tree <- list(
  accuracy    = metr_albero["accuracy",    B],
  sensitivity = metr_albero["sensitivity", B],
  specificity = metr_albero["specificity", B],
  f1          = metr_albero["f1",          B],
  fpr         = metr_albero["fpr",         B],
  fnr         = metr_albero["fnr",         B]
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - ALBERO DI CLASSIFICAZIONE",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_tree$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_tree$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_tree$specificity),
  sprintf("  %-12s %.4f", "F1",          err_tree$f1),
  sprintf("  %-12s %.4f", "FPR",         err_tree$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_tree$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# Previsioni out-of-fold con la dimensione ottima --------------------------
# (ricicla le pred già calcolate dentro il loop)
# per averle in cv_preds rifai un loop leggero solo sulle pred
# serve solo per salvare le previsioni che poi mi sevono nella roc/lift
if (!"tree" %in% names(cv_preds))
  cv_preds$tree <- NA_real_

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  m_tree_k <- tree(factor(y) ~ ., data = sss[id_train, ],
                   split = "deviance",
                   control = tree.control(nobs    = length(id_train),
                                          minsize = 2,
                                          mindev  = 0.001))
  m_tree_pot <- prune.tree(m_tree_k, best = dim_albero[B])
  cv_preds$tree[id_test] <- predict(m_tree_pot, newdata = sss[id_test, ])[, 2]
}

# Modello finale su tutti i dati -------------------------------------------
m_tree_best <- prune.tree(m_tree_full, best = dim_albero[B])

plot(m_tree_best)
text(m_tree_best, pretty = 4, label = 1, digits = 1, cex = 0.5)

# Variabili usate nell'albero finale
tail(m_tree_best$frame$var)
m_tree_best$frame$splits






# mars --------------------------------------------------------------------

pred_names <- setdiff(names(sss), "y")
factor_cols <- which(names(sss[, -which(names(sss) == "y")]) %in% cat_vars)

metrics_folds_mars <- vector("list", ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  train <- sss[id_train, ]
  test  <- sss[id_test, ]
  
  m_mars_k <- polymars(train$y,
                       train[, pred_names],
                       factors = factor_cols,
                       gcv = 2)
  
  probs <- predict(m_mars_k, test[, pred_names])
  probs <- pmax(0, pmin(1, probs))
  
  if (!"mars" %in% names(cv_preds))
    cv_preds$mars <- NA_real_
  cv_preds$mars[id_test] <- probs
  
  metrics_folds_mars[[k]] <- classification_metrics(probs, truth = test$y)
  cat(k)
}

err_mars <- list(
  accuracy    = mean(sapply(metrics_folds_mars, function(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_mars, function(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_mars, function(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_mars, function(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_mars, function(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_mars, function(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "   CV METRICHE - MODELLO MARS",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_mars$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_mars$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_mars$specificity),
  sprintf("  %-12s %.4f", "F1",          err_mars$f1),
  sprintf("  %-12s %.4f", "FPR",         err_mars$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_mars$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

# Modello finale su TUTTI i dati
m_mars_final <- polymars(sss$y,
                         sss[, pred_names],
                         factors = factor_cols)

plot(m_mars_final$fitting$size, m_mars_final$fitting$GCV,
     col = (m_mars_final$fitting[, 1] + 1), pch = 16,
     ylab = "GCV", xlab = "Numero di basi")
legend("bottomright", col = c(1, 2), c("Crescita", "Potatura"), pch = 16)
abline(v = m_mars_final$fitting$size[which.min(m_mars_final$fitting$GCV)],
       col = "gold", lty = 2, lwd = 2)

m_mars_final$model  # basi selezionate e coefficienti


# Summary del modello finale
summary(m_mars_final)
m_mars_final$model  # basi selezionate e coefficienti

pred_names_mars <- names(sss[, -id_risposta])

# 2 predittori vs risposta
plot(m_mars_final, predictor1 = 4, predictor2 = 5, phi = 10, theta = 60)
mtext(paste("predictor1 =", pred_names_mars[4],
            "| predictor2 =", pred_names_mars[5]),
      side = 1, line = 3, cex = 0.9)

# 1 predittore vs risposta
plot(m_mars_final, predictor1 = 59)
title(sub = pred_names_mars[4], cex.sub = 1, font.sub = 2)











# RANDOM FOREST -----------------------------------------------------------
library(randomForest)

# Preparazione della griglia dei parametri (mtry)
nvar <- c(10,30,40,60,80)
n_mtry <- length(nvar)
n_trees <- 50 # Definito fuori per comodità

# Matrice per accumulare l'errore per albero e per mtry sui fold
err_prog_cv <- matrix(0, nrow = n_trees, ncol = n_mtry)
colnames(err_prog_cv) <- paste0("mtry_", nvar)

# CV loop esplicito 
perf_mat_rf <- matrix(NA_real_, nrow = nrow(sss), ncol = n_mtry)
metrics_folds_rf <- vector("list", ncol(cv_id))


set.seed(1)
for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id_sb[, k]
  id_train <- as.vector(cv_id[, -k])
  id_train <- id_train[!is.na(id_train)]
  
  perf_fold <- numeric(n_mtry)
  probs_fold_list <- matrix(NA_real_, nrow = length(id_test), ncol = n_mtry)
  
  for (m in seq_along(nvar)) {
    fit_k <- randomForest(
      x        = sss[id_train, -id_risposta],
      y        = factor(sss$y[id_train]),
      xtest    = sss[id_test, -id_risposta],
      ytest    = factor(sss$y[id_test]),
      ntree    = n_trees,
      mtry     = nvar[m],
      nodesize = 10,
      do.trace = FALSE
    )
    
    # fit_k$test$err.rate[, 1] contiene il misclassification error sul test set
    err_prog_cv[, m] <- err_prog_cv[, m] + fit_k$test$err.rate[, 1]
    
    # Estraiamo le probabilità predette per la classe "1" sul test set (al 50esimo albero)
    probs_mat_m <- fit_k$test$votes[, "1"]
    probs_fold_list[, m] <- probs_mat_m
    
    # Calcolo metrica finale per questo specifico mtry
    perf_fold[m] <- classification_metrics(probs_mat_m, truth = sss$y[id_test])$accuracy
  }
  
  perf_mat_rf[id_test, ] <- matrix(rep(perf_fold, each = length(id_test)),
                                   nrow = length(id_test))
  
  best_m_idx <- which.max(perf_fold)
  probs_best <- probs_fold_list[, best_m_idx]
  
  if (!"rf" %in% names(cv_preds)) cv_preds$rf <- NA_real_
  cv_preds$rf[id_test] <- probs_best
  metrics_folds_rf[[k]] <- classification_metrics(probs_best, truth = sss$y[id_test])
  
  cat(k, " ")
}

# Mtry ottimo globale e Plot 
perf_mean_rf  <- colMeans(perf_mat_rf, na.rm = TRUE)
mtry_ottimo   <- nvar[which.max(perf_mean_rf)]
cat("\nMtry ottimo:", mtry_ottimo, "\n")

# Calcolo medie e PLOT 
# Dividiamo la somma degli errori per il numero di fold per ottenere la media
err_prog_cv <- err_prog_cv / ncol(cv_id)

# Trasformiamo l'Errore in Accuracy (se preferisci graficare l'errore, salta questo passaggio)
acc_prog_cv <- 1 - err_prog_cv

# Plot unico usando matplot (base R)
matplot(1:n_trees, acc_prog_cv, type = "l", lty = 1, lwd = 2, col = 1:n_mtry,
        xlab = "Numero di Alberi (ntree)", 
        ylab = "Accuracy OOF (Out-Of-Fold)",
        main = "Andamento dell'Accuracy allo scorrere degli alberi per mtry")
legend("bottomright", legend = paste("mtry =", nvar), col = 1:n_mtry, lty = 1, lwd = 2)
# -----------------------------------
# Stima dell'errore di generalizzazione
err_rf_bin <- list(
  accuracy    = mean(sapply(metrics_folds_rf, \(m) m$accuracy)),
  sensitivity = mean(sapply(metrics_folds_rf, \(m) m$sensitivity)),
  specificity = mean(sapply(metrics_folds_rf, \(m) m$specificity)),
  f1          = mean(sapply(metrics_folds_rf, \(m) m$f1)),
  fpr         = mean(sapply(metrics_folds_rf, \(m) m$fpr)),
  fnr         = mean(sapply(metrics_folds_rf, \(m) m$fnr))
)

cat(paste(c(
  "",
  "============================================",
  "     CV METRICHE - RANDOM FOREST",
  "============================================",
  sprintf("  %-12s %.4f", "Accuracy",    err_rf_bin$accuracy),
  sprintf("  %-12s %.4f", "Sensitivity", err_rf_bin$sensitivity),
  sprintf("  %-12s %.4f", "Specificity", err_rf_bin$specificity),
  sprintf("  %-12s %.4f", "F1",          err_rf_bin$f1),
  sprintf("  %-12s %.4f", "FPR",         err_rf_bin$fpr),
  sprintf("  %-12s %.4f", "FNR",         err_rf_bin$fnr),
  "============================================",
  ""
), collapse = "\n"), "\n")

set.seed(1)
m_rf_finale <- randomForest(
  factor(y) ~ .,
  data     = sss,
  ntree    = 50,
  mtry     = mtry_ottimo,
  nodesize = 60,
  importance = TRUE
)




# Plotta automaticamente le metriche di importanza
varImpPlot(m_rf_finale, 
           main = "Importanza delle Variabili - Modello Finale",
           pch = 16, # Cambia il pallino per renderlo più visibile
           col = "navy")








# ==============================================================================
# CONFRONTO FINALE ADATTATO AL DATASET DI CLASSIFICAZIONE
# ==============================================================================

sort_by    <- "f1"
decreasing <- TRUE

# 1. Tabella dei Risultati (Metriche Medie dai Fold) ---------------------------
modelli_list <- list(
  "Stepwise LM"     = if (exists("err_step"))       err_step,
  "Stepwise Logit"  = if (exists("err_step_logit")) err_step_logit,
  "Ridge Logit"     = if (exists("err_ridge_bin"))  err_ridge_bin,
  "Lasso Logit"     = if (exists("err_lasso_bin"))  err_lasso_bin,
  "LDA"             = if (exists("err_lda"))        err_lda,
  "QDA"             = if (exists("err_qda"))        err_qda,
  "GAM Logit"       = if (exists("err_gam_logit"))  err_gam_logit,
  "MARS"            = if (exists("err_mars"))       err_mars,
  "Albero"          = if (exists("err_tree"))       err_tree,
  "Random Forest"   = if (exists("err_rf_bin"))     err_rf_bin
)

# Rimuove le voci NULL (modelli non stimati) e convalida la classe
modelli_list <- Filter(
  \(x) !is.null(x) && inherits(x, "list"),
  modelli_list
)

risultati <- do.call(rbind, lapply(names(modelli_list), function(nm) {
  m <- modelli_list[[nm]]
  data.frame(
    modello     = nm,
    soglia      = if(!is.null(m$threshold)) m$threshold else 0.5, # soglia di default dello script
    accuracy    = m$accuracy,
    sensitivity = m$sensitivity,
    specificity = m$specificity,
    f1          = m$f1,
    stringsAsFactors = FALSE
  )
})) |>
  arrange(if (decreasing) desc(.data[[sort_by]]) else .data[[sort_by]])

cat(sprintf("\nModelli ordinati per %s (%s)\n\n",
            sort_by, ifelse(decreasing, "↓ decrescente", "↑ crescente")))
print(as.data.frame(risultati), row.names = FALSE, digits = 4)


# 2. Preparazione Grafici ROC/Lift dalle predizioni out-of-fold ----------------

safe_probs <- function(v) {
  if (is.null(v) || !is.numeric(v) || length(v) == 0) return(NULL)
  # clip: assicura che nessuna probabilità esca da [0,1]
  pmax(0, pmin(1, as.vector(v)))
}

# Estraiamo in modo sicuro le colonne esistenti dentro cv_preds
modelli_probs <- list()
if ("lm_step" %in% names(cv_preds))   modelli_probs[["Stepwise LM"]]     <- safe_probs(cv_preds$lm_step)
if ("logit_step" %in% names(cv_preds)) modelli_probs[["Stepwise Logit"]]  <- safe_probs(cv_preds$logit_step)
if ("ridge_bin" %in% names(cv_preds))  modelli_probs[["Ridge Logit"]]     <- safe_probs(cv_preds$ridge_bin)
if ("lasso_bin" %in% names(cv_preds))  modelli_probs[["Lasso Logit"]]     <- safe_probs(cv_preds$lasso_bin)
if ("lda" %in% names(cv_preds))        modelli_probs[["LDA"]]             <- safe_probs(cv_preds$lda)
if ("qda" %in% names(cv_preds))        modelli_probs[["QDA"]]             <- safe_probs(cv_preds$qda)
if ("gam_logit" %in% names(cv_preds))  modelli_probs[["GAM Logit"]]       <- safe_probs(cv_preds$gam_logit)
if ("mars" %in% names(cv_preds))       modelli_probs[["MARS"]]            <- safe_probs(cv_preds$mars)
if ("tree" %in% names(cv_preds))       modelli_probs[["Albero"]]          <- safe_probs(cv_preds$tree)
if ("rf" %in% names(cv_preds))         modelli_probs[["Random Forest"]]   <- safe_probs(cv_preds$rf)

# Rimuove eventuali modelli non estratti correttamente o con NA totali
modelli_probs <- Filter(function(p) !is.null(p) && !all(is.na(p)), modelli_probs)

cat("\nModelli disponibili per i grafici:", paste(names(modelli_probs), collapse = ", "), "\n")
if (length(modelli_probs) == 0) stop("Nessuna previsione valida in cv_preds per generare i plot.")

# Nel tuo script la variabile target reale out-of-fold è memorizzata qui:
truth <- cv_preds$y

# Funzioni di calcolo (ROC e Lift)
compute_roc <- function(probs, truth) {
  # Rimuove eventuali NA congiunti per evitare disallineamenti nel plot
  valid_idx <- !is.na(probs) & !is.na(truth)
  df    <- data.frame(p = probs[valid_idx], y = as.integer(truth[valid_idx]))
  df    <- df[order(df$p, decreasing = TRUE), ]
  n_pos <- sum(df$y == 1); n_neg <- sum(df$y == 0)
  if(n_pos == 0 || n_neg == 0) return(list(fpr = c(0,1), tpr = c(0,1), auc = NA))
  tpr   <- cumsum(df$y == 1) / n_pos
  fpr   <- cumsum(df$y == 0) / n_neg
  auc   <- sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
  list(fpr = c(0, fpr, 1), tpr = c(0, tpr, 1), auc = auc)
}

compute_lift <- function(probs, truth) {
  valid_idx <- !is.na(probs) & !is.na(truth)
  df   <- data.frame(p = probs[valid_idx], y = as.integer(truth[valid_idx]))
  df   <- df[order(df$p, decreasing = TRUE), ]
  n    <- nrow(df)
  rate <- sum(df$y == 1) / n
  pct  <- (1:n) / n
  lift <- (cumsum(df$y == 1) / (1:n)) / rate
  list(pct = c(0, pct), lift = c(lift[1], lift))
}

# Configurazione Grafica
pal <- setNames(
  rainbow(length(modelli_probs), s = .85, v = .8),
  names(modelli_probs)
)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# ── PLOT 1: Curva ROC ─────────────────────────────────────────────────────────
plot(c(0,1), c(0,1), type = "n", asp = 1, las = 1,
     xlab = "FPR (1 - specificità)",
     ylab = "TPR (sensibilità)", main = "Curva ROC (Out-of-Fold)")
abline(0, 1, lty = 2, col = "grey70")

auc_vals <- sapply(modelli_probs, function(p) compute_roc(p, truth)$auc)

for (nm in names(modelli_probs)) {
  roc <- compute_roc(modelli_probs[[nm]], truth)
  lines(roc$fpr, roc$tpr, col = pal[nm], lwd = 1.8)
}

ord <- order(auc_vals, decreasing = TRUE, na.last = TRUE)
legend("bottomright", bty = "n", cex = .65,
       title = sprintf("%-20s %s", "Modello", "AUC"), title.font = 2,
       legend = sprintf("%-20s %.3f", names(auc_vals)[ord], auc_vals[ord]),
       col = pal[ord], lwd = 1.8)

# ── PLOT 2: Curva Lift ────────────────────────────────────────────────────────
all_lifts <- lapply(modelli_probs, function(p) compute_lift(p, truth))
y_max     <- max(sapply(all_lifts, function(l) max(l$lift, na.rm = TRUE)), na.rm = TRUE)

plot(c(0,1), c(0, ceiling(y_max)), type = "n", las = 1,
     xlab = "Frazione campione (% contattato)",
     ylab = "Lift", main = "Curva Lift (Out-of-Fold)")
abline(h = 1, lty = 2, col = "grey70")

for (nm in names(modelli_probs)) {
  lft <- all_lifts[[nm]]
  lines(lft$pct, lft$lift, col = pal[nm], lwd = 1.8)
}

legend("topright", bty = "n", cex = .65,
       legend = names(modelli_probs), col = pal, lwd = 1.8)

par(mfrow = c(1, 1))
