source("utils.R")

library(tidyverse)

# per evitare errori successivi:

dati <- dati %>%
  mutate(across(where(is.character), factor))

# CODIFICA EFFETTIVAMENTE COME FACTOR LE ESPLICATIVE QUALITATIVE
# ANCHE SE SONO 0/1, IN QUESTO MODO VA TUTTO FLUIDO DOPO E NON FAI ERRORI
# COME LISCIARE UN FACTOR

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


# Variabile risposta ------------------------------------------------------

dati <- dati %>%
  rename("y" = "risposta_nome")

#### y deve eddere numerica


# tienila numerica, poi la rendi factor quando serve

# variabili numeriche e factor --------------------------------------------
id_risposta <- which(names(dati) == "y")
id_num <- setdiff(which(sapply(dati, function(x) is.numeric(x) | is.integer(x))), id_risposta)
id_factor <- which(sapply(dati, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(dati)[id_risposta]]

# Errore ------------------------------------------------------------------

# ==============================================================================
# classification_metrics()
#
# Calcola le metriche di classificazione binaria a partire da probabilità
# predette da un modello, etichette reali e una soglia di decisione.
#
# Parametri:
#   probs     - vettore numerico di probabilità predette (valori in [0, 1])
#               tipicamente l'output di predict(..., type="response") o
#               equivalenti in altri framework
#   truth     - vettore di valori reali (0/1 oppure FALSE/TRUE)
#               dove 1/TRUE = caso positivo (la classe "di interesse")
#   threshold - soglia di classificazione (default: 0.5)
#               un'osservazione è classificata come positiva se prob >= threshold
#
# Metriche restituite:
#   accuracy, sensitivity, specificity, f1, fpr, fnr
# ==============================================================================

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

# Metodo print per output leggibile
print.classification_metrics <- function(x, ...) {
  cat("══════════════════════════════════════════\n")
  cat(sprintf(" Metriche di Classificazione  (soglia = %.2f)\n", x$threshold))
  cat("══════════════════════════════════════════\n")
  cat(sprintf(" Osservazioni nell'insieme di verifica : %d\n", x$n))

  cat("\n── Confusion Matrix ──────────────────────\n")
  cat(sprintf("  TP = %4d   FP = %4d\n", x$confusion_matrix$TP, x$confusion_matrix$FP))
  cat(sprintf("  FN = %4d   TN = %4d\n", x$confusion_matrix$FN, x$confusion_matrix$TN))

  fmt <- function(v) if (is.na(v)) "     NA" else sprintf("%7.4f", v)
  cat("\n── Metriche ──────────────────────────────\n")
  cat(sprintf("  Accuracy        : %s\n", fmt(x$accuracy)))
  cat(sprintf("  Sensitivity/TPR : %s\n", fmt(x$sensitivity)))
  cat(sprintf("  Specificity/TNR : %s\n", fmt(x$specificity)))
  cat(sprintf("  F1 Score        : %s\n", fmt(x$f1)))
  cat(sprintf("  FPR             : %s  (= 1 - specificity)\n", fmt(x$fpr)))
  cat(sprintf("  FNR             : %s  (= 1 - sensitivity)\n", fmt(x$fnr)))
  cat("══════════════════════════════════════════\n")
  invisible(x)
}


# Stima/verifica ----------------------------------------------------------

id_risposta <- which(names(dati) == "y")
set.seed(123)
acaso <- sample(1:nrow(dati),0.75*NROW(dati))
sss <- dati[acaso,]
vvv <- dati[-acaso,]
# eventualmente se pesa troppo:
rm(dati)
gc()


### se sbilanciato:
set.seed(123)
prop_minoranza   <- 0.3
classe_minoranza <- "y"
valore_minoranza <- 1
acaso <- sample(1:nrow(dati), 0.75 * NROW(dati))
# Verifica: distribuzione naturale
vvv <- dati[-acaso, ]
# Stima: tutti i rari + maggioritaria per avere prop_minoranza
mm      <- acaso[dati[[classe_minoranza]][acaso] == valore_minoranza]
n_altri <- round(length(mm) * (1 - prop_minoranza) / prop_minoranza)
mm_altri <- sample(acaso[dati[[classe_minoranza]][acaso] != valore_minoranza], size = n_altri)
sss     <- dati[c(mm, mm_altri), ]
cat("Distribuzione stima:\n")
print(table(sss[[classe_minoranza]]))
cat("Distribuzione verifica:\n")
print(table(vvv[[classe_minoranza]]))

# Salvataggio dati non standardizzati e matrici design originali ----------
sss_raw <- sss
vvv_raw <- vvv
# Matrici design non standardizzate
m0_sum0_raw  <- lm(y~., data = sss_raw, contrasts = contrasts_list)
X_sum0_raw   <- model.matrix(m0_sum0_raw)[, -1]
XXv_sum0_raw <- model.matrix(
  update(formula(m0_sum0_raw), NULL ~ . - y),
  data = vvv_raw,
  contrasts.arg = contrasts_list
)[, -1]

# Standardizzazione -------------------------------------------------------

cat("Variabili quantitative standardizzate:\n")
print(id_num)

# trasformiamo il set di convalida con media e s.d. empiriche su quello di stima
params_scale = lapply(sss[, id_num], function(x) list(mean = mean(x), sd = sd(x)))

sss[, id_num] = scale(sss[, id_num])

vvv[, id_num] = mapply(function(col, par) (col - par$mean) / par$sd,
                       vvv[, id_num],
                       params_scale,
                       SIMPLIFY = FALSE) |> as.data.frame()

print_table(table(sss$y))
table(vvv$y)


# modalita in sss e vvv ---------------------------------------------------

check <- check_factor_levels(sss,vvv)
check$all_factors_match   # TRUE allora va bene
check$details             # dettagli per colonna

# Passo passo - modello lineare -------------------------------------------------------------

nomi <- names(sss)
scope <- as.formula(paste("~ ", paste(nomi[-id_risposta], collapse=" + ")))
# modello nullo
m_null = lm(y~1,sss)
# regressione passo-a-passo
m_step = step(m_null, scope = scope, direction = "forward", trace = 1)
summary(m_step)
p.step = predict(m_step, vvv)
(err_step <- classification_metrics(p.step, vvv$y))


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


# Sottocampionamento ------------------------------------------------------

# se alcuni modelli ci mettono troppo puoi sottocampionare, gia che ci sei
# fallo bilanciando le classi

set.seed(1)
idx = balanced_subsample(sss, p = 0.1, id_risposta = id_risposta)
sss_small = sss[idx, ]

table(sss$y)
table(sss_small$y)

# verifica bilanciamento
prop.table(table(sss$y))
prop.table(table(sss_small$y))

### USA UN THRESHOLD DIVERSO PER I MODELLI CHE USANO IL SOTTOCAMPIONE balanced!!!!

# oppure piu easy unbalanced:

set.seed(1)
idx = subsample(sss, p = 0.1, id_risposta = id_risposta)
sss_small = sss[idx, ]

table(sss$y)
table(sss_small$y)

prop.table(table(sss$y))
prop.table(table(sss_small$y))


# Passo passo - modello logit -------------------------------------------------------------

nomi <- names(sss)
scope <- as.formula(paste("~ ", paste(nomi[-id_risposta], collapse=" + ")))
# modello nullo
m_null = glm(factor(y)~1,sss,family="binomial")
# regressione passo-a-passo
m_step_logit = step(m_null, scope = scope, direction = "forward", trace = 1)
summary(m_step_logit)
p.step_logit = predict(m_step_logit, vvv,type="response")
(err_step_logit <- classification_metrics(p.step_logit, vvv$y))


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
m_step_logit_sum0  <- lm(formula_step, data = sss, contrasts = ctr)
summary(m_step_logit_sum0)


# Ridge - modello lineare -------------------------------------------------------------------
library(glmnet)
# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])  # o paste0(v, "_", lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(y~., data=sss,contrasts.arg = ctr)
x.vvv <- model.matrix(y~., data=vvv,contrasts.arg = ctr)

# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.ridge.cv=cv.glmnet(x[,-1], sss$y, alpha=0,lambda.min.ratio=1e-10) #togliamo intercetta dalla matrice del disegno
coef(m.ridge.cv, s = "lambda.min")
plot(m.ridge.cv)
lambda.ottimo = m.ridge.cv$lambda.min
pred.ridge.cv = predict(m.ridge.cv, newx=x.vvv[,-1],s=lambda.ottimo)
err_ridge <- classification_metrics(pred.ridge.cv,vvv$y)
print.classification_metrics(err_ridge)


# Ridge - modello logit -------------------------------------------------------------------
library(glmnet)
# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])  # o paste0(v, "_", lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(factor(y)~., data=sss,contrasts.arg = ctr)
x.vvv <- model.matrix(factor(y)~., data=vvv,contrasts.arg = ctr)

# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.ridge_logit.cv=cv.glmnet(x[,-1], factor(sss$y),
                           alpha=0,lambda.min.ratio=1e-10,
                           family="binomial",
                           trace.it = TRUE) #togliamo intercetta dalla matrice del disegno
plot(m.ridge_logit.cv)
lambda.ottimo = m.ridge_logit.cv$lambda.min
pred.ridge_logit.cv = predict(m.ridge_logit.cv, newx=x.vvv[,-1],s=lambda.ottimo,
                              type="response")
err_ridge_logit <- classification_metrics(pred.ridge_logit.cv,vvv$y)
print.classification_metrics(err_ridge_logit)


# LASSO - modello lineare ----------------------------------------------------

library(glmnet)
# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])  # o paste0(v, "_", lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(y~., data=sss,contrasts.arg = ctr)
x.vvv <- model.matrix(y~., data=vvv,contrasts.arg = ctr)

# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.lasso.cv=cv.glmnet(x[,-1], sss$y, alpha=1,
                     lambda.min.ratio=1e-10,
                     trace.it = TRUE,
                     nfolds = 5) #togliamo intercetta dalla matrice del disegno
plot(m.lasso.cv)
lambda.ottimo = m.lasso.cv$lambda.min
pred.lasso.cv = predict(m.lasso.cv, newx=x.vvv[,-1],s=lambda.ottimo)
err_lasso <- classification_metrics(pred.lasso.cv,vvv$y)
print.classification_metrics(err_lasso)

df <- extract_lasso_coefs(m.lasso.cv)
plot_lasso_coefs(df[1:20,])


# LASSO - modello logit ---------------------------------------------------

library(glmnet)
# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])  # o paste0(v, "_", lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(factor(y)~., data=sss,contrasts.arg = ctr)
x.vvv <- model.matrix(factor(y)~., data=vvv,contrasts.arg = ctr)

# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.lasso_logit.cv=cv.glmnet(x[,-1], factor(sss$y),
                           alpha=1,lambda.min.ratio=1e-10,
                           family="binomial",
                           trace.it = TRUE,
                           nfolds = 5) #togliamo intercetta dalla matrice del disegno
plot(m.lasso_logit.cv)
lambda.ottimo = m.lasso_logit.cv$lambda.min
pred.lasso_logit.cv = predict(m.lasso_logit.cv, newx=x.vvv[,-1],s=lambda.ottimo,
                              type="response")
err_lasso_logit <- classification_metrics(pred.lasso_logit.cv,vvv$y)
print.classification_metrics(err_lasso_logit)


# LASSO - modello lineare con interazioni ----------------------------------------------------

library(glmnet)
# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])  # o paste0(v, "_", lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(y~(.)^2, data=sss,contrasts.arg = ctr)
x.vvv <- model.matrix(y~(.)^2, data=vvv,contrasts.arg = ctr)

# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.lasso_int.cv=cv.glmnet(x[,-1], sss$y, alpha=1,
                     lambda.min.ratio=1e-10,
                     trace.it = TRUE,
                     nfolds = 5) #togliamo intercetta dalla matrice del disegno
plot(m.lasso_int.cv)
lambda.ottimo = m.lasso_int.cv$lambda.min
pred.lasso_int.cv = predict(m.lasso_int.cv, newx=x.vvv[,-1],s=lambda.ottimo)
err_lasso_int <- classification_metrics(pred.lasso_int.cv,vvv$y)
print.classification_metrics(err_lasso_int)

df <- extract_lasso_coefs(m.lasso_int.cv)
plot_lasso_coefs(df[1:20,])


# LDA ---------------------------------------------------------------------
### Se evenualmente vuoi stimare modelli successivi su un sottoinsieme
# intelligente di variabili, come quelle selzezionate dal lasso
# Costruisci subset con y inclusa esplicitamente
#subset_var <- names(sss)[names(sss) %in% df$variabile]
#subset_var <- c(subset_var, names(id_factor))
#subset_var <- unique(c("y", subset_var))  # assicura y ci sia
#sss_sub <- sss[, subset_var]
# Nuovo id_risposta relativo a sss_sub
#id_risposta_sub <- which(names(sss_sub) == "y")
#m_lda <- lda(y ~ ., data = sss_sub)


library(MASS)
m_lda = lda(y~.,sss)
pr_lda = predict(m_lda, vvv)
(err_lda = classification_metrics(pr_lda$posterior[,2], vvv$y))


# QDA ---------------------------------------------------------------------

# occhio che la qda non stima se c'è perfetta collinearità
m_qda = qda(y~., sss)
pr_qda = predict(m_qda, vvv)
(err_qda = classification_metrics(pr_qda$posterior[,2], vvv$y))
# POSSIBILE ERROR: se rank deficiency in group ... allora una variabile non varia
# all'interno di una categoria della risposta, quindi non stimabile Sigma.


# GAM logistico -----------------------------------------------------------

library(gam)
# Formula con smooth su tutti i predittori
fg1 = as.formula(paste("y~s(",
                       paste(names(sss[-id_risposta]), collapse = ")+s("),
                       ")"))
# Modello nullo
gam0 = gam(y ~ 1, family = binomial, data = sss)
# Scope per selezione stepwise
sc = gam.scope(sss, response = id_risposta,
               smoother = "s", arg = c("df=2", "df=3", "df=4"))
# Selezione stepwise (local-scoring)
gam1 = step.Gam(gam0, scope = sc, trace = TRUE)
# Summary e grafici degli smooth stimati
summary(gam1)
p = length(names(sss[-id_risposta]))
par(mfrow = c(ceiling(p/3), 3))
plot(gam1, se = TRUE)
par(mfrow = c(1, 1))
pred.gam1 = predict(gam1, newdata = vvv, type = "response")
err_gam1 = classification_metrics(pred.gam1, vvv$y)
print.classification_metrics(err_gam1)

#Visualizzazione effetti:
par(mfrow = c(1,2))
#metti le variabili al posto di x1 e x2:
plot(gam1,terms = c("s(x1, df = 5)", "s(x2, df = 3)"),se=T)


# errori
pr_gam = predict(gam1, vvv,type = "response")
(err_gam = classification_metrics(pr_gam, vvv$y))


# MARS --------------------------------------------------------------------

## Modello: MARS
library(polspline)
# utilizza X e y
set.seed(9)
cb1 = sample(1:NROW(sss), 0.5*NROW(sss))
sss$y[cb1][1]
cb2 = setdiff(1:NROW(sss), cb1)
x.sss = model.matrix(y~-1+.,sss[cb1,])
x.ccc = model.matrix(y~-1+.,sss[cb2,])
# classify = T ci permette, nella fase di potatura, di tagliare
# utilizzando errore di classificazione
m_mars = polymars(response = factor(sss$y[cb1]),
                  predictors = x.sss,
                  factors = id_factor,
                  ts.resp = factor(sss$y[cb2]),
                  ts.pred = x.ccc, classify = T)
sss$y[cb1][1]
# Polymars in automatico converte la risposta prendendo come prima modalità
# il primo valore che viene osservato

# due insiemi di coefficienti (uno per ogni categoria)
# L'ordine dei coefficienti e' stabilito dall'ordine detto sopra
## Interazioni identificate
m_mars$model
int = which(m_mars$model[,3] != 0)
m_mars$model[int,c(1,3)]
v0 = m_mars$model[int,"Coefs 1"]
v1 = colnames(x.sss)[m_mars$model[int,1]]
v2 = colnames(x.sss)[m_mars$model[int,3]]
data.frame(cbind(round(v0,3),v1,v2)) # Effetti di interazione e coefficienti associati
tail(m_mars$model[int,])
plot(m_mars,predictor1 = 6, predictor2 = 30)

X_verifica = model.matrix(y~-1+.,vvv)
# Le previsioni del mars non sono delle probabilità ma delle misure di propensione
# (di fatto, stima un modello lineare)
pr_mars = predict(m_mars,X_verifica)
head(pr_mars)
# Prendo la prima colonna, cioè le previsioni associate alla modalità corrispondente
# al primo valore osservato, cioè un 1
(err_mars = classification_metrics(pr_mars[,1], vvv$y))


# ALBERO ------------------------------------------------------------------

library(rpart)
library(rpart.plot)

# Pesi inversi alla frequenza
w <- ifelse(sss$y == 1,
            sum(sss$y == 0) / sum(sss$y == 1),
            1)
m_tree <- rpart(
  factor(y) ~ .,
  data    = sss,
  #weights = w,
  method  = "class",
  control = rpart.control(
    cp      = 0.0001,   # complessità minima split
    minsplit = 10,     # min osservazioni per splittare
    maxdepth = 10      # profondità massima
  )
)

rpart.plot(m_tree)

printcp(m_tree)

# Converti: nodi terminali = nsplit + 1
cptable <- as.data.frame(m_tree$cptable)
cptable$nodes <- cptable$nsplit + 1

# Plot xerror vs numero di nodi
plot(cptable$nodes, cptable$xerror,
     type = "b", pch = 19,
     xlab = "Numero di nodi terminali",
     ylab = "CV Error (xerror)",
     main = "Scelta del numero di nodi")
abline(v = 40,
       col = "red", lty = 2)

# Scegli il cp corrispondente al numero di nodi desiderato
best_nodes <- 40  # <-- cambia questo
best_cp <- cptable$CP[cptable$nodes == best_nodes]

m_pruned <- prune(m_tree, cp = best_cp)
rpart.plot(m_pruned, type = 4, extra = 104)

pr_tree <- predict(m_pruned, vvv, type = "prob")

# Prendi la colonna della classe positiva (es. "1")
(err_tree = classification_metrics(pr_tree[,"1"], vvv$y,threshold=0.5))


# boosting ----------------------------------------------------------------

# y factor
# Possiamo misurare l'errore sul dataset di convalida per capire se si stablizzi
library(ada)

set.seed(1)
cb1 = sample(1:NROW(sss), 0.5*NROW(sss))
cb2 = setdiff(1:NROW(sss), cb1)


m_boost = ada(x = sss[cb1,-id_risposta],
              y = factor(sss$y[cb1]),
              test.x = sss[cb2,-id_risposta],
              test.y = factor(sss$y[cb2]),iter=50,verbose=TRUE)
# errore sulla stima
plot(m_boost)
# errore di convalida
plot(m_boost,test = T)

# Se devi aggiungere alberi per sprecare meno tempo:
#m_boost = update(m_boost,x = sss[cb1,-id_risposta],
#               y = sss$y[cb1],
#               test.x = sss[cb2,-id_risposta],
#               test.y = sss$y[cb2],n.iter = 400)
# Si nota come l'errore di convalida si stabilizza, e proabilmente ne bastano meno (80)
#plot(m_boost,test = T)

#ristimo su tutto:
m_boost = ada(x = sss[,-id_risposta],
              y = sss$y,
              iter = 70)

pr_boost <- predict(m_boost, vvv, type = "prob")

# Prendi la colonna della classe positiva (es. "1")
(err_boost = classification_metrics(pr_boost[,2], vvv$y,threshold=0.5))


# random forest -----------------------------------------------------------

library(randomForest)
set.seed(1)
cb1 = sample(1:NROW(sss), 0.5*NROW(sss))
cb2 = setdiff(1:NROW(sss), cb1)
# default delle variabili tentate per mtry=if (!is.null(y) && !is.factor(y))
# max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))
sqrt(NCOL(sss))

nvar=c(2,4,6,8,10)
err_rf = matrix(NA,50,length(nvar))
for(m in seq_along(nvar)){
  m_tmp=randomForest(x=sss[cb1,-id_risposta],
                     y=factor(sss$y[cb1]),
                     xtest=sss[cb2,-id_risposta],
                     ytest=factor(sss$y[cb2]),
#                     classwt = c("0" = 1,
#                                "1" = sum(sss$y == 0) / sum(sss$y == 1)),
                     ntree=50,mtry=nvar[m],do.trace = TRUE)
  err_rf[,m]=m_tmp$test$err.rate[,1]
}

m_tmp$test$err.rate

matplot(err_rf,type='l')
legend("bottomright",col=1:length(nvar),lty=1,legend=nvar)

library(randomForest)
set.seed(1)
m_rf <- randomForest(
  factor(y) ~ .,
  data    = sss,
  ntree   = 50,
  mtry    = 10,
  nodesize = 10,
  do.trace=TRUE
)
# importance =TRUE


pr_rf <- predict(m_rf, vvv, type = "prob")

(err_rf <- classification_metrics(pr_rf[, "1"], vvv$y))


sort_by    <- "f1"
decreasing <- TRUE

modelli_list <- list(
  "Stepwise LM"         = if (exists("err_step"))         err_step,
  "Stepwise Logit"      = if (exists("err_step_logit"))   err_step_logit,
  "Ridge LM"            = if (exists("err_ridge"))        err_ridge,
  "Ridge Logit"         = if (exists("err_ridge_logit"))  err_ridge_logit,
  "Lasso LM"            = if (exists("err_lasso"))        err_lasso,
  "Lasso Logit"         = if (exists("err_lasso_logit"))  err_lasso_logit,
  "Lasso + interazioni" = if (exists("err_lasso_int"))    err_lasso_int,
  "LDA"                 = if (exists("err_lda"))          err_lda,
  "QDA"                 = if (exists("err_qda"))          err_qda,
  "GAM"                 = if (exists("err_gam"))         err_gam,
  "MARS"                = if (exists("err_mars"))         err_mars,
  "Albero"              = if (exists("err_tree"))         err_tree,
  "Boosting"            = if (exists("err_boost"))        err_boost,
  "Random Forest"       = if (exists("err_rf"))           err_rf
)

# rimuove le voci NULL (modelli non stimati) e quelle di classe sbagliata
modelli_list <- Filter(
  \(x) !is.null(x) && inherits(x, "classification_metrics"),
  modelli_list
)

risultati <- do.call(rbind, lapply(names(modelli_list), function(nm) {
  m <- modelli_list[[nm]]
  data.frame(
    modello     = nm,
    soglia      = m$threshold,
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


# roc/lift ----------------------------------------------------------------

safe_probs <- function(expr) {
  v <- tryCatch(expr, error = function(e) NULL)
  if (is.null(v) || !is.numeric(v) || length(v) == 0) return(NULL)
  v <- as.vector(v)
  # clip: modelli lineari / non calibrati possono uscire da [0,1]
  pmax(0, pmin(1, v))
}

modelli_probs <- Filter(Negate(is.null), list(
  "Stepwise LM"         = safe_probs(p.step),
  "Stepwise Logit"      = safe_probs(p.step_logit),
  "Ridge LM"            = safe_probs(pred.ridge.cv),
  "Ridge Logit"         = safe_probs(pred.ridge_logit.cv),
  "Lasso LM"            = safe_probs(pred.lasso.cv),
  "Lasso Logit"         = safe_probs(pred.lasso_logit.cv),
  "Lasso + interazioni" = safe_probs(pred.lasso_int.cv),
  "LDA"                 = safe_probs(pr_lda$posterior[,2]),
  "QDA"                 = safe_probs(pr_qda$posterior[,2]),
  "GAM"                 = safe_probs(pred.gam1),
  "MARS"                = safe_probs(pr_mars[,1]),
  "Albero"              = safe_probs(pr_tree[,"1"]),
  "Boosting"            = safe_probs(pr_boost[,2]),
  "Random Forest"       = safe_probs(pr_rf[,"1"])
))

cat("Modelli disponibili:", paste(names(modelli_probs), collapse = ", "), "\n")
if (length(modelli_probs) == 0) stop("Nessun modello disponibile per i plot.")

truth <- vvv$y

compute_roc <- function(probs, truth) {
  df    <- data.frame(p = probs, y = as.integer(truth))
  df    <- df[order(df$p, decreasing = TRUE), ]
  n_pos <- sum(df$y); n_neg <- nrow(df) - n_pos
  tpr   <- cumsum(df$y) / n_pos
  fpr   <- cumsum(1 - df$y) / n_neg
  auc   <- sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
  list(fpr = c(0, fpr, 1), tpr = c(0, tpr, 1), auc = auc)
}

compute_lift <- function(probs, truth) {
  df   <- data.frame(p = probs, y = as.integer(truth))
  df   <- df[order(df$p, decreasing = TRUE), ]
  n    <- nrow(df); rate <- sum(df$y) / n
  pct  <- (1:n) / n
  lift <- (cumsum(df$y) / (1:n)) / rate
  list(pct = c(0, pct), lift = c(lift[1], lift))
}

pal <- setNames(
  rainbow(length(modelli_probs), s = .85, v = .8),
  names(modelli_probs)
)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# ── ROC ───────────────────────────────────────────────────────────────
plot(c(0,1), c(0,1), type = "n", asp = 1, las = 1,
     xlab = "FPR (1 - specificità)",
     ylab = "TPR (sensibilità)", main = "Curva ROC")
abline(0, 1, lty = 2, col = "grey70")

auc_vals <- sapply(modelli_probs, function(p) compute_roc(p, truth)$auc)

for (nm in names(modelli_probs)) {
  roc <- compute_roc(modelli_probs[[nm]], truth)
  lines(roc$fpr, roc$tpr, col = pal[nm], lwd = 1.8)
}

ord <- order(auc_vals, decreasing = TRUE)
legend("bottomright", bty = "n", cex = .72,
       title = sprintf("%-22s %s", "Modello", "AUC"), title.font = 2,
       legend = sprintf("%-22s %.3f", names(auc_vals)[ord], auc_vals[ord]),
       col = pal[ord], lwd = 1.8)

# ── Lift ──────────────────────────────────────────────────────────────
# calcola ylim dai dati effettivi (dopo il taglio min_pct)
all_lifts <- lapply(modelli_probs, function(p) compute_lift(p, truth))
y_max     <- max(sapply(all_lifts, function(l) max(l$lift)))

plot(c(0,1), c(0, ceiling(y_max)), type = "n", las = 1,
     xlab = "Frazione campione (% contattato)",
     ylab = "Lift", main = "Curva Lift")
abline(h = 1, lty = 2, col = "grey70")

for (nm in names(modelli_probs)) {
  lft <- all_lifts[[nm]]
  lines(lft$pct, lft$lift, col = pal[nm], lwd = 1.8)
}

legend("topright", bty = "n", cex = .72,
       legend = names(modelli_probs), col = pal, lwd = 1.8)

par(mfrow = c(1, 1))
