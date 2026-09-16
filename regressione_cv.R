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
id_risposta <- which(names(dati) == "y")


# Se vuoi ottimizzare per il logaritmo:
y_orig <- dati$y
dati$y <- log(dati$y)
dati$y <- pmax(0,dati$y)



# variabili numeriche e factor --------------------------------------------

id_num <- setdiff(which(sapply(dati, function(x) is.numeric(x) | is.integer(x))), id_risposta)
id_factor <- which(sapply(dati, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(dati)[id_risposta]]

# Errore ------------------------------------------------------------------

# specifica min_cap e max_cap se la risposta è per definizione bounded
errore <- function(pred, vero, tipo = "mse", min_cap = NULL, max_cap = NULL, pesi = NULL) {
  tipo <- match.arg(tipo, c("mae", "mse"))
  
  if (!is.null(min_cap)) pred <- pmax(pred, min_cap)
  if (!is.null(max_cap)) pred <- pmin(pred, max_cap)
  
  if (!is.null(pesi)) {
    pesi <- pesi / sum(pesi)  # normalizza a somma 1
    switch(tipo,
           mae = sum(pesi * abs(pred - vero)),
           mse = sum(pesi * (pred - vero)^2)
    )
  } else {
    switch(tipo,
           mae = mean(abs(pred - vero)),
           mse = mean((pred - vero)^2)
    )
  }
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



# solo per riciclare codice:
sss <- dati
#rm(dati)

# Salvataggio dati non standardizzati e matrici design originali ----------
sss_raw <- sss
m0_sum0_raw  <- lm(Y ~ . - y, data = sss_raw, contrasts = contrasts_list)
X_sum0_raw   <- model.matrix(m0_sum0_raw)[, -1]

# Standardizzazione -------------------------------------------------------

cat("Variabili quantitative standardizzate:\n")
print(id_num)

sss[, id_num] = scale(sss[, id_num])


# Creazione dei fold ------------------------------------------------------

set.seed(1)
cv_id = matrix(sample(1:NROW(dati)),ncol = 4)


### se hai delle osservazioni dipendenti e non vuoi mischiarle:
set.seed(seed_cv)
soggetti      <- unique(sss$soggetto_id)
n_sogg        <- length(soggetti)
n_trunc_sogg  <- floor(n_sogg / n_fold) * n_fold
soggetti_camp <- sample(soggetti, n_trunc_sogg)
sogg_fold     <- matrix(soggetti_camp, ncol = n_fold)
cv_id <- apply(sogg_fold, 2, function(ids) which(sss$soggetto_id %in% ids))


# Baseline - media --------------------------------------------------------

err_base_folds <- numeric(ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id[, k]
  id_train <- as.vector(cv_id[, -k])
  
  m_null <- lm(y ~ 1, data = sss[id_train, ])
  pred   <- predict(m_null, newdata = sss[id_test, ])
  
  err_base_folds[k] <- errore(pred, sss$y[id_test])
}

err_base <- mean(err_base_folds)
cat("CV MSE modello nullo:", err_base, "\n")



# Passo-passo ----------------------------------------------------------

nomi <- names(sss)
scope <- as.formula(paste("~ ", paste(nomi[-id_risposta], collapse = " + ")))

err_step_folds <- numeric(ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id[, k]
  id_train <- as.vector(cv_id[, -k])
  
  train <- sss[id_train, ]
  test  <- sss[id_test, ]
  
  # Selezione sul training
  m_null_k <- lm(y ~ 1, data = train)
  m_step_k <- step(m_null_k, scope = scope, direction = "forward", trace = 0) #,k = log(nrow(train)) se vuoi bic
  
  # Errore sul test
  pred <- predict(m_step_k, newdata = test)
  err_step_folds[k] <- errore(pred, test$y)
  cat(k)
}

err_step <- mean(err_step_folds)
cat("CV errore passo-passo:", err_step, "\n")

# Modello finale su tutti i dati:
m_null_final <- lm(y ~ 1, data = sss)
m_step <- step(m_null_final, scope = scope, direction = "forward", trace = 1)#,k = log(nrow(train)) se vuoi bic
summary(m_step)

# Ristimiamo con contrasti a somma 0 (per leggere i coefficienti):
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- lvls[-length(lvls)]
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)

formula_step <- formula(m_step)
m_step_sum0 <- lm(formula_step, data = sss, contrasts = ctr)
summary(m_step_sum0)

n_scelti_vars     <- length(attr(terms(m_step), "term.labels"))
n_disponibili_vars <- length(nomi[-id_risposta])
cat("Variabili scelte:", n_scelti_vars, "su", n_disponibili_vars, "disponibili\n")




# RIDGE -------------------------------------------------------------------
library(glmnet)

# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(is.factor, sss))
# oppure anche character:
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])  # o paste0(v, "_", lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(~., data=sss[,-id_risposta],contrasts.arg = ctr)
# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono
y <- sss$y

# Griglia lambda: fissata una volta su tutti i dati così in comune nei fold
griglia_lambda <- glmnet(x[,-1], y, alpha = 0, lambda.min.ratio = 1e-8)$lambda
n_lambda       <- length(griglia_lambda)

foldid_vec <- integer(nrow(sss))
for (k in 1:ncol(cv_id)) foldid_vec[cv_id[, k]] <- k

cv_ridge <- cv.glmnet(x[,-1], y, alpha = 0,lambda = griglia_lambda,
          foldid = foldid_vec)

plot(cv_ridge)

# minimo della curva CV mediata sui fold per stimare complessità
# ottimale e errore di generalizzazione (subottimale ma amen)
lambda.ottimo = cv_ridge$lambda.min
err_ridge <- min(cv_ridge$cvm)
cat("CV errore ridge:", err_ridge, "\n")
m_ridge <-glmnet(x[,-1], y, alpha = 0, lambda = lambda.ottimo)

# Coefficienti del modello finale
coef_final <- coef(m_ridge, s = lambda.ottimo)
print(coef_final)



# LASSO -------------------------------------------------------------------
library(glmnet)
# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(is.factor, sss))
# oppure anche character:
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(~., data=sss[,-id_risposta], contrasts.arg = ctr)
# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono
y <- sss$y
# Griglia lambda: fissata una volta su tutti i dati così in comune nei fold
griglia_lambda <- glmnet(x[,-1], y, alpha = 1, lambda.min.ratio = 1e-8)$lambda
n_lambda       <- length(griglia_lambda)
foldid_vec <- integer(nrow(sss))
for (k in 1:ncol(cv_id)) foldid_vec[cv_id[, k]] <- k
cv_lasso <- cv.glmnet(x[,-1], y, alpha = 1, lambda = griglia_lambda,
                      foldid = foldid_vec)
plot(cv_lasso)
# minimo della curva CV mediata sui fold per stimare complessità
# ottimale e errore di generalizzazione (subottimale ma amen)
lambda.ottimo = cv_lasso$lambda.min
err_lasso <- min(cv_lasso$cvm)
cat("CV errore lasso:", err_lasso, "\n")
m_lasso <- glmnet(x[,-1], y, alpha = 1, lambda = lambda.ottimo)
# Coefficienti del modello finale
coef_final <- coef(m_lasso, s = lambda.ottimo)
print(coef_final)

df <- extract_lasso_coefs(m_lasso)
plot_lasso_coefs(df)

# Numero di coefficienti scelti vs disponibili (esclusa intercetta)
n_disponibili <- ncol(x) - 1  # -1 per l'intercetta
n_scelti      <- sum(coef_final[-1] != 0)
cat("Regressori scelti:", n_scelti, "su", n_disponibili, "disponibili\n")


### Se evenualmente vuoi stimare modelli successivi su un sottoinsieme
# intelligente di variabili:
# Costruisci subset con y inclusa esplicitamente
# subset_var <- names(sss)[names(sss) %in% df$variabile]
#subset_var <- c(subset_var, names(id_factor))
#subset_var <- unique(c("y", subset_var))  # assicura y ci sia
#sss_sub <- sss[, subset_var]
# Nuovo id_risposta relativo a sss_sub
#id_risposta_sub <- which(names(sss_sub) == "y")


# LASSO INTERAZIONI -------------------------------------------------------------------
library(glmnet)
# 1.Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in dd
cat_vars <- names(Filter(is.factor, sss))
# oppure anche character:
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))
make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd=sss), cat_vars)
x <- model.matrix(~(.)^2, data = sss[, -id_risposta], contrasts.arg = ctr)
# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono
y <- sss$y
# Griglia lambda: fissata una volta su tutti i dati così in comune nei fold
griglia_lambda <- glmnet(x[,-1], y, alpha = 1, lambda.min.ratio = 1e-8)$lambda
n_lambda       <- length(griglia_lambda)
foldid_vec <- integer(nrow(sss))
for (k in 1:ncol(cv_id)) foldid_vec[cv_id[, k]] <- k
cv_lasso_int <- cv.glmnet(x[,-1], y, alpha = 1,lambda = griglia_lambda,
                          foldid = foldid_vec)
plot(cv_lasso_int)
# minimo della curva CV mediata sui fold per stimare complessità
# ottimale e errore di generalizzazione (subottimale ma amen)
lambda.ottimo = cv_lasso_int$lambda.min
err_lasso_int <- min(cv_lasso_int$cvm)
cat("CV errore lasso interazioni:", err_lasso_int, "\n")
m_lasso_int <- glmnet(x[,-1], y, alpha = 1, lambda = lambda.ottimo)
# Coefficienti del modello finale
coef_final <- coef(m_lasso_int, s = lambda.ottimo)
print(coef_final)






# GAM ----------------------------------------
library(gam)
err_gam_folds <- numeric(ncol(cv_id))
for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id[, k]
  id_train <- as.vector(cv_id[, -k])
  
  train <- sss[id_train, ]
  test  <- sss[id_test, ]
  
  # Scope GAM sul training
  df_list    <- paste0("df=", c(4,8))
  gam_list_k <- gam.scope(train, response = id_risposta,
                          smoother = "s", arg = df_list)
  
  # Selezione stepwise sul training
  gam_null_k <- gam(y ~ 1, data = train)
  gam_sel_k  <- step.Gam(gam_null_k, gam_list_k, trace = TRUE) # steps= ...
  
  # Errore sul test
  pred <- predict(gam_sel_k, newdata = test)
  err_gam_folds[k] <- errore(pred, test$y)
  cat(k)
}

err_gam_cv <- mean(err_gam_folds)
cat("\nCV errore GAM:", err_gam_cv, "\n")

# Modello finale su tutti i dati -------------------------------------------
df_list          <- paste0("df=", 2:6)
gam_list_final   <- gam.scope(sss, response = id_risposta,
                              smoother = "s", arg = df_list)
gam_null_final   <- gam(y ~ 1, data = sss)
gam_full_sel     <- step.Gam(gam_null_final, gam_list_final, trace = TRUE)
summary(gam_full_sel)

# Riestimiamo con contrasti a somma 0 (per leggere i coefficienti) ---------
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))

make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- lvls[-length(lvls)]
  C
}

ctr <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)

formula_gam   <- formula(gam_full_sel)
gam_sum0      <- gam(formula_gam, data = sss, contrasts = ctr)
summary(gam_sum0)

# Numero variabili selezionate ---------------------------------------------
nomi                <- names(sss)
term_labels         <- attr(terms(gam_full_sel), "term.labels")
n_scelti_vars       <- length(term_labels)
n_disponibili_vars  <- length(nomi[-id_risposta])
cat("Variabili scelte:", n_scelti_vars, "su", n_disponibili_vars, "disponibili\n")

# Visualizzazione effetti finali -------------------------------------------
smooth_terms <- term_labels[grepl("^s\\(", term_labels)]
par(mfrow = c(1, length(smooth_terms)))
plot(gam_sum0, terms = smooth_terms, se = TRUE)






# Albero (fold allineati) -------------------------
library(tree)
#Crescita albero molto fitto su tutti i dati
m_tree <- tree(y ~ .,
               data = sss[, -id_risposta],
               control = tree.control(nobs = NROW(sss), minsize = 1, mindev = 0))
plot(m_tree)
#Griglia di dimensioni (size) da esplorare
size_max <- 50
size_grid <- seq(2,size_max, by = 1)

#CV manuale con gli stessi fold di foldid_vec
K <- ncol(cv_id)
cv_dev <- matrix(NA, nrow = K, ncol = length(size_grid))
for (k in 1:K) {
  idx_val  <- cv_id[, k]
  idx_tra  <- setdiff(1:nrow(sss), idx_val)
  
  # Passa il dataframe completo con y inclusa
  dati_tra <- sss[idx_tra, ]
  dati_val <- sss[idx_val, ]
  
  # Albero fitto sul training del fold
  m_k <- tree(y ~ .,
              data = dati_tra,  # togli solo le colonne non volute
              control = tree.control(nobs = length(idx_tra), minsize = 1, mindev = 0))
  
  for (j in seq_along(size_grid)) {
    s_eff <- min(size_grid[j], max(summary(m_k)$size))
    m_k_p <- prune.tree(m_k, best = s_eff)
    pred  <- predict(m_k_p, newdata = dati_val[, -id_risposta])
    cv_dev[k, j] <- mean((pred - dati_val$y)^2)
  }
}

#Errore CV medio per ogni size
cv_mean <- colMeans(cv_dev)
plot(size_grid, cv_mean, type = "b", xlab = "Size", ylab = "CV MSE")

#Size ottimale (la più piccola tra quelle al minimo)
J <- size_grid[max(which(cv_mean == min(cv_mean)))]
abline(v = J, col = 2)
cat("Size ottimale:", J, "\n")

#Modello finale su tutti i dati
m_tree_b <- prune.tree(m_tree, best = J)
plot(m_tree_b)
text(m_tree_b, pretty = 4, cex = 0.8)

#Errore CV
err_tree <- min(cv_mean)
cat("CV errore albero:", err_tree, "\n")





# MARS --------------------------------------------------------------------
library(polspline)

id_factor <- which(sapply(sss, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(sss)[id_risposta]]

# --- CV con k-fold ---
err_mars_folds <- numeric(ncol(cv_id))

for (k in 1:ncol(cv_id)) {
  id_test  <- cv_id[, k]
  id_train <- as.vector(cv_id[, -k])
  
  train <- sss[id_train, ]
  test  <- sss[id_test, ]
  
  # Adatta id_factor al sottoinsieme train (stessi indici, stesso dataset)
  m_mars_k <- polymars(responses  = train$y,
                       predictors = train[, -id_risposta],
                       factors    = id_factor,
                       gcv        = 4)
  
  pred <- predict(m_mars_k, test[, -id_risposta])
  err_mars_folds[k] <- errore(pred, test$y)
  cat(k)
}

err_mars <- mean(err_mars_folds)
cat("CV errore MARS:", err_mars, "\n")

# --- Modello finale su tutti i dati ---
m_mars <- polymars(responses  = sss$y,
                   predictors = sss[, -id_risposta],
                   factors    = id_factor,
                   gcv        = 4)

# Plot crescita/potatura
crescita <- m_mars$fitting[m_mars$fitting[, 1] == 1, ]
potatura <- m_mars$fitting[m_mars$fitting[, 1] == 0, ]

plot(crescita$size, crescita$GCV,
     xlab = "N. funzioni di base", ylab = "GCV", pch = 16,
     ylim=c(0,30))
points(potatura$size, potatura$GCV, col = 4, pch = 16)
abline(v = m_mars$fitting$size[which.min(m_mars$fitting$GCV)], col = 3, lwd = 2)
legend("topright", legend = c("crescita", "potatura"), col = c(1, 4), pch = 16)

# aggiungi grafici
# metti in predictor quelli che vuoi
# le altre variabili sono al loro valore mediano

summary(m_mars)

# 2 esp vs resp
plot(m_mars, predictor1 = 4, predictor2 = 5, phi = 10, theta = 60)
mtext(paste("predictor1 =", names(sss[,-id_risposta])[4], 
            "| predictor2 =", names(sss[,-id_risposta])[5]), 
      side = 1, line = 3, cex = 0.9)

# 1 esp vs resp
plot(m_mars, predictor1 = 4)
title(sub = names(sss[,-id_risposta])[4], cex.sub = 1, font.sub = 2)





# PPR ---------------------------------------------------------------------
nterms_grid <- 1:10
ppr_cv_err <- numeric(length(nterms_grid))

for (i in seq_along(nterms_grid)) {
  cat(i)
  m <- nterms_grid[i]
  fold_err <- numeric(ncol(cv_id))
  
  for (k in 1:ncol(cv_id)) {
    id_test  <- cv_id[, k]
    id_train <- as.vector(cv_id[, -k])
    
    train <- sss[id_train, ]
    test  <- sss[id_test, ]
    
    mod <- ppr(y ~ ., data = train, nterms = m, sm.method = "gcvspline", gcvpen = 3)
    pred <- predict(mod, newdata = test)
    fold_err[k] <- errore(pred, test$y)
  }
  ppr_cv_err[i] <- mean(fold_err)
}

# Miglior numero di termini
best_nterms <- nterms_grid[which.min(cv_err)]
cat("Miglior nterms:", best_nterms, "\n")

# Plot CV error
plot(nterms_grid, cv_err, type = "b", pch = 16,
     xlab = "N. termini", ylab = "CV error")
abline(v = best_nterms, col = 3, lwd = 2)

# Modello finale su tutti i dati
m_ppr <- ppr(y ~ ., data = sss, nterms = best_nterms, sm.method = "gcvspline", gcvpen = 3)
err_ppr <- min(ppr_cv_err)
cat("CV errore ppr:", err_ppr, "\n")




# random forest -----------------------------------------------------------

library(randomForest)
set.seed(1)
# default delle variabili tentate per mtry=if (!is.null(y) && !is.factor(y))
# max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))
# per la regressione circa p/3
# ntree=500
# noi vogliamo convalidare l'iperaparametro mtry

nvar <- c(25,27,30,33,35,38)
ntree <- 200
rf_cv_err <- numeric(length(nvar))

# per il grafico ntree: matrice ntree x nvar, mediata sui fold
rf_cv_mse_curve <- matrix(0, ntree, length(nvar))

for (m in seq_along(nvar)) {
  cat(m)
  fold_err      <- numeric(ncol(cv_id))
  fold_mse_mat  <- matrix(0, ntree, ncol(cv_id))  # OOB curve per fold
  
  for (k in 1:ncol(cv_id)) {
    id_test  <- cv_id[, k]
    id_train <- as.vector(cv_id[, -k])
    
    train <- sss[id_train, ]
    test  <- sss[id_test, ]
    
    mod  <- randomForest(y ~ ., data = train,
                         ntree = ntree, mtry = nvar[m],
                         keep.forest = TRUE)
    pred <- predict(mod, newdata = test)
    fold_err[k]       <- errore(pred, test$y)
    fold_mse_mat[, k] <- mod$mse   # OOB MSE cumulativo (train)
  }
  
  rf_cv_err[m]          <- mean(fold_err)
  rf_cv_mse_curve[, m]  <- rowMeans(fold_mse_mat)  # media sui fold
}

# Miglior mtry
best_mtry <- nvar[which.min(rf_cv_err)]
cat("Miglior mtry:", best_mtry, "\n")

# Plot CV error per mtry
plot(nvar, rf_cv_err, type = "b", pch = 16,
     xlab = "mtry", ylab = "CV error")
abline(v = best_mtry, col = 3, lwd = 2)

# Plot OOB curve per ntree (mediata sui fold)
matplot(rf_cv_mse_curve, type = 'l', lty = 1, col = 1:length(nvar),
        xlab = "Numero di alberi", ylab = "OOB MSE (media sui fold)")
legend("topright", col = 1:length(nvar), lty = 1, legend = paste("mtry =", nvar))

# Modello finale su tutti i dati
m_rf <- randomForest(y ~ ., data = sss,
                     ntree = ntree, mtry = best_mtry,
                     keep.forest = TRUE)

# Errore CV (media su tutti i mtry testati, come fai per ppr)
err_rf <- min(rf_cv_err)
cat("CV errore RF:", err_rf, "\n")





### variante con oob , subottimale ma veloce
# nvar <- c(25,27,30,33,35,38)
#ntree = 200
#err_rf = matrix(NA, ntree, length(nvar))

#for(m in seq_along(nvar)){
#  m_tmp = randomForest(x   = sss[, -id_risposta],
#                       y   = sss$y,
#                       ntree = ntree,
#                       mtry  = nvar[m],
#                       keep.forest = FALSE)   # risparmia memoria
#  err_rf[, m] = m_tmp$mse   # OOB MSE cumulativo albero per albero
#}

#matplot(err_rf, type = 'l', lty = 1, col = 1:length(nvar),
#        xlab = "Numero di alberi", ylab = "OOB MSE")
#legend("topright", col = 1:length(nvar), lty = 1, legend = nvar)
#m=33 #scegli dal grafico
#ntree=100 #scegli dal grafico

#m_rf=randomForest(y~.,sss,mtry=m,ntree=ntree,importance=T)
#varImpPlot(m_rf)




# confronto tra modelli ---------------------------------------------------

# ── Ranking finale dei modelli ──────────────────────────────────────────────

# ── Ranking finale dei modelli ───────────────────────────────────────────────
modelli_list <- list(
  "Baseline (media)"   = "err_base",
  "Stepwise"           = "err_step",
  "Ridge"              = "err_ridge",
  "Lasso"              = "err_lasso",
  "Lasso + interazioni"= "err_lasso_int",
  "GAM"                = "err_gam",
  "Albero"             = "err_tree",
  "MARS"               = "err_mars",
  "PPR"                = "err_ppr",
  "Random Forest"      = "err_rf"
)

risultati <- modelli_list |>
  Filter(f = function(nm) exists(nm) && is.numeric(get(nm)) && length(get(nm)) == 1) |>
  (\(lst) tibble(
    modello     = names(lst),
    errore_val  = sapply(lst, get)
  ))() |>
  arrange(errore_val)

print(as.data.frame(risultati), row.names = FALSE, digits = 6)