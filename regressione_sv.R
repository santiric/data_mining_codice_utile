source("utils.R")

library(tidyverse)

# per evitare errori successivi:

dati <- dati %>%
  mutate(across(where(is.character), as.factor))

# CODIFICA EFFETTIVAMENTE COME FACTOR LE ESPLICATIVE QUALITATIVE 
# ANCHE SE SONO 0/1, IN QUESTO MODO VA TUTTO FLUIDO DOPO E NON FAI ERRORI
# COME LISCIARE UN FACTOR
# one-hot dei fattori a troppi livelli  ------------------------------------------------

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





# Stima/verifica ----------------------------------------------------------
id_risposta <- which(names(dati) == "y")
set.seed(123)
acaso <- sample(1:nrow(dati),0.75*NROW(dati))
sss <- dati[acaso,]
vvv <- dati[-acaso,]
# eventualmente se pesa troppo:
rm(dati)
gc()

# Salvataggio dati non standardizzati e matrici design originali ----------
sss_raw <- sss
vvv_raw <- vvv
# Matrici design non standardizzate
m0_sum0_raw  <- lm(y ~ ., data = sss_raw, contrasts = contrasts_list)
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


# modalita in sss e vvv ---------------------------------------------------

check <- check_factor_levels(sss,vvv)
check$all_factors_match   # TRUE allora va bene
check$details             # dettagli per colonna

range(sss$y)
hist(sss$y)

# Baseline - media --------------------------------------------------------

m_null = lm(y~1,sss)
p.base = predict(m_null, vvv)
(err_base <- errore(p.base, vvv$y))

# Passo passo -------------------------------------------------------------

nomi <- names(sss)
scope <- as.formula(paste("~ ", paste(nomi[-id_risposta], collapse=" + ")))
# modello nullo
m_null = lm(y~1,sss)
# regressione passo-a-passo
m_step = step(m_null, scope = scope, direction = "forward", trace = 1)
summary(m_step)
p.step = predict(m_step, vvv)
err_step <- errore(p.step, vvv$y)


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



# Ridge -------------------------------------------------------------------
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
x <- model.matrix(~., data=sss[,-id_risposta],contrasts.arg = ctr)
x.vvv <- model.matrix(~., data=vvv[,-id_risposta]) 
# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.ridge.cv=cv.glmnet(x[,-1], sss$y, alpha=0,lambda.min.ratio=1e-10) #togliamo intercetta dalla matrice del disegno
plot(m.ridge.cv)
lambda.ottimo = m.ridge.cv$lambda.min
pred.ridge.cv = predict(m.ridge.cv, newx=x.vvv[,-1],s=lambda.ottimo)
err_ridge <- errore(pred.ridge.cv,vvv$y)




# Lasso -----------------------------------------------------------------
library(glmnet)

# 1. Parametrizzazione a somma 0:
# Prende tutte le colonne factor/character presenti in sss
cat_vars <- names(Filter(is.factor, sss))
# oppure anche character:
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))

make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])
  C
}

ctr <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)
x <- model.matrix(~., data = sss[, -id_risposta], contrasts.arg = ctr)
x.vvv <- model.matrix(~., data = vvv[, -id_risposta]) # se hai fatto stima/verifica
# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.lasso.cv <- cv.glmnet(x[, -1], sss$y, alpha = 1, lambda.min.ratio = 1e-10) # togliamo intercetta dalla matrice del disegno
plot(m.lasso.cv)

lambda.ottimo    <- m.lasso.cv$lambda.min
pred.lasso.cv    <- predict(m.lasso.cv, newx = x.vvv[, -1], s = lambda.ottimo)
err_lasso        <- errore(pred.lasso.cv, vvv$y)

df <- extract_lasso_coefs(m.lasso.cv)
plot_lasso_coefs(df)



# Lasso con interazioni -------------------------------------------------------
library(glmnet)

# 1. Parametrizzazione a somma 0
cat_vars <- names(Filter(\(x) is.factor(x) || is.character(x), sss))

make_contr.sum <- function(v, dd) {
  lvls <- levels(factor(dd[[v]]))
  C <- contr.sum(length(lvls))
  colnames(C) <- paste0(lvls[-length(lvls)])
  C
}
ctr <- setNames(lapply(cat_vars, make_contr.sum, dd = sss), cat_vars)

x     <- model.matrix(~(.)^2, data = sss[, -id_risposta], contrasts.arg = ctr)
x.vvv <- model.matrix(~(.)^2, data = vvv[, -id_risposta], contrasts.arg = ctr)
# se da problemi prova a rifare factor() sui factor, non va se ci sono livelli
# in piu che non esistono


set.seed(1)
m.lasso.int.cv <- cv.glmnet(x[, -1], sss$y, alpha = 1, lambda.min.ratio = 1e-10)
plot(m.lasso.int.cv)
lambda.ottimo      <- m.lasso.int.cv$lambda.min
pred.lasso.int.cv  <- predict(m.lasso.int.cv, newx = x.vvv[, -1], s = lambda.ottimo)
err_lasso.int      <- errore(pred.lasso.int.cv, vvv$y)

df <- extract_lasso_coefs(m.lasso.int.cv)
plot_lasso_coefs(df)

### Se evenualmente vuoi stimare modelli successivi su un sottoinsieme
# intelligente di variabili:
# Costruisci subset con y inclusa esplicitamente
# subset_var <- names(sss)[names(sss) %in% df$variabile]
#subset_var <- c(subset_var, names(id_factor))
#subset_var <- unique(c("y", subset_var))  # assicura y ci sia
#sss_sub <- sss[, subset_var]
# Nuovo id_risposta relativo a sss_sub
#id_risposta_sub <- which(names(sss_sub) == "y")


# GAM ---------------------------------------------------------------------

#usa aic
library(gam)
df_list = paste0("df=", 2:6) 
# Selezione tra tutte le variabili 
#gestisce i fattori in automatico
gam_list = gam.scope(sss, response = id_risposta, 
                     smoother = "s", arg = df_list)
gam_list
gam_null = gam(y~1,data=sss)
gam_full_sel = step.Gam(gam_null,gam_list,trace = T)
summary(gam_full_sel)
pred.gam = predict(gam_full_sel, vvv)
(err_gam <- errore(pred.gam, vvv$y))

#Visualizzazione effetti:
par(mfrow = c(1,2))
#metti le variabili al posto di x1 e x2:
plot(gam_full_sel,terms = c("s(x1, df = 5)", "s(x2, df = 3)"),se=T)

library(gam)
df_list <- paste0("df=", 4:6)
# --- Selezione su subsample se ci metti troppo con tutti i dati---
set.seed(1)
frac    <- 0.2
idx_sub <- sample(nrow(sss), size = floor(frac * nrow(sss)))
sss_sub <- sss[idx_sub, ]
gam_list_sub <- gam.scope(sss_sub, response = id_risposta,
                          smoother = "s", arg = df_list)
gam_null_sub <- gam(y ~ 1, data = sss_sub)
gam_sel_sub  <- step.Gam(gam_null_sub, gam_list_sub, trace = TRUE)
# --- Estrai la formula selezionata ---
formula_sel <- formula(gam_sel_sub)
formula_sel  # verifica
# --- Fit su tutti i dati con formula fissa (no nuova selezione) ---
gam_full_sel<- gam(formula_sel, data = sss)
summary(gam_full_sel)
pred.gam = predict(gam_full_sel, vvv)
(err_gam <- errore(pred.gam, vvv$y))
#Visualizzazione effetti:
par(mfrow = c(1,2))
#metti le variabili al posto di x1 e x2:
plot(gam_full_sel,terms = c("s(Consumo_Annuo, df = 6)", "s(time, df = 4)"),se=T)
rm(sss_sub)



# Albero ------------------------------------------------------------------

library(tree)
# Regolazione: crescita di un albero molto fitto, e poi potatura.
# Puo' essere condotta tramite un insieme di convalida.
# Stima di albero molto fitto
m_tree = tree(y~.,
              data = sss, # Solo i regressori che interessano e y
              control = tree.control(nobs = NROW(sss),minsize = 1, mindev = 0))
plot(m_tree)  #controlla che sia fitto, gioca con mindev
set.seed(1234)
prune = cv.tree(m_tree, K = 10)
J = prune$size[max(which(prune$dev == min(prune$dev)))] #sceglie il lambda
# ottimale più piccolo tra quelli ugualmente ottimali
plot(prune)
abline(v = J, col = 2)

m_tree_b = prune.tree(m_tree, best = J)
plot(m_tree_b)
text(m_tree_b, pretty = 4, cex = 0.8)

pred.tree    <- predict(m_tree_b, vvv)
err_tree        <- errore(pred.tree, vvv$y)




# MARS --------------------------------------------------------------------

### non so perchè ma con i regressori standardizzati è molto instabile (penso dipenda
# dalla scala della risposta ###

library(polspline)

id_factor <- which(sapply(sss, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(sss)[id_risposta]]

m_mars = polymars(responses  = sss$y,
                  predictors = sss[,-id_risposta], # tutti, anche factors
                  factors    = id_factor, # factors
                  gcv = 2)

# Plot crescita/potatura
crescita = m_mars$fitting[m_mars$fitting[,1] == 1,]
potatura = m_mars$fitting[m_mars$fitting[,1] == 0,]


plot(crescita$size, crescita$GCV, xlab="N. funzioni di base", ylab="GCV", pch=16)
points(potatura$size, potatura$GCV, col=4, pch=16)
abline(v = m_mars$fitting$size[which.min(m_mars$fitting$GCV)], col=3, lwd=2)
legend("topright", legend=c("crescita","potatura"), col=c(1,4), pch=16)

### oppure se ci metti troppo a fare selezione con tutti i dati:
library(polspline)

id_factor <- which(sapply(sss, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(sss)[id_risposta]]

# --- Selezione su subsample ---
set.seed(42)
frac      <- 0.3   # frazione per la selezione (regola a piacere)
idx_sub   <- sample(nrow(sss), size = floor(frac * nrow(sss)))
sss_sub   <- sss[idx_sub, ]

m_mars_sub <- polymars(responses  = sss_sub$y,
                       predictors = sss_sub[, -id_risposta],
                       factors    = id_factor,
                       gcv        = 4)

# Plot crescita/potatura (sul modello di selezione)
crescita <- m_mars_sub$fitting[m_mars_sub$fitting[, 1] == 1, ]
potatura <- m_mars_sub$fitting[m_mars_sub$fitting[, 1] == 0, ]
plot(crescita$size, crescita$GCV,
     xlab = "N. funzioni di base", ylab = "GCV", pch = 16,
     main = paste0("Selezione su subsample (n=", nrow(sss_sub), ")"))
points(potatura$size, potatura$GCV, col = 4, pch = 16)
abline(v = m_mars_sub$fitting$size[which.min(m_mars_sub$fitting$GCV)],
       col = 3, lwd = 2)
legend("topright", legend = c("crescita", "potatura"), col = c(1, 4), pch = 16)

# --- Estrai le funzioni di base selezionate ---
selected_terms <- m_mars_sub$model   # data.frame dei termini scelti

# --- Fit su tutti i dati con la struttura selezionata (no nuova selezione) ---
m_mars <- polymars(responses  = sss$y,
                   predictors = sss[, -id_risposta],
                   factors    = id_factor,
                   gcv        = 4,
                   startmodel = selected_terms,  # impone i termini già scelti
                   maxsize    = nrow(selected_terms) - 1)  # blocca l'aggiunta


# Predizione ed errore
pred.mars = predict(m_mars, vvv[,-id_risposta])
(err_mars  = errore(pred.mars, vvv$y))


# aggiungi grafici
# metti in predictor quelli che vuoi
# le altre variabili sono al loro valore mediano

# 2 esp vs resp
plot(m_mars, predictor1 = 4, predictor2 = 5, phi = 10, theta = 60)
mtext(paste("predictor1 =", names(sss[,-id_risposta])[4], 
            "| predictor2 =", names(sss[,-id_risposta])[5]), 
      side = 1, line = 3, cex = 0.9)

# 1 esp vs resp
plot(m_mars, predictor1 = 4)
title(sub = names(sss[,-id_risposta])[4], cex.sub = 1, font.sub = 2)






# PPR ---------------------------------------------------------------------

set.seed(123)
K <- 5
nterms_grid <- 1:10
n <- nrow(sss)
folds <- sample(rep(1:K, length.out = n))
cv_err <- numeric(length(nterms_grid))
for (i in seq_along(nterms_grid)) {
  m <- nterms_grid[i]
  fold_err <- numeric(K)
  
  for (k in 1:K) {
    train <- sss[folds != k, ]
    test  <- sss[folds == k, ]
    mod <- ppr(y ~ ., data = train, nterms = m,sm.method = "gcvspline",gcvpen=3)
    pred <- predict(mod, newdata = test)
    fold_err[k] <- mean((test$y - pred)^2)
  }
  cv_err[i] <- mean(fold_err)
}
best_m <- nterms_grid[which.min(cv_err)]
best_m
plot(nterms_grid, cv_err, type = "b",
     xlab = "nterms",
     ylab = "CV MSE")
abline(v = best_m, col = "red", lty = 2)

m_ppr <- ppr(y ~ ., data = sss, nterms = best_m,sm.method = "gcvspline",gcvpen=2)
pred.ppr = predict(m_ppr,newdata = vvv) 
(err_ppr = errore(pred.ppr, vvv$y))






# Random Forest -----------------------------------------------------------

library(randomForest)
set.seed(1)
cb1 = sample(1:NROW(sss), 0.7*NROW(sss))
cb2 = setdiff(1:NROW(sss), cb1)
# default delle variabili tentate per mtry=if (!is.null(y) && !is.factor(y))
# max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))
# per la regressione circa p/3
# ntree=500
# noi vogliamo convalidare l'iperaparametro mtry
nvar=c(2,4,6) # attorno 
ntree=200
err_rf = matrix(NA,ntree,length(nvar))
for(m in seq_along(nvar)){
  m_tmp=randomForest(x=sss[cb1,-id_risposta],
                     y=sss$y[cb1],
                     xtest=sss[cb2,-id_risposta],
                     ytest=sss$y[cb2],
                     ntree=ntree,mtry=nvar[m])
  err_rf[,m]=m_tmp$mse
}
matplot(err_rf,type='l')
legend("topright",col=1:length(nvar),lty=1,legend=nvar)

m=2 #scegli dal grafico
ntree=100 #scegli dal grafico

m_rf=randomForest(y~.,sss,mtry=m,ntree=ntree,importance=T)
varImpPlot(m_rf)

pred.rf = predict(m_rf,newdata = vvv) 
(err_rf = errore(pred.rf, vvv$y))


## se con tutti i dati troppo pesante: 
library(randomForest)
library(ranger)

set.seed(1)

# --- Sottocampionamento opzionale ---
subsample <- TRUE
frac_tr   <- 0.1
frac_te   <- 0.1

if(subsample){
  cb1 <- sample(1:NROW(sss), frac_tr * NROW(sss))
  cb2 <- sample(setdiff(1:NROW(sss), cb1), frac_te * NROW(sss))
} else {
  cb1 <- sample(1:NROW(sss), 0.7 * NROW(sss))
  cb2 <- setdiff(1:NROW(sss), cb1)
}

nvar  <- c(7, 10, 13)
ntree <- 70

# --- Diagnostica stabilizzazione ntree + scelta mtry (randomForest) ---
err_diag <- matrix(NA, ntree, length(nvar))

for(m in seq_along(nvar)){
  m_tmp <- randomForest(x     = sss[cb1, -id_risposta],
                        y     = sss$y[cb1],
                        xtest = sss[cb2, -id_risposta],
                        ytest = sss$y[cb2],
                        ntree = ntree,
                        mtry  = nvar[m])
  err_diag[, m] <- m_tmp$test$mse
  cat(m, "")
}

matplot(err_diag, type = "l", xlab = "ntree", ylab = "MSE test")
legend("topright", col = 1:length(nvar), lty = 1, legend = nvar)

m     <- 10  # scegli dal grafico
ntree <- 50  # scegli dal grafico

# --- Fit finale con ranger ---
set.seed(1)
m_rf <- ranger(y ~ .,
               data       = sss,
               num.trees  = ntree,
               mtry       = m,
               importance = "permutation")

pred.rf  <- predict(m_rf, data = vvv)$predictions
(err_rf  <- errore(pred.rf, vvv$y))

# --- Importance ---
imp <- sort(m_rf$variable.importance, decreasing = TRUE)
barplot(imp, las = 2, main = "Variable Importance")


# confronto tra modelli ---------------------------------------------------

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