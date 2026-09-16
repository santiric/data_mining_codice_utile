source("utils.R")
library(tidyverse)

# per evitare errori successivi:
dati <- dati %>%
  mutate(across(where(is.character),factor))
dati <- dati %>%
  mutate(across(where(is.factor),factor))

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
id_risposta <- which(names(dati) == "y")



# variabili numeriche e factor --------------------------------------------
id_num <- setdiff(which(sapply(dati, function(x) is.numeric(x) | is.integer(x))), id_risposta)
id_factor <- which(sapply(dati, is.factor))
id_factor <- id_factor[!names(id_factor) %in% names(dati)[id_risposta]]




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
m0_sum0_raw  <- lm(Y ~ . - y, data = sss_raw, contrasts = contrasts_list)
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



# errore ------------------------------------------------------------------

# Funzione per calcolare l'errore di classificazione
ce = function(previsto, osservato) {
  tab = table(previsto, osservato)
  1 - sum(diag(tab))/sum(tab)
}
# Salviamo direttamente gli errori
errori <- data.frame("modello" = character(),  "errore" = numeric())
conf_mat_list <- list()


# risposta con indicatrici ------------------------------------------------

#carico la libreria nnet in cui c'e' il comando class.ind che crea le 
# variabili indicatrici per le modalità della risposta
library(nnet)
Y = class.ind(sss$y) # tengo fuori dall' insieme di stima senno sicuro me
# la dimentico e la uso come regressore



# creazione di sottinsiemi di righe di insieme di  convalida --------------

set.seed(123)
cb1 = sample(1:nrow(sss), 2/3*nrow(sss))
cb2 = setdiff(1:nrow(sss), cb1)


# modello baseline --------------------------------------------------------

library(nnet)
err_base = mean(vvv$y != names(which.max(table(sss$y))))
errori <- rbind(errori, data.frame(modello = "modello base", errore = err_base))
errori


# modello lineare multivariato --------------------------------------------

m0 = lm(Y~.-y, sss)
summary(m0)

n_levels_y <- nlevels(sss$y)
# METTI IL NUMERO DI MODELLI:
m_lin = lapply(1:n_levels_y, function(k)  step(lm(Y[,k]~.-y,sss)))

# Uniamo previsioni
pr_lm = do.call(cbind, lapply(m_lin, predict, vvv))
class_lm = apply(pr_lm, 1, which.max)
# Classifichiamo in base alla categoria con valore maggiore
err_lm = ce(class_lm, vvv$y)
class_lm_fact = factor(levels(sss$y)[class_lm], levels = levels(sss$y))
conf_mat_list[["Lineare multivariato - step"]] <- table(Previsto = class_lm_fact, Osservato = vvv$y)
errori <- data.frame("modello" = character(),  "errore" = numeric())
errori <- rbind(errori, data.frame(modello = "Lineare multivariato - step", errore = err_lm))
errori


# Contrasti a somma zero 
contrasts_list <- setNames(
  lapply(names(id_factor), function(v) contr.sum(nlevels(dati[[v]]))),
  names(id_factor)
)

# Ri-stima i modelli finali con contr.sum 
m_lin_sum0 <- lapply(1:n_levels_y, function(k) {
  f <- formula(m_lin[[k]])
  lm(f, data = sss, contrasts = contrasts_list)
})

lapply(m_lin_sum0, summary)





# modello multinomiale stepwise -------------------------------------------

# multinomial (or polytomous) logit
library(nnet)
levels(sss$y)
m_multi0 = multinom(y~., data=sss, maxit=200)
m_multi_step = step(m_multi0, trace = F) # selezione passo a passo, richiede un po'

formula_step = formula(m_multi_step)  # estrae la formula trovata

m_multi = multinom(formula_step, data = sss, maxit = 200)
pr_multi = predict(m_multi, newdata = vvv)

err_multi <- ce(pr_multi, vvv$y)
errori <- rbind(errori, data.frame(modello = "Regressione multinomiale - step", errore = err_multi))
errori
conf_mat_list[["Regressione multinomiale - step"]] <- table(Previsto = pr_multi, Osservato = vvv$y)
summary(m_multi)

m_multi_sum0 <- multinom(formula_step, data = sss, maxit = 200,
                         contrasts = contrasts_list)
summary(m_multi_sum0)





# modello baseline category logit -------------------------------------------------

# multivariate logistic regression model
library(VGAM)
# Ci mette molto
m_log0 = vglm(y~., data=sss, family = multinomial)
mml_step = step4vglm(m_log0)
formula_step = formula(mml_step)
mnl_step = vglm(formula_step, data = sss, family = multinomial)
coef(mnl_step, matrix = T)
levels(sss$y) # ultima modalita è il riferimento

pr_mnl = predict(mnl_step, type = "resp", vvv)
pr_mnl=apply(pr_mnl, 1, which.max)
err_mnl <- ce(pr_mnl, vvv$y)
errori <- rbind(errori, data.frame(modello = "Regressione logistica multivariata - step", 
                                   errore = err_mnl))
errori
pr_mnl_fact = factor(levels(sss$y)[pr_mnl], levels = levels(sss$y))
conf_mat_list[["Regressione logistica multivariata - step"]] <- table(Previsto = pr_mnl_fact, Osservato = vvv$y)

m_mnl_sum0 <- vglm(formula_step, data = sss, family = multinomial,
                   contrasts.arg = contrasts_list)
coef(m_mnl_sum0, matrix = TRUE)





# lda ---------------------------------------------------------------------

library(MASS)
# Teniamo conto della selezione delle variabili tramite regressione multinomiale
ridotto.var = as.formula(m_multi)
m_lda = lda(ridotto.var, data=sss)
pr_lda  = predict(m_lda, newdata=vvv)
err_lda <- ce(pr_lda$class, vvv$y)
errori <- rbind(errori, data.frame(modello = "LDA", 
                                   errore = err_lda))
errori
conf_mat_list[["LDA"]] <- table(Previsto = pr_lda$class, Osservato = vvv$y)


# QDA ---------------------------------------------------------------------

# occhio che la qda non stima se c'è perfetta collinearità
m_qda = qda(y~., sss) 
pr_qda = predict(m_qda, vvv)
(err_qda = ce(pr_qda$class, vvv$y))
# POSSIBILE ERROR: se rank deficiency in group ... allora una variabile non varia
# all'interno di una categoria della risposta, quindi non stimabile Sigma.
conf_mat_list[["QDA"]] <- table(Previsto = pr_qda$class, Osservato = vvv$y)
errori <- rbind(errori, data.frame(modello = "QDA", errore = err_qda))




# lasso ------------------------------------------------------------------

library(glmnet)

# --- Matrice X con codifica a somma zero -----------------------------------
# Ricostruiamo il modello base con contr.sum per tutti i fattori
m0_sum0 <- lm(Y ~ . - y, data = sss, contrasts = contrasts_list)

# Matrice design con sum coding (escludiamo l'intercetta per glmnet)
X_sum0 <- model.matrix(m0_sum0)[, -1]

# --- Stima LASSO con sum coding --------------------------------------------
n_levels_y <- nlevels(sss$y)
m_lasso_sum0 <- lapply(1:n_levels_y, function(class)
  glmnet(x = X_sum0[cb1, ], y = Y[cb1, class], alpha = 1,lambda.min.ratio = 1e-08)
)

# Griglia lambda comune (dal primo modello)
lambda_seq <- m_lasso_sum0[[1]]$lambda

# --- Selezione lambda su validation set ------------------------------------
pr_lasso_s0 <- lapply(m_lasso_sum0, function(m)
  predict(m, X_sum0[cb2, ], s = lambda_seq)
)

pr_lasso_class_s0 <- matrix(NA, NROW(pr_lasso_s0[[1]]), NCOL(pr_lasso_s0[[1]]))

for (j in 1:NCOL(pr_lasso_class_s0)) {
  pr_lambda <- do.call(cbind, lapply(pr_lasso_s0, function(pr) pr[, j]))
  pr_lasso_class_s0[, j] <- apply(pr_lambda, 1, which.max)
}

err_l_s0 <- apply(pr_lasso_class_s0, 2, function(x) ce(x, sss$y[cb2]))

# --- Plot diagnostico ------------------------------------------------------
par(mfrow = c(1, 2))
plot(log(lambda_seq), err_l_s0, main = "errore vs lambda")
plot(log(lambda_seq), err_l_s0, ylim = c(0.44,0.46))

bb_s0    <- which.min(err_l_s0)
l_best_s0 <- lambda_seq[bb_s0]
abline(v = log(l_best_s0), col = 2)

# --- Previsioni su test set ------------------------------------------------
# IMPORTANTE: stessa codifica usata in training
XXv_sum0 <- model.matrix(
  update(formula(m0_sum0), NULL ~ . - y),  # solo predittori
  data = vvv,
  contrasts.arg = contrasts_list
)[, -1]
pr_lasso_v_s0 <- lapply(m_lasso_sum0, predict,
                        newx = XXv_sum0, s = l_best_s0)
pr_class_v_s0 <- apply(do.call(cbind, pr_lasso_v_s0), 1, which.max)
err_lasso <- ce(pr_class_v_s0, vvv$y)
errori <- rbind(errori, data.frame(modello = "Lasso", 
                                   errore = err_lasso))
errori
pr_lasso_fact = factor(levels(sss$y)[pr_class_v_s0], levels = levels(sss$y))
conf_mat_list[["Lasso"]] <- table(Previsto = pr_lasso_fact, Osservato = vvv$y)








# lasso per risposte binomiali --------------------------------------------
library(glmnet)
# Stima LASSO logistico
m_lasso_logit_sum0 <- lapply(1:n_levels_y, function(class) {
  cat("Stima modello classe", class, "\n")
  glmnet(x = X_sum0[cb1, ], y = as.numeric(Y[cb1, class]),
         alpha = 1, lambda.min.ratio = 1e-08, family = "binomial")
})
# Griglia lambda comune
lambda_min <- max(sapply(m_lasso_logit_sum0, function(m) min(m$lambda)))
lambda_max <- min(sapply(m_lasso_logit_sum0, function(m) max(m$lambda)))
lambda_seq <- exp(seq(log(lambda_max), log(lambda_min), length.out = 100))
# --- Selezione lambda su validation set ------------------------------------
pr_lasso_logit_s0 <- lapply(m_lasso_logit_sum0, function(m)
  predict(m, X_sum0[cb2, ], s = lambda_seq, type = "response")
)
pr_lasso_logit_class_s0 <- matrix(NA, NROW(pr_lasso_logit_s0[[1]]), NCOL(pr_lasso_logit_s0[[1]]))
for (j in 1:NCOL(pr_lasso_logit_class_s0)) {
  pr_lambda <- do.call(cbind, lapply(pr_lasso_logit_s0, function(pr) pr[, j]))
  pr_lasso_logit_class_s0[, j] <- apply(pr_lambda, 1, which.max)
}
err_l_logit_s0 <- apply(pr_lasso_logit_class_s0, 2, function(x) ce(x, sss$y[cb2]))
# --- Plot diagnostico ------------------------------------------------------
par(mfrow = c(1, 2))
plot(log(lambda_seq), err_l_logit_s0, main = "Lasso logit - errore vs lambda")
plot(log(lambda_seq), err_l_logit_s0, ylim = c(0.30, 0.35))
bb_logit_s0    <- which.min(err_l_logit_s0)
l_best_logit_s0 <- lambda_seq[bb_logit_s0]
abline(v = log(l_best_logit_s0), col = 2)
# --- Previsioni su test set ------------------------------------------------
# IMPORTANTE: stessa codifica usata in training
XXv_sum0 <- model.matrix(
  update(formula(m0_sum0), NULL ~ . - y),
  data = vvv,
  contrasts.arg = contrasts_list
)[, -1]
pr_lasso_logit_v_s0 <- lapply(m_lasso_logit_sum0, predict,
                              newx = XXv_sum0, s = l_best_logit_s0, type = "response")
pr_class_logit_v_s0 <- apply(do.call(cbind, pr_lasso_logit_v_s0), 1, which.max)
err_lasso_logit <- ce(pr_class_logit_v_s0, vvv$y)
errori <- rbind(errori, data.frame(modello = "Lasso logit",
                                   errore = err_lasso_logit))
errori

pr_lasso_logit_fact = factor(levels(sss$y)[pr_class_logit_v_s0], levels = levels(sss$y))
conf_mat_list[["Lasso logit"]] <- table(Previsto = pr_lasso_logit_fact, Osservato = vvv$y)


# Estrai coefficienti per tutte le classi
coef_multi <- extract_lasso_coefs_multi(
  lasso_models = m_lasso_logit_sum0,
  s            = l_best_logit_s0,
  class_names  = levels(sss$y),
  top_k = 10
  
)

# Visualizza
plot_lasso_coefs_multi(coef_multi)

# Puoi anche guardare solo le variabili selezionate in almeno k classi
coef_multi %>%
  group_by(variabile) %>%
  summarise(n_classi = n(), abs_medio = mean(abs_coefficiente)) %>%
  arrange(desc(n_classi), desc(abs_medio))




# mars --------------------------------------------------------------------
library(polspline)
# Modello MARS / Polyclass

#fa crescita e potatura tramite aic
m_mars = polyclass(
  sss$y[cb1],
  cov   = X_sum0[cb1, ],
  tdata = sss$y[cb2],
  tcov  = X_sum0[cb2, ]
)
names(m_mars)
head(m_mars$logl)
n_cb1 = length(cb1)
n_cb2 = length(cb2)
err_train = m_mars$logl[, "loss-trn"] / n_cb1
err_valid = m_mars$logl[, "loss-tst"] / n_cb2
plot(
  m_mars$logl[, "dim"],
  err_train,
  type = "l",
  lwd  = 2,
  xlab = "Numero di basi spline",
  ylab = "tasso di errata classificazione",
  main = "Polyclass / MARS"
)
lines(
  m_mars$logl[, "dim"],
  err_valid,
  col = 2,
  lwd = 2
)
points(
  m_mars$logl[, "dim"],
  err_valid,
  col = 2,
  pch = 16
)
legend(
  "topright",
  legend = c("Stima", "Convalida usata per far crescere il modello"),
  col    = c(1, 2),
  lwd    = 2,
  pch    = c(NA, 16),
  bty    = "n"
)
m_mars
pr_mars = cpolyclass(m_mars, cov = XXv_sum0)
err_mars <- ce(pr_mars, vvv$y)
errori <- rbind(errori, data.frame(modello = "mars",
                                   errore = err_mars))
errori
pr_mars_fact = factor(levels(sss$y)[pr_mars], levels = levels(sss$y))
conf_mat_list[["mars"]] <- table(Previsto = pr_mars_fact, Osservato = vvv$y)

fcts <- m_mars$fcts
# Separa struttura e coefficienti
struttura <- fcts[, 1:4]
coef      <- fcts[, 5:ncol(fcts)]
# Dai nomi sensati
colnames(struttura) <- c("cov1", "knot1", "cov2", "knot2")
colnames(coef)      <- levels(sss$y)
# Sostituisci gli indici covariata con i nomi reali
nomi_cov <- colnames(X_sum0)
struttura_named <- struttura
struttura_named[, "cov1"] <- nomi_cov[struttura[, "cov1"]]
struttura_named[, "cov2"] <- nomi_cov[struttura[, "cov2"]]
# Stampa finale
tab <- data.frame(struttura_named, coef)
print(tab)
nomi_classi <- levels(sss$y)

summary(m_mars)
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





# modello additivo --------------------------------------------------------

# stimare un modello multilogit additivo con backfitting è computazionalmente
# molto oneroso, ma se vuoi farlo:

gam_add = F # richiede molto tempo, possiamo stimare direttamente il modello sulle variabili 
# selezionate dalla regressione multinomiale
if(gam_add){
  #variabili quantitative e qualitative, separate
  num = which(unlist(lapply(sss, is.numeric)))
  cat = which(!unlist(lapply(sss, is.numeric)))
  cat = cat[-5] # levo diagnosi
  names(num) # Aggiungo accesso, come numerica
  ll = sprintf("s(%s, df = 3)", c(names(num), "accesso"))
  
  f_gam = paste0("diagnosi~", paste0(ll, collapse = "+"), "+", paste0(names(cat), collapse = "+"))
  f_gam
  library(VGAM)
  
  # Ci mette molto e non e' presente step. Potrebbe aver senso utilizzare le variabili selezionate dai metodi sopra
  m_gam = vgam(as.formula(f_gam), family=multinomial, data=sss, bf.maxit = 100, maxit = 100, trace = T)
  
  #summary(vg7) #molto lungo
  #par(mfrow=c(5,4))
  #plot(m_gam, se=T)
  pr_gam = predict(m_gam,newdata=vvv, type="response")
  pr_gam_class =  apply(pr_gam, 1,which.max)
  err_gam <- ce(pr_gam_class, vvv$y)
  errori <- rbind(errori, data.frame(modello = "gam logit bf",
                                     errore = err_gam))
  errori
}

# oppure proviamo un modello additivo con spline lineare, scelto tramite 
# aic
m_add = polyclass(
  sss$y[cb1],
  cov   = X_sum0[cb1, ],
  tdata = sss$y[cb2],
  tcov  = X_sum0[cb2, ],
  additive = TRUE
)

pr_add = cpolyclass(m_add, cov = XXv_sum0)
err_add <- ce(pr_add, vvv$y)
errori <- rbind(errori, data.frame(modello = "add",
                                   errore = err_add))
errori

pr_add_fact = factor(levels(sss$y)[pr_add], levels = levels(sss$y))
conf_mat_list[["add"]] <- table(Previsto = pr_add_fact, Osservato = vvv$y)




# albero ------------------------------------------------------------------

library(tree)
m_treeL = tree(y ~ ., data=sss[cb1,], 
               control=tree.control(nobs=nrow(sss[cb1,]), minsize=2, mindev=0.0001)
               ,split="deviance")
plot(m_treeL)
prune.a= prune.tree(m_treeL, newdata=sss[cb2,]) 
plot(prune.a)
J = prune.a$size[which.min(prune.a$dev)]
abline(v=J,col="darkorchid",lwd=3)
m_tree =prune.tree(m_treeL, best=J)
plot(m_tree)
#text(m_tree, pretty=0, srt = 90)
text(m_tree, pretty=0, cex = 0.7)
#
pr_tree = predict(m_tree, newdata=vvv, type="class")  # senza l'id unità 
err_tree <- ce(pr_tree, vvv$y)
errori <- rbind(errori, data.frame(modello = "albero",
                                   errore = err_tree))
errori
conf_mat_list[["albero"]] <- table(Previsto = pr_tree, Osservato = vvv$y)






# rf ----------------------------------------------------------------------
## Random forest
library(ranger)
set.seed(1)
sqrt(ncol(sss))
m_try = seq(2,10,by=2)
m_rf = lapply(m_try, function(m) ranger(y~., sss[cb1,], mtry = m))

## Errore di convalida rispetto agli alberi
tree_seq = seq(1,500,by=10)
err_rf = matrix(NA, length(tree_seq), length(m_try))
for(t in seq_along(tree_seq)){
  prev_alberi = lapply(m_rf, predict, sss[cb2,], num.trees = t)
  err_alberi = lapply(prev_alberi, function(pr) ce(pr$pred, sss$y[cb2]))
  err_rf[t,] = unlist(err_alberi)
}

matplot(err_rf, type = "l")
legend('topright', col = 1:length(m_try), legend = m_try, lwd = 2)

m=4 ## scegli qui
trees= 10 * 40 #avevo plottato ogni 10

m_rf = ranger(y~., sss, mtry = m, num.tree = trees, importance = 'permutation') 
pr_rf = predict(m_rf, vvv)$pred

err_rf <- ce(pr_rf, vvv$y)
errori <- rbind(errori, data.frame(modello = "random forest",
                                   errore = err_rf))
errori
conf_mat_list[["random forest"]] <- table(Previsto = pr_rf, Osservato = vvv$y)
library(ggplot2)
# Estrai importanza
imp <- data.frame(
  Variable = names(m_rf$variable.importance),
  Importance = m_rf$variable.importance
)
# Ordina per importanza
imp <- imp[order(imp$Importance, decreasing = TRUE), ]
# Plot
ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (Permutation)",
       x = "Variable", y = "Importance") +
  theme_minimal()



# ------------------------------------------------------------------
# CONFRONTO FINALE ERRORI E TABELLE DI CONFUSIONE
# ------------------------------------------------------------------
errori$errore <- round(as.numeric(errori$errore), 4)
errori_ord <- errori[order(errori$errore), ]

cat("\n================================================================\n")
cat("   CLASSIFICA MODELLI PER TASSO ERRORE\n")
cat("================================================================\n")
print(errori_ord, row.names = FALSE)

cat("\n================================================================\n")
cat("   MATRICI DI CONFUSIONE E METRICHE DETTAGLIATE (Set Verifica vvv)\n")
cat("================================================================\n\n")

# Iteriamo sui modelli ordinati dal migliore al peggiore
for (nome in errori_ord$modello) {
  if (!is.null(conf_mat_list[[nome]])) {
    cat(sprintf("--- %s ---\n", nome))
    
    tab <- conf_mat_list[[nome]]
    print(tab)
    
    # Metriche aggregate dalla tabella di verifica
    n_tot     <- sum(tab)
    n_correct <- sum(diag(tab))
    acc       <- n_correct / n_tot
    cat(sprintf("Accuratezza globale su Verifica: %.4f\n", acc))
    
    # Calcolo Precision e Recall basato su: Righe = Previsto, Colonne = Osservato
    # Precision = TP / (TP + FP) -> Totale di riga (quello che ho predetto)
    # Recall    = TP / (TP + FN) -> Totale di colonna (la classe reale)
    prec_per_classe   <- diag(tab) / rowSums(tab)  
    recall_per_classe <- diag(tab) / colSums(tab)  
    
    # Sostituiamo eventuali NaN dovuti a divisioni per 0 se una classe non viene mai predetta
    prec_per_classe[is.nan(prec_per_classe)] <- 0
    recall_per_classe[is.nan(recall_per_classe)] <- 0
    
    cat("Precision per classe (Valore Predittivo Positivo):\n")
    print(round(prec_per_classe, 4))
    cat("Recall per classe (Sensibilità):\n")
    print(round(recall_per_classe, 4))
    cat("\n------------------------------------------------------------\n\n")
  }
}

