
# lasso coef --------------------------------------------------------------



extract_lasso_coefs <- function(lasso_model, s = "lambda.min") {
  
  # Estrai coefficienti
  coef_matrix <- coef(lasso_model, s = s)
  
  # Converti in dataframe
  coef_df <- as.data.frame(as.matrix(coef_matrix))
  colnames(coef_df) <- "coefficiente"
  coef_df$variabile <- rownames(coef_df)
  
  # Filtra solo coefficienti != 0 (escludi intercetta se vuoi)
  coef_df <- coef_df %>%
    filter(coefficiente != 0) %>%
    mutate(abs_coefficiente = abs(coefficiente)) %>%
    arrange(desc(abs_coefficiente)) %>%
    dplyr::select(variabile, coefficiente, abs_coefficiente)
  
  return(coef_df)
}


library(ggplot2)

plot_lasso_coefs <- function(coef_df) {
  
  # Crea colonna colore per coefficienti positivi/negativi
  coef_df <- coef_df %>%
    filter(variabile != "(Intercept)") %>%
    mutate(segno = ifelse(coefficiente > 0, "Positivo", "Negativo"))
  
  ggplot(coef_df, aes(x = reorder(variabile, abs_coefficiente), 
                      y = coefficiente, 
                      fill = segno)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = round(coefficiente, 3),
                  hjust = ifelse(coefficiente > 0, -0.15, 1.15)),
              size = 3.5) +
    coord_flip() +
    scale_fill_manual(values = c("Positivo" = "#2196F3", "Negativo" = "#F44336")) +
    labs(
      title    = "Coefficienti LASSO (≠ 0)",
      subtitle = "Ordinati per valore assoluto",
      x        = NULL,
      y        = "Coefficiente",
      fill     = "Segno"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
}


extract_lasso_coefs_multi <- function(lasso_models, s, class_names = NULL, top_k = 10) {
  
  if (is.null(class_names)) class_names <- paste0("Classe_", seq_along(lasso_models))
  
  coef_list <- lapply(seq_along(lasso_models), function(k) {
    coef_matrix <- coef(lasso_models[[k]], s = s)
    coef_df <- as.data.frame(as.matrix(coef_matrix))
    colnames(coef_df) <- "coefficiente"
    coef_df$variabile <- rownames(coef_df)
    coef_df$classe    <- class_names[k]
    
    coef_df %>%
      filter(coefficiente != 0, variabile != "(Intercept)") %>%
      mutate(abs_coefficiente = abs(coefficiente)) %>%
      arrange(desc(abs_coefficiente)) %>%
      slice_head(n = top_k) %>%           # <-- top k per classe
      dplyr::select(classe, variabile, coefficiente, abs_coefficiente)
  })
  
  bind_rows(coef_list)
}
plot_lasso_coefs_multi <- function(coef_df) {
  
  coef_df <- coef_df %>%
    mutate(segno = ifelse(coefficiente > 0, "Positivo", "Negativo"))
  
  ggplot(coef_df, aes(x = reorder(variabile, abs_coefficiente),
                      y = coefficiente,
                      fill = segno)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = round(coefficiente, 3),
                  hjust = ifelse(coefficiente > 0, -0.15, 1.15)),
              size = 3) +
    coord_flip() +
    scale_fill_manual(values = c("Positivo" = "#2196F3", "Negativo" = "#F44336")) +
    facet_wrap(~ classe, scales = "free_y") +   # <-- un pannello per classe
    labs(
      title    = "Coefficienti LASSO logit per classe (≠ 0)",
      subtitle = "Ordinati per valore assoluto",
      x        = NULL,
      y        = "Coefficiente",
      fill     = "Segno"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold"),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank()
    )
}












# plot dataset ------------------------------------------------------------

plot_str <- function(df) {
  library(ggplot2)
  library(dplyr)
  
  variabili <- data.frame(
    variabile = names(df),
    tipo_r    = sapply(df, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      categoria = case_when(
        tipo_r %in% c("numeric", "integer", "double", "complex") ~ "Numerica",
        TRUE ~ "Categoriale"
      ),
      ordine = row_number()
    )
  
  ggplot(variabili, aes(x = 0, y = reorder(variabile, ordine))) +
    geom_text(aes(x = 0, label = variabile), hjust = 0,
              size = 3.5, fontface = "bold", color = "grey15") +
    geom_text(aes(x = 1, label = categoria), hjust = 0,
              size = 3.5, color = "grey40") +
    annotate("text", x = 0, y = nrow(variabili) + 0.8, label = "Variabile",
             hjust = 0, size = 3.5, fontface = "bold", color = "grey15") +
    annotate("text", x = 1, y = nrow(variabili) + 0.8, label = "Categoria",
             hjust = 0, size = 3.5, fontface = "bold", color = "grey15") +
    annotate("segment", x = -0.2, xend = 2,
             y = nrow(variabili) + 0.4, yend = nrow(variabili) + 0.4,
             color = "grey70", linewidth = 0.4) +
    geom_tile(aes(x = 0.5, fill = ordine %% 2 == 0),
              width = 1.6, height = 0.9, alpha = 0.3) +
    scale_fill_manual(values = c("TRUE" = "grey90", "FALSE" = "white")) +
    scale_x_continuous(limits = c(-0.2, 2)) +
    scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
    labs(title = paste0("str(", deparse(substitute(df)), ")"),
         subtitle = paste0(nrow(variabili), " variabili")) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title    = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey50", size = 11, margin = margin(b = 8)),
      plot.margin   = margin(12, 12, 12, 12)
    )
}





# printa bene tabella -----------------------------------------------------

print_table <- function(x, title = NULL) {
  df <- as.data.frame(x) |> setNames(c("Valore", "N"))
  df$pct <- paste0(round(df$N / sum(df$N) * 100, 1), "%")
  
  ggplot(df, aes(x = 1, y = rev(seq_len(nrow(df))))) +
    geom_text(aes(x = 1, label = df$Valore), hjust = 0, fontface = "bold", color = "#2C3E50") +
    geom_text(aes(x = 2, label = df$N), hjust = 0.5, color = "#2C3E50") +
    geom_text(aes(x = 3, label = df$pct), hjust = 0.5, color = "#7F8C8D") +
    annotate("text", x = c(1, 2, 3), y = nrow(df) + 1,
             label = c("Valore", "N", "%"), fontface = "bold", color = "white") +
    annotate("rect", xmin = 0.8, xmax = 3.3,
             ymin = nrow(df) + 0.5, ymax = nrow(df) + 1.5, fill = "#2C3E50") +
    annotate("text", x = c(1, 2, 3), y = nrow(df) + 1,
             label = c("Valore", "N", "%"), fontface = "bold", color = "white") +
    coord_cartesian(xlim = c(0.8, 3.5), ylim = c(0.5, nrow(df) + 2)) +
    labs(title = title) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", color = "#2C3E50", margin = margin(b = 8)))
}








# modalita in train e test ------------------------------------------------

check_factor_levels <- function(train, test) {
  
  common_cols <- intersect(names(train), names(test))
  results     <- list()
  
  for (col in common_cols) {
    
    is_fac_train <- is.factor(train[[col]])
    is_fac_test  <- is.factor(test[[col]])
    
    # segnala se il tipo diverge tra train e test
    if (is_fac_train != is_fac_test) {
      results[[col]] <- list(
        same_levels   = FALSE,
        only_in_train = character(0),
        only_in_test  = character(0),
        note          = paste0(
          "tipo diverso: train=",
          if (is_fac_train) "factor" else class(train[[col]]),
          ", test=",
          if (is_fac_test)  "factor" else class(test[[col]])
        )
      )
      next
    }
    
    if (!is_fac_train) next   # nessuno dei due è factor: salta
    
    # usa levels() per preservare i livelli dichiarati (anche non osservati)
    lev_train <- levels(train[[col]])
    lev_test  <- levels(test[[col]])
    
    only_train <- setdiff(lev_train, lev_test)
    only_test  <- setdiff(lev_test,  lev_train)
    
    results[[col]] <- list(
      same_levels   = identical(sort(lev_train), sort(lev_test)),
      only_in_train = only_train,
      only_in_test  = only_test
    )
  }
  
  all_ok <- all(sapply(results, function(x) x$same_levels))
  
  list(all_factors_match = all_ok, details = results)
}





# sottocampiona bilanciato classificazione --------------------------------

balanced_subsample = function(sss, p, id_risposta) {
  y = sss[[id_risposta]]
  
  idx_0 = which(y == 0)
  idx_1 = which(y == 1)
  
  n_tot = nrow(sss)
  # totale da campionare
  n_camp = round(n_tot * p)
  # diviso equamente tra le due classi
  n_per_class = round(n_camp / 2)
  
  # controllo che non si chieda più di quanto disponibile
  n_per_class = min(n_per_class, length(idx_0), length(idx_1))
  
  sort(c(
    sample(idx_0, size = n_per_class),
    sample(idx_1, size = n_per_class)
  ))
}


#oppure coin le stesse prop iniziali: 
subsample <- function(sss, p, id_risposta) {
  y   <- sss[[id_risposta]]
  idx <- sample(seq_len(nrow(sss)), size = round(nrow(sss) * p))
  sort(idx)
}



# ── SALVA ENVIRONMENT ──────────────────────────────────────────
save_environment <- function(path = "environment.RData", 
                             exclude = c()) {
  objs <- ls(envir = .GlobalEnv)
  objs <- objs[!objs %in% exclude]
  save(list = objs, file = path, envir = .GlobalEnv)
  message("✔ Environment salvato in: ", path, 
          " (", length(objs), " oggetti)")
}

# ── CARICA ENVIRONMENT ─────────────────────────────────────────
load_environment <- function(path = "environment.RData") {
  if (!file.exists(path)) stop("File non trovato: ", path)
  loaded <- load(path, envir = .GlobalEnv)
  message("✔ Caricati ", length(loaded), " oggetti da: ", path)
  invisible(loaded)
}








# checkpoint risultati modelli --------------------------------------------

# ==============================================================================
# FUNZIONI CHECKPOINT — una per file
# Uso: chiama salva_checkpoint_*() in qualsiasi momento per salvare i risultati.
#      Poi ricarica con carica_checkpoint_*() dopo un crash.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. CLASSIFICAZIONE CV  (classificazione_cv.R)
# ------------------------------------------------------------------------------
# Oggetti salvati: err_* (liste con 6 metriche), cv_preds, lambda_ottimo,
#                  lambda_ottimo_lasso, mtry_ottimo, dim_albero[B]
salva_checkpoint_class_cv <- function(path = "checkpoint_classificazione_cv.rds") {
  candidati <- c(
    "err_step", "err_step_logit", "err_ridge_bin", "err_lasso_bin",
    "err_lda", "err_qda", "err_gam_logit", "err_mars", "err_tree", "err_rf_bin",
    "cv_preds",
    "lambda_ottimo", "lambda_ottimo_lasso",
    "mtry_ottimo", "B", "dim_albero"
  )
  dati <- Filter(Negate(is.null),
                 setNames(lapply(candidati, function(nm)
                   if (exists(nm, envir = .GlobalEnv)) get(nm, envir = .GlobalEnv) else NULL),
                   candidati))
  saveRDS(dati, path)
  cat(sprintf("[checkpoint] Salvato in '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}

carica_checkpoint_class_cv <- function(path = "checkpoint_classificazione_cv.rds") {
  dati <- readRDS(path)
  list2env(dati, envir = .GlobalEnv)
  cat(sprintf("[checkpoint] Caricati da '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}


# ------------------------------------------------------------------------------
# 2. CLASSIFICAZIONE SV  (classificazione_sv.R)
# ------------------------------------------------------------------------------
# Oggetti salvati: err_* (oggetti classification_metrics), risultati (data.frame)
salva_checkpoint_class_sv <- function(path = "checkpoint_classificazione_sv.rds") {
  candidati <- c(
    "err_step", "err_step_logit",
    "err_ridge", "err_ridge_logit",
    "err_lasso", "err_lasso_logit", "err_lasso_int",
    "err_lda", "err_qda",
    "err_gam", "err_gam1",
    "err_mars", "err_tree", "err_boost", "err_rf",
    "risultati"        # data.frame confronto finale se già costruito
  )
  dati <- Filter(Negate(is.null),
                 setNames(lapply(candidati, function(nm)
                   if (exists(nm, envir = .GlobalEnv)) get(nm, envir = .GlobalEnv) else NULL),
                   candidati))
  saveRDS(dati, path)
  cat(sprintf("[checkpoint] Salvato in '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}

carica_checkpoint_class_sv <- function(path = "checkpoint_classificazione_sv.rds") {
  dati <- readRDS(path)
  list2env(dati, envir = .GlobalEnv)
  cat(sprintf("[checkpoint] Caricati da '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}


# ------------------------------------------------------------------------------
# 3. MULTICLASSE CV  (multiclasse_cv.R)
# ------------------------------------------------------------------------------
# Oggetti salvati: errori_cv (data.frame), conf_mat_list, cv_preds,
#                  err_* scalari, parametri ottimi (dim_albero[B], mtry_ottimo, ecc.)
salva_checkpoint_multi_cv <- function(path = "checkpoint_multiclasse_cv.rds") {
  candidati <- c(
    "errori_cv", "conf_mat_list", "cv_preds",
    "err_lm_multi", "err_multi", "err_vglm",
    "err_lda", "err_qda",
    "err_lasso_lin", "err_lasso_logit",
    "err_mars", "err_add",
    "err_tree", "err_rf",
    # parametri selezionati
    "lambda_ottimo_lasso_lin", "lambda_ottimo_lasso_logit",
    "lambda_per_fold_lasso_lin", "lambda_per_fold_lasso_logit",
    "B", "dim_albero",        # dimensione ottima albero
    "mtry_ottimo",
    "livelli", "K"
  )
  dati <- Filter(Negate(is.null),
                 setNames(lapply(candidati, function(nm)
                   if (exists(nm, envir = .GlobalEnv)) get(nm, envir = .GlobalEnv) else NULL),
                   candidati))
  saveRDS(dati, path)
  cat(sprintf("[checkpoint] Salvato in '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}

carica_checkpoint_multi_cv <- function(path = "checkpoint_multiclasse_cv.rds") {
  dati <- readRDS(path)
  list2env(dati, envir = .GlobalEnv)
  cat(sprintf("[checkpoint] Caricati da '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}


# ------------------------------------------------------------------------------
# 4. MULTICLASSE SV  (multiclasse_sv.R)
# ------------------------------------------------------------------------------
# Oggetti salvati: errori (data.frame), conf_mat_list, err_* scalari
salva_checkpoint_multi_sv <- function(path = "checkpoint_multiclasse_sv.rds") {
  candidati <- c(
    "errori", "conf_mat_list",
    "err_base", "err_lm", "err_multi", "err_mnl",
    "err_lda", "err_qda",
    "err_lasso", "err_lasso_logit",
    "err_mars", "err_add",
    "err_tree", "err_rf",
    # parametri lambda scelti
    "l_best_s0", "l_best_logit_s0",
    "lambda_seq"   # griglia (leggera, utile per i plot)
  )
  dati <- Filter(Negate(is.null),
                 setNames(lapply(candidati, function(nm)
                   if (exists(nm, envir = .GlobalEnv)) get(nm, envir = .GlobalEnv) else NULL),
                   candidati))
  saveRDS(dati, path)
  cat(sprintf("[checkpoint] Salvato in '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}

carica_checkpoint_multi_sv <- function(path = "checkpoint_multiclasse_sv.rds") {
  dati <- readRDS(path)
  list2env(dati, envir = .GlobalEnv)
  cat(sprintf("[checkpoint] Caricati da '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}


# ------------------------------------------------------------------------------
# 5. REGRESSIONE CV  (regressione_cv.R)
# ------------------------------------------------------------------------------
# Oggetti salvati: err_* scalari, risultati (data.frame), parametri ottimi
# NOTA: il confronto finale usa "err_lasso.int" ma la variabile si chiama
#       err_lasso_int — correggi la stringa in modelli_list (vedi commento nel file).
salva_checkpoint_reg_cv <- function(path = "checkpoint_regressione_cv.rds") {
  candidati <- c(
    "err_base", "err_step",
    "err_ridge", "err_lasso", "err_lasso_int",
    "err_gam_cv",        # in regressione_cv.R si chiama err_gam_cv
    "err_tree", "err_mars",
    "err_ppr", "err_rf",
    "risultati",         # data.frame confronto se già costruito
    # parametri ottimi
    "lambda.ottimo",
    "J",                 # size ottima albero
    "best_nterms",       # PPR
    "best_mtry"          # Random Forest
  )
  dati <- Filter(Negate(is.null),
                 setNames(lapply(candidati, function(nm)
                   if (exists(nm, envir = .GlobalEnv)) get(nm, envir = .GlobalEnv) else NULL),
                   candidati))
  saveRDS(dati, path)
  cat(sprintf("[checkpoint] Salvato in '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}

carica_checkpoint_reg_cv <- function(path = "checkpoint_regressione_cv.rds") {
  dati <- readRDS(path)
  list2env(dati, envir = .GlobalEnv)
  cat(sprintf("[checkpoint] Caricati da '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}


# ------------------------------------------------------------------------------
# 6. REGRESSIONE SV  (regressione_sv.R)
# ------------------------------------------------------------------------------
# Oggetti salvati: err_* scalari, risultati (data.frame), parametri ottimi
# NOTA: stesso bug "err_lasso.int" -> "err_lasso_int" nel confronto finale.
salva_checkpoint_reg_sv <- function(path = "checkpoint_regressione_sv.rds") {
  candidati <- c(
    "err_base", "err_step",
    "err_ridge", "err_lasso", "err_lasso.int",  # .int qui perché è il nome usato in sv
    "err_gam",
    "err_tree", "err_mars",
    "err_ppr", "err_rf",
    "risultati",
    # parametri ottimi
    "lambda.ottimo",
    "best_m",            # PPR nterms ottimo
    "J"                  # albero size ottima
  )
  dati <- Filter(Negate(is.null),
                 setNames(lapply(candidati, function(nm)
                   if (exists(nm, envir = .GlobalEnv)) get(nm, envir = .GlobalEnv) else NULL),
                   candidati))
  saveRDS(dati, path)
  cat(sprintf("[checkpoint] Salvato in '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}

carica_checkpoint_reg_sv <- function(path = "checkpoint_regressione_sv.rds") {
  dati <- readRDS(path)
  list2env(dati, envir = .GlobalEnv)
  cat(sprintf("[checkpoint] Caricati da '%s': %s\n", path, paste(names(dati), collapse = ", ")))
  invisible(dati)
}

