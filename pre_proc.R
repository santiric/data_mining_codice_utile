
# setup-librerie ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)


# IMPORT DATI -------------------------------------------------------------

# csv (stringAsFactors=TRUE se non devi operare su stringhe)
dati <- read.csv('dati.csv', header = TRUE, stringsAsFactors = TRUE)

# excel
dati <- read_excel("dati.xlsx")

# file di testo (.txt) o con delimitatori personalizzati (es. tabulazione)
dati <- read.table("dati.txt", header = TRUE, sep = "\t")

# file molto grandi (lettura ultra-veloce con data.table)
library(data.table)
dati <- fread("dati_grandi.csv")


# differenze tra df e dt:
# --- 1. FILTRO RIGHE (i) ---
df_righe <- df[df$eta > 30, ]  # df: richiede df$ e virgola finale
dt_righe <- dt[eta > 30]       # dt: nomi diretti, nessuna virgola
# --- 2. ESTRAZIONE COLONNE (j) ---
vett_df  <- df$eta             # df: vettore
vett_dt  <- dt[, eta]          # dt: vettore (senza virgolette)
tab_df   <- df[, c("id","eta")]# df: tabella
tab_dt   <- dt[, .(id, eta)]   # dt: tabella (usa .() al posto di list)
# --- 3. FILTRO + ESTRAZIONE CONTEMPORANEO ---
res_df   <- df[df$eta > 30, c("id","eta")]
res_dt   <- dt[eta > 30, .(id, eta)]
# --- 4. SELEZIONE TRAMITE VETTORE DI STRINGHE ---
vars     <- c("id", "eta")
sel_df   <- df[, vars]         # df: standard
sel_dt   <- dt[, ..vars]       # dt: richiede '..' per cercare la variabile esterna






# ESPLORATIVA ---------------------------------------------------

# --- Struttura e Dimensioni ---
dim(dati)                        # Righe e colonne
names(dati)                      # Nomi delle colonne
str(dati)                        # Struttura e tipi di dati
head(dati, 5)                    # Mostra le prime 5 righe

# --- Sintesi e Valori Mancanti ---
summary(dati)                    # Statistiche descrittive base
colSums(is.na(dati))             # Conta quanti NA per colonna

# --- Tabelle di Frequenza ---
table(dati$var, useNA = "always")          # Univariata (con NA)
table(dati$var1, dati$var2, useNA="always")# Bivariata/Incrociata
prop.table(table(dati$var))                # Frequenze relative %

# --- Valori Unici ---
unique(dati$var)                 # Elenco elementi unici
length(unique(dati$var))         # Numero di elementi unici

# --- Grafici Rapidi ---
hist(dati$var_num)               # Istogramma variabile numerica
boxplot(dati$var_num ~ dati$var_cat) # Boxplot numerica vs categorica

# --- Pacchetto DataExplorer (Opzionale) ---
# library(DataExplorer)
plot_str(dati)                   # Struttura visiva dei dati
plot_missing(dati)               # Grafico percentuale NA per colonna
plot_bar(dati)                   # Grafici a barre automatici per categoriche
plot_histogram(dati)             # Istogrammi automatici per numeriche






# SELEZIONE VARIABILI ---------------------------------

# Selezione manuale da file metadati (creare colonna 0/1 da_tenere)
cbind(names(dati), file_metadati$nome_Variabili)
nomi_selezionati <- file_metadati$nome_Variabili[file_metadati$da_tenere == 1]
dati <- dati[, names(dati) %in% nomi_selezionati, drop = FALSE]

# Tramite Regex (es. tutte le variabili con "a" nel nome)
dati_con_a <- dati[, grep("a", names(dati))]

# Eliminare variabili (per nome)
dati$var_inutile <- NULL                            # Elimina singola colonna
dati <- dati[, !names(dati) %in% c("var1", "var2")] # Elimina più colonne

# Per tipo di dato (Es. solo numeriche o categoriche)
dati_num  <- Filter(is.numeric, dati)               # Base R
dati_fact <- dati %>% select(where(is.factor))       # dplyr

# Helpers dplyr (seleziona per prefisso, suffisso o contenuto)
library(dplyr)
dati_sub <- dati %>% select(starts_with("id_"), ends_with("_anno"), contains("target"))







# MERGE -------------------------------------------------------------------

# Left Join standard con selezione colonne (tiene tutte le righe del primo df)
dati <- dati %>% left_join(dati2 %>% select(var_comune, var2_a, var2_b), by = "var_comune")
# elenca le var2 (del secondo dataset) da tenere

# Join con nomi chiave differenti nei due dataset
dati_diff <- dati %>% left_join(dati2, by = c("id_primario" = "id_secondario"))

# Altri tipi di Join comuni
dati_inner <- dati %>% inner_join(dati2, by = "var_comune") # Solo righe con corrispondenza esatta
dati_full  <- dati %>% full_join(dati2, by = "var_comune")  # Tiene tutto di entrambi (inserisce NA)
dati_anti  <- dati %>% anti_join(dati2, by = "var_comune")  # Righe di dati1 NON presenti in dati2

# Unione diretta (Append)
dati_righe <- bind_rows(dati, dati3) # Incolla sotto (allinea colonne per nome automaticamente)
dati_col   <- bind_cols(dati, dati4) # Incolla a destra (richiede stesso numero di righe)






# CONVERSIONI DI TIPO -----------------------------------------------------
library(dplyr)

# pulizia stringhe e na
# rimuove spazi bianchi e converte testi vuoti in veri na
dati <- dati %>% mutate(across(where(is.character), ~ trimws(.x) %>% na_if("") %>% na_if("NA")))

# da factor/char a numeric (robusto)
# converte factor e sistema le virgole europee (es. "12,5" -> 12.5)
to_num <- function(x) as.numeric(gsub(",", ".", as.character(x)))
dati$var_num <- to_num(dati$var_factor)

# da character a factor (globale)
dati <- dati %>% mutate(across(where(is.character), as.factor))

# da numeric/char a factor (bassa cardinalità / dummy)
# converte in factor se la colonna ha al massimo 5 valori unici
dati <- dati %>% mutate(across(where(~ !is.factor(.x) && length(unique(na.omit(.x))) <= 5), as.factor))


# da numeric/char a logical
dati$logica <- as.logical(dati$colonna_0_1) # converte 0/1, "TRUE"/"FALSE"

# auto-guessing dei tipi
dati <- type.convert(dati, as.is = FALSE) # rileva e assegna i tipi corretti








# spacchetta --------------------------------------------------------------

dati2 <- dati[,c(1:3,(4*1):(13*1))]
names(dati2)=c(names(dati2)[1:3],c("minus9","minus8"
                                           ,"minus7","minus6","minus5"
                                           ,"minus4","minus3","minus2",
                                           "minus1"
                                           ,"y"))



# comprimi ----------------------------------------------------------------

aggregate_data <- function(df, group_vars, sum_var, out_name = "n") {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(!!out_name := sum(.data[[sum_var]], na.rm = TRUE),
              .groups = "drop")
}

# utilizzo
dati <- aggregate_data(
  df        = bici2022,
  group_vars = c("date", "start_station_name", "hr_fasce", "fest", "lat", "long"),
  sum_var   = "N"
)

# GESTIONE VALORI MANCANTI (NA) -------------------------------------------

library(dplyr)
library(tidyr)

# conteggio na per colonna
sort(colSums(is.na(dati)), decreasing = TRUE)

# eliminare righe con na
dati <- na.omit(dati)
attr(dati, "na.action") <- NULL
# dati <- dati %>% drop_na(var1, var2) # alternativa per colonne specifiche

# eliminare colonne con troppi na (es. più del 50%)
dati <- dati[, colMeans(is.na(dati)) <= 0.50]

# sostituire codici errore o placeholder con na (numeric)
dati <- dati %>% mutate(across(where(is.numeric), ~ ifelse(.x %in% c(-99, 999), NA, .x)))

# sostituire codici errore o placeholder di testo con na
# usa as.character per non sballare i livelli dei factor e na_character_ per coerenza
dati <- dati %>% mutate(across(where(~ is.character(.x) || is.factor(.x)), 
                               ~ ifelse(as.character(.x) %in% c("unknown", "vuoto", "null", "nd"), NA_character_, as.character(.x))))

# imputazione rapida (mediana per numerici)
dati <- dati %>% mutate(across(where(is.numeric), ~ replace_na(.x, median(.x, na.rm = TRUE))))

# modalita na per i factor (esplicita il livello mancante)
dati <- dati %>% mutate(across(where(is.factor), ~ forcats::fct_na_value_to_level(.x, level = "Mancante")))

# discretizzare quantitative con troppi na
quantilizza <- function(data, vars = NULL, n_quantili = 4, labels = NULL, 
                        suffisso = "_cat", na_label = "NA", sostituisci = TRUE) {
  if (is.null(vars)) vars <- names(data)[sapply(data, is.numeric)]
  if (is.null(labels)) labels <- paste0("Q", seq_len(n_quantili))
  
  for (v in vars) {
    x <- data[[v]]
    breaks <- unique(quantile(x, probs = seq(0, 1, length.out = n_quantili + 1), na.rm = TRUE))
    if (length(breaks) < 2) next
    
    cat_var <- cut(x, breaks = breaks, labels = labels[seq_len(length(breaks) - 1)], include.lowest = TRUE)
    levels(cat_var) <- c(levels(cat_var), na_label)
    cat_var[is.na(cat_var)] <- na_label
    
    nome_output <- if (sostituisci) v else paste0(v, suffisso)
    data[[nome_output]] <- cat_var
  }
  return(data)
}
col_soglia <- colSums(is.na(dati)) > 50
dati[, col_soglia] <- quantilizza(dati[, col_soglia, drop = FALSE])







# TRASFORMAZIONE STRINGHE E DATE ------------------------------------------

# quali tra queste stringhe è contenuta in un'altra?
words <- c("ciao", "mondo", "casa")
text <- "ciao mondo"
sapply(words, grepl, x = text)

# prendi substringa (posizione inizio e fine)
dati$var2 <- substr(dati$var1, start = 1, stop = 10)

# unire testo di piu colonne (con o senza separatore)
dati$unito <- paste(dati$var1, dati$var2, sep = "_")
dati$attaccato <- paste0(dati$var1, dati$var2)

# lunghezza del testo (conteggio caratteri)
dati$lunghezza <- nchar(dati$var1)

# tutto in minuscolo / maiuscolo
dati$minuscolo <- tolower(dati$var1)
dati$maiuscolo <- toupper(dati$var1)

# sostituire testo o rimuovere spazi/caratteri
dati$sostituto <- gsub("vecchio", "nuovo", dati$var1)
dati$solo_numeri <- gsub("[^0-9]", "", dati$var1) # rimuove tutto tranne i numeri

# crea dummy (0/1) se la stringa contiene una parola
dati$has_parola <- as.numeric(grepl("parola_target", dati$var1))

# dividere testo ed estrarre la prima parte (es. da "id_2026" a "id")
dati$prima_parte <- sapply(strsplit(as.character(dati$var1), "_"), `[`, 1)


# Date -> estrai info
library(lubridate) # Caricato automaticamente anche con library(tidyverse)
# ------------------------------------------------------------------------------
# LEGENDA DELLE SIGLE DI LUBRIDATE (da usare nell'argomento 'orders'):
# Y = Anno a 4 cifre (es. 2026)
# y = Anno a 2 cifre (es. 26)
# m = Mese numerico o a parole (es. 05, 5, "Mag", "Maggio")
# d = Giorno del mese (es. 01 o 1)
# H = Ora (sia formato 24h che 12h)
# M = Minuto
# S = Secondo
# 
# FORMATI CHE QUESTO CODICE PUÒ GESTIRE AUTOMATICAMENTE:
# parse_date_time() è tollerante sui separatori. 
# Gestisce indifferentemente trattini (-), barre (/), punti (.), spazi o la "T" degli ISO timestamp.
# Esempi di stringhe compatibili con gli ordini impostati sotto:
# - "2026-05-01 14:30:00"        (Standard internazionale Ymd HMS)
# - "2026-05-01T14:30:00Z"       (ISO 8601, la T e la Z vengono ignorate/gestite)
# - "01/05/2026 14:30"           (Formato italiano/europeo dmy HM)
# - "05-01-2026"                 (Solo data mdy o dmy)
# - "20260501"                   (Data compatta senza separatori Ymd)
# ------------------------------------------------------------------------------
# Parsing flessibile della stringa in un oggetto Date-Time reale
# (Riconosce da solo il formato provando gli ordini indicati da sinistra a destra)
dt_pulito <- parse_date_time(
  dati$to_timestamp, 
  orders = c("Ymd HMS", "Ymd HM", "dmy HMS", "dmy HM", "mdy HMS", "Ymd")
) # to_timestamp contiene le date
# Estrazione dei componenti numerici e rimozione della vecchia colonna
dati <- dati %>%
  mutate(
    anno   = year(dt_pulito),
    mese   = month(dt_pulito),
    giorno = day(dt_pulito),
    ora    = hour(dt_pulito)
  ) %>%
  select(-to_timestamp)




# RICODIFICA, CREAZIONE VARIABILI E DUMMY ENCODING ------------------------

library(dplyr)
library(forcats)

# rinominare modalita di variabili categoriali
dati$var <- fct_recode(dati$var, "new_name_1" = "old_name_1", "new_name_2" = "old_name_2")

# raggruppare modalita con case_when
dati$var_ragg <- factor(case_when(
  dati$var %in% c("A", "B") ~ "gruppo1",
  dati$var == "C" ~ "gruppo2",
  TRUE ~ as.character(dati$var) # lascia invariati gli altri livelli
))

# raggruppare i livelli meno frequenti in una categoria "altro"
dati$var_lump <- fct_lump_prop(dati$var, prop = 0.05, other_level = "altro") # per percentuale minima
# dati$var_lump <- fct_lump_n(dati$var, n = 5, other_level = "altro")        # tiene solo i top n piu frequenti

# cambiare l'ordine dei livelli (impostare la baseline/riferimento per i modelli)
dati$var <- fct_relevel(dati$var, "livello_riferimento")

# nuova variabile dicotomica semplice
dati$var_dic <- ifelse(condizione, "si", "no")

# dummy 0/1 da una soglia numerica
dati$var_soglia <- as.numeric(dati$var_num > 100)

# discretizzare una variabile numerica in intervalli (binning)
dati$var_classi <- cut(dati$var_num, breaks = c(-Inf, 18, 65, Inf), labels = c("giovani", "adulti", "anziani"))

# da elenco categorie a dummy (es. "wifi | piscina")
mod <- trimws(unlist(strsplit(as.character(dati$var), "|", fixed = TRUE)))
mod_freq <- names(which(table(mod) > 7000)) 
for (m in mod_freq) {
  nome_col <- make.names(tolower(gsub(" ", "_", m)))
  dati[[nome_col]] <- as.numeric(grepl(m, dati$var, fixed = TRUE))
}

# one-hot encoding standard (singola variabile)
country_ids <- model.matrix(~var, dati)
dati <- cbind(dati, country_ids)
dati$var <- NULL
dati$`(Intercept)` <- NULL



# PRINT RISPOSTA E DATASET ----------------------------------------------------------

# Printa bene una table con ggplot, utile per risposta categoriale
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
print_table(table(dati$var), title = "Risposta:")

# printa dataframe post preproc
knitr::kable(data.frame(`Nome Variabile` = names(dati), check.names = FALSE), format = "pipe")




# SALVA ENV ---------------------------------------------------------------


# Salva tutto l'environment
save.image("mio_env.RData")

# Salva escludendo oggetti pesanti (versione custom/libreria o alternativa base: save())
save(list = setdiff(ls(), c("df_raw", "tmp")), file = "mio_env.RData")

# Ripristina l'environment
load("mio_env.RData")