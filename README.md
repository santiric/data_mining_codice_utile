# Data Mining – Università di Padova

Questo repository raccoglie script in R e template per la reportistica sviluppati per il corso di Data Mining dell'Università degli Studi di Padova.

Il materiale è pensato come base di lavoro per affrontare le principali fasi dell'analisi dati: 
preparazione dei dati, stima dei modelli, convalida valutazione delle performance. 
Gli script possono essere adattati a dataset e problemi diversi, sia con finalità previsive che interpretative.

Il codice non è pensato per essere utilizzato in modo automatico. 
Gli script costituiscono una traccia di partenza, ma la gestione dei valori mancanti, la trasformazione delle variabili, 
la scelta dei modelli e l'interpretazione dei risultati devono essere adattate alle caratteristiche del dataset e del problema analizzato.

## Struttura del repository

Gli script sono organizzati secondo il flusso di lavoro dell'analisi e, 
all'interno dei problemi di classificazione e regressione, in base alla strategia di convalida utilizzata:

* `CV`: Cross-Validation, cioè convalida incrociata.
* `SV`: Stima e Verifica, con suddivisione dei dati in training set e test set.

### Pre-processing e pulizia dei dati

`pre_proc.R` è lo script di riferimento per la preparazione iniziale del dataset. 
Contiene procedure per l'importazione dei dati, l'esplorazione del dataset, la gestione dei valori mancanti, la manipolazione delle stringhe, 
il parsing delle date e la ricodifica delle variabili. [1]

### Classificazione binaria

`classificazione_cv.R` e `classificazione_sv.R` sono dedicati ai problemi con variabile risposta dicotomica. 
Includono una funzione personalizzata, `classification_metrics()`, per il calcolo di Accuracy, Sensitivity, Specificity, F1 Score, FPR e FNR. 
Sono inoltre presenti procedure per la costruzione e il confronto delle curve ROC e delle curve Lift. [2, 3]

### Classificazione multiclasse

`multiclasse_cv.R` e `multiclasse_sv.R` estendono l'analisi ai problemi con più di due categorie nella variabile risposta. [4, 5]

La valutazione utilizza la funzione `ce()` per il calcolo dell'errore di classificazione e procedure basate su matrici di confusione
per ottenere Precision e Recall per le singole classi. [4, 5]

### Regressione

`regressione_cv.R` e `regressione_sv.R` sono dedicati ai problemi di regressione con variabile risposta quantitativa continua.
La funzione `errore()` consente di valutare le previsioni utilizzando MSE o MAE. [6, 7]

`regressione_nonparametrica.R` raccoglie invece procedure per la regressione non parametrica, tra cui LOESS, regression spline tramite `bs()`,
smoothing spline e lisciatori bivariati spaziali tramite il pacchetto `sm`. [8]

### Funzioni di supporto

`utils.R` contiene funzioni utilizzate trasversalmente dagli altri script. Tra queste rientrano strumenti per l'estrazione e la visualizzazione dei coefficienti 
LASSO tramite `ggplot2`, procedure per il sottocampionamento bilanciato e controlli sulla coerenza dei livelli dei `factor`. [9]

Il file include inoltre un sistema di checkpoint che permette di salvare e ricaricare i risultati dei modelli,
evitando di ripetere calcoli computazionalmente costosi. [9]

### Template per la reportistica

I file `.docx`, come `classificazione_cv_template.docx` e `regressione_sv_template.docx`, 
forniscono una struttura preimpostata per la stesura delle relazioni. 
I template sono pensati per organizzare i risultati, commentare le metriche e presentare le conclusioni dell'analisi.

## Modelli implementati

Il repository comprende diverse tecniche di classificazione e regressione:

* **Modelli lineari e logistici**: regressione lineare e logistica, modelli stepwise con selezione forward e backward. [2, 6]
* **Modelli multiclasse**: regressione multinomiale tramite `nnet` e Baseline Category Logit tramite `VGAM`. [4, 5]
* **Regolarizzazione**: Ridge, LASSO e LASSO con interazioni tramite `glmnet`, con selezione del parametro `lambda`. [2, 6]
* **Analisi discriminante**: Linear Discriminant Analysis (LDA) e Quadratic Discriminant Analysis (QDA) tramite `MASS`. [2, 4]
* **Generalized Additive Models**: modelli additivi generalizzati tramite `gam`, con selezione stepwise dei gradi di libertà. [2, 6]
* **MARS**: Multivariate Adaptive Regression Splines tramite `polymars` per regressione e classificazione binaria e `polyclass` per problemi multiclasse. [2, 4, 6]
* **Alberi decisionali**: alberi tramite `tree` e `rpart`, inclusi metodi di pruning. [2, 3, 4]
* **Metodi ensemble**: Random Forest tramite `randomForest` e `ranger`, e Boosting tramite `ada`. [3, 4, 7]
* **Projection Pursuit Regression**: regressione tramite componenti non lineari stimate con PPR. [6, 7]

## Dipendenze

Gli script utilizzano, a seconda del problema analizzato, i seguenti pacchetti R.

### Gestione dei dati e delle date

`tidyverse`, `dplyr`, `stringr`, `forcats`, `lubridate`, `data.table`, `readxl`. [1]

### Modellistica e regolarizzazione

`MASS`, `glmnet`, `nnet`. [2, 4, 6]

### Modelli avanzati e non parametrici

`VGAM`, `gam`, `polspline`, `ada`, `sm`, `splines`. [3, 4, 6, 8]

### Alberi ed ensemble

`tree`, `rpart`, `rpart.plot`, `randomForest`, `ranger`. [2, 3, 4, 6]

### Grafica

`ggplot2`, utilizzato in particolare dalle funzioni presenti in `utils.R`. [9]

## Flusso di lavoro

1. **Pre-processing**
   Adattare `pre_proc.R` al dataset in esame, occupandosi dell'importazione, della pulizia e della preparazione dei dati. [1]

2. **Scelta del problema**
   Selezionare lo script appropriato per classificazione binaria, classificazione multiclasse o regressione. [2, 4, 6]

3. **Scelta della validazione**
   Utilizzare gli script `_cv` per la cross-validation o gli script `_sv` per la suddivisione tra training set e test set. [2, 3]

4. **Stima e confronto dei modelli**
   Confrontare i diversi approcci sulla base delle metriche di valutazione e verificare eventuali problemi numerici o di convergenza. [4, 5]

5. **Salvataggio dei risultati**
   Utilizzare il sistema di checkpoint presente in `utils.R` per evitare di ripetere calcoli già eseguiti. [9]

6. **Reportistica**
   Utilizzare i template Word per organizzare i risultati, motivare le scelte metodologiche e discutere le performance dei modelli.
