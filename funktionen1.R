
# deskr_metric: Funktion für deskriptive Statistiken für metrische Variablen
# Input: - x: numerischer Vektor
# output: - Liste vershiedener Deskriptiver Werte
deskr_metric <- function(x) {
  if (!is.numeric(x)) {
    stop("Die Variable muss numerisch sein.")
  }
  
  # Berechnungen
  stats <- list(
    Mittelwert = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Standardabweichung = sd(x, na.rm = TRUE),
    Varianz = var(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    Quantile_25 = quantile(x, 0.25, na.rm = TRUE),
    Quantile_50 = quantile(x, 0.50, na.rm = TRUE),  # entspricht dem Median
    Quantile_75 = quantile(x, 0.75, na.rm = TRUE)
  )
  
  return(statistik)
}

# deskr_factor: Funktion für deskriptive Statistiken für kategoriale Variablen
# input: - x: kategorieller Vektor
# output: - Liste asu Kennwerten fuer Kategorielle Daten
deskr_factor <- function(x) {
  if (!is.factor(x) && !is.character(x)) {
    stop("Die Variable muss kategorial (Faktor oder Charakter) sein.")
  }
  
  # Häufigkeitstabelle
  freq_table <- table(x)
  
  # Prozentsätze
  percentages <- prop.table(freq_table) * 100
  
  # Gleichverteilungstest
  exp <- expected_freq(x)
  
  chi2 <- chisq.test(freq_table, p = exp, rescale.p = TRUE)
  
  # Streuung
  gini_index <- gini(x)
  
  # Ausgabe als Liste
  statistic <- list(
    Häufigkeit = freq_table,
    Prozentsätze = percentages,
    Chi2 = unname(chi2$statistic),
    Chi2.p = chi2$p.value,
    Gini = gini_index
  )
  
  return(statistic)
}

