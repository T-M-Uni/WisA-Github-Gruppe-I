install.packages("dplyr")
library(dplyr)

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
  
  return(stats)
}

# deskr_factor: Funktion für deskriptive Statistiken für kategoriale Variablen
# input: - x: kategorieller Vektor
# output: - Liste aus Kennwerten fuer Kategorielle Daten
deskr_factor <- function(x) {
  if (!is.factor(x) && !is.character(x)) {
    stop("Die Variable muss kategorial (Faktor oder Charakter) sein.")
  }
  
  # Haeufigkeitstabelle
  freq_table <- table(x)
  
  # Prozentsaetze
  percentages <- prop.table(freq_table) * 100
  
  # Gleichverteilungstest
  exp <- expected_freq(x)
  
  chi2 <- chisq.test(freq_table, p = exp, rescale.p = TRUE)
  
  # Streuung
  gini_index <- gini(x)
  
  # Ausgabe als Liste
  statistic <- list(
    Haeufigkeit = freq_table,
    Prozentsaetze = percentages,
    Chi2 = unname(chi2$statistic),
    Chi2.p = chi2$p.value,
    Gini = gini_index
  )
  
  return(statistic)
}

# Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt
# tabelle mit Anteilen zurueckgeben
bivariat_kategorial <- function(var1, var2) {
  tabelle <- table(var1, var2)
  
  prop_tabelle <- prop.table(tabelle, margin = 1)
  chi2 <- chisq.test(tabelle)
  cramerV <- sqrt(chi2$statistic / (sum(tabelle)) * (min(nrow(tabelle))-1))
  
  return(list(
    probs = prop_tabelle,
    chi2 = unname(chi2$statistic),
    p = chi2$p.value,
    cramerV = cramerV
  ))
}


# Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammengang zwischen einer metrischen und einer dichotomen Variablen berechnet und ausgibt

bivariate_statistik <- function(data, metrische_var, dichotome_var) {
  # Sicherstellen, dass die zweite Variable dichotom ist
  unique_values <- unique(data[[dichotome_var]])
  if (length(unique_values) != 2) {
    stop("Die zweite Variable muss dichotom sein.")
  }
  
  # Deskriptive Statistiken Tabelle ohne %>%
  stats <- summarise(
    group_by(data, !!as.name(dichotome_var)),
    Mean = mean(.data[[metrische_var]], na.rm = TRUE),
    Median = median(.data[[metrische_var]], na.rm = TRUE),
    SD = sd(.data[[metrische_var]], na.rm = TRUE),
    Count = n()
  )
  
  # Unterschied der Mittelwerte
  mittelwert_gruppen <- stats$Mean
  differenz_mittelwerte <- diff(mittelwert_gruppen)
  
  # Berechnung Cohen's d
  gruppe_1 <- data[data[[dichotome_var]] == unique_values[1], ][[metrische_var]]
  gruppe_2 <- data[data[[dichotome_var]] == unique_values[2], ][[metrische_var]]
  
  pooled_sd <- sqrt((var(gruppe_1, na.rm = TRUE) + var(gruppe_2, na.rm = TRUE)) / 2)
  
  cohens_d <- differenz_mittelwerte / pooled_sd
  
  ergebnisse <- list(
    Deskriptive_Statistiken = stats,
    Mittelwertsunterschied = differenz_mittelwerte,
    Cohens_d = cohens_d
  )
  
  return(ergebnisse)
}

# Wenn ihr noch Ideen für andere Maße habt, die für diese Teilaufgabe relevant sein könnten, sagt gerne Bescheid, ich bin mir nicht ganz sicher ob ich alles abgedeckt habe




# Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt

visualize_categorical <- function(data, var1, var2, var3, var4 = NULL) {
  if (is.null(var4)) {
    # Kontingenztabelle für drei Variablen
    contingency_table <- table(data[[var1]], data[[var2]], data[[var3]])
    
    # Mosaikplot für drei Variablen
    mosaicplot(contingency_table, 
               main = paste("Visualisierung von", var1, "nach", var2, "und", var3),
               xlab = var1,  
               ylab = paste(var2, "und", var3),
               color = c("red", "blue", "green"),
               las = 1,  
               cex.axis = 0.8, 
               border = "gray")
  } else {
    # Kontingenztabelle für vier Variablen
    contingency_table <- table(data[[var1]], data[[var2]], data[[var3]], data[[var4]])
    
    # Mosaikplot für vier Variablen
    mosaicplot(contingency_table, 
               main = paste("Visualisierung von", var1, "nach", var2, var3, "und", var4),
               xlab = var1,  
               ylab = paste(var2, var3, "und", var4),
               color = c("red", "blue", "green", "orange"),
               las = 1,  
               cex.axis = 0.8,
               border = "gray")
  }
}





# Freiwillig: weitere zur Deskription und Visualisierung geeignete Funktionen

# Funktion zur Berechnung der Korrelation zwischen zwei metrischen Variablen
korrelation_metrisch <- function(df, var1, var2) {
  if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) {
    stop("Beide Variablen müssen metrisch sein.")
  }
  return(cor(df[[var1]], df[[var2]], use = "complete.obs"))
}

# Funktion zur Erstellung eines Histogramms für eine metrische Variable
histogram_metrisch <- function(df, var) {
  if (!is.numeric(df[[var]])) {
    stop("Die Variable muss metrisch sein.")
  }
  hist(df[[var]], main = paste("Histogramm von", var), xlab = var, col = "lightblue", border = "black")
}

