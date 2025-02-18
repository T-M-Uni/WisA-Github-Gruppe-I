
# expected_freq: betimmt erwartete Haeufikeiten fuer Chi^2-Test
expected_freq <- function(x){
  freq_table <- table(x)
  
  exp <- rep(sum(freq_table) / length(freq_table), length(freq_table))
  p <-  exp / sum(exp)
  return(p)
}

# gini: betimmt Gini-index
gini <- function(x){
  p = prop.table(table(x))
  g = 1 - sum((p)^2)
  
  return(g)
}

# Helferfunktion berechnet Cohen's d
cohens_d <- function(gruppe_1, gruppe_2) {
  
  pooled_sd <- sqrt((var(gruppe_1, na.rm = TRUE) + var(gruppe_2, na.rm = TRUE)) / 2)
  
  differenz_mittelwerte <- mean(gruppe_1, na.rm = TRUE) - mean(gruppe_2, na.rm = TRUE)
  
  cohen <- differenz_mittelwerte / pooled_sd
  
  return(cohen)
}



# Helferfunktion, die Dichotomie überprüft

dichotomie <- function(variable) {
  unique_values <- unique(variable)
  
  if (length(unique_values) != 2) {
    stop("Die Variable muss genau zwei Kategorien haben.")
  }
  
  return(TRUE)
}
