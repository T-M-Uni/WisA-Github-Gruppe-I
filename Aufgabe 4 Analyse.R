# Aufgabe 4 Analyse


library(dplyr)
library(tidyr)

#1. Deskriptive Analyse für metrische Variablen

metrische_variablen <- c("Age", "Fare")
for (var in metrische_variablen) {
  print(paste("Deskriptive Statistik für:", var))
  print(deskr_metric(titanic[[var]]))
}

# [1] "Deskriptive Statistik für: Age"
# $Mittelwert
# [1] 29.69912
# 
# $Median
# [1] 28
# 
# $Standardabweichung
# [1] 14.5265
# 
# $Varianz
# [1] 211.0191
# 
# $Minimum
# [1] 0.42
# 
# $Maximum
# [1] 80
# 
# $Quantile_25
# 25% 
# 20.125 
# 
# $Quantile_50
# 50% 
# 28 
# 
# $Quantile_75
# 75% 
# 38 
# 
# [1] "Deskriptive Statistik für: Fare"
# $Mittelwert
# [1] 32.20421
# 
# $Median
# [1] 14.4542
# 
# $Standardabweichung
# [1] 49.69343
# 
# $Varianz
# [1] 2469.437
# 
# $Minimum
# [1] 0
# 
# $Maximum
# [1] 512.3292
# 
# $Quantile_25
# 25% 
# 7.9104 
# 
# $Quantile_50
# 50% 
# 14.4542 
# 
# $Quantile_75
# 75% 
# 31 



#2. Deskriptive Analyse für kategoriale Variablen (z. B. Geschlecht, Einschiffungshafen)

kategorische_variablen <- c("Sex", "Embarked")
for (var in kategorische_variablen) {
  print(paste("Deskriptive Statistik für:", var))
  print(deskr_factor(titanic[[var]]))
}

# [1] "Deskriptive Statistik für: Sex"
# $Haeufigkeit
# x
# female   male 
# 314    577 
# 
# $Prozentsaetze
# x
# female    male 
# 35.2413 64.7587 
# 
# $Chi2
# [1] 77.63075
# 
# $Chi2.p
# [1] 1.24221e-18
# 
# $Gini
# [1] 0.4564362
# 
# [1] "Deskriptive Statistik für: Embarked"
# $Haeufigkeit
# x
# C   Q   S 
# 2 168  77 644 
# 
# $Prozentsaetze
# x
# C          Q          S 
# 0.2244669 18.8552189  8.6419753 72.2783389 
# 
# $Chi2
# [1] 1124.232
# 
# $Chi2.p
# [1] 2.012828e-243
# 
# $Gini
# [1] 0.4345588


#3. Analyse des Zusammenhangs zwischen Überlebensrate und anderen Variablen

print("Bivariate Analyse von Überlebensrate mit Klasse der Reisenden")
print(bivariat_kategorial(titanic$Survived, titanic$Pclass))

# "Bivariate Analyse von Überlebensrate mit Klasse der Reisenden"

# $probs
#     var2
# var1         1         2         3
# 0 0.1457195 0.1766849 0.6775956
# 1 0.3976608 0.2543860 0.3479532
# 
# $chi2
# [1] 102.889
# 
# $p
# [1] 4.549252e-23
# 
# $cramerV
# X-squared 
# 0.3398174 


print("Bivariate Analyse von Überlebensrate mit Geschlecht")
print(bivariat_kategorial(titanic$Survived, titanic$Sex))

# [1] "Bivariate Analyse von Überlebensrate mit Geschlecht"
# $probs
#   var2
# var1    female      male
# 0 0.1475410 0.8524590
# 1 0.6812865 0.3187135
# 
# $chi2
# [1] 260.717
# 
# $p
# [1] 1.197357e-58
# 
# $cramerV
# X-squared 
# 0.5409359 


#4. Untersuchung des Ticketpreises in Bezug auf das Überleben
print("Analyse des Ticketpreises zwischen Überlebenden und Nicht-Überlebenden:")
bivariate_statistik(titanic, "Fare", "Survived")

# [1] "Analyse des Ticketpreises zwischen Überlebenden und Nicht-Überlebenden:"

# $Deskriptive_Statistiken
# A tibble: 2 × 5
# Survived  Mean Median    SD Count
#       <int> <dbl>  <dbl> <dbl> <int>
#   1      0  22.1   10.5  31.4   549
#   2      1  48.4   26    66.6   342
# 
# $Mittelwertsunterschied
# [1] 26.27752
# 
# $Cohens_d
# [1] 0.5047595

# Ergänzung 4:
bivariate_statistik(titanic, "Fare", "Sex")
# $Deskriptive_Statistiken
# A tibble: 2 × 5
#  Sex     Mean Median    SD Count
#  <chr>  <dbl>  <dbl> <dbl> <int>
# 1 female  44.5   23    58.0   314
# 2 male    25.5   10.5  43.1   577

# $Mittelwertsunterschied
# [1] -18.95592

# $Cohens_d
# [1] -0.3708785
bivariat_kategorial(titanic$Pclass, titanic$Sex)
#$probs
#   var2
#var1    female      male
#   1 0.2932790 0.7067210
#   2 0.4130435 0.5869565
#   3 0.4351852 0.5648148

#$chi2
#[1] 16.9715

#$p
#[1] 0.0002063886

#$cramerV
#X-squared 
#0.1951804 

#5. Visualisierung: Histogramm des Ticketpreises
histogram_metrisch(titanic, "Fare")

#Visualisierung: Mosaikplot für Überlebensrate nach Klasse und Geschlecht
visualize_categorical(titanic, "Survived", "Pclass", "Sex")

#Weitere optionale Analysen
#Korrelation zwischen Alter und Ticketpreis
print("Korrelation zwischen Alter und Ticketpreis:")
print(korrelation_metrisch(titanic, "Age", "Fare"))

# [1] "Korrelation zwischen Alter und Ticketpreis:"
# [1] 0.09606669
      
