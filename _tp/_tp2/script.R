library(tidyverse)

# importation des données
library(readr)
library(readxl)
revenu_faible_incapacite <- read_excel("_tp/_tp2/Donnees/revenu_faible_incapacite.xlsx")

life_expectancy <- read.table( "_tp/_tp2/Donnees/E0per.txt", header = TRUE, skip = 1)
View(life_expectancy)

# Nettoyage de la base de données sur l'espérance de vie
# On garde les données observés seulement entre 2013 et 2021
esperance_vie <- life_expectancy %>%
  rename(Annee = Year, EsperanceDeVie = Total)

esperance_vie <- esperance_vie %>% select(-Male, -Female) %>% filter(Annee >= 2013)

#nettoyage de la base de données sur la pauvreté des personnes en état d'incapacité

pauvrete_incapacite <- revenu_faible_incapacite %>% rename(Annee = Statistiques,
                                                      Pourcentage = Percent_low_rev)

pauvrete_incapacite <- pauvrete_incapacite %>% filter(Annee<=2019)

Base_donnees_final <- merge(pauvrete_incapacite, esperance_vie, by = "Annee")

 # Créer le graphique montrant une régression linéaire
  graphique_regression <- ggplot(Base_donnees_final, aes(x = EsperanceDeVie, y = Pourcentage)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, 
                linetype = "solid", linewidth = 1.5) +
    labs(title = "Régression entre l'Espérance de Vie et l'Évolution de la Pauvreté chez les Personnes Âgées",
         x = "Espérance de Vie",
         y = "Évolution de la Pauvreté") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
    theme_minimal()
  
print(graphique_regression)  
  

