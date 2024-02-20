################################################## MBC et EN analyse #####################################################################


#### ___ ####

# 0.1 - Package ----

library(fs)          # Direction vers tes fichiers
library(crayon)      # couleurs dans le code
library(quanteda)    # Package pour le lire les dictionnaires 
library(tidyverse)   # Data wrangling 
library(clessnverse) # Package pour utiliser plus facilement un dictionnaire


# 0.2 - Dictionnaire ----

# Dictionnaire anglais

lexicoder_en <- dictionary(file = "_dictionary/policy_agendas_english.lcd", format = "yoshikoder") # Fichier lcd

# Dictionnaire français

lexicoder_fr <- read_csv("_dictionary/lexicoder_merged.csv") |> # Lire le csv
  select(categorie, traductionGoogle) |>  # On sélectionne les bonnes variables (catégories + mots clés)
  unstack(traductionGoogle~categorie) |>  # On transforme la catégorie en liste en spécifiant bien les catégories et les mots clés
  dictionary()                            # On transforme le tout en dictionnaire avec la fonction de quanteda

# Dictionnaire "home made"

cultural_value_dictionary <- list(racisme      = c("racisme* systémique*", "personne* racisée*", "personne* de couleur", "ethni*", "discrimination raciale*", "préjugé* racia*", "islamophobie*", "antisémitisme*", "colonialisme*", "xénophobie*", "islam*", "musulman*"),
                                  nationalisme = c("souveraineté du québec", "nationalisme*", "patriotisme*", "chauvinisme*", "autodétermination*", "séparatisme*", "souverainisme*")) |> # on prépare une liste avec des catégories + mots clés
  dictionary() # On transforme la list() en dictionnaire avec la fonction de quanteda

# 0.2.1 - Fusionner deux dictionnaires ----

cultural_value_dictionary_m <- cultural_value_dictionary |> stack() # On transforme le dictionnaire en base de données

lexicoder_fr_m <- lexicoder_fr |> stack() # Même chose ici

new_dictionary <- bind_rows(cultural_value_dictionary_m, lexicoder_fr_m) |> # On fusionne les deux dictionnaires
  unstack(values~ind) |>  # Tansformation en list()
  dictionary()            # Puis en dictionnaire

# 0.3 - Fonction ----

# Pour utiliser la fonction si vous n'avez pas réussi à télécharger clessnverse 

run_dictionary <- function(data, text, dictionary) {
  tictoc::tic()
  if ( is.data.frame(data) != "TRUE") {
    stop(crayon::yellow('the argument "data" needs to be a dataframe'))
  }
  data <- data %>% dplyr::mutate(text = {{text}})
  if ( is.character(data$text) != "TRUE") {
    stop(crayon::yellow('The variable "text" needs to be a character vector'))
  }
  corpus <- quanteda::tokens(data$text)
  if ( quanteda::is.dictionary(dictionary) != "TRUE") {
    stop(crayon::yellow('Your "dictionary" needs to be in a dictionary format\n For more information:" https://quanteda.io/reference/dictionary.html'))
  }
  dfm    <- quanteda::dfm(quanteda::tokens_lookup(corpus, dictionary, nested_scope = "dictionary"))
  message(crayon::green("100% expressions/words found"))
  dataFinal   <- quanteda::convert(dfm, to = "data.frame")
  tictoc::toc()
  return(dataFinal)
}

# 0.4 - Exemple avec les données de LIPAD: https://www.lipad.ca/data/ ----

# 2019-6-10.csv (données du 10 juin 2019)

Data_parl_1 <- read_csv("_data/_parlement_canadien/2019-6-10.csv")

# 2019-6-11.csv (données du 11 juin 2019)

Data_parl_2 <- read_csv("_data/_parlement_canadien/2019-6-11.csv")

# Fusion des bases de données

Data_parl_merged <- bind_rows(Data_parl_1, Data_parl_2)

# 0.4.1 - Autres façon de charger nos données (avec un loop) ----

# Note --> Il faut avoir une structure dans nos fichiers pour faire ça (va de "2019-6-3" à "2019-6-20")

files <- dir_ls("_data/_parlement_canadien", regexp = "2019") # pour naviguer dans ses dossiers

Data_parl <- files |> 
  map(~read_csv(files)) |>
  reduce(bind_rows) |>
  distinct()

#### ___ ####

# 1 - Analyse de dictionnaire de base avec lexicoder en anglais ----

# Nettoyage de base et sélection des variables pour exemple 1

Data_parl_pre_clean_1 <- Data_parl |> 
  select(speechdate, speechtext, speakerparty) |> 
  ## Nettoyage de base ##
  mutate(speechtext = tolower(speechtext)) |> 
  ## remove NA's ##
  na.omit()

# Nettoyage de base et sélection des variables pour exemple 2

Data_parl_pre_clean_2 <- Data_parl |> 
  select(speechtext, speakerparty) |> 
  ## Nettoyage de base ##
  mutate(speechtext = tolower(speechtext)) |> 
  ## remove NA's ##
  na.omit()

# 1.1 - Exemple 1: analyse du dictionnaire par date ----

run_dictionary(data       = Data_parl_pre_clean_1,  # Data
               text       = speechtext,             # Text
               dictionary = lexicoder_en) |>        # dictionnaire
  bind_cols(Data_parl_pre_clean_1) |>                    # On remet les variables que je veux
  select(-c(doc_id, speechtext)) |>                      # On retire les variables qu'on a plus besoin
  pivot_longer(!c(speechdate, speakerparty), names_to = "categorie", values_to="n") |> # On structure notre base de données
  ungroup() |> 
  na.omit() |> 
  group_by(speechdate, speakerparty, categorie) |> 
  summarise(n=sum(n)) |> 
  ## On calcule les proportions + nettoyage des variables ##
  mutate(prop = round(n/sum(n),4)*100,
         speakerparty = case_when(speakerparty == "Conservative"          ~ "Conservateur",
                                  speakerparty == "Green Party"           ~ "Vert",
                                  speakerparty == "New Democratic Party"  ~ "NDP",
                                  speakerparty == "Liberal"               ~ "Libéral",
                                  speakerparty == "Independent"           ~ "Indépendant"),
         categorie = case_when(categorie == "macroeconomics" ~ "Économie",
                               categorie == "crime" ~ "Crime",
                               categorie == "healthcare" ~ "Assurance maladie",
                               categorie == "defence" ~ "Défense",
                               T ~ as.character(categorie))) |> 
  ### On filtre les catégories du dictionnaire qui nous intéressent ###
  filter(categorie %in% c("Économie", "Crime", "Assurance maladie", "Défense")) |> 
  ggplot(aes(x = speechdate, y = prop, color = categorie)) +
  geom_line(linewidth=1) +
  facet_wrap(~speakerparty) +
  scale_color_manual("", values = c("#e60049", "#0bb4ff", "#50e991", "#e6d800")) +
  labs(x = "",
       y = "Proportion (%)\n",
       title = "Enjeux les plus discutés lors des débats parlementaires au Canada") +
  theme_bw()

# 1.2 - Exemple 2: analyse du dictionnaire par parti  ----

run_dictionary(data       = Data_parl_pre_clean_2, 
                    text       = speechtext, 
                    dictionary = lexicoder_en) |> 
  # On reprend les mêmes colonnes pour l'analyse #
  bind_cols(Data_parl_pre_clean_2) |> 
  select(-c(doc_id,speechtext)) |> 
  # Pivote la base de données pour voir la proportion par speaker #
  pivot_longer(!speakerparty, names_to = "categorie", values_to="n") |> 
  ungroup() |> 
  group_by(speakerparty, categorie) |> 
  summarise(n=sum(n)) |> 
  mutate(prop = round(n/sum(n),4)*100,
         speakerparty = case_when(speakerparty == "Conservative"          ~ "Conservateur",
                                  speakerparty == "Green Party"           ~ "Vert",
                                  speakerparty == "New Democratic Party"  ~ "NDP",
                                  speakerparty == "Liberal"               ~ "Libéral",
                                  T ~ as.character(speakerparty)),
         categorie = case_when(categorie == "macroeconomics" ~ "Économie",
                               categorie == "crime" ~ "Crime",
                               categorie == "healthcare" ~ "Assurance maladie",
                               categorie == "defence" ~ "Défense",
                               T ~ as.character(categorie))) |> 
  na.omit() |> 
  # À des fins d'exemple on garde seulement certains thèmes + on enlève les indépendants #
  filter(categorie %in% c("Économie", "Crime", "Assurance maladie", "Défense"),
         !speakerparty == "Independent") |> 
  ggplot(aes(x = categorie, y = prop, fill = speakerparty)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = c("#0088CE", "#002395", "#EA6D6A", "#FF5800", "#3D9B35")) +
  coord_flip() +
  labs(x = "Enjeux\n",
       y = "\nProportion (%)",
       title = "Enjeux les plus discutés par les partis politiques canadiens") +
  theme_bw() +
  ## theme() en fonction des dimensions dans Quarto ##
  theme(title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20, color = "black"))



