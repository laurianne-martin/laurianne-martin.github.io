library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Importer la base de données
PPB <- read_excel("votre_chemin_dacces_vers_PPB.xlsx", col_names = TRUE)

# Séparer les variables en colonnes
PPB <- PPB %>%
  separate(col = 1, 
           into = c("id", "first_last", "province", "district", "gender", 
                    "year_of_birth", "year_of_death", "place_of_birth", 
                    "career", "religion", "type", "mmd", "year", 
                    "candidate_votes", "total_valid_votes", "candidate_percent", 
                    "margin", "exit_year", "party", "minister_after_election", 
                    "pre_relative", "post_relative", "familytie1name", 
                    "familytie1link", "familytie2name", "familytie2link", 
                    "familytie3link", "familytie3name", "familytie4link", 
                    "familytie4name", "familytie5link", "familytie5name", 
                    "familytie6link", "familytie6name", "familytie7link", 
                    "familytie7name", "familytie8link", "familytie8name", 
                    "familytie9link", "familytie9name", "familytie10link", 
                    "familytie10name", "familytie11link", "familytie11name", 
                    "familytie12link", "familytie12name", "familytie13link", 
                    "familytie13name"), 
           sep = ",")


#Filtrer pour les variables nécessaires à l'analyse
PPB <- PPB %>%
  select(province, career, year) %>%

# Préparer les données pour la visualisation
PPB_summary <- PPB %>%
  group_by(year, province, career) %>%
  summarise(count = n(), .groups = "drop")

# Effectuer les premières visualisations proposées par ChatGPT

ggplot(PPB_summary, aes(x = year, y = count, color = career)) +
  geom_line(size = 1) +
  facet_wrap(~ province) +
  labs(title = "Évolution des carrières des politiciens par province",
       x = "Année",
       y = "Nombre de politiciens",
       color = "Carrière") +
  theme_minimal()


ggplot(PPB_summary, aes(x = year, y = count, fill = career)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ province) +
  labs(title = "Répartition des carrières des politiciens par année et province",
       x = "Année",
       y = "Nombre de politiciens",
       fill = "Carrière") +
  theme_minimal()

### Problème 1: beaucoup trop de variables. Solution : Regrouper les carrières en catégories.
### Problème 2: l'échelle n'est pas absolue. Solution : Remplacer l'axe des y avec un pourcentage plutôt que le nombre de parlementaires.
### Problème 3: axe des x illisible à cause des années. Solution: Afficher chaque 50 ans
### Le geom_line permet de mieux voir l'évolution, mais la ligne est trop épaisse

#Regrouper les carrières en catégories

PPB <- PPB %>%
  mutate(career_grouped = case_when(
    career %in% c("lawyer", "Lawyer", "law", "jurist", "judge", "Judge", "Attorney", "notarialpractice") ~ "Droit",
    career %in% c("journalism", "Journalist", "journalsim", "communications", "author", "Author", "Printer", "publisher", "writer", "Surveyor", "surveying") ~ "Journalisme & Communication",
    career %in% c("business", "Business", "Entrepreneur", "merchant", 'Merchant', "Banker", "Fur trader", "Contractor", "realestateagent", "Landowner") ~ "Monde des affaires",
    career %in% c("agriculture", "farmer", "Farmer", "forestry", "butcher") ~ "Agriculture",
    career %in% c("Doctor", "medicine", "medicalcare", "Physician", "dentalmedicine", "pharmacy", "optometry", "Surgeon", "psychology", "veterinarymedicine") ~ "Santé",
    career %in% c("Engineer", "engineering", "computerscience", "locomotiveengineer") ~ "Ingénierie",
    career %in% c("teacher", "education") ~ "Éducation",
    career %in% c("politicalstaff", "Politician", "politics", "Public official", "Public servant", "publicservice", "politicalscience", "Immigration agent", "civilservice", "Mayor", "Office holder", "Mohawk chief") ~ "Politique & Fonction publique",
    career %in% c("catholic", "Methodist clergyman", "methodist minister", "religion", "Religion", "Reeve") ~ "Religion",
    career %in% c("architecture", "Architect", "architect") ~ "Architecture",
    career %in% c("blacksmith", "Blacksmith", "carpentry", "Carpenter", "Miller", "lumberman") ~ "Construction",
    career %in% c("military", "Military", "police", "policeofficer", "Sheriff") ~ "Police et militaire",
    career %in% c("navigation", "pilot") ~ "Transport",
    career %in% c("tradeunionist", "unionism") ~ "Syndicalisme",
    career %in% c("NA", "worker", "Innkeeper", "innkeeper", "Tanner", "shorthand", "communitywork", "artist", "athlete") ~ "Autre/NA",
    TRUE ~ as.character(career) # Garder les autres valeurs non prévues
  ))

#Remplacer l'axe des y avec un pourcentage plutôt que le nombre de parlementaires

PPB_summary <- PPB %>%
  count(province, year, career_grouped, name = "count")  # Compter et nommer la colonne "count"

PPB_summary <- PPB_summary %>%
  group_by(year, province) %>%
  mutate(total_count = sum(count, na.rm = TRUE)) %>%  # Calculer le total des occurrences par groupe
  ungroup() %>%
  mutate(percent = (count / total_count) * 100)  # Calculer le pourcentage

#Solution: Afficher chaque 50 ans

PPB_summary <- PPB_summary %>%
  mutate(year = as.numeric(year))  # Convertir 'year' en numérique

#Le graphique final

ggplot(PPB_summary, aes(x = year, y = percent, color = career_grouped, group = career_grouped)) +
  geom_line(size = 0.5) +  # Utilisation d'une ligne pour chaque groupe
  facet_wrap(~ province, scales = "fixed") +  # Graphiques par province
  scale_x_continuous(
    breaks = seq(min(PPB_summary$year), max(PPB_summary$year), by = 50)  # Tous les 50 ans
  ) +
  labs(
    title = "Évolution des carrières des politiciens par année et province (en %)",
    x = "Année",
    y = "Pourcentage des politiciens",
    color = "Carrière (regroupée)"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")




