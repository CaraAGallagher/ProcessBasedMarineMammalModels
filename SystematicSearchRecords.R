# Cara Gallagher
# July 1st, 2025
# Beneath the surface: Approaches to represent biological processes and responses to disturbance in marine mammal models
# Literature search
# Search was done in two phases, the combined results are presented in the manuscript

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Packages:
library(revtools) 
library(tidyverse)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

scopus_results <- read_bibliography(file="LitSearch/Scopus2ndSearch.bib")
wos_results <- read_bibliography(file="LitSearch/WoS2ndSearch.bib")
wos_results <-  select(wos_results, which(names(wos_results) %in% colnames(scopus_results)))
scopus_results <-  select(scopus_results, which(names(scopus_results) %in% colnames(wos_results)))

all_results <- merge_columns(wos_results, scopus_results)

dups <- find_duplicates(all_results, match_function = "fuzzdist")

uniq <- extract_unique_references(all_results, dups)

# screen_duplicates(uniq)

all_results_no_dups <- read_bibliography(file="LitSearch/all_results_no_dups_2ndSearch.csv")

# First screen of titles only looking at relevance for marine mammals (could be selected, unknown, or excluded)
# screen_titles(all_results_no_dups)

all_results_no_dups_screen1 <- read_bibliography(file="LitSearch/all_results_no_dups_screen1_2ndSearch.csv")
all_results_no_dups_screen1 <- all_results_no_dups_screen1[which(all_results_no_dups_screen1$screened_titles != "excluded"),]

# Go through abstracts for "unknown" records for records relevant to marine mammals
# screen_abstracts(all_results_no_dups_screen1[which(all_results_no_dups_screen1$screened_titles == "unknown"),])

all_results_no_dups_screen1_unknowns <- read_bibliography(file="LitSearch/all_results_no_dups_screen1_unknowns_2ndSearch.csv")
all_results_no_dups_screen1_unknowns <- all_results_no_dups_screen1_unknowns[which(all_results_no_dups_screen1_unknowns$screened_abstracts == "selected"),]
all_results_no_dups_screen1_unknowns <- all_results_no_dups_screen1_unknowns[,1:15]

# Combine results for selected titles and unknowns which passed "marine mammal" abstract check
all_results_no_dups_screen1_selected <- all_results_no_dups_screen1[which(all_results_no_dups_screen1$screened_titles == "selected"),]
all_results_no_dups_screen1_selected <- all_results_no_dups_screen1_selected[,1:15]

all_results_no_dups_screen1 <- rbind(all_results_no_dups_screen1_unknowns,all_results_no_dups_screen1_selected)

# Go through abstracts for records which passed screen 1 for records relevant to modelling and which are not reviews
screen_abstracts(all_results_no_dups_screen1)

all_results_no_dups_screen2 <- read_bibliography(file="LitSearch/all_results_no_dups_screen2_2ndSearch.csv")
all_results_no_dups_screen2_selected <- all_results_no_dups_screen2[which(all_results_no_dups_screen2$screened_abstracts == "selected"),]

arnds2s <- as_tibble(all_results_no_dups_screen2_selected)
arnds2s <- arnds2s %>% select(author, year, title, journal, abstract)
write.csv(arnds2s, "LitSearch/all_results_no_dups_screen2_forDrive_2ndSearch.csv")
