# Cara Gallagher
# June 27th, 2025
# Prepping table for Shiny

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Packages:
library(tidyverse)
library(readxl)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# path the file 
path <- "Data/Literature table - All sheets.xlsx"

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#### Read overview table ####

SourcesDoc <- read_excel(path, sheet = "All sources")

SourcesDoc <- SourcesDoc %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = FALSE) %>% 
  select(-c(6,7,9,10)) 

# Categorize outcomes
outs <-  SourcesDoc %>% 
  select(ID, 38:53) %>% 
  pivot_longer(cols = !ID, names_to = "Outcomes") %>% 
  filter(value == TRUE) %>% 
  select(-value)

t <- SourcesDoc %>% 
  select(ID, other = 54) %>% 
  mutate(val = ifelse(!is.na(other), TRUE, FALSE)) %>% 
  rename(Outcomes = other) %>% 
  drop_na()  %>% 
  select(-val) %>% 
  separate(Outcomes, into = as.character(seq(1:5)), sep = ", ") %>% 
  pivot_longer(cols = 2:6) %>% 
  drop_na() %>% 
  select(-name) %>% 
  rename(Outcomes = value) 

outs <- bind_rows(outs, t)

outs <- outs %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Energy expenditure",	"Metabolic fuel use",	"Energy use",	"Fasting endurance",	"Energy balance"), "Energy use & balance", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Prey capture rates",	"Diet composition",	"Biomass consumption",	"Consumption of different prey species",	"Consumption rates",	"Foraging success",	"Energy intake",	"Diet",	"Foraging effort"), "Foraging & consumption", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Individual toxin exposure or load",	"Population toxin exposure or load",	"Toxicant transfer rates"), "Toxicant dynamics", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Pathogen prevalence", "Epidemic dynamics"), "Pathogen dynamics", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Offspring mortality & recruitment",	"Conception characteristics",	"Gestation & lactation characteristics"), "Reproduction & recruitment", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Juvenile mortality",	"Unusual mortality events",	"Adult mortality"), "Mortality rates", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Distance to ships"), "Vessel exposure", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Extinction risk",	"PBR",	"Population decline",	"Recovery time"), "Population viability", Outcomes)) %>% 
  mutate(Outcomes = ifelse(Outcomes %in% c("Trophic dynamics"), "Community dynamics & structure", Outcomes)) %>% 
  distinct() %>% 
  mutate(TF = TRUE) %>% 
  pivot_wider(names_from = Outcomes, values_from = TF, values_fill = FALSE)

SourcesDoc <- SourcesDoc %>% 
  select(-c(38:54)) %>% 
  left_join(outs, by = "ID")

# Combine processes, disturbances, and outcomes to single comma sep columns, rename columns, and order
SourcesDoc <- SourcesDoc %>% 
  rowwise() %>% 
  mutate(Processes = paste(names(across(18:28))[c_across(18:28) == TRUE], collapse = ", "),
         Disturbances = paste(names(across(29:37))[c_across(29:37) == TRUE], collapse = ", "),
         Outcomes = paste(names(across(50:75))[c_across(50:75) == TRUE], collapse = ", "),
         Reuse = paste(names(across(42:45))[c_across(42:45) == TRUE], collapse = ", "),
         ) %>%
  ungroup() %>% 
  select(-c(18:37,42:45,50:75)) %>% 
  rename(Literature_type = 'Type of literature',
         Species_group = 'Species group',
         Age_class = 'Modelled age(s)/age classes',
         Multigenerational = 'Multigenerational?',
         Sex = 'Modelled sex',
         Ecological_level = 'Ecological level',
         Spatially_explicit = 'Spatially explicit?',
         Spatial_extent = 'Spatial extent',
         Spatial_resolution = 'Spatial resolution',
         Temporal_extent = 'Temporal extent',
         Temporal_resolution = 'Temporal resolution',
         Code_available = 'Code available?',
         Uncertainty = 'Uncertainty in outputs?',
         Standard_documentation = 'Standard approach to documentation?', 
         Standard_documentation_format = 'Which standard documentation protocol used?',
         Model_type_full = 'Model type full',
         Model_type = 'Model type',
         Model_name = 'Name of model or framework (for original and reapplications)',
         Model_purpose = 'Primary model purpose') %>% 
  mutate(Code_available = ifelse(Code_available == "FALSE", "No", "Yes"),
         Uncertainty = ifelse(Uncertainty == "FALSE", "No", "Yes"),
         Standard_documentation = ifelse(Standard_documentation == "FALSE", "No", "Yes")) %>% 
  relocate(Reuse, .after = Standard_documentation_format)
         

write_csv(SourcesDoc, "Data/PBM3OverviewList.csv")


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#### Read topical sheets ####

# Energetics 
Energetics <- read_excel(path, sheet = "Energetics", skip = 2)  %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3))

Energetics <- Energetics %>% 
  pivot_longer(cols = c(3:7), names_to = "Approach") %>% 
  relocate(Approach, .after = ID) %>% 
  filter(value != FALSE) %>% 
  select(-value) %>% 
  separate_rows(`Processes included`, sep = ", ") %>% 
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Subprocesses_considered" = `Processes included`, 
         "Emergence_energy_intake" = "3a) Does energy intake emerge from interactions with the environment and other processes?",
         "Emergence_energy_use" = "3b) Does energy use emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects) %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  mutate(Process = "Energetics") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 
  
#### Health & physiology #### 
HandP <- read_excel(path, sheet = "Health & Physiology", skip = 2) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3))

HandP <- HandP %>% 
  mutate(AnOther = ifelse(is.na(Other), FALSE, TRUE)) %>% 
  pivot_wider(names_from = Other, values_from = AnOther, values_fill = FALSE) %>% 
  select(-"NA") %>% 
  pivot_longer(cols = c(3:7,13:16), names_to = "Approach") %>% 
  relocate(Approach, .after = ID) %>% 
  filter(value != FALSE) %>% 
  select(-value) %>% 
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects) %>% 
  separate_rows(Focus, sep = ", ") %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  mutate(Process = "Health & Physiology") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 


#### Movement #### 
Movement <- read_excel(path, sheet = "Movement", skip = 2)  %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>%
  select(-c(2,3)) 

Movement_approaches <- Movement %>% 
  select(ID, 3:15) %>% 
  pivot_longer(cols = -ID, names_to = "Approach") %>%
  filter(value != FALSE) %>%
  select(-value) %>% 
  rename(Value = Approach) %>% 
  mutate(Attribute = "Approach",
         Process = "Movement")


Movement_drivers <- Movement %>% 
  select(ID, 16:22) %>% 
  pivot_longer(cols = -ID, names_to = "Drivers") %>%
  filter(value != FALSE) %>%
  select(-value) %>% 
  rename(Value = Drivers) %>% 
  mutate(Attribute = "Drivers",
         Process = "Movement")
  

Movement_attributes <- Movement %>% 
  select(ID, `1) At which level is the process modelled?`, 
         `3) Does the process emerge from interactions with the environment and other processes?`,
         `4) Were process outputs validated using independent empirical data or knowledge?`,
         `5) Is disturbance modelled to affect this process?`,
         `5a) If so, how is the process affected by disturbance?`) %>%
  rename(Ecological_level = `1) At which level is the process modelled?`,
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>%
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>%
  select(-Disturbance_effects) %>%
  separate_rows(Disturbance_impacts, sep = ", ") %>%
  separate_rows(Ecological_level, sep = ", ") %>%
  mutate(Process = "Movement") %>%
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value")
  
  
Movement <- bind_rows(Movement_approaches, Movement_drivers, Movement_attributes)
  
  
#### Behavior & adaptation #### 
BandA <- read_excel(path, sheet = "Behavior & Adaptation", skip = 2) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE)  %>% 
  select(-c(2,3)) 


BandA_approaches <- BandA %>%
  select(ID, 3:7) %>%
  mutate(AnOther = ifelse(is.na(Other...11), FALSE, TRUE)) %>%
  separate_rows(Other...11, sep = ", ") %>% 
  pivot_wider(names_from = Other...11, values_from = AnOther, values_fill = FALSE) %>%
  select(-"NA") %>%
  pivot_longer(cols = -ID, names_to = "Approach") %>%
  filter(value != FALSE) %>%
  select(-value) %>%
  rename(Value = Approach) %>%
  mutate(Attribute = "Approach",
         Process = "Behavior & Adaptation")

BandA_strategies <- BandA %>%
  select(ID, 8:12) %>%
  mutate(AnOther = ifelse(is.na(Other...16), FALSE, TRUE)) %>%
  separate_rows(Other...16, sep = ", ") %>% 
  pivot_wider(names_from = Other...16, values_from = AnOther, values_fill = FALSE) %>%
  select(-"NA") %>%
  pivot_longer(cols = -ID, names_to = "Strategy") %>%
  filter(value != FALSE) %>%
  select(-value) %>%
  rename(Value = Strategy) %>%
  mutate(Attribute = "Implementation strategy",
         Process = "Behavior & Adaptation")

BandA_attributes <- BandA %>%
  select(ID,
         `1) At which level is the process modelled?`,
         "3a) Is the behavior represented as an explicit tradeoff?",
         `3b) Does the process emerge from interactions with the environment and other processes?`,
         "3c) Does the behavior act to seek a specific element of fitness (survival, growth, etc.)?",
         `4) Were process outputs validated using independent empirical data or knowledge?`,
         `5) Is disturbance modelled to affect this process?`,
         `5a) If so, how is the process affected by disturbance?`) %>%
  rename(Ecological_level = `1) At which level is the process modelled?`,
         Behavior_tradeoffs = `3a) Is the behavior represented as an explicit tradeoff?`,
         "Emergence" = "3b) Does the process emerge from interactions with the environment and other processes?",
         Fitness_seeking = `3c) Does the behavior act to seek a specific element of fitness (survival, growth, etc.)?`,
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>%
  mutate(
    Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts),
    Emergence = ifelse(Emergence == "FALSE", "No", "Yes"),
    Behavior_tradeoffs = ifelse(Behavior_tradeoffs == "FALSE", "No", "Yes"),
    Fitness_seeking = ifelse(Fitness_seeking == "FALSE", "No", "Yes")
  ) %>%
  select(-Disturbance_effects) %>%
  separate_rows(Disturbance_impacts, sep = ", ") %>%
  separate_rows(Ecological_level, sep = ", ") %>%
  pivot_longer(cols = -ID, names_to = "Attribute", values_to = "Value") %>%
  mutate(Process = "Behavior & Adaptation")

BandA <- bind_rows(BandA_approaches, BandA_strategies, BandA_attributes)


#### Evolution & Genetics #### 
EandG <- read_excel(path, sheet = "Evolution & Genetics", skip = 2) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3))

EandG <- EandG %>%  
  pivot_longer(cols = c(3:4), names_to = "Cultural_or_genetic_trait") %>% 
  filter(value != FALSE) %>% 
  select(-value) %>% 
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Trait_gene_representation" = "How are traits/genes represented?",
         "Evolving_trait" = "Evolving trait",
         "Drivers" = "Driver of evolution or genetic drift",
         "Population_outcomes" = "Population-level result of interest",
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>%
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects, -Other) %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  separate_rows(Trait_gene_representation, sep = ", ") %>% 
  separate_rows(Evolving_trait, sep = ", ") %>% 
  separate_rows(Drivers, sep = ", ") %>% 
  separate_rows(Population_outcomes, sep = ", ") %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  mutate(Process = "Evolution & Genetics") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 

#### Mortality #### 
Mortality <- read_excel(path, sheet = "Mortality", skip = 2) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3))

Mortality <- Mortality %>%  
  mutate(AnOther = ifelse(is.na(Other), FALSE, TRUE)) %>% 
  pivot_wider(names_from = Other, values_from = AnOther, values_fill = FALSE) %>% 
  select(-"NA") %>% 
  pivot_longer(cols = c(3:12,17:21), names_to = "Approach") %>% 
  relocate(Approach, .after = ID) %>% 
  filter(value != FALSE) %>% 
  select(-value) %>% 
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects) %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  separate_rows(Approach, sep = ", ") %>% 
  mutate(Process = "Mortality") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 


#### Reproduction #### 
Reproduction <- read_excel(path, sheet = "Reproduction", skip = 2) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3)) 

Reproduction <- Reproduction %>% 
  mutate(AnOther = ifelse(is.na(Other), FALSE, TRUE)) %>% 
  separate_rows(Other, sep = ",") %>% 
  pivot_wider(names_from = Other, values_from = AnOther, values_fill = FALSE) %>% 
  select(-"NA") %>% 
  pivot_longer(cols = c(3:7,12:15), names_to = "Approach") %>% 
  relocate(Approach, .after = ID) %>% 
  filter(value != FALSE) %>% 
  select(-value) %>% 
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects) %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  mutate(Process = "Reproduction") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 



#### Aggregate Demographics #### 
AgDemo <- read_excel(path, sheet = "Aggregated Demographics", skip = 2) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3))

AgDemo <- AgDemo %>% 
  pivot_longer(cols = c(3:10), names_to = "Approach") %>% 
  relocate(Approach, .after = ID) %>% 
  filter(value != FALSE) %>% 
  select(-value, -Other) %>% 
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects) %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  mutate(Process = "Aggregated demographics") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 


#### Immigration & emigration #### 
IandE <- read_excel(path, sheet = "Immigration & Emigration", skip = 2) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3))

IandE <- IandE %>% 
  mutate(AnOther = ifelse(is.na("Other...12"), FALSE, TRUE)) %>% 
  pivot_wider(names_from = Other...12, values_from = AnOther, values_fill = FALSE) %>% 
  select(-"NA") %>% 
  pivot_longer(cols = c(3:7), names_to = "Approach") %>% 
  relocate(Approach, .after = ID) %>% 
  filter(value != FALSE) %>% 
  select(-value) %>% 
  mutate(AnOther = ifelse(is.na("Other...18"), FALSE, TRUE)) %>% 
  pivot_wider(names_from = Other...18, values_from = AnOther, values_fill = FALSE) %>% 
  select(-"NA") %>% 
  pivot_longer(cols = c(4:8), names_to = "Aspects_considered") %>% 
  filter(value != FALSE) %>% 
  select(-value) %>%   
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects) %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  mutate(Process = "Immigration & Emigration") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 



#### Density dependence #### 
DD <- read_excel(path, sheet = "Density Dependence", skip = 1) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3)) 


DD_regulation <- DD %>%
  select(ID, 3:6) %>%
  pivot_longer(cols = -ID, names_to = "Regulation_considered") %>%
  filter(value != FALSE) %>%
  select(-value) %>%
  mutate(Attribute = "Regulation_considered",
         Process = "Density dependence") %>%
  rename(Value = Regulation_considered)


DD_direction <- DD %>%
  select(ID, 7:8) %>%
  pivot_longer(cols = -ID, names_to = "Direction") %>%
  filter(value != FALSE) %>%
  select(-value) %>%
  mutate(Attribute = "Direction",
         Process = "Density dependence") %>%
  rename(Value = Direction)


DD_affected_processes <- DD %>%
  select(ID, 9:11) %>%
  pivot_longer(cols = -ID, names_to = "Affected_process") %>%
  filter(value != FALSE) %>%
  select(-value) %>%
  mutate(Attribute = "Affected_process",
         Process = "Density dependence") %>%
  rename(Value = Affected_process)


DD_attributes <- DD %>%
  select(ID,
         `1) At which level is the process modelled?`,
         `3) Does the process emerge from interactions with the environment and other processes?`,
         `4) Were process outputs validated using independent empirical data or knowledge?`,
         `5) Is disturbance modelled to affect this process?`,
         `5a) If so, how is the process affected by disturbance?`) %>%
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>%
  select(-Disturbance_effects) %>%
  separate_rows(Ecological_level, sep = ", ") %>%
  separate_rows(Disturbance_impacts, sep = ", ") %>%
  pivot_longer(cols = -ID, names_to = "Attribute", values_to = "Value") %>%
  mutate(Process = "Density dependence")

DD <- bind_rows(DD_regulation, DD_direction, DD_affected_processes, DD_attributes)

#### Biotic interactions #### 
Interact <- read_excel(path, sheet = "Biotic Interactions", skip = 1) %>% 
  unite("ID", c("Authors", "Year", "Title"), remove = TRUE) %>% 
  select(-c(2,3))

Interact <- Interact %>% 
  pivot_longer(cols = c(5:6), names_to = "Intra_or_inter") %>% 
  filter(value != FALSE) %>% 
  select(-value) %>% 
  rename("Ecological_level" = `1) At which level is the process modelled?`,
         "Interactions_considered" = "Type of BI",
         "Bidirectional" = "Bi-directional",
         "Emergence" = "3) Does the process emerge from interactions with the environment and other processes?",
         "Evaluation" = "4) Were process outputs validated using independent empirical data or knowledge?",
         "Disturbance_effects" = "5) Is disturbance modelled to affect this process?",
         "Disturbance_impacts" = "5a) If so, how is the process affected by disturbance?") %>% 
  mutate(Disturbance_impacts = if_else(Disturbance_effects == "No", "None", Disturbance_impacts)) %>% 
  select(-Disturbance_effects) %>% 
  separate_rows(Ecological_level, sep = ", ") %>% 
  separate_rows(Disturbance_impacts, sep = ", ") %>% 
  separate_rows(Interactions_considered, sep = ", ") %>% 
  mutate(Bidirectional = ifelse(Bidirectional == "FALSE", "No", "Yes"),
         Process = "Biotic interactions") %>% 
  pivot_longer(cols = -c(ID, Process), names_to = "Attribute", values_to = "Value") 


#### All together #### 

processSheets <- bind_rows(Energetics,
                           HandP,
                           Movement, 
                           BandA, 
                           EandG,
                           Mortality,
                           Reproduction,
                           AgDemo,
                           IandE,
                           DD,
                           Interact)

write_csv(processSheets, "Data/PBM3ProcessList.csv")






