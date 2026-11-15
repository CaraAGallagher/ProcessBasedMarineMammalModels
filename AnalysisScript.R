# Cara Gallagher
# July 1st, 2025
# Beneath the surface: Approaches to represent biological processes and responses to disturbance in marine mammal models
# Review analysis

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Packages:
library(tidyverse)
library(readxl)
library(patchwork)
library(viridis)
library(ggforce)
library(showtext)
library(scales)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

font_add_google('Josefin Sans', 'Josefin_Sans')
showtext_auto()
showtext_opts(dpi = 300)

# text sizes
dpiFct <- 3
textSm <- 12 / dpiFct
textSmMed <- 15 / dpiFct
textMed <- 20 / dpiFct
textMedLg <- 28 / dpiFct
textLg <- 35 / dpiFct
textGT <- 3
thickness <- 0.5
lineheight <- 2
lineheightSm <- 1

baseColor <- "#2D2A4A"

#### Read table ####

SourcesDoc <- read_csv("Data/PBM3OverviewList.csv")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#### Overview figures ####

#### Papers per year #### 

totCountPlot <- ggplot(SourcesDoc, aes(x = as.numeric(Year), fill = Literature_type)) +
  geom_histogram(binwidth = 1,  col = "black", size = thickness)+
  theme_classic() +
  scale_fill_manual(values = c("#9A9A9A", baseColor), guide = "none") +
  labs(x = "Year", y = "Number of studies") +
  theme(text = element_text(family = "Josefin_Sans", size = textLg))


#### Helper for pie charts #### 
pie_data <- function(df, var) {
  df %>%
    count({{ var }}) %>%
    mutate(p   = n / sum(n) * 100,
           lab = paste0({{ var }}, "\n(", sprintf("%.1f", p), "%)"))
}

#### Grey versus peer reviewed lit #### 

litTypeDat <- pie_data(SourcesDoc, Literature_type)

litTypePlot <- ggplot(litTypeDat, aes(x = 2, y = p, fill = Literature_type)) +
  geom_col(col = "black", linewidth = thickness) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#9A9A9A", baseColor)) +
  theme(legend.position = "none") +
  geom_text(aes(x = 3.3, label = lab),
            position = position_stack(vjust = 0.5),
            family = "Josefin_Sans", size = textSm,
            lineheight = lineheight / 2, col = "grey15") +
  xlim(.2, 3.5) +
  annotate("text", x = 0.2, y = 2, label = "Literature\ntype",
           family = "Josefin_Sans", fontface = "bold",
           size = textSm, lineheight = lineheight / 2)


subPlot1 <- totCountPlot + inset_element(litTypePlot, 0, 0.2, 0.5, 1.1) 


#### Species and species groups #### 

spColors <- c("#7E5AA8",  "#E60101", "#4DAF4A", "#E66101", "#377EB8", "#E6B502")
genColors <- c("#7E5AA8",  "#4DAF4A", "#E66101", "#377EB8", "#E6B502")

SourcesDocSp <- SourcesDoc %>% 
  select(Species) %>% 
  separate_rows(Species, sep = ", ") %>% 
  group_by(Species) %>% 
  mutate(count_sp_occurr = n()) %>% 
  mutate(Species = str_replace_all(string = Species, 
                                   pattern = "[[:space:]]", 
                                   replacement = " "))


spToSpGr <- read_excel("Data/SpeciesToGroup.xlsx")
spToSpGr <- spToSpGr %>% 
  mutate(Species = str_replace_all(string = Species, 
                                   pattern = "[[:space:]]", 
                                   replacement = " "))

SourcesDocSp <- left_join(SourcesDocSp, spToSpGr, by = "Species")

genusPlot <- SourcesDocSp %>% 
  filter(Genus != "NA") %>% 
  group_by(Genus) %>% 
  mutate(count_genus = n()) %>% 
  ggplot(aes(x = reorder(Genus,-count_genus), fill = `Species group`)) +
  geom_histogram(binwidth = 1, col = "black", stat ="count", size = thickness)+
  theme_classic() +
  labs(x = NULL, y = "Number of studies") +
  scale_fill_manual(values = genColors, guide = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(family = "Josefin_Sans", size = textLg))


SourcesDocSpGrp <- SourcesDoc %>% separate_rows(Species_group, sep = ", ") %>% pie_data(Species_group)

speciesGrPlot <- ggplot(SourcesDocSpGrp, aes(x = 2, y = p, fill = Species_group)) +
  geom_histogram(stat ="identity", col = "black", size = thickness)+
  coord_polar("y", start=1.5) +
  theme_void() +
  scale_fill_manual(values = genColors) +
  labs(fill = NULL) +
  geom_text(aes(x = 3.3, y = p, label = lab), 
                  position = position_stack(vjust = 0.5), 
                  family = "Josefin_Sans", 
                  size = textSm,
                  lineheight = lineheight / 2,
                  col = "grey15"
  ) +
  xlim(.2,3.5) +
  theme(legend.position = "none") +
  annotate("text", x = 0.2, y = 2, label = "Species\ngroup", family = "Josefin_Sans", fontface = "bold", size = textSm, lineheight = lineheight / 2)


genPlot <- genusPlot + inset_element(speciesGrPlot, 0.5, 0.2, 1, 1.1)

figure1 <- subPlot1 / genPlot

# ggsave("Figures/Figure1.pdf", figure1, width = 210, height = 200, units = "mm")

# Species level plot

speciesPlot <- ggplot(SourcesDocSp, aes(x = reorder(Species,-count_sp_occurr), fill = `Species group`)) +
  geom_histogram(binwidth = 1, col = "black", stat ="count", size = thickness)+
  theme_classic() +
  labs(x = NULL, y = "Number of studies") +
  coord_flip() +
  scale_fill_manual(values = spColors, guide = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(family = "Josefin_Sans", size = textLg))
speciesPlot

# ggsave("Figures/Appendix/FigureA4.pdf", speciesPlot, width = 210, height = 310, units = "mm")

# Compare IUCN status of considered / not considered species

spIUCN <- read_excel("Data/SpeciesIUCN.xlsx")

# Clean names + rename columns
spIUCN <- spIUCN %>%
  rename(Species = `Common Name`, IUCN = `IUCN Status`) %>% 
  mutate(Species = str_replace_all(Species, "[[:space:]]+", " "))

# Join IUCN + add study counts
allSp <- spIUCN %>%
  left_join(
    SourcesDocSp %>% 
      select(Species, count_sp_occurr) %>% 
      distinct(),
    by = "Species"
  ) %>%
  mutate(
    count_sp_occurr = ifelse(is.na(count_sp_occurr), 0, count_sp_occurr),
    Studied = ifelse(count_sp_occurr > 0, "Studied", "Not studied")
  )

iucn_levels <- c("Extinct", 
                 "Critically Endangered (Possibly Extinct)", 
                 "Critically Endangered", 
                 "Endangered", 
                 "Vulnerable",
                 "Near Threatened",
                 "Least Concern", 
                 "Data Deficient",
                 "Not Evaluated")

allSp$IUCN <- factor(allSp$IUCN, levels = iucn_levels)

iucn_unweighted <- allSp %>%
  group_by(Studied, IUCN) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Studied) %>%
  mutate(p = n / sum(n))

iucn_all <- allSp %>%
  group_by(IUCN) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Studied = "All species",
    p = n / sum(n)
  )

iucn_comparison_full <- bind_rows(
  iucn_unweighted,
  iucn_all
)

iucn_comparison_full$IUCN <- factor(iucn_comparison_full$IUCN, levels = rev(iucn_levels))

iucn_comparison_full$Studied <- factor(
  iucn_comparison_full$Studied,
  levels = c("Studied", "Not studied", "All species")
)

plot_iucn <- ggplot(iucn_comparison_full, 
                    aes(x = IUCN, y = p, fill = Studied)) +
  geom_col(position = "dodge", col = "black", linewidth = thickness) +
  theme_classic() +
  labs(x = "IUCN status", y = "Proportion of species") +
  scale_fill_manual(values = c(
    "Studied" = "#DC6361",
    "Not studied" = "#C1DD3D",
    "All species" = baseColor
  )) +
  theme(
    text = element_text(family = "Josefin_Sans", size = textLg),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )
plot_iucn

# ggsave("Figures/Appendix/FigureA12.pdf", plot_iucn, width = 210, height = 150, units = "mm")


#### Processes & disturbances overview ####

procsFactLevs <- c("Energetics", "Health & Physiology", "Movement", "Behavior & Adaptation", "Evolution & Genetics", "Mortality", "Reproduction", "Aggregated Demographics", "Immigration & Emigration", "Density Dependence", "Biotic Interactions")

distsFactLevs <- c("Vessel Traffic & Noise", "Direct Takes & Collisions", "Fisheries Competition", "Chemical Pollution & Toxins", "Infectious Disease", "Climate Change Variability & Extreme Events", "Generalized Disturbance Scenarios", "Habitat Change", "No Disturbance")    

procs <- SourcesDoc %>% 
  select(ID, Processes) %>% 
  separate_rows(Processes, sep = ", ") %>% 
  mutate(Processes = str_trim(Processes),
         Processes = str_to_title(Processes),
         Processes = factor(Processes, levels = procsFactLevs))

dists <-  SourcesDoc %>% 
  select(ID, Disturbances) %>% 
  separate_rows(Disturbances, sep = ", ") %>% 
  mutate(Disturbances = str_trim(Disturbances),
         Disturbances = str_to_title(Disturbances),
         Disturbances = factor(Disturbances, levels = distsFactLevs))

procsColors <- c("#E6533C","#FAB846","#BCC13B","#50B697","#3C95B8",rep("#3963BB",5),"#6B4099")

# Proc overview 

procsOv <- procs %>% 
  group_by(Processes) %>% 
  summarise(count_occurr = n()) 

procOvPlot <- ggplot(procsOv, aes(x = Processes, y = count_occurr, fill = Processes)) +
  geom_bar(stat = "identity", col = "black", size = thickness) +
  geom_text(aes(label=count_occurr), vjust= -(max(procsOv$count_occurr) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  scale_x_discrete(labels = label_wrap(28)) +
  scale_fill_manual(values = procsColors, guide = FALSE) +
  ylim(0, max(procsOv$count_occurr) + (max(procsOv$count_occurr) * 0.05)) + 
  labs(x = NULL, y = "Number of studies") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(family = "Josefin_Sans", size = textLg))


# Dist overview 

distsOv <- dists %>% 
  group_by(Disturbances) %>% 
  summarise(count_occurr = n())

distOvPlot <- ggplot(distsOv, aes(x = Disturbances, y = count_occurr, fill = Disturbances)) +
  geom_bar(stat = "identity", col = "black", fill = baseColor, size = thickness) +
  geom_text(aes(label=count_occurr), vjust= -(max(distsOv$count_occurr) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  scale_x_discrete(labels = label_wrap(35)) +
  ylim(0, max(distsOv$count_occurr) + (max(distsOv$count_occurr) * 0.05)) + 
  labs(x = NULL, y = "Number of studies") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, lineheight = 1.5),
        text = element_text(family = "Josefin_Sans", size = textLg))


distN <- dists %>% 
  mutate(Used = ifelse(Disturbances == "No Disturbance", 0, 1)) %>% 
  group_by(ID) %>% 
  summarise(count=sum(Used)) %>% 
  group_by(count) %>% 
  summarise(n=n())  


distNPlot <- ggplot(distN, aes(x = as.factor(count), y = n)) +
  geom_bar(stat = "identity", col = "black", size = thickness, fill = baseColor) +
  geom_text(aes(label=n), vjust= -(max(distN$n) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  ylim(0, max(distN$n) + (max(distN$n) * 0.05)) + 
  labs(x = NULL, y = "Number of studies") +
  theme(text = element_text(family = "Josefin_Sans", size = textLg))

# Comb plot
combPlotProcDist <- 
  (procOvPlot + plot_spacer() + plot_layout(widths = c(1.3,0.4))) / 
  (distOvPlot + distNPlot + plot_layout(ncol = 2, nrow = 1, axis_titles = "collect", widths = c(1,0.5))) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = textLg, family = "Josefin_Sans"))
combPlotProcDist

# ggsave("Figures/Figure2.pdf", combPlotProcDist, width = 210, height = 210, units = "mm")


#### Modelled age classes ####
appendColors <- colorRampPalette(c(baseColor, "#53287D", "#A52494","#DC6361","#C1DD3D"))

  
ageClassDat <- SourcesDoc %>% 
  select(Age_class) %>% 
  group_by(Age_class) %>% 
  summarise(count_occurr = n()) 

ageClassDat <- ageClassDat %>% 
  mutate(Age_class=factor(Age_class)) %>% 
  mutate(Age_class=fct_relevel(Age_class,c("Not modelled","Offspring","Juveniles", "Adults", "Offspring, Adults", "Juveniles, Adults", "Offspring, Juveniles, Adults", "All")))

ageClassPlot <- ggplot(ageClassDat, aes(x = Age_class, y = count_occurr, fill = Age_class)) +
  geom_bar(stat="identity", col = "black", size = thickness)+
  geom_text(aes(label=count_occurr), vjust= -(max(ageClassDat$count_occurr) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  scale_fill_manual(values = appendColors(length(unique(ageClassDat$Age_class))), guide = FALSE) + 
  ylim(0, max(ageClassDat$count_occurr) + (max(ageClassDat$count_occurr) * 0.05)) + 
  labs(x = NULL, y = "Number of studies") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(family = "Josefin_Sans", size = textLg))
ageClassPlot


#### Multigenerational ####

multiGenDat <- pie_data(SourcesDoc, Multigenerational)

multiGenPlot <- ggplot(multiGenDat, aes(x = 2, y = p, fill = Multigenerational)) +
  geom_histogram(stat ="identity", col = "black", size = thickness)+
  coord_polar("y", start=1) +
  theme_void() +
  scale_fill_manual(values = appendColors(length(unique(multiGenDat$Multigenerational)))) +
  theme(legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null")) +
  geom_text(aes(x = 3.3, y = p, label = lab),
                  position = position_stack(vjust = 0.5), 
                  family = "Josefin_Sans", 
                  size = textSm,
                  lineheight = 1,
                  col = "grey15"
  ) +
  xlim(.2,3.5) +
  annotate("text", x = 0.2, y = 2, label = "Multi\ngenerational", family = "Josefin_Sans", fontface = "bold", size = textSm, lineheight = 1)
multiGenPlot



#### Sex ####

sexDat <- pie_data(SourcesDoc, Sex)

sexPlot <- ggplot(sexDat, aes(x = 2, y = p, fill = Sex)) +
  geom_histogram(stat ="identity", col = "black", size = thickness)+
  coord_polar("y", start = 1.57, direction=1) +
  theme_void() +
  scale_fill_manual(values = appendColors(length(unique(sexDat$Sex))), guide = FALSE) +
  theme(legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null")) +
  geom_text(aes(x = 3.3, y = p, label = lab),
                  position = position_stack(vjust = 0.5), 
                  family = "Josefin_Sans", 
                  size = textSm,
                  lineheight = 1,
                  col = "grey15"
  ) +
  xlim(.2,3.5) +
  annotate("text", x = 0.2, y = 2, label = "Sex", family = "Josefin_Sans", fontface = "bold", size = textSm, lineheight = 1)
sexPlot


#### Ecological level ####

eLevDat <- SourcesDoc %>% 
  select(ID, Ecological_level) %>% 
  group_by(Ecological_level) %>% 
  summarise(count_occurr = n()) 

eLevDat <- eLevDat %>% 
  mutate(Ecological_level=factor(Ecological_level)) %>% 
  mutate(Ecological_level=fct_relevel(Ecological_level,c("Ind","Ind -> Pop","Ind -> Com", "Pop", "Pop -> Com", "Com")))

eLevPlot <- ggplot(eLevDat, aes(x = Ecological_level, y = count_occurr, fill = Ecological_level)) +
  geom_bar(stat="identity", col = "black", size = thickness)+
  geom_text(aes(label=count_occurr), vjust= -(max(eLevDat$count_occurr) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  scale_fill_manual(values = appendColors(length(unique(eLevDat$Ecological_level))), guide = FALSE) + 
  ylim(0, max(eLevDat$count_occurr) + (max(eLevDat$count_occurr) * 0.05)) + 
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(family = "Josefin_Sans", size = textLg))
eLevPlot



#### Spatially explicit ####  

spaceDat <- SourcesDoc %>% 
  select(ID, Year, Spatially_explicit) %>% 
  filter(Spatially_explicit  != "Not provided")

spacePie <- pie_data(spaceDat, Spatially_explicit)

spacePlot <- ggplot(spacePie, aes(x = 2, y = p, fill = Spatially_explicit)) +
  geom_histogram(stat ="identity", col = "black", size = thickness)+
  coord_polar("y", start=1.5) +
  theme_void() +
  scale_fill_manual(values = appendColors(length(unique(spacePie$Spatially_explicit)))) +
  theme(legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null")) +
  geom_text(aes(x = 3.3, y = p, label = lab), 
                  position = position_stack(vjust = 0.5), 
                  family = "Josefin_Sans", 
                  size = textSm,
                  lineheight = 1,
                  col = "grey15"
  ) +
  xlim(.2,3.5) +
  annotate("text", x = 0.2, y = 2, label = "Spatially\nexplicit", family = "Josefin_Sans", fontface = "bold", size = textSm, lineheight = 1)
spacePlot



#### Code availability #### 

SourcesDocCA <- pie_data(SourcesDoc, Code_available)

CAplot <- ggplot(SourcesDocCA, aes(x = 2, y = p, fill = Code_available)) +
  geom_histogram(stat ="identity", col = "black", size = thickness)+
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values =  appendColors(length(unique(SourcesDocCA$Code_available)))) +
  theme(legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null")) +
  geom_text(aes(x = 3.3, y = p, label = lab), 
                  position = position_stack(vjust = 0.5), 
                  family = "Josefin_Sans", 
                  size = textSm,
                  lineheight = 1,
                  col = "grey15"
  ) +
  xlim(.2,3.5) +
  annotate("text", x = 0.2, y = 2, label = "Code\navailable", family = "Josefin_Sans", fontface = "bold", size = textSm, lineheight = 1)
CAplot


# Code availability for last 10 years 

SourcesDoc %>% filter(Year >= 2016) %>% summarise(p = mean(Code_available == "Yes"))


#### Uncertainty #### 

SourcesDocUncert <- pie_data(SourcesDoc, Uncertainty)

UncertPlot <- ggplot(SourcesDocUncert, aes(x = 2, y = p, fill = Uncertainty)) +
  geom_histogram(stat ="identity", col = "black", size = thickness)+
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values =  appendColors(length(unique(SourcesDocUncert$Uncertainty)))) +
  theme(legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null")) +
  geom_text(aes(x = 3.3, y = p, label = lab), 
                  position = position_stack(vjust = 0.5), 
                  family = "Josefin_Sans", 
                  size = textSm,
                  lineheight = 1,
                  col = "grey15"
  ) +
  xlim(.2,3.5) +
  annotate("text", x = 0.2, y = 2, label = "Uncertainty\nin outputs", family = "Josefin_Sans", fontface = "bold", size = textSm, lineheight = 1)
UncertPlot


#### Standard documentation #### 

SourcesDocStdDoc <- pie_data(SourcesDoc, Standard_documentation)

SDplot <- ggplot(SourcesDocStdDoc, aes(x = 2, y = p, fill = Standard_documentation)) +
  geom_histogram(stat ="identity", col = "black", size = thickness)+
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values =  appendColors(length(unique(SourcesDocStdDoc$Standard_documentation)))) +
  theme(legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null")) +
  geom_text(aes(x = 3.3, y = p, label = lab),
                  position = position_stack(vjust = 0.5), 
                  family = "Josefin_Sans", 
                  size = textSm,
                  lineheight = 1,
                  col = "grey15"
  ) +
  xlim(.2,3.5) +
  annotate("text", x = 0.2, y = 2, label = "Standardized\ndocumentation\napproach", family = "Josefin_Sans", fontface = "bold", size = textSm, lineheight = 1)
SDplot


###
combPlotAtts <- 
  (((spacePlot | multiGenPlot | sexPlot) /
      ((ageClassPlot | eLevPlot) + plot_layout(axis_titles = "collect")) / 
      (UncertPlot | CAplot | SDplot)) + 
     plot_layout(heights = c(2,1.5,2))) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = textLg + 2, family = "Josefin_Sans"))
combPlotAtts

# ggsave("Figures/Appendix/FigureA6.pdf", combPlotAtts, width = 210, height = 210, units = "mm")


#### Model reuse #### 

SourcesDocReuse <- SourcesDoc %>% 
  select(Reuse) %>% 
  separate_rows(Reuse, sep = ", ") %>% 
  group_by(Reuse) %>% 
  summarise(count_occurr = n()) %>% 
  mutate(Reuse = ifelse(Reuse == "Direct reapplication (excl scenarios)?", "Direct reapplication",
                        ifelse(Reuse == "Reapplication (extended / modified)?", "Extension or modification",
                               ifelse(Reuse == "Reparameterization of existing model / framework", "Reparameterization", Reuse))))

SourcesDocReuse$Reuse <- factor(SourcesDocReuse$Reuse, levels=SourcesDocReuse$Reuse[order(-SourcesDocReuse$count_occurr)], ordered=TRUE)

reusePlot <- ggplot(SourcesDocReuse, aes(x = Reuse, y = count_occurr)) +
  geom_bar(stat = "identity", col = "black", size = thickness, fill = baseColor) +
  geom_text(aes(label=count_occurr), vjust= -(max(SourcesDocReuse$count_occurr) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  ylim(0, max(SourcesDocReuse$count_occurr) + (max(SourcesDocReuse$count_occurr) * 0.05)) + 
  labs(x = NULL, y = "Number of studies", title = "Reuse") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Josefin_Sans", size = textLg))


#### Model type #### 

SourcesDocModelType <- SourcesDoc %>% 
  select(Model_type) %>% 
  separate_rows(Model_type, sep = ", ") %>% 
  group_by(Model_type) %>% 
  summarise(count_occurr = n())

SourcesDocModelType$Model_type <- factor(SourcesDocModelType$Model_type, levels=SourcesDocModelType$Model_type[order(-SourcesDocModelType$count_occurr)], ordered=TRUE)

modelTypePlot <- ggplot(SourcesDocModelType, aes(x = Model_type, y = count_occurr)) +
  geom_bar(stat = "identity", col = "black", size = thickness, fill = baseColor) +
  geom_text(aes(label=count_occurr), vjust= -(max(SourcesDocModelType$count_occurr) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  ylim(0, max(SourcesDocModelType$count_occurr) + (max(SourcesDocModelType$count_occurr) * 0.05)) + 
  labs(x = NULL, y = NULL, title = "Class") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Josefin_Sans", size = textLg))

#### Name of reused model (when relevant) #### 

SourcesDocModelName <- SourcesDoc %>% 
  select(Model_name) %>% 
  separate_rows(Model_name, sep = ", ") %>% 
  drop_na() %>% 
  group_by(Model_name) %>% 
  summarise(count_occurr = n())

SourcesDocModelName$Model_name <- factor(SourcesDocModelName$Model_name, levels=SourcesDocModelName$Model_name[order(-SourcesDocModelName$count_occurr)], ordered=TRUE)

modelNamePlot <- ggplot(SourcesDocModelName, aes(x = Model_name, y = count_occurr)) +
  geom_bar(stat = "identity", col = "black", size = thickness, fill = baseColor) +
  geom_text(aes(label=count_occurr), vjust= -(max(SourcesDocModelName$count_occurr) * 0.02), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  ylim(0, max(SourcesDocModelName$count_occurr) + (max(SourcesDocModelName$count_occurr) * 0.05)) + 
  labs(x = NULL, y = NULL, title = "Name") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Josefin_Sans", size = textLg))

#### Model purpose ####

SourcesDocPurpose <- SourcesDoc %>% 
  select(Model_purpose) %>% 
  separate_rows(Model_purpose, sep = ", ") %>% 
  drop_na() %>% 
  group_by(Model_purpose) %>% 
  summarise(count_occurr = n())

SourcesDocPurpose$Model_purpose <- factor(SourcesDocPurpose$Model_purpose, levels=SourcesDocPurpose$Model_purpose[order(-SourcesDocPurpose$count_occurr)], ordered=TRUE)

modelPurposePlot <- ggplot(SourcesDocPurpose, aes(x = Model_purpose, y = count_occurr)) +
  geom_bar(stat = "identity", col = "black", size = thickness, fill = baseColor) +
  geom_text(aes(label=count_occurr), vjust= -(max(SourcesDocPurpose$count_occurr) * 0.01), family = "Josefin_Sans", size = textGT) +
  theme_classic() +
  scale_fill_manual(values = appendColors(length(unique(SourcesDocPurpose$Model_purpose))), guide = FALSE) + 
  ylim(0, max(SourcesDocPurpose$count_occurr) + (max(SourcesDocPurpose$count_occurr) * 0.1)) + 
  labs(x = NULL, y = "Number of studies", title = "Purpose") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Josefin_Sans", size = textLg))

###
combPlotUse <- 
  (reusePlot + modelTypePlot + modelPurposePlot + modelNamePlot) +
  plot_layout(ncol = 2, nrow = 2, widths = c(0.5,1))+ 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = textLg, family = "Josefin_Sans"))
combPlotUse

# ggsave("Figures/Appendix/FigureA5.pdf", combPlotUse, width = 210, height = 200, units = "mm")

#### Temporal & spatial extents and res ####

SourcesDocTime <- SourcesDoc %>% 
  select(ID, Temporal_extent, Temporal_resolution)

TE <- SourcesDocTime %>%
  select(Temporal_extent) %>% 
  group_by(Temporal_extent) %>%      
  summarise(n = n()) %>% 
  mutate(Temporal_extent = factor(Temporal_extent)) %>% 
  mutate(Temporal_extent = fct_relevel(Temporal_extent, '<= 1 day', '> 1 day & <= 1 week', '> 1 week & <= 1 month', '> 1 month & <= 1 year', '> 1 year & <= 10 years', '> 10 years & <= 100 years', '> 100 years', 'Nonuniform', 'Not specified')) 

TEplot <- ggplot(TE, aes(x = Temporal_extent, y = n)) +
  geom_point(size = 2, col = viridis(100, option = "viridis")[45]) + 
  geom_segment(size = 1,  aes(x=Temporal_extent, xend=Temporal_extent, y=0, yend=n), col = viridis(100, option = "viridis")[45]) + 
  labs(y = "Number of studies", x = NULL) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(family = "Josefin_Sans", size = textLg))

TR <- SourcesDocTime %>%
  select(Temporal_resolution) %>% 
  group_by(Temporal_resolution) %>%      
  summarise(n = n()) %>% 
  mutate(Temporal_resolution = factor(Temporal_resolution)) %>% 
  mutate(Temporal_resolution = fct_relevel(Temporal_resolution, '<= 10 minutes', '> 10 minutes & <= 1 hour', '> 1 hour & <= 1 day', '> 1 day & <= 1 year', '> 1 year', 'Continuous', 'Nonuniform', 'Not specified')) 

TRplot <- ggplot(TR, aes(x = Temporal_resolution, y = n)) +
  geom_point(size = 2, col = viridis(100, option = "viridis")[45]) + 
  geom_segment(size = 1,  aes(x=Temporal_resolution, xend=Temporal_resolution, y=0, yend=n), col = viridis(100, option = "viridis")[45]) + 
  labs(y = "Number of studies", x = NULL) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(family = "Josefin_Sans", size = textLg))

SourcesDocSpace <- SourcesDoc %>% 
  select(ID, Spatial_extent, Spatial_resolution)


SE <- SourcesDocSpace %>%
  select(Spatial_extent) %>% 
  filter(Spatial_extent != 'NA') %>% 
  group_by(Spatial_extent) %>%      
  summarise(n = n()) %>% 
  mutate(Spatial_extent = factor(Spatial_extent)) %>% 
  mutate(Spatial_extent = fct_relevel(Spatial_extent, '<= 5000 km2', '> 5000 km2 & <= 25000 km2', '> 25000 km2 & <= 100000 km2', '> 100000 km2', 'Not specified')) 

SEplot <- ggplot(SE, aes(x = Spatial_extent, y = n)) +
  geom_point(size = 2, col = viridis(100, option = "viridis")[20]) + 
  geom_segment(size = 1,  aes(x=Spatial_extent, xend=Spatial_extent, y=0, yend=n), col = viridis(100, option = "viridis")[20]) + 
  labs(y = "Number of studies", x =NULL) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(family = "Josefin_Sans", size = textLg))

SR <- SourcesDocSpace %>%
  select(Spatial_resolution) %>% 
  filter(Spatial_resolution != 'NA') %>% 
  group_by(Spatial_resolution) %>%      
  summarise(n = n()) %>% 
  mutate(Spatial_resolution = factor(Spatial_resolution)) %>% 
  mutate(Spatial_resolution = fct_relevel(Spatial_resolution, '<= 10 m', '> 10 m & <= 100 m', '> 100 m & <= 1000 m', '> 1000 m & <= 10000 m', '> 10000 m', 'Continuous', 'Nonuniform', 'Not specified')) 

SRplot <- ggplot(SR, aes(x = Spatial_resolution, y = n)) +
  geom_point(size = 2, col = viridis(100, option = "viridis")[20]) + 
  geom_segment(size = 1,  aes(x=Spatial_resolution, xend=Spatial_resolution, y=0, yend=n), col = viridis(100, option = "viridis")[20]) + 
  labs(y = NULL, x = NULL) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(family = "Josefin_Sans", size = textLg))

SRallplot <- (TEplot + TRplot + SEplot + SRplot) + plot_layout(ncol = 2, nrow = 2, axis_titles = "collect")

# ggsave("Figures/Appendix/FigureA7.pdf", SRallplot, width = 210, height = 210, units = "mm")


#### Process to disturbance plot #### 

comb <- left_join(procs, dists, by = "ID", relationship = "many-to-many")

comb <- comb %>% 
  select(-ID) %>% 
  group_by(Processes,Disturbances) %>% 
  summarise(n=n())  

maxDist <- dists %>% 
  group_by(Disturbances) %>% 
  summarise(N = n())

comb <- comb %>% 
  left_join(maxDist, by = "Disturbances") %>% 
  mutate(prop = n / N)

comb$Processes <-factor(comb$Processes, levels= procsFactLevs)
comb$Disturbances <- factor(comb$Disturbances, levels=distsFactLevs) 

procsDistsPlot <- ggplot(comb, aes(x = Processes, y = prop, fill = Processes)) + 
  geom_col(col = "black", size = thickness) + 
  facet_grid(Disturbances ~ ., switch = "y", labeller = label_wrap_gen(multi_line = TRUE)) +  
  scale_fill_manual(values = procsColors, guide = FALSE) +
  theme_classic() +
  scale_y_continuous(breaks = c(0,1)) +
  labs(y = NULL, x = NULL) +
  theme(strip.placement = "outside", 
        strip.background = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Josefin_Sans", size = textLg),
        panel.background = element_rect(fill = 'grey95', color = "white"))
procsDistsPlot

# ggsave("Figures/Figure10.pdf", procsDistsPlot, width = 210, height = 250, units = "mm")

# Calculating Shannon Evenness

# Shannon evenness function
shannon_evenness <- function(props) {
  props <- props[props > 0]   # remove zero props
  H <- -sum(props * log(props))  # Shannon diversity
  S <- length(props)            # number of categories with >0
  Hmax <- log(S)
  if (Hmax == 0) return(NA)
  return(H / Hmax)
}

# Apply per disturbance
evenness_results <- comb %>%
  group_by(Disturbances) %>%
  mutate(p = n / sum(n)) %>%
  summarise(Shannon_Evenness = shannon_evenness(p)) %>%
  arrange(desc(Shannon_Evenness))
evenness_results

evennessPlot <- ggplot(evenness_results, aes(x = reorder(Disturbances, Shannon_Evenness), y = Shannon_Evenness)) +
  geom_point(col = "black", fill = baseColor, size = 3, shape = 21) +
  labs(x = "Disturbance source", y = "Shannon Evenness") +
  theme_classic() +
  theme(text = element_text(family = "Josefin_Sans", size = textLg),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
evennessPlot

# ggsave("Figures/Appendix/FigureA10.pdf", evennessPlot, width = 210, height = 150, units = "mm")

#### Outcomes plot ####

outs <-  SourcesDoc %>% 
  select(Outcomes) %>% 
  separate_rows(Outcomes, sep = ", ") 

outsWC <- outs %>% 
  group_by(Outcomes) %>% 
  mutate(count = n())

outcomePlot <- ggplot(outsWC, aes(x = reorder(Outcomes,-count))) +
  geom_histogram(binwidth = 1, col = "black", fill = baseColor, stat ="count", size = thickness)+
  theme_classic() +
  labs(x = NULL, y = "Number of studies") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(family = "Josefin_Sans", size = textLg))
outcomePlot

# ggsave("Figures/Appendix/FigureA11.pdf", outcomePlot, width = 210, height = 210, units = "mm")

outsProp <- outsWC %>% 
  distinct() %>% 
  mutate(prop = count / nrow(SourcesDoc) * 100) %>% 
  arrange(-prop)


#### PROCESS ANALYSIS ####

# #### Process specific plotting functions ####

HeatMapFunction <- function(data, colors, Atts, overall_title = NULL,
                            alternate = TRUE,
                            margin_t  = NULL,
                            margin_b  = NULL,
                            margin_l  = 30,
                            margin_r  = 10) {
  library(patchwork)
  library(stringr)
  
  dark_grey   <- "grey20"
  tick_colour <- "grey20"
  tile_lwd    <- 1.1
  tick_len    <- 0.3
  tick_thick  <- 1.1
  nudge_amt   <- 1
  
  margin_t <- margin_t %||% if (alternate) 40 else 5
  margin_b <- margin_b %||% if (alternate) 40 else 30
  
  # ── 1. Pre-compute summaries & global colour scale ─────────────────────────
  summaries <- lapply(Atts, function(i) {
    data %>%
      filter(Attribute == i) %>%
      distinct() %>%
      group_by(Value) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(
        Value = factor(Value, levels = {
          vals <- Value[order(-n)]
          if ("None" %in% vals) c(setdiff(vals, "None"), "None") else vals
        })
      ) %>%
      arrange(Value) %>%
      mutate(
        pos_idx    = row_number(),
        above      = if (alternate) pos_idx %% 2 == 0 else FALSE,
        label_wrap = str_wrap(as.character(Value), width = 12)
      )
  })
  
  scale_limit <- length(unique(data$ID)) 
  
  # ── Shared fill scale ───────────────────────────────────────────────────────
  shared_fill <- scale_fill_gradient(
    low    = "white",
    high   = colors,
    limits = c(0, scale_limit),
    breaks = c(0, scale_limit),      
    name   = "Number of studies",
    guide  = guide_colorbar(
      title.position  = "top",
      title.hjust     = 0.5,
      barwidth        = unit(12,      "lines"),
      barheight       = unit(0.8,     "lines"),
      direction       = "horizontal",
      frame.colour    = "black",
      frame.linewidth = tile_lwd,
      ticks           = FALSE           
    )
  )
  
  # ── 2. Build plots ──────────────────────────────────────────────────────────
  plot_list <- vector("list", length(Atts))
  
  for (idx in seq_along(Atts)) {
    df       <- summaries[[idx]]
    is_first <- idx == 1
    
    df_above <- filter(df, above)
    df_below <- filter(df, !above)
    
    ylim_top <- if (alternate || nrow(df_above) > 0) 1.6 else 1.55
    
    p <- ggplot(df, aes(x = Value, y = 1, fill = n)) +
      geom_tile(color = "black", linewidth = tile_lwd, height = 1) +
      geom_text(aes(label = n),
                size = textSm, family = "Josefin_Sans", color = dark_grey) +
      {if (nrow(df_above) > 0)
        geom_segment(data = df_above,
                     aes(x = Value, xend = Value, y = 1.5, yend = 1.5 + tick_len),
                     color = tick_colour, linewidth = tick_thick, inherit.aes = FALSE)
      } +
      geom_segment(data = df_below,
                   aes(x = Value, xend = Value, y = 0.5, yend = 0.5 - tick_len),
                   color = tick_colour, linewidth = tick_thick, inherit.aes = FALSE) +
      {if (nrow(df_above) > 0)
        geom_text(data = df_above, aes(x = Value, y = 1, label = label_wrap),
                  vjust = 0, nudge_y = nudge_amt, size = textSm * 0.85,
                  family = "Josefin_Sans", lineheight = lineheightSm,
                  color = dark_grey, inherit.aes = FALSE)
      } +
      geom_text(data = df_below, aes(x = Value, y = 1, label = label_wrap),
                vjust = 1, nudge_y = -nudge_amt, size = textSm * 0.85,
                family = "Josefin_Sans", lineheight = lineheightSm,
                color = dark_grey, inherit.aes = FALSE) +
      shared_fill +
      scale_x_discrete(labels = NULL, expand = expansion(add = 0.5)) +
      scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      labs(x = NULL, y = NULL, title = NULL, caption = Atts[idx]) +
      coord_cartesian(ylim = c(0.4, ylim_top), clip = "off") +
      theme_classic() +
      theme(
        legend.position      = if (is_first) c(0.99, 0.99) else "none",
        legend.justification = c(1, 1),
        legend.direction     = "horizontal",
        legend.background    = element_rect(fill = NULL, colour = NA),
        legend.title         = element_text(family = "Josefin_Sans",
                                            size = textMedLg, color = "black",
                                            margin = margin(b = 3)),
        legend.text          = element_text(family = "Josefin_Sans",
                                            size = textMed, color = "black"),
        legend.margin        = margin(4, 4, 4, 4),
        text         = element_text(family = "Josefin_Sans", size = textLg,
                                    lineheight = lineheightSm, color = dark_grey),
        plot.caption = element_text(family = "Josefin_Sans", size = textLg,
                                    hjust = 0.5, vjust = -5, color = "black"),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.line  = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid   = element_blank(),
        panel.border = element_blank(),
        plot.margin  = margin(t = margin_t, b = margin_b,   # ← same for all plots
                              l = margin_l,  r = margin_r)
      )
    
    plot_list[[idx]] <- p
  }
  
  # ── 3. Combine with title ───────────────────────────────────────────────────
  combined <- wrap_plots(plot_list, ncol = 1) +
    plot_annotation(
      title      = overall_title,
      tag_levels = "A",
      theme      = theme(
        plot.title = element_text(         
          family   = "Josefin_Sans",
          face     = "bold",
          size     = textLg * 1.6,
          hjust    = 0.5,
          color    = "black",
          margin   = margin(t = 10)),
        plot.tag   = element_text(
          family = "Josefin_Sans",
          size   = textLg,
          face   = "bold",
          color  = dark_grey)
      )
    )
  
  return(combined)
}

library(patchwork)

# Energetics 

EnergeticsAtts <- c("Approach", "Subprocesses_considered", "Disturbance_impacts")

Energetics <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Energetics",
         Attribute %in% EnergeticsAtts) 

EnergeticsPlot <- HeatMapFunction(Energetics, procsColors[1], EnergeticsAtts, overall_title = "Energetics", alternate = FALSE)

# ggsave("Figures/Figure3.pdf", EnergeticsPlot, width = 210, height = 110, units = "mm")

# Health and physiology

HandPAtts <- c("Approach", "Focus", "Disturbance_impacts")

HandP <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Health & Physiology",
         Attribute %in% HandPAtts) 

HandPPlot <- HeatMapFunction(HandP, procsColors[2], HandPAtts, overall_title = HandP$Process[1], alternate = TRUE)

# ggsave("Figures/Figure4.pdf", HandPPlot, width = 210, height = 160, units = "mm")

# Movement

MovementAtts <- c("Drivers", "Approach", "Disturbance_impacts")

Movement <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Movement",
         Attribute %in% MovementAtts) 

MovementPlot <- HeatMapFunction(Movement, procsColors[3], MovementAtts, overall_title = Movement$Process[1], alternate = TRUE)

# ggsave("Figures/Figure5.pdf", MovementPlot, width = 210, height = 160, units = "mm")

# Behavior and adaptation

BandAAtts <- c("Approach", "Behavior_tradeoffs", "Implementation strategy", "Fitness_seeking", "Disturbance_impacts")

BandA <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Behavior & Adaptation",
         Attribute %in% BandAAtts) 

BandAPlot <- HeatMapFunction(BandA, procsColors[4], BandAAtts, overall_title = BandA$Process[1], alternate = FALSE)

# ggsave("Figures/Figure6.pdf", BandAPlot, width = 210, height = 190, units = "mm")

# Evolution and genetics

EandGAtts <- c("Cultural_or_genetic_trait", "Drivers", "Evolving_trait", "Trait_gene_representation", "Disturbance_impacts")

EandG <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Evolution & Genetics",
         Attribute %in% EandGAtts) 

EandGPlot <- HeatMapFunction(EandG, procsColors[5], EandGAtts, overall_title = EandG$Process[1], alternate = FALSE)

# ggsave("Figures/Figure7.pdf", EandGPlot, width = 210, height = 190, units = "mm")


# Mortality

MortAtts <- c("Approach", "Disturbance_impacts")

Mortality <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Mortality",
         Attribute %in% MortAtts) 

MortPlot <- HeatMapFunction(Mortality, procsColors[6], MortAtts, overall_title = Mortality$Process[1], alternate = TRUE)

# ggsave("Figures/Figure8M.pdf", MortPlot, width = 210, height = 110, units = "mm")

# Reproduction

ReproAtts <- c("Approach", "Disturbance_impacts")

Reproduction <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Reproduction",
         Attribute %in% ReproAtts) 

ReproPlot <- HeatMapFunction(Reproduction, procsColors[7], ReproAtts, overall_title = Reproduction$Process[1], alternate = FALSE)

# ggsave("Figures/Figure8R.pdf", ReproPlot, width = 210, height = 90, units = "mm")


# Aggregated Demographics

AgDemoAtts <- c("Approach", "Disturbance_impacts")

AgDemo <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Aggregated demographics",
         Attribute %in% AgDemoAtts) 

AgDemoPlot <- HeatMapFunction(AgDemo, procsColors[8], AgDemoAtts, overall_title = AgDemo$Process[1], alternate = FALSE)

# ggsave("Figures/Figure8AG.pdf", AgDemoPlot, width = 210, height = 90, units = "mm")

# Immigration & Emigration

IandEAtts <- c("Approach", "Aspects_considered", "Disturbance_impacts")

IandE <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Immigration & Emigration",
         Attribute %in% IandEAtts) 

IandEPlot <- HeatMapFunction(IandE, procsColors[9], IandEAtts, overall_title = IandE$Process[1], alternate = FALSE)

# ggsave("Figures/Figure8IE.pdf", IandEPlot, width = 210, height = 120, units = "mm")

# Density Dependence

DDAtts <- c("Regulation_considered", "Direction", "Affected_process","Disturbance_impacts")

DensDep <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Density dependence",
         Attribute %in% DDAtts) 

DDPlot <- HeatMapFunction(DensDep, procsColors[10], DDAtts, overall_title = DensDep$Process[1], alternate = FALSE)

# ggsave("Figures/Figure8DD.pdf", DDPlot, width = 210, height = 150, units = "mm")

# Biotic interactions

BIAtts <- c("Interactions_considered", "Intra_or_inter", "Bidirectional","Disturbance_impacts")

BioInteract <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Process == "Biotic interactions",
         Attribute %in% BIAtts) 

BIPlot <- HeatMapFunction(BioInteract, procsColors[11], BIAtts, overall_title = BioInteract$Process[1], alternate = FALSE)

# ggsave("Figures/Figure9.pdf", BIPlot, width = 210, height = 150, units = "mm")

#### Process ecological level plot #####

cols <- tibble(Process = procsFactLevs, 
               Colors = procsColors)

labels <- c("A. Energetics", "B. Health & Physiology", "C. Movement", 
            "D. Behavior & Adaptation", "E. Evolution & Genetics", 
            "F. Mortality", "G. Reproduction", "H. Aggregated Demographics", 
            "I. Immigration & Emigration", "J. Density Dependence", 
            "K. Biotic Interactions")


eLevels <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Attribute == "Ecological_level") %>% 
  distinct() %>% 
  group_by(Process, Value)  %>% 
  summarise(count_occurr = n()) %>% 
  mutate(Process = str_trim(Process),
         Process = str_to_title(Process)) %>% 
  left_join(cols, by = "Process") %>% 
  mutate(
    Process = factor(Process, levels = procsFactLevs, labels = labels),
    Value = factor(Value, levels = c("Individual", "Population", "Community"))
  )

eLevelPlot <- ggplot(eLevels, aes(x = Value, y = count_occurr, fill = Process)) +
  geom_bar(stat = "identity", col = "black", size = thickness) +
  geom_text(aes(label = count_occurr),
            vjust = -0.3,  
            family = "Josefin_Sans", size = textGT) +
  facet_wrap(. ~ Process, nrow = 4, scales = "free_y") +
  labs(x = NULL, y = "Number of studies") +
  scale_fill_manual(values = procsColors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  
  theme_classic() +
  theme(strip.placement = "outside", 
        strip.background = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Josefin_Sans", size = textLg),
        panel.background = element_rect(fill = 'grey95', color = "white"))
eLevelPlot

# ggsave("Figures/Appendix/FigureA8.pdf", eLevelPlot, width = 210, height = 210, units = "mm")

#### Process evaluation plot #####

cols <- tibble(Process = procsFactLevs, 
               Colors = procsColors)

labels <- c("A. Energetics", "B. Health & Physiology", "C. Movement", 
            "D. Behavior & Adaptation", "E. Evolution & Genetics", 
            "F. Mortality", "G. Reproduction", "H. Aggregated Demographics", 
            "I. Immigration & Emigration", "J. Density Dependence", 
            "K. Biotic Interactions")


eval <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Attribute == "Evaluation") %>% 
  distinct() %>% 
  group_by(Process, Value)  %>% 
  summarise(count_occurr = n()) %>% 
  mutate(Process = str_trim(Process),
         Process = str_to_title(Process)) %>% 
  left_join(cols, by = "Process") %>% 
  mutate(
    Process = factor(Process, levels = procsFactLevs, labels = labels),
    Value = factor(Value, levels = c("Yes", "No"))
  )

evalPlot <- ggplot(eval, aes(x = Value, y = count_occurr, fill = Process)) +
  geom_bar(stat = "identity", col = "black", size = thickness) +
  geom_text(aes(label = count_occurr),
            vjust = -0.3,  
            family = "Josefin_Sans", size = textGT) +
  facet_wrap(. ~ Process, nrow = 4, scales = "free_y") +
  labs(x = NULL, y = "Number of studies") +
  scale_fill_manual(values = procsColors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  
  theme_classic() +
  theme(strip.placement = "outside", 
        strip.background = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Josefin_Sans", size = textLg),
        panel.background = element_rect(fill = 'grey95', color = "white"))
evalPlot

# ggsave("Figures/Appendix/FigureA9.pdf", evalPlot, width = 210, height = 210, units = "mm")

# Eval by decade 
library(stringr)
library(stringi)

clean_id <- function(x) {
  x %>%
    str_replace_all("\u00a0", " ") %>%            # non-breaking spaces
    stri_trans_general("Latin-ASCII") %>%         # Molnár -> Molnar
    str_squish() %>%                              # double spaces
    str_remove("(?<=phenology)7$") %>%            # stray "7" in Bailleul et al.
    str_replace(
      fixed("A mass-balance model of the Falkland Islands fisheries and ecosystems"),
      "Simulations of the Falkland Islands marine ecosystem: climate, penguins and squid fisheries"
    )
}

t <- read_csv("Data/PBM3ProcessList.csv") %>%
  filter(Attribute == "Evaluation") %>%
  distinct() %>%
  mutate(
    ID     = clean_id(ID),
    year   = as.numeric(str_extract(ID, "(?<=_)\\d{4}(?=_)")),
    decade = paste0(floor(year / 10) * 10, "s"),
    Value  = str_trim(Value)
  ) %>%
  group_by(ID, decade) %>%
  summarise(evaluated = any(Value == "Yes"), .groups = "drop") %>%
  group_by(decade) %>%
  summarise(
    n_studies   = n(),
    n_evaluated = sum(evaluated),
    prop        = n_evaluated / n_studies,
    .groups = "drop"
  )

# overall
t %>% summarise(n_studies = sum(n_studies),
                n_evaluated = sum(n_evaluated),
                prop = n_evaluated / n_studies)


#### Process emergence plot #####

cols <- tibble(Process = procsFactLevs, 
               Colors = procsColors)

labels <- c("A. Energetics", "B. Health & Physiology", "C. Movement", 
            "D. Behavior & Adaptation", "E. Evolution & Genetics", 
            "F. Mortality", "G. Reproduction", "H. Aggregated Demographics", 
            "I. Immigration & Emigration", "J. Density Dependence", 
            "K. Biotic Interactions")


emerg <- read_csv("Data/PBM3ProcessList.csv") %>% 
  filter(Attribute == "Emergence") %>% 
  distinct() %>% 
  group_by(Process, Value)  %>% 
  summarise(count_occurr = n()) %>% 
  mutate(Process = str_trim(Process),
         Process = str_to_title(Process)) %>% 
  left_join(cols, by = "Process") %>% 
  mutate(
    Process = factor(Process, levels = procsFactLevs, labels = labels),
    Value = factor(Value, levels = c("Yes", "No"))
  )

emergPlot <- ggplot(emerg, aes(x = Value, y = count_occurr, fill = Process)) +
  geom_bar(stat = "identity", col = "black", size = thickness) +
  geom_text(aes(label = count_occurr),
            vjust = -0.3,  
            family = "Josefin_Sans", size = textGT) +
  facet_wrap(. ~ Process, nrow = 4, scales = "free_y") +
  labs(x = NULL, y = "Number of studies") +
  scale_fill_manual(values = procsColors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  
  theme_classic() +
  theme(strip.placement = "outside", 
        strip.background = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Josefin_Sans", size = textLg),
        panel.background = element_rect(fill = 'grey95', color = "white"))
emergPlot

