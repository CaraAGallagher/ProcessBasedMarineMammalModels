rm(list = ls())

library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(dplyr)
library(bsplus)
library(DT)
library(plotly)
library(ggsankey)
library(showtext)
library(sysfonts)

# Load your data
overview <- fread("Data/PBM3OverviewList.csv")
process <- fread("Data/PBM3ProcessList.csv")

# Define column description for Overview Table Tab
column_tooltips <- list(
  "Source"="Journal title or report source",                       
  "Literature_type"="Peer-reviewed of grey",
  "Species_group"="Marine mammal taxonomic group(s)",
  "Age_class"="Which age classes are modelled",
  "Multigenerational"="Whether the model includes more than one generation",
  "Ecological_level"="The span of modelled ecological levels",
  "Spatial_extent"="Binned",
  "Spatial_resolution"="Binned",           
  "Temporal_extent"="Binned",
  "Temporal_resolution"="Binned",
  "Uncertainty"="Whether any measure of undcertainty was presented around model results",
  "Reuse"="Whether the model was based on an exisiting model",
  "Model_type"= "What general type of model was employed",
  "Processes"="List of all the processes incorporated",
  "Disturbances"="List of all the disturbance types included",
  "Outcomes"="List of the all the key outcomes of the modelling effort"  
)

# Define column description for Process Analysis Tab
process_tooltips <- list(
  "Energetics"="Representation of animal energy dynamics including intake, expenditure, and balance, expressed in energy units, mass, or dimensionless metrics. Encompasses hunger and starvation processes.",                       
  "Health & Physiology"="Non-energetic health and physiological processes including disease dynamics, toxicant effects, hypoxia, and thermal constraints.",
  "Movement"="Representation of spatial position changes, modeled either explicitly through coordinates or implicitly through habitat transitions.",
  "Behavior & Adaptation"="Animal responses to biotic and abiotic cues, encompassing both consistent behavioral patterns and adaptive responses that may influence other processes (e.g., adaptive movement behaviors).",
  "Evolution & Genetics"="Representation and transmission of trait variation and the processes by which these traits change over time, whether through genetic mechanisms or culturally learned behaviors.",
  "Mortality"="Representation of death processes across different causative mechanisms and demographic patterns.",
  "Reproduction"="Representation of reproductive processes including mating, pregnancy, parturition, and offspring care.",
  "Aggregated demographics"="Population- or higher-level demographic processes emphasizing net population changes without detailed process representation.",           
  "Immigration & Emigration"="Representation of individual movement into and out of populations.",
  "Density dependence"="Representation of population processes that vary with population density, including both imposed model assumptions and emergent effects from resource competition, habitat exclusion, or mate competition.",
  "Biotic interactions"="Representation of intra- and interspecific interactions occurring across temporal and spatial scales including competition, predation, information transfer, and cooperative behaviors."  
)


# Helper function to check if a column contains comma-separated multiple values 
needs_split <- function(column_data) {
  any(str_detect(column_data, ", "), na.rm=T)
}

# Helper function to split comma-separated values in a specified column into multiple rows
split_multi <- function(df, col) {
  df %>% 
    mutate(across({{ col }}, ~ strsplit(as.character(.), ", "))) %>% 
    unnest({{ col }})
}

# Helper function to split comma-separated values and get a list of unique values
split_multi_unique <- function(df, col, sep=",") {
  df %>%
    separate_rows({{ col }}, sep = sep) %>%     
    mutate({{ col }} := str_trim({{ col }})) %>%   
    distinct({{ col }}) %>%                     
    arrange({{ col }}) %>%
    pull({{ col }})
}

# Columns that can have multiple comma-separated values and need special filtering
multi_cols <- c("Species", "Species_group", "Age_class", "Model_type_full", "Model_type", "Model_name", 
                "Model_purpose", "Processes", "Disturbances", "Outcomes")

# ====== Sankey data preparation ======
# Create long format data for Sankey
Processes <- overview %>% 
  select(ID, Processes) %>% 
  separate_rows(Processes, sep = ", ") %>% 
  mutate(Category = "Processes") %>% 
  rename(value = Processes) %>%
  filter(!is.na(value) & value != "")

Disturbances <- overview %>% 
  select(ID, Disturbances) %>% 
  separate_rows(Disturbances, sep = ", ")  %>% 
  mutate(Category = "Disturbances") %>% 
  rename(value = Disturbances) %>%
  filter(!is.na(value) & value != "")

Outcomes <- overview %>% 
  select(ID, Outcomes) %>% 
  separate_rows(Outcomes, sep = ", ")  %>% 
  mutate(Category = "Outcomes") %>% 
  rename(value = Outcomes) %>%
  filter(!is.na(value) & value != "")

# Factor levels and colors
procsFactLevs <- c("Energetics", "Health & physiology", "Movement", "Behavior & adaptation", "Evolution & genetics", "Mortality", "Reproduction", "Aggregated demographics", "Immigration & emigration", "Density dependence", "Biotic interactions")

distsFactLevs <- c("Vessel traffic & noise", "Direct takes & collisions", "Fisheries competition", "Chemical pollution & toxins", "Infectious disease", "Climate change variability & extreme events", "Generalized disturbance scenarios", "Habitat change", "No disturbance")    

outsFactLevs <- c("Morphometrics", "Energy use & balance", "Cardiorespiratory performance", "Foraging & consumption", "Behavior", "Genetic or behavioral traits", "Song characteristics", "Communication space & masking", "Sound exposure", "Auditory threshold shifts", "Toxicant dynamics", "Pathogen dynamics", "Reproduction & recruitment", "Mortality rates", "Injury", "Collision risk & mortality", "Bycatch rate", "Vessel exposure", "Population dynamics & structure", "Population viability", "Detection rate", "Spatial distribution", "Niche characteristics", "Predation rates", "Individual interactions", "Community dynamics & structure")

Processes$value <- factor(Processes$value, levels = procsFactLevs)
Disturbances$value <- factor(Disturbances$value, levels = distsFactLevs)
Outcomes$value <- factor(Outcomes$value, levels = outsFactLevs)

procsColors <- c("#E6533C","#FAB846","#BCC13B","#50B697","#3C95B8",rep("#3963BB",5),"#6B4099")

all_categories <- bind_rows(Processes, Disturbances, Outcomes)

# Function to create Sankey data based on selected categories
create_sankey_data <- function(selected_types, all_data) {
  
  # Filter data to only selected types
  filtered_data <- all_data %>%
    filter(Category %in% selected_types)
  
  # Get all unique IDs that have data for ALL selected types
  ids_with_all_types <- filtered_data %>%
    group_by(ID) %>%
    summarise(types_present = n_distinct(Category)) %>%
    filter(types_present == length(selected_types)) %>%
    pull(ID)
  
  # Filter to only those IDs
  complete_data <- filtered_data %>%
    filter(ID %in% ids_with_all_types)
  
  # Create combinations between the selected types
  if (length(selected_types) == 2) {
    # Two types: direct connections
    type1_data <- complete_data %>% filter(Category == selected_types[1])
    type2_data <- complete_data %>% filter(Category == selected_types[2])
    
    connections <- inner_join(type1_data, type2_data, by = "ID", relationship = "many-to-many") %>%
      count(value.x, value.y, name = "flow_value") %>%
      rename(source = value.x, target = value.y) 
    
  } else if (length(selected_types) == 3) {
    # Three types: Process -> Disturbance -> Outcome flow
    ordered_types <- c("Processes", "Disturbances", "Outcomes")
    
    # First connection (type 1 -> type 2)
    type1_data <- complete_data %>% filter(Category == ordered_types[1])
    type2_data <- complete_data %>% filter(Category == ordered_types[2])
    
    conn1 <- inner_join(type1_data, type2_data, by = "ID", relationship = "many-to-many") %>%
      count(value.x, value.y, name = "flow_value") %>%
      rename(source = value.x, target = value.y)
    
    # Second connection (type 2 -> type 3)
    type3_data <- complete_data %>% filter(Category == ordered_types[3])
    
    conn2 <- inner_join(type2_data, type3_data, by = "ID", relationship = "many-to-many") %>%
      count(value.x, value.y, name = "flow_value") %>%
      rename(source = value.x, target = value.y)
    
    connections <- bind_rows(conn1, conn2)
  }
  
  return(connections)
}

# Define ocean theme colors
ocean_colors <- list(
  primary = "#0077be",     
  secondary = "#00a8cc",    
  accent = "#40e0d0",       
  light = "#e6f3ff",        
  dark = "#003d5c",         
  coral = "#ff6b6b",        
  seafoam = "#98fb98",      
  sand = "#f4a460"          
)

#### UI ####
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Josefin+Sans:wght@300;400;600;700&display=swap", rel = "stylesheet"),
    tags$style(HTML(paste0("
      @import url('https://fonts.googleapis.com/css2?family=Josefin+Sans:wght@300;400;600;700&display=swap');
      
      body {
        font-family: 'Josefin Sans', sans-serif !important;
        background: linear-gradient(135deg, ", ocean_colors$light, " 0%, ", ocean_colors$secondary, " 100%) !important;
        min-height: 100vh;
      }
      
      .navbar {
        background: linear-gradient(90deg, ", ocean_colors$primary, " 0%, ", ocean_colors$secondary, " 100%) !important;
        border: none !important;
        box-shadow: 0 4px 6px rgba(0, 119, 190, 0.1) !important;
        padding: 15px 0 !important;
      }
      
      .navbar-brand, .navbar-nav > li > a {
        color: white !important;
        font-weight: 600 !important;
        font-size: 16px !important;
        text-shadow: 0 1px 2px rgba(0,0,0,0.1) !important;
      }
      
      .navbar-nav > li > a:hover, .navbar-nav > li > a:focus {
        color: ", ocean_colors$accent, " !important;
        background-color: rgba(255,255,255,0.1) !important;
        border-radius: 5px !important;
      }
      
      .container-fluid {
        padding: 30px !important;
      }
      
      .well, .panel, .form-control, .selectize-input {
        background: rgba(255, 255, 255, 0.95) !important;
        border: 1px solid rgba(0, 119, 190, 0.2) !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 15px rgba(0, 119, 190, 0.1) !important;
        font-family: 'Josefin Sans', sans-serif !important;
      }
      
      .sidebar {
        background: rgba(255, 255, 255, 0.98) !important;
        border-radius: 15px !important;
        padding: 25px !important;
        margin-right: 20px !important;
        box-shadow: 0 8px 25px rgba(0, 119, 190, 0.15) !important;
        border: 1px solid rgba(0, 168, 204, 0.2) !important;
      }
      
      .main-panel {
        background: rgba(255, 255, 255, 0.98) !important;
        border-radius: 15px !important;
        padding: 25px !important;
        box-shadow: 0 8px 25px rgba(0, 119, 190, 0.15) !important;
        border: 1px solid rgba(0, 168, 204, 0.2) !important;
      }
      
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Josefin Sans', sans-serif !important;
        color: ", ocean_colors$primary, " !important;
        font-weight: 600 !important;
        margin-bottom: 20px !important;
      }
      
      .btn {
        font-family: 'Josefin Sans', sans-serif !important;
        font-weight: 500 !important;
        border-radius: 25px !important;
        padding: 10px 25px !important;
        transition: all 0.3s ease !important;
        border: none !important;
      }
      
      .btn-primary {
        background: linear-gradient(45deg, ", ocean_colors$primary, ", ", ocean_colors$secondary, ") !important;
        color: white !important;
        box-shadow: 0 4px 15px rgba(0, 119, 190, 0.3) !important;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 6px 20px rgba(0, 119, 190, 0.4) !important;
        background: linear-gradient(45deg, ", ocean_colors$secondary, ", ", ocean_colors$accent, ") !important;
      }
      
      .btn-success {
        background: linear-gradient(45deg, ", ocean_colors$seafoam, ", ", ocean_colors$accent, ") !important;
        color: ", ocean_colors$dark, " !important;
        box-shadow: 0 4px 15px rgba(152, 251, 152, 0.3) !important;
      }
      
      .btn-success:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 6px 20px rgba(152, 251, 152, 0.4) !important;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: ", ocean_colors$accent, " !important;
        box-shadow: 0 0 0 0.2rem rgba(64, 224, 208, 0.25) !important;
      }
      
      .checkbox input[type='checkbox']:checked + span::before {
        background-color: ", ocean_colors$primary, " !important;
        border-color: ", ocean_colors$primary, " !important;
      }
      
      .selectize-dropdown {
        border: 1px solid rgba(0, 119, 190, 0.2) !important;
        border-radius: 8px !important;
        box-shadow: 0 4px 15px rgba(0, 119, 190, 0.1) !important;
      }
      
      .selectize-dropdown-content .option:hover {
        background-color: ", ocean_colors$light, " !important;
        color: ", ocean_colors$primary, " !important;
      }
      
      .info-card {
        background: linear-gradient(135deg, ", ocean_colors$light, " 0%, rgba(255,255,255,0.9) 100%) !important;
        border: 1px solid rgba(0, 168, 204, 0.3) !important;
        border-radius: 15px !important;
        padding: 25px !important;
        margin-bottom: 20px !important;
        box-shadow: 0 6px 20px rgba(0, 119, 190, 0.1) !important;
      }
      
      .wave-divider {
        height: 30px;
        background: url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1200 120\" preserveAspectRatio=\"none\"><path d=\"M321.39,56.44c58-10.79,114.16-30.13,172-41.86,82.39-16.72,168.19-17.73,250.45-.39C823.78,31,906.67,72,985.66,92.83c70.05,18.48,146.53,26.09,214.34,3V0H0V27.35A600.21,600.21,0,0,0,321.39,56.44Z\" fill=\"", ocean_colors$primary, "\"></path></svg>') no-repeat center;
        background-size: cover;
        margin: 20px 0;
      }
      
      table {
        font-family: 'Josefin Sans', sans-serif !important;
        background: white !important;
        border-radius: 10px !important;
        overflow: hidden !important;
        box-shadow: 0 4px 15px rgba(0, 119, 190, 0.1) !important;
      }
      
      table th {
        background: linear-gradient(90deg, ", ocean_colors$primary, " 0%, ", ocean_colors$secondary, " 100%) !important;
        color: white !important;
        font-weight: 600 !important;
        padding: 15px !important;
        border: none !important;
      }
      
      table td {
        padding: 12px 15px !important;
        border-bottom: 1px solid rgba(0, 168, 204, 0.1) !important;
      }
      
      table tr:hover {
        background-color: ", ocean_colors$light, " !important;
      }
      
      .fa-info-circle {
        color: ", ocean_colors$secondary, " !important;
        font-size: 16px !important;
        margin-left: 8px !important;
        cursor: pointer !important;
        transition: color 0.3s ease !important;
      }
      
      .fa-info-circle:hover {
        color: ", ocean_colors$accent, " !important;
      }
      
      .checkbox-group-container {
        background: rgba(255, 255, 255, 0.7) !important;
        border-radius: 10px !important;
        padding: 15px !important;
        margin: 10px 0 !important;
        border: 1px solid rgba(0, 168, 204, 0.2) !important;
      }
      
      .tab-content {
        padding: 0 !important;
      }
      
      .ocean-gradient-text {
        background: linear-gradient(45deg, ", ocean_colors$primary, ", ", ocean_colors$secondary, ", ", ocean_colors$accent, ");
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        font-weight: 700;
      }
      
      .floating-card {
        background: rgba(255, 255, 255, 0.95);
        border-radius: 20px;
        padding: 30px;
        margin: 20px 0;
        box-shadow: 0 10px 30px rgba(0, 119, 190, 0.15);
        border: 1px solid rgba(64, 224, 208, 0.2);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
      }
      
      .floating-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 40px rgba(0, 119, 190, 0.2);
      }
      
      .page-header {
        text-align: center;
        padding: 40px 0;
        background: linear-gradient(135deg, rgba(255,255,255,0.9) 0%, rgba(230,243,255,0.9) 100%);
        border-radius: 20px;
        margin-bottom: 30px;
        border: 1px solid rgba(64, 224, 208, 0.3);
      }
      
      .feature-icon {
        font-size: 24px;
        color: ", ocean_colors$secondary, ";
        margin-right: 10px;
        vertical-align: middle;
      }
      
      .control-section {
        margin-bottom: 25px;
        padding: 20px;
        background: rgba(255, 255, 255, 0.8);
        border-radius: 12px;
        border: 1px solid rgba(0, 168, 204, 0.2);
        box-shadow: 0 2px 10px rgba(0, 119, 190, 0.05);
      }
      
      .section-divider {
        height: 2px;
        background: linear-gradient(90deg, transparent, ", ocean_colors$accent, ", transparent);
        margin: 20px 0;
        border-radius: 1px;
      }
      
      .download-section {
        background: linear-gradient(135deg, rgba(152, 251, 152, 0.1), rgba(64, 224, 208, 0.1));
        border: 1px solid rgba(152, 251, 152, 0.3);
      }
      
      .js-plotly-plot .sankey-link[style*='fill-opacity: 0.4'] {
        fill: ", ocean_colors$secondary, " !important;
        fill-opacity: 0.8 !important;
      }

    ")))
  ),
  
  navbarPage(
    title = tags$span(
      tags$i(class = "fas fa-water feature-icon"),
      "Marine Mammal Modelling Explorer"
    ),
    
    # Read me tab
    tabPanel(
      tags$span(tags$i(class = "fas fa-book-open"), " Read Me"), 
      div(class = "container-fluid",
          div(class = "page-header",
              h1(class = "ocean-gradient-text", "Welcome to the Marine Mammal Modelling Explorer"),
              p("An interactive platform for exploring process-based marine mammal models", 
                style = "font-size: 18px; color: #0077be; font-weight: 400;")
          ),
          
          div(class = "floating-card",
              h3(tags$i(class = "fas fa-compass feature-icon"), "Navigation Guide"),
              p("This application is supplementary material for Gallagher et al (2026): 
            Beneath the surface: Approaches to represent biological processes and responses 
            to disturbance in marine mammal models.", 
                style = "font-size: 16px; line-height: 1.6; margin-bottom: 20px;"),
              
              div(class = "row",
                  div(class = "col-md-4",
                      div(class = "info-card",
                          h4(tags$i(class = "fas fa-table feature-icon"), "Overview List"),
                          p("Explore, filter, visualize and download the complete dataset with interactive tools.")
                      )
                  ),
                  div(class = "col-md-4",
                      div(class = "info-card",
                          h4(tags$i(class = "fas fa-cogs feature-icon"), "Process Analysis"),
                          p("Examine data from the process perspective with filtering and visualization options.")
                      )
                  ),
                  div(class = "col-md-4",
                      div(class = "info-card",
                          h4(tags$i(class = "fas fa-sitemap feature-icon"), "Interactive Sankey"),
                          p("Explore connections between processes, disturbances, and outcomes through flow diagrams.")
                      )
                  )
              )
          )
      )
    ),       
    
    # Overview List Tab
    tabPanel(
      tags$span(tags$i(class = "fas fa-database"), " Overview List"), 
      div(class = "container-fluid",
          div(class = "floating-card", style = "margin-bottom: 30px;",
              h3(tags$i(class = "fas fa-search feature-icon"), "Interactive Data Explorer"),
              p("Filter, visualize, and download the full dataset. Use the controls below to customize your view and analysis.", 
                style = "color: #0077be; font-size: 16px;")
          ),
          
          sidebarLayout(
            sidebarPanel(width = 3, class = "sidebar",
                         # Column Selection Section
                         div(class = "control-section",
                             h4(tags$i(class = "fas fa-columns feature-icon"), "Select Columns"),
                             div(class = "info-card", style = "margin-bottom: 15px; padding: 10px;",
                                 tags$i(class = "fas fa-info-circle", style = "color: #00a8cc; margin-right: 8px;"),
                                 tags$span("For visualization: select up to 2 columns only", style = "color: #0077be; font-size: 14px;")
                             ),
                             uiOutput("OverviewTable_withInfo")
                         ),
                         
                         div(class = "section-divider"),
                         
                         # Filter Section
                         div(class = "control-section",
                             h4(tags$i(class = "fas fa-filter feature-icon"), "Filter Data"),
                             
                             selectizeInput("filter_authors", 
                                            label = tags$span(tags$i(class = "fas fa-user-edit"), " Authors:"),
                                            choices = sort(unique(overview$Authors)),
                                            selected = NULL, multiple = TRUE,
                                            options = list(plugins = list("remove_button"))
                             ),
                             
                             selectizeInput("filter_year", 
                                            label = tags$span(tags$i(class = "fas fa-calendar-alt"), " Year:"),
                                            choices = sort(unique(overview$Year)),
                                            selected = NULL, multiple = TRUE,
                                            options = list(plugins = list("remove_button"))
                             ),
                             
                             selectizeInput("filter_species", 
                                            label = tags$span(tags$i(class = "fas fa-otter"), " Species:"),
                                            choices = sort(split_multi_unique(overview, Species)),
                                            selected = NULL, multiple = TRUE,
                                            options = list(plugins = list("remove_button"))                                         
                             ),
                             
                             selectizeInput("filter_species_group", 
                                            label = tags$span(tags$i(class = "fas fa-layer-group"), " Species Group:"),
                                            choices = sort(split_multi_unique(overview, Species_group)),
                                            selected = NULL, multiple = TRUE,
                                            options = list(plugins = list("remove_button"))                                         
                             ),
                             
                             selectizeInput("filter_processes", 
                                            label = tags$span(tags$i(class = "fas fa-cog"), " Processes:"),
                                            choices = sort(split_multi_unique(overview, Processes)),
                                            selected = NULL, multiple = TRUE,
                                            options = list(plugins = list("remove_button"))                                         
                             ),
                             
                             selectizeInput("filter_disturbances", 
                                            label = tags$span(tags$i(class = "fas fa-exclamation-triangle"), " Disturbances:"),
                                            choices = sort(split_multi_unique(overview, Disturbances)),
                                            selected = NULL, multiple = TRUE,
                                            options = list(plugins = list("remove_button"))                                         
                             ),
                             
                             selectizeInput("filter_outcomes", 
                                            label = tags$span(tags$i(class = "fas fa-bullseye"), " Outcomes:"),
                                            choices = sort(split_multi_unique(overview, Outcomes)),
                                            selected = NULL, multiple = TRUE,
                                            options = list(plugins = list("remove_button"))                                         
                             )
                         ),
                         
                         div(class = "section-divider"),
                         
                         # Download Section
                         div(class = "control-section download-section",
                             h4(tags$i(class = "fas fa-download feature-icon"), "Download Data"),
                             p("Download the currently filtered dataset:", style = "color: #0077be; margin-bottom: 15px;"),
                             downloadButton("download_overview_data", 
                                            label = tags$span(tags$i(class = "fas fa-file-csv"), " Download CSV"),
                                            class = "btn-success",
                                            style = "width: 100%;")
                         )
            ),
            
            mainPanel(class = "main-panel",
                      # Toggle between table and plot
                      div(style = "margin-bottom: 20px;",
                          radioButtons("overview_view_type", 
                                       label = tags$span(tags$i(class = "fas fa-eye"), " View Type:"),
                                       choices = list("Data Table" = "table", "Visualization" = "plot"),
                                       selected = "table",
                                       inline = TRUE)
                      ),
                      
                      # Conditional display
                      conditionalPanel(
                        condition = "input.overview_view_type == 'table'",
                        div(style = "overflow-x: auto;",
                            DT::dataTableOutput("OverviewTable_Interactive")
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.overview_view_type == 'plot'",
                        conditionalPanel(
                          condition = "!input.pickColumn || input.pickColumn.length == 0",
                          div(class = "floating-card", style = "text-align: center; margin-top: 50px;",
                              tags$i(class = "fas fa-columns", style = "font-size: 48px; color: #00a8cc; margin-bottom: 20px;"),
                              h4("Select Columns", style = "color: #0077be;"),
                              p("Please select 1-2 columns from the sidebar to create a visualization", 
                                style = "color: #6c757d; font-size: 16px;")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.pickColumn && input.pickColumn.length > 2",
                          div(class = "floating-card", style = "text-align: center; margin-top: 50px; border-color: #ff6b6b;",
                              tags$i(class = "fas fa-exclamation-triangle", style = "font-size: 48px; color: #ff6b6b; margin-bottom: 20px;"),
                              h4("Too Many Columns", style = "color: #ff6b6b;"),
                              p("Please select only 1-2 columns for visualization. You have selected:", 
                                tags$span(textOutput("selected_columns_count", inline = TRUE), style = "font-weight: bold;"), 
                                style = "color: #6c757d; font-size: 16px;")
                          )
                        ),
                        conditionalPanel(
                          condition = "input.pickColumn && input.pickColumn.length > 0 && input.pickColumn.length <= 2",
                          div(style = "padding: 0; margin: 0; background: white; border-radius: 12px;",
                              plotOutput("OverviewTablePlot_Hist", height = "auto")
                          )
                        )
                      )
            )
          )
      )
    ),
    
    # Process Analysis Tab
    tabPanel(
      tags$span(tags$i(class = "fas fa-project-diagram"), " Process Analysis"),
      div(class = "container-fluid",
          div(class = "floating-card", style = "margin-bottom: 30px;",
              h3(tags$i(class = "fas fa-microscope feature-icon"), "Process Deep Dive"),
              p("Explore specific processes and their attributes across the publications. Select processes first, then choose attributes to view.", 
                style = "color: #0077be; font-size: 16px;")
          ),
          
          sidebarLayout(
            sidebarPanel(width = 3, class = "sidebar",
                         # Process Selection Section
                         div(class = "control-section",
                             h4(tags$i(class = "fas fa-cogs feature-icon"), "Select Processes"),
                             div(class = "checkbox-group-container",
                                 uiOutput("process_checkbox_ui")
                             )
                         ),
                         
                         # Attribute Selection Section (only show when processes are selected)
                         conditionalPanel(
                           condition = "input.filter_process_selection && input.filter_process_selection.length > 0",
                           div(class = "control-section",
                               h4(tags$i(class = "fas fa-tags feature-icon"), "Select Attributes"),
                               div(class = "info-card", style = "margin-bottom: 15px; padding: 10px;",
                                   tags$i(class = "fas fa-info-circle", style = "color: #00a8cc; margin-right: 8px;"),
                                   tags$span("For visualization: select up to 2 attributes only", style = "color: #0077be; font-size: 14px;")
                               ),
                               uiOutput("ProcessAttributes_withInfo")
                           )
                         ),
                         
                         div(class = "section-divider"),
                         
                         # Filter Section
                         conditionalPanel(
                           condition = "input.filter_process_selection && input.filter_process_selection.length > 0",
                           div(class = "control-section",
                               h4(tags$i(class = "fas fa-filter feature-icon"), "Additional Filters"),
                               
                               uiOutput("dynamic_attribute_filter"),
                               uiOutput("dynamic_value_filter")
                           )
                         ),
                         
                         div(class = "section-divider"),
                         
                         # Download Section
                         div(class = "control-section download-section",
                             h4(tags$i(class = "fas fa-download feature-icon"), "Download Data"),
                             p("Download the currently filtered dataset:", style = "color: #0077be; margin-bottom: 15px;"),
                             downloadButton("download_process_data", 
                                            label = tags$span(tags$i(class = "fas fa-file-csv"), " Download CSV"),
                                            class = "btn-success",
                                            style = "width: 100%;")
                         )
            ),
            
            mainPanel(class = "main-panel",
                      # Toggle between table and plot
                      div(style = "margin-bottom: 20px;",
                          radioButtons("process_view_type", 
                                       label = tags$span(tags$i(class = "fas fa-eye"), " View Type:"),
                                       choices = list("Data Table" = "table", "Visualization" = "plot"),
                                       selected = "table",
                                       inline = TRUE)
                      ),
                      
                      
                      
                      
                      conditionalPanel(
                        condition = "input.process_view_type == 'plot' && input.filter_process_selection && input.filter_process_selection.length > 0 && input.pickProcessAttributes && input.pickProcessAttributes.length > 0 && input.pickProcessAttributes.length <= 2",
                        div(style = "padding: 0; margin: 0; background: white; border-radius: 12px;",
                            plotOutput("ProcessPlot_Hist", height = "auto")
                        )
                      ),
                      
                      # Use uiOutput for dynamic content instead of conditionalPanel
                      uiOutput("process_main_content")
            )
          )
      )
    ),
    
    # Sankey tab 
    tabPanel(
      tags$span(tags$i(class = "fas fa-sitemap"), " Interactive Sankey"), 
      div(class = "container-fluid",
          div(class = "floating-card", style = "margin-bottom: 30px;",
              h3(tags$i(class = "fas fa-circle-nodes feature-icon"), "Relationship Explorer"),
              p("Discover connections between processes, disturbances, and outcomes through interactive flow diagrams.", 
                style = "color: #0077be; font-size: 16px;")
          ),
          
          sidebarLayout(
            sidebarPanel(width = 3, class = "sidebar",
                         h4(tags$i(class = "fas fa-network-wired feature-icon"), "Flow Configuration"),
                         div(class = "checkbox-group-container",
                             checkboxGroupInput("sankey_categories", 
                                                label = tags$span(tags$i(class = "fas fa-stream"), " Choose Categories:"),
                                                choices = c("Processes", "Disturbances", "Outcomes"),
                                                selected = c("Processes", "Disturbances"))
                         ),
                         
                         div(class = "info-card",
                             tags$i(class = "fas fa-info-circle feature-icon"),
                             "Select at least 2 categories to generate the Sankey diagram."
                         )
            ),
            
            mainPanel(class = "main-panel",
                      conditionalPanel(
                        condition = "output.sankey_ready",
                        plotlyOutput("sankey_plot", height = "600px")
                      ),
                      conditionalPanel(
                        condition = "!output.sankey_ready",
                        div(class = "floating-card", style = "text-align: center; margin-top: 50px;",
                            tags$i(class = "fas fa-info-circle", style = "font-size: 48px; color: #00a8cc; margin-bottom: 20px;"),
                            h4("Select Categories", style = "color: #0077be;"),
                            p("Please choose at least 2 categories to display the interactive Sankey diagram", 
                              style = "color: #6c757d; font-size: 16px;")
                        )
                      )
            )
          )
      )
    )
  )
)




#### SERVER ####
server <- function(input, output, session) {
  
  # Add Font
  font_add(family = "Josefin Sans", regular = "Typeface/JosefinSans-Regular.ttf", bold = "Typeface/JosefinSans-Bold.ttf")
  showtext_auto()
  
  # Custom ggplot theme for ocean styling
  ocean_theme <- function() {
    theme_minimal() +
      theme(
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 9, face = "bold"),
        text = element_text(family = "Josefin Sans", color = ocean_colors$primary, size = 8),
        panel.grid.major = element_line(color = alpha(ocean_colors$secondary, 0.3), size = 0.4),
        panel.grid.minor = element_line(color = alpha(ocean_colors$secondary, 0.2), size = 0.2),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }
  
  # ------------- Overview Table Functions -------------
  
  # Column selection UI
  output$OverviewTable_withInfo <- renderUI({
    all_cols <- colnames(overview)[-c(1,5)] # removing ID and Source from the list
    
    checkbox_list <- lapply(all_cols, function(col) {
      info_icon <- NULL
      if (!is.null(column_tooltips[[col]])) {
        icon_tag <- tags$i(class = "fas fa-info-circle", style = "color: #00a8cc; cursor: pointer; margin-left: 8px;")
        info_icon <- bs_embed_tooltip(icon_tag, title = column_tooltips[[col]],
                                      placement = "right", options = list(container = "body"))
      }
      
      tags$div(style = "margin-bottom: 12px; padding: 8px; border-radius: 8px; background: rgba(230,243,255,0.3);",
               tags$label(class = "checkbox-inline",
                          style = "display: inline-flex; align-items: center; gap: 8px; margin-left: 0; font-weight: 500;",
                          tags$input(type = "checkbox", name = "pickColumn", value = col,
                                     checked = NULL, style = "margin: 0; transform: scale(1.2);"),
                          tags$span(gsub("_", " ", col), style = "color: #0077be; font-size: 14px; margin-left: 20px;"),
                          info_icon
               )
      )
    })
    
    tags$div(class = "form-group shiny-input-checkboxgroup", id = "pickColumn", checkbox_list)
  })
  
  observe({
    session$sendCustomMessage("bindCheckboxGroup", "pickColumn")
  })
  
  
  # Reactive data filtering
  filtered_overview_data <- reactive({
    comma_separated_cols <- c("Species", "Species_group", "Processes", "Disturbances", "Outcomes")
    normal_cols <- c("Authors", "Year")
    
    filtered <- overview
    original <- overview %>% mutate(row_id = row_number())
    
    all_filter_cols <- c(comma_separated_cols, normal_cols)
    
    any_selected <- any(sapply(all_filter_cols, function(col) {
      input_id <- paste0("filter_", tolower(col))
      !is.null(input[[input_id]]) && length(input[[input_id]]) > 0
    }))
    
    if (!any_selected) {
      return(overview)
    }
    
    for (col in all_filter_cols) {
      input_id <- paste0("filter_", tolower(gsub(" ", "_", col)))
      selected_vals <- input[[input_id]]
      
      if (!is.null(selected_vals) && length(selected_vals) > 0) {
        
        if (col %in% comma_separated_cols) {
          matching_ids <- original %>%
            select(row_id, all_of(col)) %>%
            separate_rows(all_of(col), sep = ",") %>%
            mutate(across(all_of(col), str_trim)) %>%
            filter(.data[[col]] %in% selected_vals) %>%
            pull(row_id)
          
          filtered <- filtered %>%
            mutate(row_id = row_number()) %>%
            filter(row_id %in% matching_ids) %>%
            select(-row_id)
          
        } else {
          filtered <- filtered %>%
            filter(.data[[col]] %in% selected_vals)
        }
      }
    }
    
    return(filtered %>% distinct())
  })
  
  # Column subset reactive
  dataColumnsubset <- reactive({
    data <- filtered_overview_data()
    if (!is.null(input$pickColumn) && length(input$pickColumn) > 0) {
      # Remove ID if it exists in selection
      selected_cols <- input$pickColumn[!input$pickColumn %in% c("ID")]
      if (length(selected_cols) > 0) {
        data %>% select(all_of(selected_cols))
      } else {
        data[, -c(1), with = FALSE] # Show all except ID 
      }
    } else {
      data[, -c(1), with = FALSE] # Show all except ID 
    }
  })
  
  # Output for selected columns count
  output$selected_columns_count <- renderText({
    if (!is.null(input$pickColumn)) {
      length(input$pickColumn)
    } else {
      0
    }
  })
  
  # Interactive DataTable
  output$OverviewTable_Interactive <- DT::renderDataTable({
    DT::datatable(
      dataColumnsubset(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      style = 'bootstrap',
      class = 'table-striped table-hover',
      rownames = FALSE
    )
  })
  
  # Plot output
  output$OverviewTablePlot_Hist <- renderPlot({
    req(input$pickColumn)
    cols <- input$pickColumn
    
    if (length(cols) == 0 || length(cols) > 2) {
      return(NULL)
    }
    
    # Use filtered data for plotting
    plot_data <- filtered_overview_data()
    
    if (length(cols) == 1) {
      if (cols %in% multi_cols || needs_split(plot_data[[cols]])) {
        data <- split_multi(plot_data, all_of(cols))
      } else {
        data <- plot_data
      }
      
      count_data <- data %>% count(.data[[cols]])
      
      ggplot(count_data, aes(x = reorder(.data[[cols]], -n), y = n)) +
        geom_bar(stat = "identity", fill = ocean_colors$secondary, alpha = 0.8, color = ocean_colors$primary, size = 0.5) +
        coord_flip() +
        labs(x = cols, y = "Number of studies", 
             title = paste("Distribution of", cols),
             subtitle = "Marine Mammal Modelling Data (Filtered)") +
        ocean_theme() 
      
    } else {
      if (cols[1] %in% multi_cols || needs_split(plot_data[[cols[1]]])) {
        data <- split_multi(plot_data, all_of(cols[1]))
      } else {
        data <- plot_data
      }
      
      if (cols[2] %in% multi_cols || needs_split(plot_data[[cols[2]]])) {
        data <- split_multi(data, all_of(cols[2]))
      }
      
      count_data <- data %>%
        count(.data[[cols[1]]], .data[[cols[2]]]) %>%
        mutate(across(c(cols[1], cols[2]), ~ droplevels(factor(.))))
      
      ggplot(count_data, aes(x = reorder(.data[[cols[1]]], -n), y = n)) +
        geom_bar(stat = "identity", fill = ocean_colors$secondary, alpha = 0.8, color = ocean_colors$primary, size = 0.5) +
        coord_flip() +
        facet_wrap(as.formula(paste("~", cols[2])), drop = TRUE, scales = "free_y") +
        labs(x = cols[1], y = "Count",
             title = paste("Distribution of", cols[1], "by", cols[2]),
             subtitle = "Marine Mammal Modelling Data (Filtered)") +
        ocean_theme() +
        theme(strip.background = element_rect(fill = ocean_colors$light, color = ocean_colors$secondary))
    }
  },
  height = function() {
    req(input$pickColumn)
    cols <- input$pickColumn
    
    if (length(cols) == 0 || length(cols) > 2) {
      return(500)
    }
    
    if (length(cols) == 1) {
      plot_data <- filtered_overview_data()
      if (cols %in% multi_cols || needs_split(plot_data[[cols]])) {
        data <- split_multi(plot_data, all_of(cols))
      } else {
        data <- plot_data
      }
      
      n_categories <- nrow(data %>% count(.data[[cols]]))
      base_height <- 400
      height_per_category <- 25
      min(base_height + n_categories * height_per_category, 1200)
      
    } else if (length(cols) == 2) {
      plot_data <- filtered_overview_data()
      data <- plot_data
      if (cols[1] %in% multi_cols || needs_split(plot_data[[cols[1]]])) {
        data <- split_multi(data, all_of(cols[1]))
      }
      if (cols[2] %in% multi_cols || needs_split(data[[cols[2]]])) {
        data <- split_multi(data, all_of(cols[2]))
      }
      
      n_facets <- n_distinct(data[[cols[2]]])
      rows_of_facets <- ceiling(n_facets / 2)
      height <- 400 + rows_of_facets * 200
      min(height, 1400)
    } else {
      return(500)
    }
  })
  
  # Download handler
  output$download_overview_data <- downloadHandler(
    filename = function() {
      paste("marine_mammal_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataColumnsubset(), file, row.names = FALSE)
    }
  )
  

  # ------------- Process Table Functions -------------
  
  # Process checkbox UI
  output$process_checkbox_ui <- renderUI({
    
    procsFactLevs <- c("Energetics", "Health & Physiology", "Movement", "Behavior & Adaptation", "Evolution & Genetics", "Mortality", "Reproduction", "Aggregated demographics", "Immigration & Emigration", "Density dependence", "Biotic interactions")
    
    all_processes <- factor(unique(process$Process), levels = procsFactLevs)
      
    
    checkboxGroupInput("filter_process_selection",
                       label = NULL,
                       choices = setNames(all_processes, all_processes),
                       selected = NULL)
    
    checkbox_list <- lapply(all_processes, function(proc) {
      info_icon <- NULL
      proc_key <- gsub(" & ", " & ", proc) # Handle the key matching
      proc_key <- gsub("Aggregated demographics", "Aggregated demographics", proc_key) # Exact match
      
      if (!is.null(process_tooltips[[proc]])) {
        icon_tag <- tags$i(class = "fas fa-info-circle", style = "color: #00a8cc; cursor: pointer; margin-left: 8px;")
        info_icon <- bs_embed_tooltip(icon_tag, title = process_tooltips[[proc]],
                                      placement = "right", options = list(container = "body"))
      }
      
      tags$div(style = "margin-bottom: 12px; padding: 8px; border-radius: 8px; background: rgba(230,243,255,0.3);",
               tags$label(class = "checkbox-inline",
                          style = "display: inline-flex; align-items: center; gap: 8px; margin-left: 0; font-weight: 500;",
                          tags$input(type = "checkbox", name = "filter_process_selection", value = proc,
                                     checked = NULL, style = "margin: 0; transform: scale(1.2);"),
                          tags$span(proc, style = "color: #0077be; font-size: 14px; margin-left: 20px;"),
                          info_icon
               )
      )
    })
    
    tags$div(class = "form-group shiny-input-checkboxgroup", id = "filter_process_selection", checkbox_list)
    
  })
  
  observe({
    session$sendCustomMessage("bindCheckboxGroup", "filter_process_selection")
  })
  
  
  
  # Reactive data for selected processes (base filtering)
  process_data_filtered_by_selection <- reactive({
    if (is.null(input$filter_process_selection) || length(input$filter_process_selection) == 0) {
      return(data.frame()) # This is correct
    }
    
    # DEBUG: Print to console what processes are selected
    cat("Selected processes:", paste(input$filter_process_selection, collapse = ", "), "\n")
    
    result <- process %>% filter(Process %in% input$filter_process_selection)
    
    # DEBUG: Print number of rows returned
    cat("Filtered data has", nrow(result), "rows\n")
    
    return(result)
  })
  
  # Dynamic attribute filter based on selected processes
  output$dynamic_attribute_filter <- renderUI({
    process_data <- process_data_filtered_by_selection()
    
    if (nrow(process_data) == 0) {
      return(NULL)
    }
    
    available_attributes <- sort(unique(process_data$Attribute))
    
    selectizeInput("filter_attribute", 
                   label = tags$span(tags$i(class = "fas fa-tags"), " Specific Attributes:"),
                   choices = available_attributes,
                   selected = NULL, multiple = TRUE,
                   options = list(plugins = list("remove_button"))
    )
  })
  
  # Dynamic value filter based on selected processes and attributes
  output$dynamic_value_filter <- renderUI({
    process_data <- process_data_filtered_by_selection()
    
    if (nrow(process_data) == 0) {
      return(NULL)
    }
    
    # Further filter by attribute if attribute filter is applied
    if (!is.null(input$filter_attribute) && length(input$filter_attribute) > 0) {
      process_data <- process_data %>% filter(Attribute %in% input$filter_attribute)
    }
    
    available_values <- sort(unique(process_data$Value))
    
    selectizeInput("filter_value", 
                   label = tags$span(tags$i(class = "fas fa-list-ul"), " Specific Values:"),
                   choices = available_values,
                   selected = NULL, multiple = TRUE,
                   options = list(plugins = list("remove_button"))
    )
  })
  
  # Attribute selection UI based on selected processes
  output$ProcessAttributes_withInfo <- renderUI({
    process_data <- process_data_filtered_by_selection()
    
    if (nrow(process_data) == 0) {
      return(div(class = "info-card",
                 tags$i(class = "fas fa-info-circle", style = "color: #00a8cc; margin-right: 8px;"),
                 "Please select processes first to see available attributes"))
    }
    
    available_attributes <- sort(unique(process_data$Attribute))
    
    checkboxGroupInput("pickProcessAttributes",
                       label = NULL,
                       choices = setNames(available_attributes, available_attributes),
                       selected = available_attributes)
  })
  
  # Reactive data filtering for process analysis
  filtered_process_data <- reactive({
    # Start with process-selected data
    filtered <- process_data_filtered_by_selection()
    
    if (nrow(filtered) == 0) {
      return(filtered)
    }
    
    # Validate that required columns exist
    required_cols <- c("Process", "Attribute", "Value")
    if (!all(required_cols %in% names(filtered))) {
      warning("Missing required columns in process data")
      return(data.frame())
    }
    
    # Apply additional filters
    if (!is.null(input$filter_attribute) && length(input$filter_attribute) > 0) {
      filtered <- filtered %>% filter(Attribute %in% input$filter_attribute)
    }
    
    if (!is.null(input$filter_value) && length(input$filter_value) > 0) {
      filtered <- filtered %>% filter(Value %in% input$filter_value)
    }
    
    # FIX 4b: Remove any rows with missing critical data
    filtered <- filtered %>%
      filter(!is.na(Process), !is.na(Attribute), !is.na(Value)) %>%
      distinct()
    
    return(filtered)
  })
  
  # Attribute subset reactive for process data
  processAttributeSubset <- reactive({
    data <- filtered_process_data()
    
    # DEBUG: Print data info
    cat("processAttributeSubset - filtered data rows:", nrow(data), "\n")
    
    if (nrow(data) == 0) {
      return(data.frame())
    }
    
    # If no attributes are selected, show the long format data as-is
    if (is.null(input$pickProcessAttributes) || length(input$pickProcessAttributes) == 0) {
      # Show all data in long format for table view
      result <- data %>% 
        select(ID, Process, Attribute, Value) %>%
        arrange(ID, Process, Attribute)
      
      cat("Returning long format data with", nrow(result), "rows\n")
      return(result)
    }
    
    # If attributes are selected, create wide format for specific attributes only
    cat("Selected attributes:", paste(input$pickProcessAttributes, collapse = ", "), "\n")
    
    # Filter to only the selected attributes first
    attr_filtered <- data %>%
      filter(Attribute %in% input$pickProcessAttributes)
    
    if (nrow(attr_filtered) == 0) {
      cat("No data for selected attributes\n")
      return(data.frame())
    }
    
    # Create wide format
    data_wide <- attr_filtered %>%
      pivot_wider(
        names_from = Attribute,
        values_from = Value,
        values_fn = list(Value = ~paste(unique(.), collapse = "; "))
      ) %>%
      select(ID, Process, all_of(input$pickProcessAttributes))
    
    cat("Returning wide format data with", nrow(data_wide), "rows\n")
    return(data_wide)
  })
  
  
  
  # Output for selected attributes count
  output$selected_process_attributes_count <- renderText({
    if (!is.null(input$pickProcessAttributes)) {
      length(input$pickProcessAttributes)
    } else {
      0
    }
  })
  
  # Interactive DataTable for process data
  output$ProcessTable_Interactive <- DT::renderDataTable({
    data <- processAttributeSubset()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    DT::datatable(
      data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      style = 'bootstrap',
      class = 'table-striped table-hover',
      rownames = FALSE
    )
  })
  
  # Plot output for process data
  output$ProcessPlot_Hist <- renderPlot({
    # Require both process selection and attribute selection
    req(input$filter_process_selection)
    req(input$pickProcessAttributes)
    
    attrs <- input$pickProcessAttributes
    
    if (length(attrs) == 0 || length(attrs) > 2) {
      return(NULL)
    }
    
    # Use filtered data for plotting
    plot_data <- filtered_process_data()
    
    if (nrow(plot_data) == 0) {
      return(NULL)
    }
    
    if (length(attrs) == 1) {
      # Single attribute plotting
      attr_data <- plot_data %>% 
        filter(Attribute == attrs[1]) %>%
        count(Process, Value, name = "n") %>%
        arrange(desc(n))
      
      if (nrow(attr_data) == 0) {
        return(NULL)
      }
      
      
      process_levels <- unique(attr_data$Process)
      fill_col <- setNames(
        unlist(ocean_colors)[2:(2 + length(process_levels))],
        process_levels
      )
      
      ggplot(attr_data, aes(x = reorder(Value, -n), y = n, fill = Process)) +
        geom_bar(stat = "identity", alpha = 0.8, 
                 color = ocean_colors$primary, linewidth = 0.5) +
        scale_fill_manual(values = fill_col) +
        coord_flip() +
        labs(x = attrs[1], y = "Number of studies", 
             title = paste("Distribution of", attrs[1]),
             subtitle = "Marine Mammal Process Data (Filtered)") +
        ocean_theme()
      
    } else if (length(attrs) == 2) {
      
      
      # Filter to relevant attributes and clean
      plot_data_clean <- plot_data %>%
        filter(Attribute %in% attrs) %>%
        select(ID, Process, Attribute, Value) %>%
        filter(!is.na(Value), Value != "") %>%
        mutate(study_key = paste(ID, Process, sep = "_"))
      
      # Separate data by attribute
      attr1_data <- plot_data_clean %>%
        filter(Attribute == attrs[1]) %>%
        select(Process, study_key, Value) %>%
        rename(Value1 = Value)
      
      attr2_data <- plot_data_clean %>%
        filter(Attribute == attrs[2]) %>%
        select(Process, study_key, Value) %>%
        rename(Value2 = Value)
      
      # Join to get combinations
      combo_data <- inner_join(attr1_data, attr2_data, by = "study_key", relationship = "many-to-many") %>%
        select(Process = Process.x, study_key, Value1, Value2)
      
      
      # Count combinations
      count_data <- combo_data %>%
        count(Process, Value1, Value2, name = "n", .drop = FALSE)
      
      
      process_levels <- unique(count_data$Process)
      fill_col <- setNames(
        unlist(ocean_colors)[2:(2 + length(process_levels))],
        process_levels
      )
      
      
      # Plot
      ggplot(count_data, aes(x = reorder(Value1, -n), y = n, fill = Process)) +
        geom_bar(stat = "identity", alpha = 0.8, 
                 color = ocean_colors$primary, linewidth = 0.5) +
        coord_flip() +
        scale_fill_manual(values = fill_col) +
        facet_wrap(vars(Value2), scales = "free_y", ncol = 2) +
        labs(x = attrs[1], y = "Count",
             title = paste("Distribution of", attrs[1], "by", attrs[2]),
             subtitle = "Marine Mammal Process Data (Filtered)") +
        ocean_theme() +
        theme(
          strip.background = element_rect(fill = ocean_colors$light, color = ocean_colors$secondary),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 0)
        )
      
      
    }
  }, 
  
  height = function() {
    req(input$pickProcessAttributes)
    attrs <- input$pickProcessAttributes
    
    if (length(attrs) == 0 || length(attrs) > 2) {
      return(500)
    }
    
    if (length(attrs) == 1) {
      plot_data <- filtered_process_data()
      if (nrow(plot_data) == 0) return(500)
      
      attr_data <- plot_data %>% 
        filter(Attribute == attrs[1]) %>%
        count(Value, name = "n")
      
      n_categories <- nrow(attr_data)
      base_height <- 400
      height_per_category <- 25
      min(base_height + n_categories * height_per_category, 1200)
      
    } else if (length(attrs) == 2) {
      plot_data <- filtered_process_data()
      if (nrow(plot_data) == 0) return(500)
      
      n_facets <- plot_data %>% 
        filter(Attribute == attrs[2]) %>% 
        pull(Value) %>% 
        n_distinct()
      
      rows_of_facets <- ceiling(n_facets / 2)
      height <- 400 + rows_of_facets * 200
      min(height, 1400)
    } else {
      return(500)
    }
  })
  
  # Download handler for process data
  output$download_process_data <- downloadHandler(
    filename = function() {
      paste("marine_mammal_process_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processAttributeSubset(), file, row.names = FALSE)
    }
  )
  
  output$process_main_content <- renderUI({
    if (input$process_view_type == "table") {
      # Table view logic
      if (is.null(input$filter_process_selection) || length(input$filter_process_selection) == 0) {
        div(class = "floating-card", style = "text-align: center; margin-top: 50px;",
            tags$i(class = "fas fa-cogs", style = "font-size: 48px; color: #00a8cc; margin-bottom: 20px;"),
            h4("Select Processes", style = "color: #0077be;"),
            p("Please select one or more processes from the sidebar to view the data", 
              style = "color: #6c757d; font-size: 16px;")
        )
      } else {
        div(style = "overflow-x: auto;",
            DT::dataTableOutput("ProcessTable_Interactive")
        )
      }
    } else {
      # Plot view
      if (is.null(input$filter_process_selection) || length(input$filter_process_selection) == 0) {
        div(class = "floating-card", style = "text-align: center; margin-top: 50px;",
            tags$i(class = "fas fa-cogs", style = "font-size: 48px; color: #00a8cc; margin-bottom: 20px;"),
            h4("Select Processes", style = "color: #0077be;"),
            p("Please select one or more processes from the sidebar to create visualizations", 
              style = "color: #6c757d; font-size: 16px;")
        )
      } else if (is.null(input$pickProcessAttributes) || length(input$pickProcessAttributes) == 0) {
        div(class = "floating-card", style = "text-align: center; margin-top: 50px;",
            tags$i(class = "fas fa-tags", style = "font-size: 48px; color: #00a8cc; margin-bottom: 20px;"),
            h4("Select Attributes", style = "color: #0077be;"),
            p("Please select 1-2 attributes from the sidebar to create a visualization", 
              style = "color: #6c757d; font-size: 16px;")
        )
      } else if (length(input$pickProcessAttributes) > 2) {
        div(class = "floating-card", style = "text-align: center; margin-top: 50px; border-color: #ff6b6b;",
            tags$i(class = "fas fa-exclamation-triangle", style = "font-size: 48px; color: #ff6b6b; margin-bottom: 20px;"),
            h4("Too Many Attributes", style = "color: #ff6b6b;"),
            p(paste("Please select only 1-2 attributes for visualization. You have selected:", 
                    length(input$pickProcessAttributes)), 
              style = "color: #6c757d; font-size: 16px;")
        )
      }
    }
  })
  
  # ------------- Sankey Diagrams -------------
  
  # Check if enough categories are selected
  output$sankey_ready <- reactive({
    length(input$sankey_categories) >= 2
  })
  outputOptions(output, "sankey_ready", suspendWhenHidden = FALSE)
  
  # Create Sankey plot
  output$sankey_plot <- renderPlotly({
    req(input$sankey_categories)
    
    if (length(input$sankey_categories) < 2) {
      return(NULL)
    }
    
    # Create sankey data
    sankey_data <- create_sankey_data(input$sankey_categories, all_categories)
    
    if (nrow(sankey_data) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "No connections found for selected categories", 
                               showarrow = FALSE,
                               font = list(family = "Josefin Sans", size = 16, color = ocean_colors$primary)))
    }
    
    # Create nodes with ocean-themed colors
    all_nodes <- unique(c(as.character(sankey_data$source), as.character(sankey_data$target)))
    node_df <- data.frame(
      label = all_nodes,
      id = 0:(length(all_nodes)-1)
    ) %>%
      mutate(
        color = case_when(
          label %in% (Processes %>% pull(value) %>% as.character()) ~ ocean_colors$primary,    
          label %in% (Disturbances %>% pull(value) %>% as.character()) ~ ocean_colors$coral,   
          label %in% (Outcomes %>% pull(value) %>% as.character()) ~ ocean_colors$accent,     
          TRUE ~ ocean_colors$seafoam  
        )
      )
    
    # Map source and target to node IDs
    sankey_data <- sankey_data %>%
      left_join(node_df, by = c("source" = "label")) %>%
      rename(source_id = id) %>%
      left_join(node_df, by = c("target" = "label")) %>%
      rename(target_id = id)
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      
      node = list(
        label = node_df$label,
        color = node_df$color,
        pad = 20,
        thickness = 25,
        line = list(color = ocean_colors$dark, width = 1),
        hovertemplate = '<b>%{label}</b><br>Total connections: %{value:.0f}<extra></extra>'
      ),
      
      link = list(
        source = sankey_data$source_id,
        target = sankey_data$target_id,
        value = sankey_data$flow_value,
        color = paste0(ocean_colors$secondary, "20"),  # Semi-transparent links
        hovertemplate = '<b>%{source.label}</b> → <b>%{target.label}</b><br>Connections: %{value:.0f}<extra></extra>'
      )
    ) %>%
      layout(
        title = list(
          text = paste("Marine Mammal Model Connections:", paste(input$sankey_categories, collapse = " → ")),
          font = list(family = "Josefin Sans", size = 20, color = ocean_colors$primary)
        ),
        font = list(family = "Josefin Sans", size = 12, color = ocean_colors$dark),
        paper_bgcolor = "rgba(255,255,255,0.9)",
        plot_bgcolor = "rgba(255,255,255,0.9)",
        margin = list(t = 80)  
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d")
      )
  })
}

#### JS Handler ####
js <- "
Shiny.addCustomMessageHandler('bindCheckboxGroup', function(id) {
  var el = document.getElementById(id);
  if (el) {
    $(el).find('input[type=checkbox]').on('change', function() {
      var values = $(el).find('input:checked').map(function() {
        return this.value;
      }).get();
      Shiny.setInputValue(id, values);
    });
    var initVals = $(el).find('input:checked').map(function() {
      return this.value;
    }).get();
    Shiny.setInputValue(id, initVals);
  }
});
"

js_additional <- "
// Fix for process attribute selection reactivity
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'pickProcessAttributes' || event.name === 'filter_process_selection') {
    // Force re-evaluation of conditional panels
    setTimeout(function() {
      $(window).trigger('resize');
    }, 100);
  }
});
"

#### RUN APP ####
shinyApp(
  ui = ui,
  server = server
)