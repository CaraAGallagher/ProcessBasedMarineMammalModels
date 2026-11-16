# Beneath the Surface: Approaches to Represent Biological Processes and Responses to Disturbance in Marine Mammal Models

This repository contains the data, code, and supporting materials associated with the manuscript:

> Gallagher, C.A., Chudzińska, M., Railsback, S., Nabe-Nielsen, J., Beltran, R.S., Srinivasan, M., Speakman, C., Barber-Meyer, S., Buren, A., Szangolies, L., Joy, R., Adamczak, S., Desforges, J.-P., Myers, L.S., Tuhuteru, N.A.M., Hin, V., Keen, K.A., & Chimienti, M.
>
> **Beneath the Surface: Approaches to Represent Biological Processes and Responses to Disturbance in Marine Mammal Models**
>
> Accepted in *Global Change Biology*.

## Overview

This repository includes:

- The compiled database from a systematic review of marine mammal process-based models.
- Data used for analyses and the accompanying Shiny application.
- R scripts used for data preparation, analysis, and figure generation.
- Literature search records from Web of Science and Scopus.
- Files required to reproduce the analyses presented in the manuscript.

## Repository Structure

```text
Data/
├── Data files used in analyses and the Shiny application

Figures/
├── Figures included in the manuscript and supplementary materials

LitSearch/
├── Literature search outputs, including CSV and BibTeX files from
│   Web of Science and Scopus searches

Typeface/
├── Josefin Sans TrueType font files used in figures and the Shiny application

AnalysisScript.R
├── Main analysis script used to generate results and figures

SystematicSearchRecords.R
├── Script used to process and summarize literature search records

PreppingTableforShiny.R
├── Script used to prepare review data for the Shiny application

app.R
├── Shiny application source code

LICENSE
├── MIT License governing code in this repository

README.md
├── Repository documentation
```

## Data Description

The dataset was compiled through a systematic review of marine mammal process-based models published between 1990 and 2025.

Information extracted from each study includes model characteristics, biological process representation, disturbance sources, ecological scales, outcomes, code availability, and other metadata relevant to model structure and application.

Descriptions of all variables, classifications, and category definitions are provided in the manuscript and Appendix A, which serve as the metadata record and data dictionary for this repository.

An interactive version of the database is available through the accompanying Shiny application:

https://caragallagher.shinyapps.io/pbm3shiny/

## Reproducibility

Analyses were conducted in R.

To reproduce the analyses:

1. Clone or download this repository.
2. Open the R scripts in your preferred R environment.
3. Install required package dependencies.
4. Run the scripts following the workflow described in the code comments.

## Citation

### Manuscript

To be updated upon publication.

### Dataset and Code

Zenodo. https://doi.org/10.5281/zenodo.24276564

Recommended citation format:

> Gallagher, C.A., Chudzińska, M., & Chimienti, M. (2026). *Data and code for "Beneath the Surface: Approaches to Represent Biological Processes and Responses to Disturbance in Marine Mammal Models"*. Zenodo. https://doi.org/10.5281/zenodo.24276564


## Licensing

### Code

All code in this repository is released under the MIT License. See the `LICENSE` file for details.

### Data

The compiled review dataset is released under the Creative Commons Attribution 4.0 International (CC BY 4.0) License.

Users are free to copy, redistribute, adapt, and build upon the dataset provided appropriate attribution is given.

## Authors

### Cara Alyse Gallagher

- ORCID: 0000-0001-7094-1752
- Aarhus University, Denmark
- Email: cara.gallagher@ecos.au.dk

### Magda Chudzińska

- ORCID: 0000-0001-9568-1504
- SMRU Consulting, University of St Andrews, UK
- Email: mc@smruconsulting.com

### Marianna Chimienti

- ORCID: 0000-0002-8236-9332
- Bangor University, UK
- Email: m.chimienti@bangor.ac.uk

## Funding

We acknowledge the DISCAR (Population Consequences of DISturbance on Small CARnivores) working group at CESAB (Centre for the Synthesis and Analysis of Biodiversity), whose discussions contributed to this work.

Funding for Cara Alyse Gallagher was provided through the DFG-funded research training group BioMove (Deutsche Forschungsgemeinschaft Grant GRK 2118).

## Contact

For questions regarding the dataset, code, or repository, please contact:

**Cara Alyse Gallagher**  
cara.gallagher@ecos.au.dk
