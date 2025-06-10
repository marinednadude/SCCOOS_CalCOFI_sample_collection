# Archived DNA Reveals Marine Heatwave-Associated Shifts in Fish Assemblages

Zachary Gold<sup>1 , 2</sup> , Ryan P. Kelly<sup>3</sup>, Andrew Olaf Shelton<sup>2</sup>, Andrew Thompson<sup>4</sup>, Kelly D. Goodwin<sup>5</sup>, Ramon Gallego<sup>2</sup>, Kim Parsons<sup>2</sup>, Luke R. Thompson<sup>5 , 6</sup>,  Dovi Kacev<sup>7</sup>, Paul H. Barber<sup>8</sup>

<sup>1</sup> Cooperative Institute for Climate, Ocean, & Ecosystem Studies, UW, Seattle, WA <br />
<sup>2</sup> Northwest Fisheries Science Center, NMFS/NOAA, Seattle, WA <br />
<sup>3</sup> School of Marine and Environmental Affairs, UW, Seattle, WA <br />
<sup>4</sup> Southwest Fisheries Science Center, NMFS/NOAA, La Jolla, CA <br />
<sup>5</sup> Ocean Chemistry and Ecosystems Division, Atlantic Oceanographic and Meteorological Laboratory, Miami, FL <br />
<sup>6</sup> Northern Gulf Institute, Mississippi State University, Mississippi State, MS <br />
<sup>7</sup> Scripps Institution of Oceanography, UCSD, La Jolla <br />
<sup>8</sup> Department of Ecology and Evolutionary Biology, UCLA, Los Angeles, CA <br />

## Description
This page is dedicated to hosting data and code generated for the manuscript.

Included on this page is
1. Scripts used to Conduct Analyses

    /analysis

      1. *CalCOFI_results_short_20210928.Rmd* This script does the main analyses in the paper.
      2. *calcofi_metadata_20210907.Rmd* This script organizes metadata.
      3. *Calcofi_satellite_data_2021090-7_.Rmd* This script obtains SST data for analyses.
      4. *Calcofi_edna_vs_morphology_20210908.Rmd* This script formats data for the STAN model.
      5. *Run_Mifish_Joint_Model.R* This script runs the joint STAN model
      6. *Mifish_Joint_Model.stan* This is the STAN model.

  /decon

      1. *20210927_Calcofi_decontam_zjg.R* This script processes the "raw" *Anacapa Toolkit* output and runs decontamination to remove poorly sequenced samples and contaminant ASVs
      2. *20210927_Merge_data.Rmd* This script formats the output from decontamination and sums ASVs by taxonomy.
2. Data

  /anacapa_output_20210507

    1. *combo_q30* *Anacapa Toolkit* Output of MiFish 12S data using the *CRUX* Global Reference database from [Gold et al. 2021](https://onlinelibrary.wiley.com/doi/epdf/10.1111/1755-0998.13450       
    2. *fishcard_q30* *Anacapa Toolkit* Output of MiFish 12S data using the *CRUX* Local Reference database from Gold et al. 2021. 

  /CalCOFI_Database_194903-201907_csv_30Apr2020

    1. Directory includes metadata files

    **Note** *Bottle data file needed for metadata is not uploaded due to size*

  /.

    1. *larval_counts_20210305.csv* Microscopy derived morphological data
    2. *mifish_library_prep.csv* Relevant sequence library preparation information needed for the joint model.
    3. *20210622_species_mapping_file.csv* Species mapping file that links taxonomy obtained from morphological and molecular data.
    4. *habitat_association_to_check_art.csv* Habitat association data for all species
    5. Other files are various metadata and intermediate outputs needed to link data to each other.


**Raw sequence inputs and large model outputs are available on [Dryad](https://doi.org/10.5068/D1267D). Reference databases from Gold et al. 2021 are available [GitHub](https://github.com/zjgold/FishCARD) & [Dryad](https://doi.org/10.5068/D1H963)**

## Dependencies

**R (v4.0)**

    1. tidyverse
    2. rstan
    3. bayesplot
    4. here
    5. ncdf4
    6. parsedate
    7. plotdap
    8. rerddap
    9. sp
    10. rerddapXtracto
    11. devtools
    12. gganimate
    13. ggplot2
    14. plotdap
    15. lme4
    16. rstanarm
    17. modelr
    18. tidybayes
    19. heatmaply
    20. wesanderson
    21. dendextend
    22. ggpmisc
    23. vegan
    24. ggdendro
    25. rioja
    26. cluster
    27. ggrepel
    28. grid
    29. RColorBrewer
    30. phyloseq
    31. metagMisc
    32. proxy
    33. reshape2
    34. microDecon
    35. stringr
    36. knitr
    37. cowplot
    38. reshape2
    39. microDecon
    40. stringr
    41. ranacapa
    42. plotly
    43. optparse
    44. fitdistrplus
    45. broom
    46. analyze.stuff
    47. ggallin
    48. patchwork


*See individual packages for installation*

# 1: Organize and Format Metadata

### a. Run Calcofi_satellite_data_2021090-7_.Rmd
This script generates SST data needed for downstream analyses.
### b. Run calcofi_metadata_20210907.Rmd
This script pulls relevant metadata for each CalCOFI jar analyzed in this study.

# 2: Decontamination

### a. Run 20210927_Calcofi_decontam_zjg.R
This script imports "raw" *Anacapa Toolkit* data and runs a 4 step decontamination process following the methods of [Kelly et al. 2018](https://peerj.com/articles/4521/)
##### Cleaning Process 0: Remove all Forward, Reverse, and Unmerged reads & Remove Singletons

##### Cleaning Process 1: Estimation of *Tag-jumping* or sample *Cross-talk* or *Index hopping*
Recent evidence has found that there is the potential for indexes to *hop* from one DNA molecule to another, leading to the incorrect sample assignment during demultiplexing [Costello et al., 2018](https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-018-4703-0). To estimate the frequency of index hopping we included two positive controls of a non-native terrestrial taxa. To estimate the frequency of index hopping we model the composition of environmental sequences observed on the positive controls and subtract these sequences from the environmental samples run.

##### Cleaning Process 2: Discard PCR replicates with low number of reads
The minimum read cutoff was 30,000 reads.

##### Cleaning Process 3: Discard PCR (technical) replicates with unusually high dissimilarity
We removed all PCR replicates that were > 95% probability of belonging to a fit beta distribution of pairwise Bray-Curtis dissimilarities among all technical replicates.

##### Cleaning Process 4: Remove known lab contaminants
We removed human and pig sequences as these are known contaminants in our lab and reagents.

### b. Run 20210927_Merge_data.Rmd
We sum ASV read counts by taxonomy [e.g. the reads from 4 ASVs all assigned to Northern Anchovy (*Engraulis mordax*) are summed together]

# 3: STAN Joint model

### a. Run Calcofi_edna_vs_morphology_20210908.Rmd
Format morphological and microscopy data for joint model run.

### b. Run Run_Mifish_Joint_Model.R
Runs joint STAN model.

# 4: Run Analyses

### a. Run CalCOFI_results_short_20210928.Rmd
Generates figures for the manuscript and has the main analyses conducted.

### b. Run Suppl_analyses_RPK_20210907.Rmd
Generates additional supplemental tables confirming no degradation over time.
