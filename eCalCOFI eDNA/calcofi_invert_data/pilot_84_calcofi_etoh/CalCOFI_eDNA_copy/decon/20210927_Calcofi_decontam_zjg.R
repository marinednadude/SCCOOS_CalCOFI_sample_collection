# ---
# title: "Generalized Decontamination Script"
# author: "Zack Gold"
# date: "9/27/2021"
# output: html_document
# ---

# Load Libraries ----------------------------------------------------------------------------------------------------------------
library(cowplot)
library (tidyverse)
library (vegan)
library (proxy)
library(reshape2)
library(microDecon)
library(stringr)
library(knitr)
library(ranacapa)
library(dplyr)
library(tibble)
library(reshape2)
library(phyloseq)
library(ggplot2)
library(plotly)
library(optparse)
library(fitdistrplus)
library(broom)
library(analyze.stuff)
library(here)
source(here("decon", "decontamination_utilities.R"))

#Test inputs ----------------------------------------------------------------------------------------------------------------
working_directory <- here()
input_anacapa_path_1 <-
  here(
    "data",
    "anacapa_output_20210507",
    "fishcard_q30",
    "fishcard_12S_all_taxonomy_tables",
    "summary_by_percent_confidence",
    "60",
    "fishcard_12S_all_ASV_raw_taxonomy_60.txt"
  )

input_anacapa_path_3 <-
  here(
    "data",
    "anacapa_output_20210507",
    "combo_q30",
    "c19_fishcard_taxonomy_tables",
    "summary_by_percent_confidence",
    "60",
    "c19_fishcard_ASV_raw_taxonomy_60.txt"
  )


input_meta_path <- here("data", "sequence_mapping_file_20210907.csv")

number_anacapa_tables <- "two"

read_type <- "merged_only"

low_dist_probability_cutoff <- 0.025
high_dist_probability_cutoff <- 1

minimum_read_cutoff <- 30000

step3_runs <- 2
step3_thresh <- 0.7
step3_prop.thresh <- 5e-05
step3_regression <- 0

max_dist_probability_cutoff <- 0.75
#---

#---

#Load Data ----------------------------------------------------------------------------------------------------------------
setwd(working_directory)
dir.create(here("decon", "Output_csv"))
dir.create(here("decon", "Output_R"))
dir.create(here("decon", "Output_plots"))

metadata <-
  read.table(
    input_meta_path,
    header = 1,
    sep = ",",
    stringsAsFactors = F
  )

anacapa_table_1 <-
  read.table(
    input_anacapa_path_1,
    header = 1,
    sep = "\t",
    stringsAsFactors = F,
    quote = "",
    fill = FALSE
  )
anacapa_table_3 <-
  read.table(
    input_anacapa_path_3,
    header = 1,
    sep = "\t",
    stringsAsFactors = F,
    quote = "",
    fill = FALSE
  )

#Format Anacapa Table 1
anacapa_table_1[1, 1] %>% str_remove(., "forward_") %>% str_sub(., end =
                                                                  -3) -> barcode_1
anacapa_table_1$Miseq_run <- barcode_1

###Fix Names
anacapa_table_1_names <- colnames(anacapa_table_1)
anacapa_table_1_names %>% as.data.frame() -> anacapa_table_1_names
anacapa_table_1_names[1, 1] <- "seq_number"
colnames(anacapa_table_1_names) <- c("Seq_number")
anacapa_table_1_names %>% mutate(.,  Seq_number = str_remove(Seq_number, paste0(barcode_1, "_"))) -> anacapa_table_1_names

left_join(anacapa_table_1_names, metadata) %>% dplyr::select(Seq_number, New_name) %>%
  mutate(final_names = coalesce(New_name, Seq_number)) %>%  dplyr::select(final_names) %>% as.list() -> anacapa_table_1_names
colnames(anacapa_table_1) <- anacapa_table_1_names$final_names

#Format Anacapa Table 3
anacapa_table_3[1, 1] %>% str_remove(., "forward_") %>% str_sub(., end =
                                                                  -3) -> barcode_3
anacapa_table_3$Miseq_run <- barcode_3

###Fix Names
anacapa_table_3_names <- colnames(anacapa_table_3)
anacapa_table_3_names %>% as.data.frame() -> anacapa_table_3_names
anacapa_table_3_names[1, 1] <- "seq_number"
colnames(anacapa_table_3_names) <- c("Seq_number")
anacapa_table_3_names %>% mutate(.,  Seq_number = str_remove(Seq_number, paste0(barcode_3, "_"))) -> anacapa_table_3_names

left_join(anacapa_table_3_names, metadata) %>% dplyr::select(Seq_number, New_name) %>%
  mutate(final_names = coalesce(New_name, Seq_number)) %>%  dplyr::select(final_names) %>% as.list() -> anacapa_table_3_names
colnames(anacapa_table_3) <- anacapa_table_3_names$final_names

#Generate Hash.key
anacapa_table_1 %>%
  dplyr::select(seq_number, sum.taxonomy) %>%
  filter(., str_detect(seq_number, "merged")) %>%
  filter(.,!str_detect(seq_number, "unmerged")) -> hash.key_1

anacapa_table_3 %>%
  filter(., str_detect(seq_number, "merged")) %>%
  filter(.,!str_detect(seq_number, "unmerged")) %>%
  dplyr::select(sum.taxonomy.c19 = sum.taxonomy)  -> c19_hashes

dim(hash.key_1)
dim(c19_hashes)

cbind(hash.key_1, c19_hashes) %>%
  mutate(., mammal = str_detect(sum.taxonomy.c19, "Mammalia")) %>%
  mutate(., bird = str_detect(sum.taxonomy.c19, "Aves")) %>%
  mutate_if(is.character, list( ~ na_if(., ""))) %>%
  mutate(., empty = is.na(sum.taxonomy)) %>%
  mutate(., empty_2 = is.na(sum.taxonomy.c19)) %>%
  mutate(
    .,
    sum.taxonomy_final = case_when(
      mammal == TRUE ~ sum.taxonomy.c19,
      bird == TRUE ~ sum.taxonomy.c19,
      (empty == TRUE &
         empty_2 == FALSE) ~ sum.taxonomy.c19,
      (empty == FALSE &
         empty_2 == TRUE) ~ sum.taxonomy,
      (empty == FALSE &
         empty_2 == FALSE) ~ sum.taxonomy,
      sum.taxonomy == "Eukaryota;Chordata;Actinopteri;;;;" &
        sum.taxonomy.c19 != "Eukaryota;Chordata;Actinopteri;;;;" &
        sum.taxonomy.c19 != "" ~ sum.taxonomy.c19,
      sum.taxonomy == "Eukaryota;Chordata;;;;;" &
        sum.taxonomy.c19 != "Eukaryota;Chordata;;;;;" ~ sum.taxonomy.c19,
      TRUE ~ sum.taxonomy
    )
  ) %>%
  dplyr::select(seq_number, sum.taxonomy = sum.taxonomy_final) -> hash.key_1_updated

rbind(hash.key_1_updated) -> hash.key

saveRDS(hash.key, file = here("decon", "Output_R", "hash.key.RDS"))

# Convert to Long Data

###Format for Long Data
anacapa_table_1$seq_number <- factor(anacapa_table_1$seq_number)
anacapa_table_1$Miseq_run <- factor(anacapa_table_1$Miseq_run)

columns <- colnames(anacapa_table_1)
remove <- c("seq_number", "sum.taxonomy", "Miseq_run")

gathercols <-  columns[!columns %in% remove]

anacapa_table_1 %>%
  pivot_longer(.,
               cols = gathercols,
               names_to = "sample",
               values_to = "reads") -> anacapa_table_1_long

anacapa_table_1_long$reads <- as.numeric(anacapa_table_1_long$reads)

#Merge All Tables
ASV.table_long <- rbind(anacapa_table_1_long)


ASV.table_long %>% 
  dplyr::summarise(sum(reads))


#---

#---

# Cleaning Process 0: Remove all Forward, Reverse, and Unmerged reads & Remove Singletons ----------------------------------------------------------------------------------------------------------------

#Filter Merged only reads
ASV.table_long %>%
  filter(., str_detect(seq_number, "merged")) %>%
  filter(.,!str_detect(seq_number, "unmerged")) -> ASV.table_used

#Calculate % ASVs Kept
ASV.table_long %>%  dim() -> all_dim
ASV.table_used %>%  dim() -> used_only_dim

paste0(round(used_only_dim[[1]] / all_dim[[1]] * 100, 2), "% ASVs retained")

saveRDS(ASV.table_long,
        file = here("decon", "Output_R", "ASV.table_long.RDS"))
saveRDS(ASV.table_used,
        file = here("decon", "Output_R", "ASV.table_used.RDS"))

hash.key <- readRDS(file = here("decon", "Output_R", "hash.key.RDS"))

ASV.table_used  <-
  readRDS(file = here("decon", "Output_R", "ASV.table_used.RDS"))


#---

#---

# Cleaning Process 1: Estimation of *Tag-jumping* or sample *Cross-talk* ----------------------------------------------------------------------------------------------------------------

## Step 1: Nest the dataset by origin of ASVs

###Identify Positives, Negatives, and Samples

###Create list of control samples
metadata %>%
  filter(Sample_Control == "Control") %>%
  dplyr::select(New_name) %>% unique() -> controls
controls <- controls$New_name

metadata %>%
  filter(Control_Type == "Pos") %>%
  dplyr::select(New_name) %>% unique() -> pos_controls
pos_controls <- pos_controls$New_name

metadata %>%
  filter(Control_Type == "Neg") %>%
  dplyr::select(New_name) %>% unique() -> neg_controls
neg_controls <- neg_controls$New_name

###New column that labels each ASV as from Positive (control) or Sample
ASV.table_used %>%
  mutate(
    source = case_when(
      sample %in% pos_controls ~ "Positives",
      sample %in% neg_controls ~ "Blanks",
      TRUE ~ "Samples"
    )
  ) -> ASV.table_used


###Convert to tibble
ASV.table_used <- as_tibble(ASV.table_used)

###Remove empty sequences
ASV.table_used %>%
  filter(reads != 0)  -> ASV.table_used

###Rename Columns and remove seq_number
ASV.table_used %>%
  mutate(sample = as.character(sample),
         nReads = reads)  -> ASV.table_used

###ASVs in Positive Controls
ASV.table_used %>%
  filter (source == "Positives") %>%
  dplyr::group_by(seq_number) %>%
  dplyr::summarise(tot = sum(reads)) %>%
  arrange(desc(tot)) %>%
  pull(seq_number) -> all.seqs.in.positives

hash.key %>%
  filter(seq_number %in% all.seqs.in.positives) %>% as_tibble() -> pos.contam.species

write.csv(pos.contam.species,
          file = here("decon", "Output_csv", "pos.contam.species.csv"))

###ASVs in Negative Controls

ASV.table_used %>%
  filter (source == "Blanks") %>%
  dplyr::group_by(seq_number) %>%
  dplyr::summarise(tot = sum(reads)) %>%
  arrange(desc(tot)) %>%
  pull(seq_number) -> all.seqs.in.blanks

hash.key %>%
  filter(seq_number %in% all.seqs.in.blanks) %>% as_tibble() -> blank.contam.species

write.csv(blank.contam.species,
          file = here("decon", "Output_csv", "blank.contam.species.csv"))

### Visualize Read Counts Across Samples for Barcode_1
ASV.table_used %>%
  group_by(sample) %>%
  filter(., Miseq_run == barcode_1) %>%
  mutate (TotalReadsperSample = sum(nReads)) %>%
  arrange(desc(TotalReadsperSample)) %>%
  ggplot(., aes(x = sample, y = TotalReadsperSample, color = source)) +
  geom_point() + ggtitle("Read Count Across Samples") +
  theme(axis.text.x = element_text(angle = 90)) -> plot_1

ggsave(
  plot = plot_1,
  here("decon", "Output_plots/Sample_Read_Depth_Barcode_1.png"),
  device = "png",
  width = 12,
  height = 8,
  units = "in"
)


###Nesting the dataset
ASV.table_used %>%
  dplyr::group_by(Miseq_run, source) %>%
  nest() %>%
  pivot_wider(names_from = source, values_from = data) -> ASV.nested


####Summary.file.1
ASV.nested %>%
  ungroup() %>%
  dplyr::transmute(., Miseq_run, Summary = purrr::map(Samples, ~ how.many(ASVtable = ., round = 0)))  -> ASV.summary

ASV.summary$Summary

## Step 2: Model the composition of the positive controls of each run

###Jumping vector

ASV.nested %>%
  mutate (contam.tibble = purrr::map(Positives,
                                     function(.x) {
                                       .x %>%
                                         ungroup() %>%
                                         group_by(sample) %>%
                                         mutate (TotalReadsperSample = sum(nReads)) %>%
                                         mutate (proportion = nReads / TotalReadsperSample) %>%
                                         group_by(seq_number) %>%
                                         dplyr::summarise (vector_contamination = max(proportion))
                                     })) -> ASV.nested

###Vector Contamination in Barcode_1
ASV.nested$contam.tibble[[1]] %>% as.data.frame() %>%
  ggplot(aes(x = vector_contamination)) +
  geom_histogram() -> vc_plot_1 # Check how it looks
ggsave(
  plot = vc_plot_1,
  here("decon", "Output_plots/Vector_Contamination_Barcode_1.png"),
  device = "png",
  width = 12,
  height = 8,
  units = "in"
)


##Step 3: Substract the composition of the positive controls from the environment samples

ASV.nested %>%
  ungroup() %>%
  mutate(Step1.cleaned.tibble = map2(Samples, contam.tibble, function(.x, .y) {
    .x %>%
      dplyr::group_by (sample) %>%
      mutate (TotalReadsperSample = sum (nReads)) %>%
      left_join(.y, by = "seq_number") %>%
      mutate (Updated_nReads = ifelse (!is.na(vector_contamination),  nReads - (
        ceiling(vector_contamination * TotalReadsperSample)
      ), nReads)) %>%
      filter (Updated_nReads > 0) %>%
      ungroup() %>%
      dplyr::select (sample, seq_number, nReads = Updated_nReads)
  })) -> ASV.nested

###Add this step to the summary table we were creating

####Summary.file.2
ASV.nested %>%
  transmute(Miseq_run, Summary.1 = purrr::map(Step1.cleaned.tibble, ~ how.many(ASVtable = ., round = "1.Jump"))) %>%
  left_join(ASV.summary) %>% #use left join when there are many miseq runs to join
  mutate(Summary   = map2(Summary, Summary.1, bind_rows)) %>%
  dplyr::select(-Summary.1) -> ASV.summary

ASV.summary$Summary

#---

#---


# Cleaning Process 2: **Discarding PCR replicates with low number of reads** ----------------------------------------------------------------------------------------------------------------

###Pull out Sample Read Depth

ASV.nested %>%
  dplyr::select(Miseq_run, Step1.cleaned.tibble) %>%
  unnest(Step1.cleaned.tibble) %>%
  group_by(Miseq_run, sample) %>%
  dplyr::summarise(tot = sum(nReads)) %>%
  arrange(desc(tot)) -> all.reps


all.reps %>%
  filter(., Miseq_run == barcode_1) -> all.reps_1

all.reps_1 %>%
  pull(tot) -> reads.per.sample_1

names(reads.per.sample_1) <- all.reps_1 %>% pull(sample)

### Fit Normal Distribution

fit_1 <-
  fitdist(
    reads.per.sample_1,
    "gamma",
    lower = c(0, 0),
    start = list(scale = 1, shape = 1)
  )

all.reps_1 %>%
  mutate(
    prob = pgamma(
      tot,
      shape = fit_1$estimate[[2]],
      scale = fit_1$estimate[[1]],
      lower.tail = TRUE,
      log.p = FALSE
    )
  ) -> all.reps_1

### Remove Outlier Samples
outliers_1 <- all.reps_1 %>%
  filter(
    prob < low_dist_probability_cutoff  |
      tot < minimum_read_cutoff |
      prob > high_dist_probability_cutoff
  ) # changed to 0.05 to save the two samples

outliers <- rbind(outliers_1)
ASV.nested %>%
  mutate(Step.2.low.read.depth = purrr::map (
    Step1.cleaned.tibble,
    ~ filter(., !sample %in% outliers$sample) %>% ungroup
  )) -> ASV.nested

ASV.nested %>%
  transmute(Miseq_run,
            Summary.1 = purrr::map(
              Step.2.low.read.depth,
              ~ how.many(ASVtable = ., round = "2.Low.Read.Depth")
            )) %>%
  left_join(ASV.summary) %>%
  mutate(Summary   = map2(Summary, Summary.1, bind_rows)) %>%
  dplyr::select(-Summary.1) -> ASV.summary

ASV.summary$Summary

#---

#---

# Cleaning Process 3: **Dissimilarity between PCR (technical) replicates** ----------------------------------------------------------------------------------------------------------------

ASV.nested %>%
  dplyr::select(Miseq_run, Step.2.low.read.depth) %>%
  unnest(Step.2.low.read.depth) %>%
  as.data.frame() %>%
  ungroup() %>%
  left_join(metadata, by = c("sample" = "New_name")) -> cleaned.tibble.pre_occ

## How many samples, how many ASVs
cleaned.tibble.pre_occ %>%
  dplyr::summarise(n_distinct(sample),
                   n_distinct(seq_number))


## Proportions
cleaned.tibble.pre_occ %>%
  dplyr::group_by(sample, Miseq_run) %>%
  mutate (Tot = sum(nReads),
          Normalized.reads = nReads / Tot) -> cleaned.tibble.pre_occ #transforms raw number of reads to eDNA index


#Calculate Vegan Distances
tibble_to_vegdist_all(cleaned.tibble.pre_occ) -> all.distances.full.pre

as.tibble(subset(melt(as.matrix(
  all.distances.full.pre
)))) -> all.distances.melted.pre

# Now, create a three variables for all distances, they could be PCR replicates, BIOL replicates, or from the same site
all.distances.melted.pre %>%
  separate(
    Var1,
    into = c("Miseq_run1", "New_name"),
    sep = ":",
    remove = F
  ) %>%
  mutate(
    .,
    Site1 = str_sub(New_name, 1, -5),
    jar_rep1 = str_sub(New_name, -3, -3),
    tech_rep1 = str_sub(New_name, -1, -1)
  ) %>%
  separate(
    Var2,
    into = c("Miseq_run2", "New_name2"),
    sep = ":",
    remove = F
  ) %>%
  mutate(
    .,
    Site2 = str_sub(New_name2, 1, -5),
    jar_rep2 = str_sub(New_name2, -3, -3),
    tech_rep2 = str_sub(New_name2, -1, -1)
  ) %>%
  unite(Site1, jar_rep1, col = "jar1", remove = F) %>%
  unite(Site2, jar_rep2, col = "jar2", remove = F) %>%
  ungroup() %>%
  mutate(
    Distance.type = case_when(
      Miseq_run1 == Miseq_run2 & jar1 == jar2 ~ "tech.replicates",
      Miseq_run1 == Miseq_run2 &
        Site1 == Site2 ~ "Jar.replicates",
      Miseq_run1 == Miseq_run2 ~ "Same Barcode Different Site",
      TRUE ~ "Different Barcode"
    )
  ) %>%
  dplyr::select(
    Sample1 = Var1,
    Sample2 = Var2 ,
    value ,
    Distance.type,
    Miseq_run1,
    Miseq_run2
  ) %>%
  filter (Sample1 != Sample2) -> all.distances.to.plot.pre


# Checking all went well
sapply(all.distances.to.plot.pre, function(x)
  summary(is.na(x)))

all.distances.to.plot.pre$Distance.type <-
  all.distances.to.plot.pre$Distance.type  %>% fct_relevel(
    "Jar.replicates",
    "tech.replicates",
    "Same Barcode Different Site",
    "Different Barcode"
  )

plot_5 <-
  ggplot (all.distances.to.plot.pre ,
          aes (fill = Distance.type, x = value, after_stat(density))) +
  geom_histogram(stat = 'bin',
                 alpha = 0.9,
                 binwidth = 0.05) + #xlim(0, 1) +
  facet_wrap(~ Distance.type) +
  labs (
    x = "Pairwise Dissimilarity",
    y = "Density" ,
    fill = "Groups",
    title = "eDNA Pairwise Dissimilarity Between Samples",
    subtitle = "Pre Occupancy"
  ) + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
ggsave(
  plot = plot_5,
  here(
    "decon",
    "Output_plots/eDNA_Pairwise_Dissimilarity_Between_Samples_pre_occupancy.png"
  ),
  device = "png",
  width = 12,
  height = 8,
  units = "in"
)


# Instead of chosing based on the pairwise distances, we will use distance to centroid
#Need to Remove Single Sampels
cleaned.tibble.pre_occ  %>%
  group_by(Miseq_run, Location) %>%
  dplyr::summarise(cases = n_distinct(tech_rep)) %>%
  filter(., cases == 1) %>%
  unite(col = "location_id",
        Miseq_run,
        Location,
        remove = FALSE,
        sep = ":") %>%
  ungroup() %>%
  dplyr::select(location_id) -> singles
#Currently this method does not filter out these single tech reps. There are only 3

cleaned.tibble.pre_occ  %>%
  group_by(Miseq_run, Location) %>%
  unite(col = "location_id",
        Miseq_run,
        Location,
        remove = FALSE,
        sep = ":") %>%
  unite(
    col = "distances_id",
    Miseq_run,
    Location,
    tech_rep,
    remove = FALSE,
    sep = ":"
  ) %>%
  filter(., location_id %in% singles$location_id) %>%
  dplyr::select(distances_id) %>%  distinct() -> single_distance_ID

#Nest Data
cleaned.tibble.pre_occ  %>%
  unite(col = "location_id",
        Miseq_run,
        Location,
        remove = FALSE,
        sep = ":") %>%
  unite(
    col = "distances_id",
    Miseq_run,
    Location,
    tech_rep,
    remove = FALSE,
    sep = ":"
  ) %>%
  filter(.,!location_id %in% singles$location_id) %>%
  group_by(location_id) %>% nest() -> nested.cleaning

nested.cleaning %>%
  mutate(matrix = purrr::map(data, tibble_to_vegdist_bio_rep_single)) -> nested.cleaning

# Convert
nested.cleaning %>% mutate(ncomparisons = purrr::map(matrix, length)) -> nested.cleaning

#Calculate distances to centroid
nested.cleaning <-
  nested.cleaning %>% mutate(distances = map2(matrix, location_id, dist_to_centroid))

nested.cleaning %>%
  separate(location_id, c("Miseq_run", "Site"), sep = ":") %>%
  unnest_longer(distances) %>%
  dplyr::select(Miseq_run, Site, distances_id, distances) -> all_distances.groups #unnest data

#Calculate beta distribution of distances to centroid

normparams.step5 <-
  MASS::fitdistr(all_distances.groups$distances,
                 "beta",
                 list(shape1 = 2, shape2 = 5))$estimate

#Calculate Probability
probs.step5 <-
  pbeta(
    all_distances.groups$distances,
    shape1 = normparams.step5[1],
    shape2 = normparams.step5[2],
    log.p = FALSE
  )

all_distances.groups %>%
  saveRDS(file = here("decon", " all_distances.groups.rds"))

max_dist_probability_cutoff <-
  qbeta(.95, normparams.step5[1], normparams.step5[2])


all_distances.groups$x <- all_distances.groups$distances

plot_bc <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(
    fun = dbeta,
    n = 101,
    args = list(shape1 = normparams.step5[[1]], shape2 = normparams.step5[[2]])
  ) +
  scale_y_continuous(breaks = NULL) +
  geom_density(
    data = all_distances.groups,
    color = "blue",
    fill = "blue",
    alpha = 0.2
  ) +
  geom_vline(
    xintercept = qbeta(.95, normparams.step5[1], normparams.step5[2]),
    linetype = "dashed",
    color = "red"
  ) + xlab("BC Dissimilarity")

ggsave(
  plot = plot_bc,
  here(
    "decon",
    "Output_plots",
    "bray_curtis_dissimilarity_density_plot.png"
  ),
  device = "png",
  width = 12,
  height = 8,
  units = "in"
)

#Determine Outliers
outliers.step5 <-
  which(all_distances.groups$distances > max_dist_probability_cutoff)

#Remove Outliers
discard.step5 <-
  names(all_distances.groups$distances[outliers.step5])

to_write_discarded.step5 <- tibble(distances_id = discard.step5,
                                   distance = all_distances.groups$distances[outliers.step5])

to_write_discarded.step5 <-
  to_write_discarded.step5 %>% left_join(tibble(distances_id = discard.step5,
                                                probs = probs.step5[outliers.step5]))
write_csv(
  to_write_discarded.step5 ,
  here("decon", "Output_csv", "step5.discared_samples.csv")
)

## Plot Final Replication Levels
cleaned.tibble.pre_occ %>%
  unite(
    col = "distances_id",
    Miseq_run,
    Location,
    tech_rep,
    remove = FALSE,
    sep = ":"
  ) -> cleaned.tibble.post_occ_for_plotting

all_distances.groups %>%
  dplyr::select(-Miseq_run, -Site) -> all_distances.groups_for_plotting


cleaned.tibble.post_occ_for_plotting %>%
  mutate(.,
         Discarded = if_else(distances_id %in% discard.step5, "Discarded", "Kept")) %>%
  dplyr::group_by(Miseq_run, Location, tech_rep, Discarded) %>%
  dplyr::summarise(cases = n_distinct(distances_id)) %>%
  ggplot() +
  geom_raster(aes(x = Location, y = tech_rep, fill = Discarded)) +
  geom_text(aes(x = Location, y = tech_rep, label = cases), color = "white") +
  ggtitle("Sample Replication Level") + facet_wrap( ~ Miseq_run) +
  theme(axis.text.x = element_text(angle = 90)) -> plot_7

plot_7
ggsave(
  plot = plot_7,
  here(
    "decon",
    "Output_plots",
    "Final_sample_replication_level.png"
  ),
  device = "png",
  width = 20,
  height = 8,
  units = "in"
)

metadata %>% ungroup() %>% dplyr::select(-Seq_number, -Barcode) %>% distinct() -> metadata

##Remove Samples
ASV.nested %>%
  mutate (Step3.tibble_edited = purrr::map(Step.2.low.read.depth,
                                           function(.x) {
                                             .x %>%
                                               left_join(metadata, by =
                                                           c("sample" = "New_name")) %>%
                                               mutate(., Miseq_run = if_else(str_detect(seq_number, barcode_1), barcode_1, "barcode_2")) %>%
                                               unite(
                                                 col = "distances_id",
                                                 Miseq_run,
                                                 Location,
                                                 tech_rep,
                                                 remove = FALSE,
                                                 sep = ":"
                                               )
                                           })) -> ASV.nested


#Filter Sample
ASV.nested %>%
  mutate(Step4.tibble = purrr::map (
    Step3.tibble_edited,
    ~ filter(., !distances_id %in% to_write_discarded.step5$distances_id)
  )) -> ASV.nested


#Visualize Results of Clearence Process 4
ASV.nested %>%
  transmute(Miseq_run, Summary.1 = purrr::map(Step4.tibble, ~ how.many(ASVtable = ., round =
                                                                         "3.Dissimilarity"))) %>%
  left_join(ASV.summary) %>%
  mutate(Summary   = map2(Summary, Summary.1, bind_rows)) %>%
  dplyr::select(-Summary.1) -> ASV.summary


ASV.summary$Summary



# Final Check of Dissimilarity

ASV.nested %>%
  dplyr::select(Step4.tibble) %>%
  unnest(Step4.tibble) %>%
  as.data.frame() %>%
  ungroup() %>%
  left_join(metadata, by = c("sample" = "New_name")) -> cleaned.tibble.step_4

## How many samples, how many ASVs
cleaned.tibble.step_4 %>%
  dplyr::summarise(n_distinct(sample),
                   n_distinct(seq_number))


## Proportions
cleaned.tibble.step_4 %>%
  dplyr::group_by(sample, Miseq_run) %>%
  mutate (Tot = sum(nReads),
          Normalized.reads = nReads / Tot) -> cleaned.tibble.step_4 #transforms raw number of reads to eDNA index


#Calculate Vegan Distances
tibble_to_vegdist_all(cleaned.tibble.step_4) -> all.distances.full.4

as.tibble(subset(melt(as.matrix(
  all.distances.full.4
)))) -> all.distances.melted.4

# Now, create a three variables for all distances, they could be PCR replicates, BIOL replicates, or from the same site
all.distances.melted.4 %>%
  separate(
    Var1,
    into = c("Miseq_run1", "New_name"),
    sep = ":",
    remove = F
  ) %>%
  mutate(
    .,
    Site1 = str_sub(New_name, 1, -5),
    jar_rep1 = str_sub(New_name, -3, -3),
    tech_rep1 = str_sub(New_name, -1, -1)
  ) %>%
  separate(
    Var2,
    into = c("Miseq_run2", "New_name2"),
    sep = ":",
    remove = F
  ) %>%
  mutate(
    .,
    Site2 = str_sub(New_name2, 1, -5),
    jar_rep2 = str_sub(New_name2, -3, -3),
    tech_rep2 = str_sub(New_name2, -1, -1)
  ) %>%
  unite(Site1, jar_rep1, col = "jar1", remove = F) %>%
  unite(Site2, jar_rep2, col = "jar2", remove = F) %>%
  ungroup() %>%
  mutate(
    Distance.type = case_when(
      Miseq_run1 == Miseq_run2 & jar1 == jar2 ~ "tech.replicates",
      Miseq_run1 == Miseq_run2 &
        Site1 == Site2 ~ "Jar.replicates",
      Miseq_run1 == Miseq_run2 ~ "Same Barcode Different Site",
      TRUE ~ "Different Barcode"
    )
  ) %>%
  dplyr::select(
    Sample1 = Var1,
    Sample2 = Var2 ,
    value ,
    Distance.type,
    Miseq_run1,
    Miseq_run2
  ) %>%
  filter (Sample1 != Sample2) -> all.distances.to.plot.4


# Checking all went well
sapply(all.distances.to.plot.4, function(x)
  summary(is.na(x)))

all.distances.to.plot.4$Distance.type <-
  all.distances.to.plot.4$Distance.type  %>% fct_relevel(
    "Jar.replicates",
    "tech.replicates",
    "Same Barcode Different Site",
    "Different Barcode"
  )

plot_8 <-
  ggplot (all.distances.to.plot.4 ,
          aes (fill = Distance.type, x = value, after_stat(density))) +
  geom_histogram(stat = 'bin',
                 alpha = 0.9,
                 binwidth = 0.05) + #xlim(0, 1) +
  facet_wrap(~ Distance.type) +
  labs (
    x = "Pairwise Dissimilarity",
    y = "Density" ,
    fill = "Groups",
    title = "eDNA Pairwise Dissimilarity Between Samples",
    subtitle = "Pre Occupancy"
  ) + theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
plot_8
ggsave(
  plot = plot_8,
  here(
    "decon",
    "Output_plots",
    "eDNA_Pairwise_Dissimilarity_Between_Samples_post_outlier_removal.png"
  ),
  device = "png",
  width = 12,
  height = 8,
  units = "in"
)

# Cleaning Process 4: **Remove Known Lab Contaminants** ----------------------------------------------------------------------------------------------------------------

##Remove Samples
hash.key %>%
  filter(., str_detect(
    sum.taxonomy,
    c(
      "Eukaryota;Chordata;Mammalia;Primates;Hominidae;Homo;Homo sapiens"
    )
  )) -> bad_taxa_1

hash.key %>%
  filter(., str_detect(
    sum.taxonomy,
    c(
      "Eukaryota;Chordata;Mammalia;Artiodactyla;Suidae;Sus;Sus scrofa"
    )
  )) -> bad_taxa_2


bad_taxa <- rbind(bad_taxa_1, bad_taxa_2)
#Filter Sample
ASV.nested %>%
  mutate(Step5.tibble = purrr::map (Step4.tibble,  ~ filter(., !seq_number %in% bad_taxa$seq_number))) -> ASV.nested


#Visualize Results of Clearence Process 4
ASV.nested %>%
  transmute(Miseq_run, Summary.1 = purrr::map(Step5.tibble, ~ how.many(ASVtable = ., round =
                                                                         "4.Lab_contam"))) %>%
  left_join(ASV.summary) %>%
  mutate(Summary   = map2(Summary, Summary.1, bind_rows)) %>%
  dplyr::select(-Summary.1) -> ASV.summary

ASV.summary$Summary

##Save Data -----
saveRDS(ASV.nested,
        file = here("decon", "Output_R", "ASV.nested_final.RDS"))
saveRDS(ASV.summary,
        file = here("decon", "Output_R", "ASV.summary_final.RDS"))

ASV.summary <- readRDS(,
        file = here("decon", "Output_R", "ASV.summary_final.RDS"))

ASV.summary$Summary
