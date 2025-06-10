# Code to assemble data and run stan model
# to jointly estimate fish abundance from genetic and manual counts
# Gold et al. CalCOFI manuscript
# Code by Ole Shelton, Ryan Kelly, Zack Gold 2021

library(tidyverse)
library(rstan)
library(bayesplot)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options("auto_write" = TRUE)

#library prep data, w PCR pipetting info
lib_mifish <- read.csv(here("data", "mifish_library_prep.csv"))
lib_mifish$New_name <-  paste0(# to match format of other dataset
  substr(lib_mifish$New_name, 1, 4),
  "_",
  substr(lib_mifish$New_name, 8, nchar(as.character(
    lib_mifish$New_name
  ))))

#sequence data for mifish locus
dat.mifish <- readRDS(here("data", "mifish_tech_nReads.RDS")) %>%
  filter(!ID_mifish %in% c("MISSING", ""),!is.na(mifish_reads)) %>%
  dplyr::select(Sample,
                ID_mifish,
                mifish_reads,
                "station_id",
                "ext_rep",
                "tech_rep",
  ) %>%
  distinct() %>%
  unite(
    c("station_id", "ext_rep", "tech_rep"),
    col = "New_name",
    sep = "_",
    remove = F
  ) %>% #to join to library-prep info
  filter(ext_rep == 1) %>%
  left_join(lib_mifish) %>%
  mutate(name = ID_mifish, nReads = mifish_reads) %>%   #for compatibility w earlier versions
  group_by(Sample) %>%
  mutate(tot = sum(mifish_reads)) %>%
  filter(!is.na(tot)) %>%   #omit failed replicates
  filter(!is.na(name)) %>%
  na.omit() %>%
  group_by(ID_mifish, Sample, station_id, tech_rep, Percent.of.PCR.added) %>%
  dplyr::summarise(nReads = sum(nReads)) %>%
  distinct() %>%
  mutate(log_r_mifish = log(Percent.of.PCR.added)) %>%
  ungroup() %>%
  dplyr::select(-Percent.of.PCR.added)

dat.mifish <-
  dat.mifish %>% group_by(ID_mifish, Sample, tech_rep, station_id, log_r_mifish) %>% summarise(nReads = sum(nReads))

drop.these.mifish <-
  dat.mifish %>% ungroup() %>% group_by(ID_mifish) %>% tally(nReads > 0) %>% filter(n <
                                                                        10) %>% pull(ID_mifish) #Note change here to filter out rarely occurring species
dat.mifish <-
  dat.mifish %>%  filter (!ID_mifish %in% drop.these.mifish)

#make a new file dat.mifish.init that has been filtered.  Do not overwrite dat.mifish
dat.mifish.init <- dat.mifish

#data from ichthyoplankton counts
dat.count <- readRDS(here("data", "microscopy_tech_nReads.RDS")) %>%
  filter(!ID_count %in% c("MISSING", "", "Disintegrated fish larvae")) %>%
  filter(station_id %in% unique(dat.mifish$station_id)) %>%
  group_by(
    ID_count,
    station_id,
    standard.haul.factor,
    volume.filtered,
    proportion.sorted
  ) %>%
  dplyr::summarise(larval_counts = sum(larval_counts))

#synonymize all Sebastes in counts
dat.count$ID_count[grepl("Sebastes", dat.count$ID_count)] <-
  "Sebastes"
dat.count <-
  dat.count %>% group_by(
    ID_count,
    station_id,
    standard.haul.factor,
    volume.filtered,
    proportion.sorted
  ) %>% dplyr::summarise(larval_counts = sum(larval_counts))

#require data from both of the datasets at a given station
keepstations <-
dat.count$station_id %>% intersect(dat.mifish$station_id)
dat.count <- dat.count %>% filter(station_id %in% keepstations)
dat.mifish <- dat.mifish %>% filter(station_id %in% keepstations)

#restrict to taxa that aren't incredibly rare
common_count <- dat.count %>%
  group_by(ID_count) %>%
  dplyr::summarise(tot = sum(larval_counts)) %>%
  arrange(desc(tot)) %>%
  top_n(25) %>%
  pull(ID_count)

common_count <- append(common_count, c("Diaphus theta","Microstomus pacificus"))

#this is a very high percentage of all counts
# dat.count %>%  filter (ID_microscopy %in% common_count) %>% ungroup() %>% dplyr::summarize(sum(larval_counts)) /
#   dat.count %>% ungroup() %>% dplyr::summarize(sum(larval_counts))

#make a new object dat.count.init that has been filtered.  Do not overwrite dat.count
dat.count.init <- dat.count %>%
  filter(ID_count %in% common_count)  #take just common species, for now

# pull appropriate taxonomic annotation from microscopy counts
species_mapping <-
  read.csv(here("data", "20210622_species_mapping_file.csv")) %>%
  dplyr::select(-ID_sebastes) %>%
  filter(
    ID_mifish %in% dat.mifish.init$ID_mifish  |
      ID_microscopy %in% dat.count.init$ID_microscopy
  ) %>%
  arrange(Unique_ID)
species_mapping[species_mapping == ""] <- "MISSING"
species_mapping <-
  species_mapping %>% filter(species_mapping$Unique_ID != "Clupeiformes")
species_mapping$Unique_ID[grepl("Sebastes", species_mapping$Unique_ID)] <-
  "Sebastes"   #synonymizing Sebastes in counts
species_mapping <- distinct(species_mapping)
names(species_mapping) <- c("ID_main", "ID_mifish", "ID_count")
species_mapping <-
  species_mapping %>% arrange(ID_main, ID_count, ID_mifish)

# Re-filter dat.count to include only the species found in species_mapping, as a safety check
dat.count <- dat.count %>%
  ungroup() %>%
  dplyr::rename(Abund = larval_counts) %>%
  filter(ID_count %in% species_mapping$ID_count)

# calculate scaling offset in larval counts for jars representing incomplete sampling fractions
MAX.VOL <- max(dat.count$volume.filtered)
dat.count <-
  dat.count %>% ungroup %>%  mutate(
    vol.ratio = volume.filtered / MAX.VOL,
    log_V = log(vol.ratio),
    log_P = log(proportion.sorted)
  )
# Re-filter amplicon data too
dat.mifish <- dat.mifish %>%
  filter(ID_mifish %in% species_mapping$ID_mifish)

### ONLY KEEP INSTANCES WHERE EITHER READS or COUNTS for one of the observations > 0
pos.count <-
  dat.count %>% filter(Abund > 0) %>% dplyr::select(ID_count, station_id) %>%
  left_join(., species_mapping)
pos.mifish <-
  dat.mifish %>% ungroup %>% filter(nReads > 0) %>% dplyr::select(ID_mifish, station_id) %>%
  distinct() %>% left_join(., species_mapping)

pos.all <- pos.count %>% dplyr::select(ID_main, station_id) %>%
  bind_rows(., pos.mifish %>% dplyr::select(ID_main, station_id)) %>%
  distinct() %>% arrange(ID_main, station_id) %>%
  full_join(.,
            species_mapping %>% dplyr::select(ID_main, ID_count, ID_mifish))

# This should weed out any samples with all zeros...
dat.count <- semi_join(dat.count, pos.count)
dat.mifish <- semi_join(dat.mifish, pos.mifish)

#create species indices
species_mapping$sp_index_main <-
  match(species_mapping$ID_main, unique(species_mapping$ID_main))
species_mapping$sp_index_mifish <-
  data.frame(ID_mifish = unique(dat.mifish$ID_mifish)) %>%
  mutate(sp_index_mifish = 1:n()) %>%
  right_join(species_mapping) %>%
  arrange(sp_index_main) %>%
  pull(sp_index_mifish)
species_mapping$sp_index_count <-
  data.frame(ID_count = unique(dat.count$ID_count)) %>%
  mutate(sp_index_count = 1:n()) %>%
  right_join(species_mapping) %>%
  arrange(sp_index_main) %>%
  pull(sp_index_count)


## Count number of species/entities
I_main <- length(species_mapping$sp_index_main)

SP_count <-
  species_mapping$ID_count[!is.na(species_mapping$sp_index_count)] %>% unique()
I_count <- length(SP_count)

SP_mifish <-
  species_mapping$ID_mifish[!is.na(species_mapping$sp_index_mifish)] %>% unique()
I_mifish <- length(SP_mifish)

## make a matrix from the main list to each primer / data
M_to_count <- matrix(0, I_count, I_main)
M_to_mifish <- matrix(0, I_mifish, I_main)

for (i in 1:I_count) {
  M_to_count[species_mapping$sp_index_count[SP_count[i] == species_mapping$ID_count],
             species_mapping$sp_index_main[SP_count[i] == species_mapping$ID_count]] <-
    1
}
for (i in 1:I_mifish) {
  M_to_mifish[species_mapping$sp_index_mifish[SP_mifish[i] == species_mapping$ID_mifish],
              species_mapping$sp_index_main[SP_mifish[i] == species_mapping$ID_mifish]] <-
    1
}

colnames(M_to_count)  <- species_mapping$ID_main
colnames(M_to_mifish) <- species_mapping$ID_main
rownames(M_to_count)   <-
  species_mapping %>% filter(!is.na(species_mapping$sp_index_count)) %>% dplyr::select(ID_count, sp_index_count) %>%
  distinct() %>% pull(ID_count)
rownames(M_to_mifish)  <-
  species_mapping %>% filter(!is.na(species_mapping$sp_index_mifish)) %>% dplyr::select(ID_mifish, sp_index_mifish) %>%
  distinct() %>% pull(ID_mifish)

# if desired, write out identity matrices as a check
# write.csv(M_to_count, "M_to_count.csv")
# write.csv(M_to_mifish, "M_to_mifish.csv")

## CHECK THESE MATRICES. CHARACTERISTICS SHOULD BE:
# COLSUMS all are 0 or 1.  Most should be 1.
# ROWSUMS 0, 1, >1 all are possible.
# does each ID_main map to (at most) a single entity in each dataset?
MAP.ERROR <-
  max(colSums(M_to_count), colSums(M_to_mifish)) #,colSums(M_to_sebastes))
names(which(colSums(M_to_count) > 1))  -> names
species_mapping %>%  filter(., ID_main %in% names)

# does each ID_main map to at least one entity across all datasets?
MIN.ERROR <- M_to_count %>%
  rbind(M_to_mifish) %>%
  colSums() %>%
  `==`(0) %>%
  sum()

##Technical PCR replicate count and index
J_all_count <- dat.count %>%
  group_by(station_id, ID_count) %>%
  summarise(J = length(station_id)) %>%
  group_by(station_id) %>%
  summarise(MIN = min(J), MAX = max(J)) %>%
  filter(MIN == MAX) %>%
  dplyr::select(station_id, N_rep = MIN) %>%
  rename(N_count = N_rep)
J_count  = max(J_all_count$N_count)

J_all_mifish <-  dat.mifish %>%
  group_by(station_id, ID_mifish) %>%
  summarise(J = length(station_id)) %>%
  group_by(station_id) %>%
  summarise(MIN = min(J), MAX = max(J)) %>%
  filter(MIN == MAX) %>%
  dplyr::select(station_id, N_rep = MIN) %>%
  rename(N_mifish = N_rep)
J_mifish  = max(J_all_mifish$N_mifish)

##Create station/location index
STATION_ID <-
  sort(unique(dat.count$station_id)) %>% as.data.frame() %>%
  rename(station_id = ".")
STATION_ID$station_idx <- 1:nrow(STATION_ID)

##Station/location count
K_main = max(nrow(J_all_mifish))
K_mifish = nrow(J_all_mifish)
K_count = nrow(J_all_count)

#create index of unique PCR reactions for mifish (station-replicates)
D_mifish <- dat.mifish %>%
  ungroup() %>%
  left_join(STATION_ID) %>%
  left_join(species_mapping %>% dplyr::select(ID_mifish, sp_index_mifish) %>% distinct()) %>%
  unite(station_idx,
        tech_rep,
        sep = "_",
        remove = F,
        col = "temp") %>%
  mutate(mifish_station_rep_idx = match(temp, unique(temp))) %>%
  dplyr::select(-temp)

#add station_index and species index to dat.count
D_count <- dat.count %>%
  left_join(STATION_ID) %>%
  left_join(species_mapping %>% dplyr::select(ID_count, sp_index_count) %>% distinct())

N_mifish_obs <- nrow(D_mifish)
N_count_obs  <- nrow(D_count)

#### Checks that all of the gymnastics didn't get messed up.  Each of these should = 1
D_count$sp_index_count %>% unique() %>% sort() %>% length() / D_count$sp_index_count %>% max()
D_mifish$sp_index_mifish %>% unique() %>% sort() %>% length() / D_mifish$sp_index_mifish %>% max()
D_count$station_idx %>% max() / D_count$station_idx %>% unique() %>% sort() %>% length()
D_mifish$station_idx %>% max() / D_mifish$station_idx %>% unique() %>% sort() %>% length()

#we want to keep all stations/species for which there is at least one observation in one dataset (count or mifish)
Obs <- full_join(
  pos.all %>% dplyr::select(ID_main, station_id),
  species_mapping %>% dplyr::select(ID_main, sp_index_main)
) %>%
  left_join(., STATION_ID)

N_station_species_main <- nrow(Obs)

N_pcr_mifish = 39


###############################


stan_data <- list(
  "N_mifish_obs" = N_mifish_obs,
  "N_count_obs"  = N_count_obs,
  
  "D_count_obs"  = as.vector(D_count$Abund),
  "D_mifish_obs" = D_mifish$nReads,
  
  # log of fraction of amplicons observed for each replicate-community pair
  "log_r_mifish" = D_mifish %>% dplyr::select(mifish_station_rep_idx, log_r_mifish) %>%
    distinct() %>% pull(log_r_mifish),
  
  ## Species count
  "I_main" = I_main ,
  "I_count"  = I_count,
  "I_mifish" = I_mifish,
  
  ##Technical PCR replicate count and index
  "J_mifish" = J_mifish,
  "J_mifish_vec" = J_all_mifish$N_mifish,
  
  ##Station / Site count
  "K_main" = K_main,
  "K_mifish" = K_mifish,
  "K_count" = K_count,
  
  "N_pcr_mifish" = N_pcr_mifish,
  # Number of PCR cycles (assumed constant across all PCRs)
  
  # main species list indexes and counters
  "N_station_species_main" = N_station_species_main,
  "main_station_idx" = Obs$station_idx,
  "main_sp_idx" = Obs$sp_index_main,
  
  # Indexes and covariate for mifish data
  "mifish_station_idx" = D_mifish$station_idx,
  # station index for mifish data.
  "mifish_sp_idx" = D_mifish$sp_index_mifish,
  # Species index for mifish data
  "mifish_station_rep_idx" = D_mifish$mifish_station_rep_idx,
  "N_mifish_station_rep_idx" = length(unique(D_mifish$mifish_station_rep_idx)),
  
  # Indexes and covariate for count data
  "N_count_obs" = N_count_obs,
  "log_V" = D_count$log_V,
  # Volume of water sampled relative to the reference volume.
  "log_P" = D_count$log_P,
  # Fraction of larvae sorted.
  "count_station_idx" = D_count$station_idx,
  # station index for count data.
  "count_sp_idx" = D_count$sp_index_count,
  # Species index for count data
  
  # Identity matrices relating identifications for species across datasets
  "M_to_count" = M_to_count,
  "M_to_mifish" = M_to_mifish,
  
  "log_eta_prior" = c(-4, 0.5),
  #this parameter is shared for the mifish and sebastes datasets, reflecting the assumption that the same fraction of amplicons are sequenced in the two datasets
  "beta_prior_mifish" = c(1, 1)  # priors for the beta distribution from which values of mifish amp efficiencies are drawn
)


stan_pars = c(
  "b_main",
  "b_main_grid",
  "b_mifish",
  "b_count",
  "a_mifish",
  "tau_mifish_0",
  "tau_mifish_1",
  "log_eta_mifish",
  "log_lik",
  "log_lambda_mifish"
)

stan_init_f2 <- function(n.chain, I_mifish, N_station_rep_idx) {
  A <- list()
  for (i in 1:n.chain) {
    A[[i]] <- list(
      a_mifish = runif(I_mifish, 0.4, 0.6),
      tau_mifish_0 = runif(1, -3, -1),
      tau_mifish_1 = runif(1, 0, 0.1),
      tau_mifish = runif(1, 0.5, 1.5)
    )
  }
  return(A)
}


# Define the MCMC, and run
N_CHAIN = 5
Warm = 1000
Iter = 4000
Treedepth = 10
Adapt_delta = 0.80

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


if (MAP.ERROR > 1 |
    MIN.ERROR > 0) {
  print("YOU HAVE A MAPPING MATRIX ERROR, SPECIES ARE BEING DOUBLE COUNTED")
}

stanMod = stan(
  file = here("analysis", "StanModel", "Mifish_Joint_Model.stan") ,
  data = stan_data,
  verbose = FALSE,
  chains = N_CHAIN,
  thin = 2,
  warmup = Warm,
  iter = Warm + Iter,
  control = list(
    max_treedepth = Treedepth,
    stepsize = 0.01,
    adapt_delta = Adapt_delta,
    metric = "diag_e"
  ),
  pars = stan_pars,
  #refresh = 10,
  boost_lib = NULL,
  #sample_file = paste0("./OutputFiles/tmp.csv"),
  init = stan_init_f2(
    n.chain = N_CHAIN,
    I_mifish = I_mifish,
    N_station_rep_idx = stan_data$N_station_rep_idx
  )
)



# get_adaptation_info(stanMod)
pars <- rstan::extract(stanMod, permuted = TRUE)
samp_params <- get_sampler_params(stanMod)
stanMod_summary <- summary(stanMod)$summary

TRACE <- list()
TRACE[[as.name("Var")]] <-
  traceplot(
    stanMod,
    pars = c(
      "lp__",
      "tau_mifish_0",
      "tau_mifish_1",
      "log_eta_mifish[1]",
      "log_eta_mifish[11]",
      "log_eta_mifish[31]",
      "log_eta_mifish[70]",
      "log_eta_mifish[83]",
      "log_eta_mifish[99]"
    ),
    inc_warmup = FALSE
  )
TRACE[[as.name("a_mifish")]] <-
  traceplot(stanMod,
            pars = c("lp__", "a_mifish"),
            inc_warmup = FALSE)

# some diagnostics, if desired
# stanMod_summary %>% as.data.frame %>% dplyr::select(Rhat) %>% rownames_to_column("name") %>% arrange(desc(Rhat)) %>% head(20)
# traceplot(stanMod, pars = c("a_mifish[43]", "a_mifish[12]", "a_mifish[13]"))
# plot(stanMod, par = c("tau_mifish_0","tau_mifish_1")) #"log_eta_mifish_mean"))
# plot(stanMod, par = "log_eta_mifish")
# plot(stanMod, par = "a_mifish")


# Summarize predictions to make predicted-observed plots
mifish_pred <- exp(pars$log_lambda_mifish) %>% as.data.frame()
mifish_pred_tau <-
  exp(pars$tau_mifish_0 + apply(pars$log_lambda_mifish, 2, "*", pars$tau_mifish_1))

D_mifish <-
  D_mifish %>% mutate(
    post_mean = colMeans(mifish_pred),
    post_median = apply(mifish_pred, 2, median),
    post_tau = exp(
      mean(pars$tau_mifish_0) + mean(pars$tau_mifish_1) * log(post_median)
    )
  )

# predicted - observed for all species & sample combinations.
#posterior mean
po_1a <- ggplot(D_mifish) +
  geom_point(aes(y = nReads, x = post_mean), alpha = 0.2) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red") +
  theme_bw()
#posterior median
po_1b <- ggplot(D_mifish) +
  geom_point(aes(y = nReads, x = post_median), alpha = 0.2) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red") +
  theme_bw()

# For just Anchovy
po_2 <-
  ggplot(D_mifish %>% filter(ID_mifish == "Engraulis mordax")) +
  geom_point(aes(y = nReads, x = post_median, col = station_id), alpha =
               0.5) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red") +
  theme_bw()

# For just Sardine
po_3 <-
  ggplot(D_mifish %>% filter(ID_mifish == "Sardinops sagax")) +
  geom_point(aes(y = nReads, x = post_median, col = station_id), alpha =
               0.5) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red") +
  theme_bw()

# General plot of how sd of Neg binom changes with sample size.
NB.log.mean <- seq(1, 13, by = 0.25)
tau <-
  exp(mean(pars$tau_mifish_0) + mean(pars$tau_mifish_1) * NB.log.mean)
NB.sd   <- sqrt(exp(NB.log.mean) + exp(NB.log.mean) ^ 2 / tau)

plot(NB.sd ~ exp(NB.log.mean), type = "both")
abline(0, 1, col = "red")
p_mean_sd <- ggplot() +
  geom_point(aes(y = log(NB.sd), x = NB.log.mean)) +
  geom_line(aes(y = log(NB.sd), x = NB.log.mean)) +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red") +
  theme_bw()

# Pull out the # smallest predicted mean,
# largest predicted mean,
# predicted means near quartiles 0.1,0.25,0.5, 0.75, 0.9
temp.min <- D_mifish[which.min(D_mifish$post_median), ]
temp.q.10 <-
  D_mifish %>% filter(post_median > quantile(D_mifish$post_median, 0.1)) %>%
  slice(which.min(post_median))
temp.q.25 <-
  D_mifish %>% filter(post_median > quantile(D_mifish$post_median, 0.25)) %>%
  slice(which.min(post_median))
temp.q.50 <-
  D_mifish %>% filter(post_median > quantile(D_mifish$post_median, 0.50)) %>%
  slice(which.min(post_median))
temp.q.75 <-
  D_mifish %>% filter(post_median > quantile(D_mifish$post_median, 0.75)) %>%
  slice(which.min(post_median))
temp.q.90 <-
  D_mifish %>% filter(post_median > quantile(D_mifish$post_median, 0.90)) %>%
  slice(which.min(post_median))
temp.max <- D_mifish[which.max(D_mifish$post_median), ]

temp.all <-
  bind_rows(temp.min,
            temp.q.10,
            temp.q.25,
            temp.q.50,
            temp.q.75,
            temp.q.90,
            temp.max)
temp.all$lab <-
  c("min", "q.10", "q.25", "q.50", "q.75", "q.90", "max")

plot.hist <- list()
for (i in 1:nrow(temp.all)) {
  pred.val <-
    rnbinom(1e5, mu = temp.all$post_median[i], size = temp.all$post_tau[i])
  
  plot.hist[[as.name(temp.all$lab[i])]] <-
    ggplot() + geom_histogram(aes_string(pred.val), bins = 100, alpha = 0.5) +
    geom_vline(xintercept = temp.all$post_median[i], color =
                 "red") +
    geom_vline(xintercept = temp.all$nReads[i], color =
                 "blue") +
    theme_bw() +
    ggtitle(temp.all$lab[i])
}


# Parse model output.
Output <- list(
  Nspecies = I_main,
  Ncommunities = K_main,
  Nreplicates = J_mifish,
  N_pcr_mifish = N_pcr_mifish,
  
  species_mapping = species_mapping,
  
  stan_data = stan_data,
  stan_pars = stan_pars,
  stanMod = stanMod,
  
  D_count = D_count,
  
  D_mifish = D_mifish,
  
  Stations = STATION_ID,
  
  #Plots
  po_1a = po_1a,
  po_1b = po_1b,
  po_2 = po_2,
  po_3 = po_3,
  plot.hist = plot.hist,
  p_mean_sd = p_mean_sd
)

save(Output, file = here("data", paste0(
  "Stan_Fit_combined_",
  format(Sys.time(), "%Y%m%d_%H%M"),
  ".RData"
)))

