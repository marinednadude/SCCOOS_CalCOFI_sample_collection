library(tidyverse)
library(rstan)
library(bayesplot)
library(scales) # to access break formatting functions
library(ggpmisc)
library(wesanderson)
library(here)
library(ggpmisc)
library(ggpubr)
library(patchwork)

# Figure 3. Model Outputs ----

load(here("data","Stan_Fit_combined_20221114_1424.RData"))

logtheta <- extract(Output$stanMod, par = "b_count")$b_count %>% colMeans()

logtheta <- logtheta %>% 
  as.data.frame() %>% 
  mutate(station_id = Output$Stations$station_id) 
dim(logtheta)

names(logtheta)[1:48] <- 1:48
pred <-  logtheta %>% 
  pivot_longer(-station_id, names_to = "sp_index_count") %>% 
  mutate(sp_index_count = as.numeric(sp_index_count)) 

Output$D_count %>% 
  dplyr::select(ID_count, Abund , station_id, sp_index_count) %>% 
  pivot_wider(., names_from="station_id", values_from ="Abund", values_fill=0) %>% 
  pivot_longer(cols=`1999_93.3_60`:`2013_86.7_50`, names_to="station_id", values_to ="Abund")-> Output$D_count_zero


obs <- Output$D_count_zero %>% 
  left_join(Output$species_mapping %>% dplyr::select(sp_index_main, sp_index_count)) %>% 
  left_join(Output$Stations) %>% 
  dplyr::select(station_id, station_idx, sp_index_count,sp_index_main, Abund)

pred %>% 
  left_join(obs) %>% 
  drop_na() %>% 
  rename(predicted = value,
         observed = Abund) -> model_and_counts


round(cor(model_and_counts$observed, model_and_counts$predicted),3)


model_and_counts %>% 
  ggplot(aes(x = observed, y = predicted))  + ylab("Predicted Counts") + xlab("Observed Morphological Counts") +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0,slope=1, linetype=2, color="tomato", size=2) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^4),
                     breaks=c(0,10^(1:4)))+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^3),
                     breaks=c(0,10^(1:3))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14, angle = 0),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = 'white'),
    strip.text.x = element_text(size = 15),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    axis.line = element_line(colour = 'black')
  ) + annotate("text", label= paste0("~italic(r)== ", round(cor(model_and_counts$observed, model_and_counts$predicted),2)),
parse=TRUE,
               x = 500,
               y = 10,size = 7) -> model_vs_counts

Output$D_mifish %>% 
  dplyr::select(station_id, nReads, sp_index_mifish) %>% 
  group_by(station_id, sp_index_mifish) %>% 
  summarize(nReads = sum(nReads)) %>% 
  pivot_wider(., names_from="station_id", values_from ="nReads", values_fill=0) %>% 
  pivot_longer(cols=`1996_80_60`:`2019_93.3_60`, names_to="station_id", values_to ="nReads") %>% 
  left_join(Output$species_mapping %>% dplyr::select(ID_main,sp_index_main, sp_index_count, sp_index_mifish)) %>% 
  right_join(Output$D_count_zero) %>% 
  filter(!is.na(nReads)) -> reads_and_counts

reads_and_counts %>% 
  ggplot(aes(x = Abund, y = nReads))  + ylab("Observed Sequence Reads") +
  geom_point(alpha = 0.5) +
  xlab("Observed Morphological Counts") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^7),
                     breaks=c(0,10^(1:7)))+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^3),
                     breaks=c(0,10^(1:3)))+
  theme_bw() +
  geom_abline(intercept = 0,slope=1, linetype=2, color="tomato", size=2) +
  theme(
    axis.text.x = element_text(size = 14),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14, angle = 0),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = 'white'),
    strip.text.x = element_text(size = 15),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    axis.line = element_line(colour = 'black')
  ) +  annotate("text", label= paste0("~italic(r)== ", round(cor(reads_and_counts$Abund, reads_and_counts$nReads),2)),
                parse=TRUE,
                x = 500,
                y = 10,size = 7) -> reads_vs_counts

#posterior mean
po_1a <- Output$D_mifish %>% 
  ggplot(., aes(y = post_mean, x = nReads) ) +
  geom_point( data=Output$D_mifish, aes(y = post_mean, x = nReads), alpha = 0.2, color="black")  +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^6),
                     breaks=c(0,10^(1:6)))+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^6),
                     breaks=c(0,10^(1:6)))+
  geom_abline(intercept = 0,slope=1, linetype=2, color="tomato", size=2) +
  xlab("Observed Sequence Reads") +
  ylab("Predicted Sequence Reads") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14, angle = 0),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = 'white'),
    axis.line = element_line(colour = 'black')) +  annotate("text", label= paste0("~italic(r)== ", round(cor(Output$D_mifish$nReads, Output$D_mifish$post_mean),3)),
                                                            parse=TRUE,
                                                            x = 100000,
                                                            y = 10,size = 7)

pred_seq_vs_count <-  Output$D_mifish %>% 
  dplyr::select(station_id, post_mean, sp_index_mifish) %>% 
  group_by(station_id, sp_index_mifish) %>% 
  summarize(post_mean = sum(post_mean)) %>% 
  pivot_wider(., names_from="station_id", values_from ="post_mean", values_fill=0) %>% 
  pivot_longer(cols=`1996_80_60`:`2019_93.3_60`, names_to="station_id", values_to ="post_mean_reads") %>% 
  left_join(Output$species_mapping %>% dplyr::select(ID_main,sp_index_main, sp_index_count, sp_index_mifish)) %>% 
  right_join(model_and_counts) %>% 
  filter(!is.na(post_mean_reads)) 

plot_4 <- pred_seq_vs_count %>% 
  ggplot(., aes(y = post_mean_reads, x = predicted) ) +
  geom_point(aes(y = post_mean_reads, x = predicted), alpha = 0.2, color="black")  +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^7),
                     breaks=c(0,10^(1:7)))+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10),
                     limits=c(0, 10^4),
                     breaks=c(0,10^(1:4)))+
  geom_abline(intercept = 0,slope=1, linetype=2, color="tomato", size=2) +
  xlab("Predicted Morphological Counts") +
  ylab("Predicted Sequence Reads") +
  theme_bw() +
  geom_smooth(
    method="glm",
    method.args=list(family="binomial"))+ 
  theme(
    axis.text.x = element_text(size = 14),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14, angle = 0),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = 'white'),
    axis.line = element_line(colour = 'black')) +  annotate("text", label= paste0("~italic(r)== ", round(cor(pred_seq_vs_count$post_mean_reads, pred_seq_vs_count$predicted),2)),
                                                            parse=TRUE,
                                                            x = 1000,
                                                            y = 10,size = 7)
plot_4

Output$D_mifish %>% 
  filter(.,nReads ==0) %>% 
  arrange(desc(post_mean))

po_1a

reads_vs_counts /(model_vs_counts + po_1a )  + plot_annotation(tag_levels = 'a')

ggsave(
  file = here::here("analysis", "figures", "Figure_3_model_output.png"),
  width = 12,
  height = 8
)

# Figure 2. Co-detection of eDNA and Microscopy Data -----

Output$D_mifish %>% 
  dplyr::select(station_id, nReads, sp_index_mifish) %>% 
  group_by(station_id, sp_index_mifish) %>% 
  summarize(nReads = sum(nReads)) %>% 
  pivot_wider(., names_from="station_id", values_from ="nReads", values_fill=0) %>% 
  pivot_longer(cols=`1996_80_60`:`2019_93.3_60`, names_to="station_id", values_to ="nReads") -> mifish_data_ready

Output$Stations -> station_holder 
Output$species_mapping -> species_mapping_holder
station_holder$fake <- 1
species_mapping_holder$fake <- 1
my_cross_join <- full_join(station_holder, species_mapping_holder, by = "fake") %>%
  select(-fake)

my_cross_join %>% 
  left_join(mifish_data_ready) %>% 
  left_join(Output$D_count_zero) %>% 
  dplyr::mutate(nReads = replace_na(nReads, 0),
                Abund = replace_na(Abund, 0))-> cross_all_jars

cross_all_jars %>% 
  mutate(., Morph_detect = ifelse(Abund > 0, 1,0)) %>% 
  mutate(., MiFish_detect = ifelse(nReads > 0, 1,0)) %>% 
  group_by(ID_main,station_id) %>% 
  mutate(type = case_when(MiFish_detect == 1 & Morph_detect == 1 ~"Both_detected",
                          MiFish_detect == 0 & Morph_detect == 0 ~"Both_absent",
                          MiFish_detect == 1 & Morph_detect == 0 ~"eDNA_only",
                          MiFish_detect == 0 & Morph_detect == 1 ~"Morph_only",
                          TRUE~"low_data")) %>% 
  mutate(., ID_main = str_replace(ID_main,"AANOTHER","sp.")) %>% 
  mutate(., ID_main = str_replace(ID_main,"nannochir","cold variant")) %>% 
  dplyr::select(Species=ID_main,station_id, MiFish_detect, Morph_detect, type) ->b_heat

wesanderson::wes_palette("Darjeeling1") -> col_dar
wesanderson::wes_palette("Darjeeling2") -> col_dar2

b_heat  %>% 
  separate(station_id, into = c("Year","Rpt_Line","Rpt_Sta"), remove = F, sep="_") %>%
  unite(., "station" , c("Rpt_Line","Rpt_Sta"), remove=FALSE) %>% 
  mutate(., Station = case_when(station =="80_60"~"1. Pt. Conception",
                                       station =="86.7_50"~"2. San Nicholas Island",
                                       station =="93.3_60"~"4. San Diego Offshore",
                                       station =="93.3_30"~"3. San Diego Inshore")) %>% 
  mutate(.,
    Species = if_else(Species== "Stenobrachius leucopsarus",  "Stenobrachius leucopsarus V1" , Species),
    Species = if_else(Species== "Stenobrachius cold variant",  "Stenobrachius leucopsarus V2" , Species)) %>% 
  mutate(., Year=as.numeric(Year)) ->b_heat

b_heat %>% 
  ungroup() %>% 
  dplyr::select(Species) %>% distinct() -> species_detected

write_csv(species_detected, file=here("data","taxa_detected_by_all_methods.csv"))
  
b_heat %>% 
  ggplot(., aes(x=Species, y=Year, fill=type)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name = "Detection Type", labels =c("Both Absent","Both Detected","eDNA Only","Microscopy Only"),values = c("white",col_dar[2],col_dar[5],col_dar2[3])) +
  ylab("Year")+facet_wrap(~Station, nrow=4) + 
  scale_y_continuous(labels = c(1996,2000,2004,2008,2012,2016,2019), 
                     breaks = c(1996,2000,2004,2008,2012,2016,2019),
                     expand = c(0,0))+
  theme(
    axis.text.x = element_text(size = 15, angle = 90),
    axis.text.y = element_text(size = 15, angle = 0),
    axis.title.x = element_text(size = 18,  face = "bold"),
    axis.title.y = element_text(size = 18, angle = 90, face = "bold"),
    strip.background = element_rect(fill = NA, color = 'black'),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.text = element_text(colour = 'black'),
    strip.text.x = element_text(size = 16, face = 'italic'),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"))  +
    scale_x_discrete(labels = c(expression(paste("Arctozenus risso")),
      expression(paste("Argyropelecus sladeni")),
      expression(paste("Artedius corallinus"   ))    ,  
      expression(paste(bold("Bathylagidae"))),
      expression(paste("Bathylagoides wesethi")),
      expression(paste("Bathylagus pacificus"   ))    , 
      expression(paste(bold("Benthalbella dentata"))),
      expression(paste("Brosmophycis marginata")),
      expression(paste("Cataetyx rubrirostris"     ))  ,
      expression(paste(bold("Ceratoscopelus townsendi"))),
      expression(paste("Chauliodus macouni")),
      expression(paste("Citharichthys sordidus"      )),
      expression(paste("Citharichthys stigmaeus")),
      expression(paste("Cololabis saira")),
      expression(paste("Cyclothone atraria"     ))     ,
      expression(paste("Danaphos oculatus")),
      expression(paste(bold("Diaphus theta"  ))),
      expression(paste("Diogenichthys atlanticus"    )),
      expression(paste("Engraulis mordax" )),
      expression(paste("Genyonemus lineatus")),
      expression(paste("Halichoeres californica"    )) ,
      expression(paste("Ichthyococcus irregularis")),
      expression(paste("Icichthys lockingtoni"   )),
      expression(paste("Lepidopsetta bilineata"   ))   ,
      expression(paste("Leuroglossus stilbius"  )),
      expression(paste("Lipolagus ochotensis"  )),
      expression(paste("Lyopsetta exilis"       )),     
      expression(paste("Melamphaes lugubris"  )),
      expression(paste("Melamphaes parvus" ))   ,
      expression(paste("Merluccius productus"    )),    
      expression(paste(bold("Microstomus pacificus"    ))),
      expression(paste("Nannobrachium sp."      ))  ,
      expression(paste("Nannobrachium regale"   ))     ,
      expression(paste("Nannobrachium ritteri"     )),
      expression(paste("Nansenia candida"       ))  ,
      expression(paste(bold("Opisthoproctidae"       )))     ,
      expression(paste("Paralepididae"           )) ,
      expression(paste("Paralichthys californicus" )),
      expression(paste(bold("Parophrys vetulus"   )))     ,   
      expression(paste(bold("Peprilus simillimus"   )))    ,
      expression(paste("Poromitra crassiceps"    )),
      expression(paste("Protomyctophum crockeri"   ))  ,
      expression(paste("Rathbunella hypoplecta"   )),
      expression(paste("Rhinogobiops nicholsii"   )),
      expression(paste("Ruscarius creaseri"    ))      ,
      expression(paste("Sardinops sagax"    ))      ,
      expression(paste("Scomber japonicus"      ))   ,
      expression(paste("Sebastes"      ))              ,
      expression(paste("Sebastolobus sp." ))        ,
      expression(paste(bold("Stenobrachius sp."   )))     ,
      expression(paste("Stenobrachius leucopsarus"  )) ,
      expression(paste(bold("Stenobrachius cold variant" ))),
      expression(paste("Stomias atriventer"     ))    ,
      expression(paste("Symbolophorus californiensis")),
      expression(paste("Tarletonbeania crenularis"   )),
      expression(paste("Tetragonurus cuvieri"    )),
      expression(paste("Trachurus symmetricus" )),
      expression(paste("Triphoturus mexicanus"   )),
      expression(paste("Vinciguerria sp." ))))-> b_heat_plot

b_heat_plot

ggsave(
  b_heat_plot,
  file = here::here("analysis", "figures", "Figure_2_codetection_of_taxa.png"),
  width = 18,
  height = 20
)

# Overlap Statistics ----
b_heat %>% filter(., type=="Morph_only") %>% dplyr::select(ID_main=Species,station_id) %>% 
  left_join(cross_all_jars) %>% 
  arrange(desc(Abund))

cross_all_jars %>% 
  filter(., nReads <1) %>% 
  filter(., Abund >0) %>% arrange(desc(Abund)) %>% 
  dplyr::summarise(max(Abund),min(Abund),mean(Abund),sd(Abund))

b_heat %>% 
  group_by(Species) %>% 
  count(type) %>% 
  filter(.,type!="Both_absent") %>% 
  pivot_wider(names_from = type, values_from = n, values_fill=0) %>% 
  mutate(class = case_when(Both_detected>0 ~"Detected by Both at a station",
                           eDNA_only>0 & Morph_only >0 ~"Detected by Both but different stations",
                           eDNA_only>0 ~"eDNA Only",
                           Morph_only>0~ "Morph Only")) %>% 
  mutate(., Type = case_when(Both_detected>eDNA_only & Both_detected>Morph_only ~"Mostly Double Detected",
                            eDNA_only>Morph_only  & eDNA_only>Both_detected ~"Mostly eDNA Detected",
                              Morph_only>eDNA_only  & Morph_only>Both_detected ~"Mostly Morph Detected",
                              TRUE~ "Evenly Detected")) %>%
  ungroup() %>% 
  group_by(Type) %>% 
  count()

b_heat %>% 
  group_by(Species) %>% 
  dplyr::summarise(eDNA_detect = sum(MiFish_detect), 
                   morph_detect = sum(Morph_detect)) %>% 
  mutate(., Type = case_when(eDNA_detect>morph_detect ~"Mostly eDNA Detected",
                             morph_detect>eDNA_detect ~"Mostly Morph Detected",
                             TRUE~ "Evenly Detected")) %>% 
  ungroup() %>% 
  group_by(Type) %>% 
  count()

b_heat%>% 
  group_by(Species) %>% 
  count(type) %>% 
  filter(.,type!="Both_absent") %>% 
  pivot_wider(names_from = type, values_from = n, values_fill=0) %>% 
  mutate(class = case_when(Both_detected>0 ~"Detected by Both at a station",
                           eDNA_only>0 & Morph_only >0 ~"Detected by Both but different stations",
                           eDNA_only>0 ~"eDNA Only",
                           Morph_only>0~ "Morph Only")) %>% 
  mutate(., Type = case_when(Both_detected>eDNA_only & Both_detected>Morph_only ~"Mostly Double Detected",
                             eDNA_only>Morph_only  & eDNA_only>Both_detected ~"Mostly eDNA Detected",
                             Morph_only>eDNA_only  & Morph_only>Both_detected ~"Mostly Morph Detected",
                             TRUE~ "Evenly Detected")) %>%
  filter(., Type %in% c("Mostly Double Detected","Evenly Detected")) %>% 
  dplyr::select(Species)-> most_double_species

cross_all_jars %>% 
  group_by(ID_main) %>% 
  dplyr::summarise(tot_reads=sum(nReads),tot_counts=sum(Abund)) %>% 
  ungroup() %>% 
  mutate(., sum_reads =sum(tot_reads),
         sum_counts = sum(tot_counts),
         perc_reads =tot_reads/sum_reads,
         perc_counts =tot_counts/sum_counts) -> df4

df4 %>% 
  ungroup() %>% 
  arrange(desc(perc_reads)) %>%  top_n(10) -> top_10_perc_reads

df4 %>% 
  ungroup() %>%
  arrange(desc(perc_counts)) %>%  top_n(10) -> top_10_perc_counts

df4 %>% 
  filter(., ID_main %in% most_double_species$Species) -> double_detect_df4

intersect(top_10_perc_reads$ID_main, double_detect_df4$ID_main)
# 6 in the top 10, 8 in the top 20
intersect(top_10_perc_counts$ID_main, double_detect_df4$ID_main)
# 6in the top 10, 8 in the top 20
intersect(top_10_perc_counts$ID_main, top_10_perc_reads$ID_main)
# same species in top 10 for both

most_double_species

b_heat %>% 
  group_by(type) %>% 
  count() %>% 
  ungroup() %>% 
  mutate( tot=sum(n),
    perc = n/tot)


# Supplemental Figure 2 -----

MiFish_data<-readRDS(here("data", "mifish_tech_nReads.RDS"))
larvae_data <- readRDS(here("data", "microscopy_tech_nReads.RDS"))

mi_reads <-MiFish_data  %>%
  filter(., !is.na(mifish_reads)) %>% 
  mutate(bio_rep = paste(station_id, ext_rep, sep = "_")) %>%
  mutate(tech_unique = paste(station_id, ext_rep,tech_rep, sep = "_")) %>%
  select(-c(Sample, ID_main)) %>%
  distinct() 

mi_larvae <- larvae_data  %>%
  filter(., !is.na(larval_counts)) %>% 
  select(-c( ID_main)) %>%
  distinct() 

shared_bottles <- intersect(mi_larvae$station_id,mi_reads$station_id)

mi_reads <- mi_reads %>% filter(., station_id %in% shared_bottles)
mi_larvae <- mi_larvae %>% filter(., station_id %in% shared_bottles)

counts_plus_reads <- left_join(MiFish_data, larvae_data) %>% filter(., station_id %in% shared_bottles)

# lets get the data into wide form 
counts_plus_reads <- counts_plus_reads %>%
  mutate(bio_rep = paste(station_id, ext_rep, sep = "_")) %>%
  select(-c(Sample, ID_main)) %>%
  dplyr::mutate(larval_counts = replace_na(larval_counts, 0)) %>% 
  dplyr::mutate(mifish_reads = replace_na(mifish_reads, 0)) %>% 
  distinct() 

counts_plus_reads %>% 
  unite(., Sample_ID, c("station_id","ext_rep","tech_rep"), sep=":") %>% 
  dplyr::select(-larval_counts) %>% 
  group_by(Sample_ID) %>% 
  dplyr::summarise(tot_reads=sum(mifish_reads)) -> mifish_tot_reads

counts_plus_reads %>% 
  unite(., Sample_ID, c("station_id","ext_rep","tech_rep"), sep=":") %>% 
  dplyr::select(-mifish_reads) %>% 
  group_by(Sample_ID) %>% 
  dplyr::summarise(tot_counts=sum(larval_counts)) -> larvae_tot_counts

counts_plus_reads %>% 
  unite(., Sample_ID, c("station_id","ext_rep","tech_rep"), sep=":",remove=FALSE) %>% 
  left_join(mifish_tot_reads) %>% 
  left_join(larvae_tot_counts) %>% 
  mutate(., prop_reads= mifish_reads/tot_reads,
         prop_counts=larval_counts/tot_counts)  %>% 
  filter(., tot_reads>0)-> calcofi_dont

calcofi_dont %>% 
  dplyr::select(ID_mifish,Sample_ID,larval_counts) %>% 
  distinct() %>% 
  filter(., larval_counts >0) %>% 
  group_by(ID_mifish) %>% 
  dplyr::summarise(Sample_count_micro=n_distinct(Sample_ID), tot_obs_micro = sum(larval_counts)) -> micro_obs

counts_plus_reads %>% 
  filter(., larval_counts !=0) %>%  
  filter(., mifish_reads >0) %>% 
  dplyr::select(ID_mifish) %>% 
  distinct() -> unique_species_mock_calcofi2

unique_species_mock_calcofi2 %>% 
  dplyr::summarise(n_distinct(ID_mifish))

counts_plus_reads %>% 
  filter(., !is.na(mifish_reads)) %>% 
  unite(., Sample_ID, c("station_id","ext_rep","tech_rep"), sep=":",remove=FALSE) %>% 
  left_join(mifish_tot_reads) %>% 
  left_join(larvae_tot_counts) %>% 
  mutate(., prop_reads= mifish_reads/tot_reads,
         prop_counts=larval_counts/tot_counts) %>% 
  mutate(larval_abundance = case_when(larval_counts >10  ~ ">10 Larvae",
                                      larval_counts >0  ~ ">0 Larvae",
                                      TRUE ~"No Larvae"))  %>% 
  filter(., tot_reads >0)-> calcofi_use_all

calcofi_use_all %>% 
  # filter(larval_counts >0) %>% 
  group_by(ID_mifish,station_id,ext_rep) %>% 
  dplyr::summarise(non_na_tech_count = n_distinct(tech_rep),
                   mean_prop_reads = mean(prop_reads),
                   mean_reads = mean(mifish_reads),
                   max_reads = max(mifish_reads),
                   min_reads = min(mifish_reads),
                   larval_counts=mean(larval_counts),
                   Drop_outs = length(mifish_reads[mifish_reads==0]),
                   max_prop_reads=max(prop_reads),
                   min_prop_reads=min(prop_reads),
                   mean_prop_counts = mean(prop_counts),
                   max_prop_counts=max(prop_counts),
                   min_prop_counts=min(prop_counts),
                   drop_prop=Drop_outs/non_na_tech_count) %>% 
  mutate(Drop_outs_cat = case_when(drop_prop ==1 ~"Non Detection",
                                   drop_prop >0.66 ~"2/3rds of reps are zero",
                                   drop_prop >0.49 ~"1/2 of reps are zero",
                                   drop_prop >0.3 ~"1/3 of reps are zero",
                                   min_prop_reads >0 ~"Detected Across all Reps")) -> calcofi_dropout_prop_all

calcofi_dropout_prop_all %>% 
  filter(., !is.na(mean_prop_reads)) %>% 
  mutate(., Species =ID_mifish)-> plotting_drop_outs_all


plotting_drop_outs_all %>% 
  filter(., ID_mifish !="MISSING") %>% 
  filter(., ID_mifish !="Paralepididae") %>% 
  ggplot(., aes(x=(larval_counts+0.1), y=drop_prop))+
  geom_count(alpha =0.5) + 
  scale_size(range = c(2, 12), breaks=c(1,10,100,1000,5000)) +
  geom_smooth(
    method="glm",
    method.args=list(family="binomial"))+ 
  theme(axis.text.x = element_text(angle =30, hjust=1)) +
  xlab("Larvae Counts") +
  ylab("Proportion Non-Detect") + theme_bw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, angle = 0),
    strip.text.x=element_text(size=16),
    axis.title.x =element_text(size = 16,  face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    #legend.position = "none"
  ) +scale_x_log10(limits=c(10^0, 10^3), breaks=10^(0:3),labels=c(expression(10^0),expression(10^1),expression(10^2),expression(10^3)))+
  labs(size="Count") -> sup_fig_2

sup_fig_2


ggsave(
  sup_fig_2,
  file = here::here("analysis", "figures", "Figure_S2_non_detections.png"),
  width = 12,
  height = 8
)


plotting_drop_outs_all %>%
  filter(., ID_mifish !="MISSING") %>% 
  filter(., ID_mifish !="Paralepididae") %>% 
  mutate(count_non_detections = if_else(larval_counts >0, 0,1)) %>%
  ggplot(., aes(x=(mean_prop_reads+0.1), y=count_non_detections))+
  geom_count(alpha =0.5) + 
  geom_smooth(
    method="glm",
    method.args=list(family="binomial"))+ 
  theme(axis.text.x = element_text(angle =30, hjust=1)) +
  xlab("Mean Proportion of Reads") +
  ylab("Proportion Non-Detect") + theme_bw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, angle = 0),
    strip.text.x=element_text(size=16),
    axis.title.x =element_text(size = 16,  face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    #legend.position = "none"
  ) +
  labs(size="Count") -> sup_fig_2

sup_fig_2

Output$D_mifish$station_id %>% unique()
Output$D_mifish$Sample %>% unique()
Output$Stations


