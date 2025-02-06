
## Load in the Bottle Data
```{r, echo=FALSE, hide=TRUE, warnings=FALSE,message=FALSE, include=FALSE}

calcofi_tows_all

bottle_data <- read.csv(here("data","CalCOFI_Database_194903-202105_csv","194903-202105_Bottle.csv"), header = T,check.names = F) #CTD bottle data

cast_data <- read.csv(here("data","CalCOFI_Database_194903-202105_csv","194903-202105_Cast.csv"), header = T,check.names = F) #CTD Cast data

cast_data %>% 
  left_join(bottle_data, relationship="many-to-many") -> cast_bottle

cast_bottle %>% 
  mutate(., Station = as.double(Rpt_Sta),
         Line = as.double(Rpt_Line),
         year=Year) %>% 
  mutate(., Date = as.Date(Date, "%m/%d/%Y"),
         year= year(Date),
         month= month(Date),
         season = case_when(month < 3 ~"Winter",
                            month < 6 ~"Spring",
                            month < 9 ~"Summer",
                            month < 12 ~"Fall",
                            TRUE ~"Winter")) %>% 
  mutate(., site= str_c(Line,"_",Station),
         year_site = str_c(year,":",Line,"_",Station),
         year_season_site = str_c(year,":",season,":",Line,"_",Station),
         cruise_site = str_c(Cruise,":",Line,"_",Station)) -> cast_bottle_fixed
# We will merge using cruise_site

calcofi_tows_a8_erdapp %>% 
  left_join(cast_bottle_fixed, by=c("cruise_site"="cruise_site")) -> calcofi_merged_a8_erdapp

```

## Load in the Krill Count Data
```{r, echo=FALSE, hide=TRUE, warnings=FALSE,message=FALSE, include=FALSE}
# Load in the Brinton and Townsend Euphausiid Database - CCE-LTER
# aka krill
BTEDB <- read.csv(here("knb-lter-cce.313.1","BTEDB_Abundances.csv"))

# Pull in Ed Weber's awesome code to merge the Cast and Bottle Data
ed_key <- read.csv(here("data","20240612_key_ed_webber.csv"))

# Clean Up the Metadata
BTEDB %>%
  mutate(., PIC_RowNumber=RowNumber, # Clean Up Header
         TowBegin= as.POSIXct(TowBegin, format = "%Y-%m-%d %H:%M:%S"), # Fix Times
         TowEnd= as.POSIXct(TowEnd,  format = "%Y-%m-%d %H:%M:%S")) %>% # Fix Times
  pivot_longer(cols=`Euphausia_brevis_adult_Abundance`:`Thysanopoda_pectinata_juvenile_Abundance`,names_to = "Ohman_tax", values_to = "Count_permsq2") %>% 
  separate(Ohman_tax, into=c("Genus","species","phase","type"), sep="_", remove = F) %>% 
  mutate(., type = replace_na(type, "")) %>% 
  mutate(ID_Microscopy=str_c(Genus,species, sep=" "), 
         Phase=str_c(phase,type,sep="-")) %>% 
  dplyr::select(PIC_RowNumber,
                Date,
                MaxDepth,TowBegin,TowEnd,Ohman_tax,Genus,species, phase,type,Count_permsq2,ID_Microscopy,Phase
  ) %>% 
  left_join(ed_key)  %>%  # Note that there are missing Cast_Cnt values
  mutate(., Station = as.numeric(N_Station),
         Line = N_Line) %>% 
  filter(., N_lat > 27) %>% #There are some samples from around the world and we don't want them
  filter(., N_lat < 40) %>% 
  filter(., N_lon < -110) %>% 
  mutate(., site= str_c(N_Line,"_",N_Station),
         cruise_site = str_c(N_Cruise,":",N_Line,"_",N_Station))-> long_microscopy
# We will merge using cruise_site


```