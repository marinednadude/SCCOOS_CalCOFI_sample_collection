library(tidyr)

stations <- read.csv("/Users/nastassiapatin/GitHub/SCCOOS_CalCOFI_sample_collection/erddap_calcofi/erdCalCOFIstns_a96f_826e_91fb.csv")

pic <- read.csv("/Users/nastassiapatin/GitHub/SCCOOS_CalCOFI_sample_collection/samples_in_PIC_DB.csv")

pic <- pic %>% rename(line = Staline)
pic <- pic %>% rename(station = StaNo)
pic <- pic %>% rename(cruise = Cruise)

stations <- stations %>% separate_wider_delim(time, delim = "T", names = c("SampleDate", "Time"))

pic_bottom <- pic %>% left_join(stations, by=c("line", "station", "cruise", "SampleDate"))

write.csv(pic_bottom, "/Users/nastassiapatin/GitHub/SCCOOS_CalCOFI_sample_collection/samples_in_PIC_DB_bottomdepth.csv")

