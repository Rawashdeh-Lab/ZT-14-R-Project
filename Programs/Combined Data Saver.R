#### Hypnogram Data ####
filelist = list.files(path = "Raw Data/Experimental Group 3/",pattern = ".*Hypnogram.csv",recursive = T,full.names = T)
header <- vroom(filelist[1],col_names = T, n_max = 0)
Headinglist <- colnames(header)
datalist <- list()

for(i in seq_along(filelist)){
  File_i<- vroom(filelist[i],col_names = FALSE, skip = 2)
  colnames(File_i) <- unlist(Headinglist)
  File_i$FileName <- filelist[i]
  datalist[[i]] <- File_i
}

CombHyp <- bind_rows(datalist)

CombHyp$FileRed <- gsub(pattern = "^.*?Per1",replacement = "Per1",x = CombHyp$FileName)
CombHyp$FileRed <- gsub(pattern = "^.*?WT",replacement = "WT",x = CombHyp$FileRed)
unique(CombHyp$FileRed)

CombHypEdit <- CombHyp %>%
  mutate(DateTime = as.POSIXct(TimeStamp,format = "%d/%m/%Y %H:%M:%S %p",tz = "UTC")+43200,
         Date = date(DateTime),
         DateTimeFix = case_when(Date == '2020-04-05' ~ DateTime - 43200, T~DateTime),
         Mouse = as.numeric(gsub(pattern = " Hypnogram.csv|Per1KO |WT|MONO |PFF ", replacement = "", x = FileRed)),
         Genotype = gsub(pattern = "MONO |PFF |[0-9]+ |Hypnogram.csv| ", replacement = "", x = FileRed),
         Injection = gsub(pattern = "WT |Per1KO |[0-9]+ |Hypnogram.csv| ", replacement = "", x = FileRed),
         GenoInj = paste(Genotype, Injection, sep = " ")) %>%
  select(-...3)

vroom_write(CombHypEdit, "Mid Data/ExperimentalGroupThree_ZT14LP_Hypnogram.csv.gz")

rm(list = ls())

#### HZ Analysis ####
filelist = list.files(path = "Raw Data/Experimental Group 3/",pattern = ".*.txt",recursive = T,full.names = T)
datalist <- list()

for(i in seq_along(filelist)){
  File_i<- read_delim(filelist[i],"\t", escape_double = FALSE, col_types = cols(Time = col_datetime(format = "%m/%d/%Y %H:%M:%S")), 
                      trim_ws = TRUE, skip = 10)
  File_i$FileName <- filelist[i]
  datalist[[i]] <- File_i
}
beep(sound = 6)
AllTextFiles <- bind_rows(datalist)
unique(AllTextFiles$FileName)

AllTextFiles$FileRed <- gsub(pattern = "^.*?Per1",replacement = "Per1",x = AllTextFiles$FileName)
AllTextFiles$FileRed <- gsub(pattern = "^.*?WT",replacement = "WT",x = AllTextFiles$FileRed)

AllTextFiles <- AllTextFiles %>%
  select(-X414)

vroom_write(AllTextFiles, "Mid Data/ExperimentalGroupThreeSD_HZ.csv.gz")
beep(sound = 5)

unique(AllTextFiles$FileRed)
AllSDHZ2 <- AllTextFiles %>%
  rename(DateTime = Time) %>%
  mutate(DateTime = as.POSIXct(DateTime,format = "%d/%m/%Y %H:%M:%S",tz = "UTC"),
         Date = as.Date(DateTime),
         Mouse = as.numeric(gsub(pattern = "_EEG_FFT.txt|Per1KO|WT|MONO|PFF| ", replacement = "", x = FileRed)),
         Genotype = gsub(pattern = "MONO|PFF|[0-9]+|_EEG_FFT.txt| ", replacement = "", x = FileRed),
         Injection = gsub(pattern = "WT|Per1KO|[0-9]+|_EEG_FFT.txt| ", replacement = "", x = FileRed),
         GenoInj = paste(Genotype, Injection, sep = " "))
unique(AllSDHZ2$GenoInj)

vroom_write(AllSDHZ2, "Mid Data/ExperimentalGroupThreeSD_HZ_LP.csv.gz")

#### Sleep Band Data ####
filelist = list.files(path = "Raw Data/Experimental Group 2/ZT14 LP 1/",pattern = ".*Sleep States.csv",recursive = T,full.names = T)
header <- vroom(filelist[1],col_names = T, n_max = 0)
Headinglist <- colnames(header)
datalist <- list()

for(i in seq_along(filelist)){
  File_i<- vroom(filelist[i],col_names = FALSE, skip = 2)
  colnames(File_i) <- unlist(Headinglist)
  File_i$FileName <- filelist[i]
  datalist[[i]] <- File_i
}

SleepBands <- bind_rows(datalist)
SleepBands$FileRed <- gsub(pattern = "^.*?Per1",replacement = "Per1",x = SleepBands$FileName)
SleepBands$FileRed <- gsub(pattern = "^.*?WT",replacement = "WT",x = SleepBands$FileRed)
unique(SleepBands$FileRed)

SleepBandsEdit <- SleepBands %>%
  mutate(DateTime = as.POSIXct(`Time Stamp`,format = "%d/%m/%Y %H:%M:%S",tz = "UTC"), 
         Mouse = as.numeric(gsub(pattern = " Sleep States.csv|Per1KO |WT|MONO |PFF ", replacement = "", x = FileRed)),
         Genotype = gsub(pattern = "MONO |PFF |[0-9]+ |Sleep States.csv| ", replacement = "", x = FileRed),
         Injection = gsub(pattern = "WT |Per1KO |[0-9]+ |Sleep States.csv| ", replacement = "", x = FileRed),
         GenoInj = paste(Genotype, Injection, sep = " "),
         ZTDateTime = DateTime,
         Day = yday(ZTDateTime)-228)

vroom_write(SleepBandsEdit, "Raw Data/ExperimentalGroupTwo_ZT14LP_1_SleepStates.csv.gz")

rm(list = ls())

#### Sleep Band Data ####
filelist = list.files(path = "Raw Data/Experimental Group 2/ZT14 LP 2/",pattern = ".*Sleep States.csv",recursive = T,full.names = T)
header <- vroom(filelist[1],col_names = T, n_max = 0)
Headinglist <- colnames(header)
datalist <- list()

for(i in seq_along(filelist)){
  File_i<- vroom(filelist[i],col_names = FALSE, skip = 2)
  colnames(File_i) <- unlist(Headinglist)
  File_i$FileName <- filelist[i]
  datalist[[i]] <- File_i
}

SleepBands <- bind_rows(datalist)
SleepBands$FileRed <- gsub(pattern = "^.*?Per1",replacement = "Per1",x = SleepBands$FileName)
SleepBands$FileRed <- gsub(pattern = "^.*?WT",replacement = "WT",x = SleepBands$FileRed)
unique(SleepBands$FileRed)

SleepBandsEdit <- SleepBands %>%
  mutate(DateTime = as.POSIXct(`Time Stamp`,format = "%d/%m/%Y %H:%M:%S",tz = "UTC"), 
         Mouse = as.numeric(gsub(pattern = " Sleep States.csv|Per1KO |WT|MONO |PFF ", replacement = "", x = FileRed)),
         Genotype = gsub(pattern = "MONO |PFF |[0-9]+ |Sleep States.csv| ", replacement = "", x = FileRed),
         Injection = gsub(pattern = "WT |Per1KO |[0-9]+ |Sleep States.csv| ", replacement = "", x = FileRed),
         GenoInj = paste(Genotype, Injection, sep = " "),
         ZTDateTime = DateTime,
         Day = yday(ZTDateTime)-228)

vroom_write(SleepBandsEdit, "Raw Data/ExperimentalGroupTwo_ZT14LP_2_SleepStates.csv.gz")

rm(list = ls())
