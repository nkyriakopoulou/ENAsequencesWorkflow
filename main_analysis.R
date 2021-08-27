
# Name: Niki Kyriakopoulou
# Date:2021

install.packages(c("jsonlite", "tidyverse", "httr", "zoo", "roll", "ggplot2", "scales"))

citation()
citation("base")
citation("jsonlite")
citation("tidyverse")
citation("httr")
citation("zoo")
citation("roll")
citation("ggplot2")


# Programmatic access to sequence records held in the European Nucleotide Archive (ENA).
# We will retrieve public records in JSON format, stored in the "sequence" dataset
# utilizing the ENA portal API (documentation available here: https://www.ebi.ac.uk/ena/portal/api/doc?format=pdf).

# The URL was dowloaded using curl and Git CMD
# curl -o seq-all.json "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,collection_date,last_updated,first_public,bio_material,culture_collection,specimen_voucher,sample_accession,study_accession&limit=0&format=json"

# Using Cygwin Terminal, the json file was split into multiple pieces using the following:
# split -l 2000000 --additional-suffix=.json C:/.../seq-all.json C:/.../seq-all/
  

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(jsonlite)
library(tidyverse)

seq_all <- list.files(path = "C:/.../seq-all", pattern = ".json", full.names = TRUE)

no.rows <- lapply(X = seq_all, FUN = function(x) {
  length(count.fields(x, skip = 1))
})

class(no.rows)
# [1] "list"

no.rows <- unlist(no.rows)

class(no.rows)
# [1] "integer"

sum(no.rows)
# [1] 556480200

# Since the files are in json format, the number of rows includes those with ",".
556480200/2
# [1] 278240100 sequence accessions in the ENA database


# Create data frames with sequence accessions that have source identifiers.

for (i in 1:length(seq_all)){
  # load the json that corresponds to the i element in the list of files and convert it to a data frame
  data <- fromJSON(seq_all[[i]], flatten = TRUE)
  # filter each data frame by keeping the rows with non-empty source qualifier columns
  data_source <- data %>% filter(!(bio_material == "" & specimen_voucher == "" & culture_collection == ""))
  rm(data)
  write.csv(data_source, file = paste0("D:/.../results/","results_", i,".csv"), row.names = F)
}


source_files <- list.files(path = "D:/.../results", pattern = ".csv", full.names = TRUE)

nrows <- lapply(X = source_files, FUN = function(x) {
  length(count.fields(x, skip = 1))
})

nrows <- unlist(nrows)

sum(nrows)
# [1] 9513397 sequences have source identifiers

source_df <- list.files(path = "D:/.../results", pattern = ".csv", full.names = TRUE) %>% 
    lapply(read_csv, col_types = cols(
      accession = col_character(), 
      description = col_character(), 
      scientific_name = col_character(),
      collection_date = col_date(),
      last_updated = col_date(),
      first_public = col_date(),
      bio_material = col_character(), 
      culture_collection = col_character(), 
      specimen_voucher = col_character(), 
      sample_accession = col_character(), 
      study_accession = col_character())) %>% 
    bind_rows 

source_df[is.na(source_df)] <- " "

write.csv(source_df, file = "D:/.../results/resultsTOT.csv", row.names = F)


# Create data frames with sequence accessions that do not have source identifiers.

for (i in 1:length(seq_all)){
  # load the json that corresponds to the i element in the list of files
  data <- fromJSON(seq_all[[i]], flatten = TRUE)
  # filter each data frame by keeping the rows with empty source qualifier columns
  data_nosource <- data %>% filter(bio_material == "" & specimen_voucher == "" & culture_collection == "")
  rm(data)
  write.csv(data_nosource, file = paste0("D:/.../data.nosource/","data.nosource_", i,".csv"), row.names = F)
}

nosource_files <- list.files(path = "D:/.../data.nosource", pattern = ".csv", full.names = TRUE)


# Skip the header row with skip = 1
nrows <- lapply(X = nosource_files, FUN = function(x) {
  length(count.fields(x, skip = 1))
})

nrows <- unlist(nrows)

sum(nrows)
# [1] 268726703 sequences lacking source identifiers


## Some sequences lack source annotation but as a result of filtering by using "" and not " ", they were included in those associated with annotation.

for (i in 1:length(seq_all)){
  # load the json that corresponds to the i element in the list of files
  data <- fromJSON(seq_all[[i]], flatten = TRUE)
  data_nosource <- data %>% filter(bio_material == " " & specimen_voucher == " " & culture_collection == " ")
  rm(data)
  write.csv(data_nosource, file = paste0("D:/.../data.nosource/","data.nosource_last", i,".csv"), row.names = F)
}

no_source_df <- read.csv(file = "D:/.../data.nosource_last.csv")

length(no_source_df$accession)
# [1] 1606 sequences


268726703+1606
# [1] 268,728,309 accessions in total, lack source information

(268728309/278240100)*100
# [1] 96.58144% of the 278 million accessions stored in the ENA database do not have source identifiers


## Since 1,606 accessions were found to have no source information, we need to extract them from the resultsTOT.csv file.

source_df <- read.csv(file = "D:/.../resultsTOT.csv")

resultsTOT_df <- anti_join(source_df, no_source_df, by='accession')

length(resultsTOT_df$accession)
# [1] 9,511,791 accessions have source information

(9511791/278240100)*100
# [1] 3.419132% of the 278 million accessions stored in the ENA database have source identifiers

# write the new resultsTOT data frame into a csv file
write.csv(resultsTOT_df, file = "D:/.../results/resultsTOT.csv", row.names = F)


#  How many accessions from the 3.4% have a study and sample accession ID? ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(tidyverse)

source_df <- read.csv(file = "D:/.../resultsTOT.csv")

sampleID <- source_df %>% filter(sample_accession != " ")

length(sampleID$accession)
# [1] 1769618

length(sampleID$accession)/length(source_df$accession)*100
# [1] 18.60447% of the sequence accessions that have source annotation, is also linked to a sample accession number

studyID <- source_df %>% filter(study_accession != " ")

length(studyID$accession)
# [1] 4167686

length(studyID$accession)/length(source_df$accession)*100
# [1] 43.816% of the sequence accessions that have source annotation, is also linked to a study accession number


#  How many accessions from the remaining 96.6% have a study and sample accession ID? ---------------------------------

no_source_files <- list.files(path = "D:/.../data.nosource", pattern = ".csv", full.names = TRUE)


# Create data frames with sequence accessions that do not have source identifiers but they have sample accession ID.

for (i in 1:length(no_source_files)){
  # load the csv that corresponds to the i element in the list of files
  no_source <- read.csv(no_source_files[[i]])
  sampleID <- no_source %>% filter(sample_accession != "")
  rm(no_source)
  sampleID[is.na(sampleID)] <- " "
  write.csv(sampleID, file = paste0("D:/.../sample_accessionID/","sample_accessionID_", i,".csv"), row.names = F)
}

sampleID_files <- list.files(path = "D:/.../sample_accessionID", pattern = ".csv", full.names = TRUE)

nrows <- lapply(X = sampleID_files, FUN = function(x) {
  length(count.fields(x, skip = 1))
})

nrows <- unlist(nrows)

sum(nrows)
# [1] 71,196,764

(71196764/268728309)*100
# [1] 26.49396% of the 268.7 million accessions that do not have source identifiers, have sample accession ID

100-26.49396
# [1] 73.50604% of the 268.7 million accessions that do not have source identifiers, do not have sample accession ID either


# Create data frames with sequence accessions that do not have source identifiers but they have study accession ID.

for (i in 1:length(no_source_files)){
  # load the csv that corresponds to the i element in the list of files
  no_source <- read.csv(no_source_files[[i]])
  studyID <- no_source %>% filter(study_accession != "")
  rm(no_source)
  studyID[is.na(studyID)] <- " "
  write.csv(studyID, file = paste0("D:/.../study_accessionID/","study_accessionID_", i,".csv"), row.names = F)
}

studyID_files <- list.files(path = "D:/.../study_accessionID", pattern = ".csv", full.names = TRUE)

nrows <- lapply(X = studyID_files, FUN = function(x) {
  length(count.fields(x, skip = 1,  quote = ""))
})

nrows <- unlist(nrows)

sum(nrows)
# [1] 93,034,685

(93034685/268728309)*100
# [1] 34.62035% of the 268.7 million accessions that do not have source identifiers, have study accession ID

100-34.62035
# [1] 65.37965% of the 268.7 million accessions that do not have source identifiers, do not have study accession ID either


#  Examine the sequences lacking source identifiers and determine if they are extracted from uncultured/unvouchered bacterial/archaeal, viral and eukaryotic sources OR if they are synthetic ones  ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(jsonlite)
library(tidyverse)

no_source_files <- list.files(path = "D:/.../data.nosource", pattern = ".csv", full.names = TRUE)


# The function grepl() was used to see whether the following patterns (model organisms, synthetic sequences) exist inside the column "scientific_name" 
# (scientific name of the organism from which the sample was derived).

# create a vector of model organisms
model_organisms <- read.csv(file = "D:/.../Model organisms.csv")

model_organisms <- model_organisms$Model.organisms

# create a vector of other terms
terms <- c("synthetic construct", "unidentified", "uncultured", "unclassified", "Cloning vector", "Homo sapiens", "Virus", "virus", "Phage", "phage", "Bacteria", "bacteria", "Bacterium", "bacterium", "bacter", "coccus", "cocci")

# Concatenate these two vectors
patterns <- c(model_organisms, terms)

for (i in 1:length(no_source_files)){
  # read the csv that corresponds to the i element in the list of files
  no_source <- read.csv(no_source_files[[i]])
  # subset the data frame
  no_source <- no_source %>% select("accession", "first_public", "scientific_name")
  # filter each data frame, keeping the rows where either of the terms in the vector are found in the column "scientific_name"
  result <- filter(no_source, grepl(paste(patterns, collapse="|"), scientific_name))
  write.csv(result, file = paste0("D:/.../scientName_analysis/patterns_analysis/","patterns_analysis_", i,".csv"), row.names = F)
}


patterns_analysis_files <- list.files(path = "D:/.../patterns_analysis", pattern = ".csv", full.names = TRUE)

# Count the number of sequences extracted from humans, viruses, bacteria, model organisms etc.

nrows <- lapply(X = patterns_analysis_files, FUN = function(x) {
  length(count.fields(x, skip = 1, quote = ""))
})

nrows <- unlist(nrows)

sum(nrows)
# [1] 153,369,554 sequences 


sum(nrows)/268728309*100
# [1] 57.07235% of the 268.7 million sequences lacking source identifiers


# Get the rest of the sequences that do not contain any of the patterns (model organisms, terms) used above

no_source_files <- list.files(path = "D:/.../data.nosource", pattern = ".csv", full.names = TRUE)


# Remove the rows that contain these patterns

for (i in 1:length(no_source_files)){
  # read the csv that corresponds to the i element in the list of files
  no_source <- read.csv(no_source_files[[i]])
  # subset the data frame
  no_source <- no_source %>% select("accession", "first_public", "scientific_name")
  # select the rows that do not match any of the terms in the vector, in each data frame
  result <- filter(no_source, !grepl(paste(patterns, collapse="|"), scientific_name))
  write.csv(result, file = paste0("D:/.../scientName_analysis/nopatterns_analysis/","nopatterns_analysis_", i,".csv"), row.names = F)
} 


nopatterns_analysis_files <- list.files(path = "D:/.../nopatterns_analysis", pattern = ".csv", full.names = TRUE)


# Count the number of sequences lacking source identifiers, that do not contain any of the terms in the column "scientific_name".

nrows1 <- lapply(X = nopatterns_analysis_files, FUN = function(x) {
  length(count.fields(x, skip = 1, quote = ""))
})

nrows1 <- unlist(nrows1)

sum(nrows1)
# [1] 115,358,755 sequences 

sum(nrows1)/268728309*100
# [1] 42.92765% of the 268.7 million sequences lacking source identifiers



#  Examine the data frame that contains sequences with source identifiers and split into smaller data frames ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(tidyverse)

source_df <- read.csv(file = "D:/.../resultsTOT.csv")


# Sequence accessions can originate from more than one bio_material, culture_collection and/or specimen_voucher and consequently can have more than
# one identifiers separated by ; =================================

### Check bio_material column for the presence/absence of ";".

bio_material <- source_df %>% filter(str_detect(source_df$bio_material, ";"))

length(bio_material$accession)
# [1] 240 accessions have more than one bio_material identifiers


### Separate the identifiers in the bio_material column 

# Count the number of ';'s in each string of the column 'bio_material' and create a new column
bio_material$biomat.no.of.semicolons <- str_count(bio_material$bio_material, ";")

length(which(bio_material$biomat.no.of.semicolons == 1))
# [1] 239 accessions have 2 bio_material identifiers

length(which(bio_material$biomat.no.of.semicolons == 2))
# [1] 1 accession have 3 bio_material identifiers


multi_bio_material <- separate(data = bio_material, col = bio_material, into = c("bio_material1", "bio_material2", "bio_material3"), sep = ";", remove = FALSE)
# Warning message:
# Expected 3 pieces. Missing pieces filled with `NA` in 239 rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...]. 

multi_bio_material[is.na(multi_bio_material)] <- " "

write.csv(multi_bio_material, file = "D:/.../mulitple_identifiers_analysisA/multi_bio_material.csv", row.names = F)


### Check culture_collection column for the presence/absence of ";".

culture_collection <- source_df %>% filter(str_detect(df$culture_collection, ";"))

length(culture_collection$accession)
# [1] 592 accessions have more than one culture_collection identifiers


### Separate the identifiers in the culture_collection column 

# Count the number of ';'s in each string of the column 'culture_collection' and create a new column
culture_collection$culture.no.of.semicolons <- str_count(culture_collection$culture_collection, ";")

length(which(culture_collection$culture.no.of.semicolons == 1))
# [1] 519 accessions have 2 culture_collection identifiers

length(which(culture_collection$culture.no.of.semicolons == 2))
# [1] 58 accessions have 3 culture_collection identifiers

length(which(culture_collection$culture.no.of.semicolons == 3))
# [1] 9 accessions have 4 culture_collection identifiers

length(which(culture_collection$culture.no.of.semicolons == 4))
# [1] 5 accessions have 5 culture_collection identifiers
...
length(which(culture_collection$culture.no.of.semicolons == 9))
# [1] 1 accessions has 10 culture_collection identifiers


multi_culture_collection <- separate(data = culture_collection, col = culture_collection, into = c("culture_collection1", "culture_collection2", "culture_collection3", "culture_collection4", "culture_collection5", "culture_collection6", "culture_collection7", "culture_collection8", "culture_collection9", "culture_collection10"), sep = ";", remove = FALSE)
# Warning message:
# Expected 10 pieces. Missing pieces filled with `NA` in 568 rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...]. 

multi_culture_collection[is.na(multi_culture_collection)] <- " "

write.csv(multi_culture_collection, file = "D:/.../mulitple_identifiers_analysisA/multi_culture_collection.csv", row.names = F)


### Check specimen_voucher column for the presence/absence of ";".

specimen_voucher <- source_df %>% filter(str_detect(df$specimen_voucher, ";"))

length(specimen_voucher$accession)
# [1] 35392 accessions have more than one specimen_voucher identifiers


### Separate the identifiers in the specimen_voucher column 

# Count the number of ';'s in each string of the column 'specimen_voucher' and create a new column
specimen_voucher$specimen.no.of.semicolons <- str_count(specimen_voucher$specimen_voucher, ";")

length(which(specimen_voucher$specimen.no.of.semicolons == 1))
# [1] 33834 accessions have 2 specimen_voucher identifiers

length(which(specimen_voucher$specimen.no.of.semicolons == 2))
# [1] 1380 accessions have 3 specimen_voucher identifiers

length(which(specimen_voucher$specimen.no.of.semicolons == 3))
# [1] 79 accessions have 4 specimen_voucher identifiers

length(which(specimen_voucher$specimen.no.of.semicolons == 4))
# [1] 77 accessions have 5 specimen_voucher identifiers

length(which(specimen_voucher$specimen.no.of.semicolons == 5))
# [1] 8 accessions have 6 specimen_voucher identifiers

length(which(specimen_voucher$specimen.no.of.semicolons == 6))
# [1] 12 accessions have 7 specimen_voucher identifiers

length(which(specimen_voucher$specimen.no.of.semicolons == 7))
# [1] 1 accession has 8 specimen_voucher identifiers
...
length(which(specimen_voucher$specimen.no.of.semicolons == 13))
# [1] 1 accession has 14 specimen_voucher identifiers


multi_specimen_voucher <- separate(data = specimen_voucher, col = specimen_voucher, into = c("specimen_voucher1", "specimen_voucher2", "specimen_voucher3", "specimen_voucher4", "specimen_voucher5", "specimen_voucher6", "specimen_voucher7", "specimen_voucher8", "specimen_voucher9", "specimen_voucher10", "specimen_voucher11", "specimen_voucher12", "specimen_voucher13", "specimen_voucher14"), sep = ";", remove = FALSE)
# Warning message:
# Expected 14 pieces. Missing pieces filled with `NA` in 35350 rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...]. 

multi_specimen_voucher[is.na(multi_specimen_voucher)] <- " "

write.csv(multi_specimen_voucher, file = "D:/.../mulitple_identifiers_analysisA/multi_specimen_voucher.csv", row.names = F)


### Merge bio_material, culture_collection and specimen_voucher data frames

bio_material <- bio_material[,-12]
culture_collection <- culture_collection[,-12]
specimen_voucher <- specimen_voucher[,-12]

multiple_identifiers <- rbind(bio_material, culture_collection, specimen_voucher)

# Remove duplicated rows based on the column "accession" (function distinct() from the dplyr package):
  
multiple_identifiers <- multiple_identifiers %>% distinct(accession, .keep_all = TRUE)

length(multiple_identifiers$accession)
# [1] 36224 accessions have more than one identifier in the same source qualifier column

write.csv(multiple_identifiers, file = "D:/.../mulitple_identifiers_analysisA/multiple_identifiers.csv", row.names = F)


# Create a data frame with the rows in 'source_df' that do not match those in 'multiple_identifiers'. The column "accession" 
# is considered in the 'by' argument. (function anti_join() from the dplyr package)

one_identifier <- anti_join(source_df, multiple_identifiers, by='accession')

length(one_identifier$accession)
# [1] 9,475,567 accessions have one identifier in the same source qualifier column

write.csv(one_identifier, file = "D:/.../mulitple_identifiers_analysisA/one_identifier.csv", row.names = F)


# CONCLUSIONS: 36,224 accessions had multiple identifiers in the same source qualifier field, 9,475,567 accessions had one identifier 
# in the same source qualifier column and 268,726,703 + 1,606 = 268,728,309 accessions did not have source information.


#  Analyse multi_ bio_material, culture_collection and specimen_voucher data frames separately ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd() 

library(tidyverse)

### multi_bio_material data frame
=================================

multi_bio_material <- read.csv(file = "D:/.../multi_bio_material.csv")

multi_bio_material[multi_bio_material == " "] <- NA


sum(!is.na(multi_bio_material$culture_collection))
# [1] 0, there are no accessions that have culture_collection information

sum(!is.na(multi_bio_material$specimen_voucher))
# [1] 53 accessions have multiple bio_material identifiers in each cell and specimen_voucher identifiers (one accession has two specimen_voucher id.)


# split the specimen_voucher column

str_count(test1$specimen_voucher, ":")

# [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

# all specimen_voucher identifiers have the format /specimen_voucher = specimen_id


# one accession has 2 specimen_voucher identifiers so we delete them since they are analysed in another data frame
multi_bio_material[1,12] <- NA

multi_bio_material <- multi_bio_material %>% add_column(specimen_id = NA, .after = "specimen_voucher") # add a new column 

multi_bio_material$specimen_id[is.na(multi_bio_material$specimen_id)] <- as.character(multi_bio_material$specimen_voucher[is.na(multi_bio_material$specimen_id)]) 
# replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

multi_bio_material[is.na(multi_bio_material)] <- " "

write.csv(multi_bio_material, file = "D:/.../mulitple_identifiers_analysisA/multi_bio_materialV2.csv", row.names = F)


multi_bio_material[multi_bio_material == " "] <- NA

# store values from the "bio_material" columns in a single vector
bio_material_vec <- as.vector(as.matrix(multi_bio_material[,c(8:10)]))

#remove NA values
bio_material_vec <- bio_material_vec[!is.na(bio_material_vec)]

#convert to df
bio_material_df <- data.frame(bio_material_vec)

names(bio_material_df)[1] <- "bio_material"

# Remove duplicated rows based on the column "bio_material" (function distinct() from the dplyr package):

bio_material_df <- bio_material_df %>% distinct(bio_material, .keep_all = TRUE)

length(bio_material_df$bio_material)
# [1] 97 unique bio_material identifiers

# Count the number of ':'s in each string of the column 'bio_material' and create a new column
bio_material_df$biomat.no.of.colons <- str_count(bio_material_df$bio_material, ":")

length(which(bio_material_df$biomat.no.of.colons == 0))
# [1] 97 bio_material identifiers have the format /bio_material = material_id


# CONCLUSIONS
# 240 accessions are linked to multiple bio_material identifiers. In total we have 97 unique bio_material identifiers with the format 
# /bio_material = material_id.


# Create a new data frame with bio_material identifiers for later analysis.

# add a new column
bio_material_df <- bio_material_df %>% add_column(material_id = NA, .after = "bio_material") 

# replace NA values in column "material_id" with the values in the adjacent column "bio_material"
bio_material_df$material_id[is.na(bio_material_df$material_id)] <- as.character(bio_material_df$bio_material[is.na(bio_material_df$material_id)]) 

write.csv(bio_material_df, file = "D:/.../mulitple_identifiers_analysisB/bio_material_analysis.csv", row.names = F)


### multi_culture_collection data frame
=======================================

multi_culture_collection <- read.csv(file = "D:/.../multi_culture_collection.csv")


multi_culture_collection[multi_culture_collection == " "] <- NA

test <- 
  multi_culture_collection %>% 
  filter(!(is.na(bio_material)))

length(test$accession)
# [1]0, there are no accessions that have bio_material identifiers


test1 <- 
  multi_culture_collection %>% 
  filter(!(is.na(specimen_voucher)))

length(test1$accession)
# [1] 27 accessions have multiple culture_collection identifiers in each cell and specimen_voucher identifiers

# split the specimen_voucher column

str_count(test1$specimen_voucher, ":")

# [1] 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1

# specimen_voucher identifiers have either the format /specimen_voucher = institution_code:specimen_id (10 accessions) OR /specimen_voucher = specimen_id
# (17 accessions)


multi_culture_collection <- multi_culture_collection %>% add_column(specimen_id = NA, .after = "specimen_voucher") # add a new column 

multi_culture_collection$specimen_id[is.na(multi_culture_collection$specimen_id)] <- as.character(multi_culture_collection$specimen_voucher[is.na(multi_culture_collection$specimen_id)]) 
# replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

multi_culture_collection$specimen_id = sub(".*:", "", as.character(multi_culture_collection$specimen_id)) # remove part of string before last ":"

multi_culture_collection <- multi_culture_collection %>% add_column(institution_code = NA, .after = "specimen_voucher") # add a new column

multi_culture_collection$institution_code[is.na(multi_culture_collection$institution_code)] <- as.character(multi_culture_collection$specimen_voucher[is.na(multi_culture_collection$institution_code)]) 
# replace NA values in column "institution_code" with the values in the column "specimen_voucher"

# replace values that do not contain ":" with empty cells.

multi_culture_collection <- multi_culture_collection %>%
  mutate(institution_code = case_when(
    !str_detect(institution_code, ":") ~ "",
    TRUE ~ institution_code
  )
)

multi_culture_collection$institution_code = sub("\\:[^:]*$", "", as.character(multi_culture_collection$institution_code)) # remove part of string after last ":"

multi_culture_collection[is.na(multi_culture_collection)] <- " "

write.csv(multi_culture_collection, file = "D:/.../mulitple_identifiers_analysisA/multi_culture_collectionV2.csv", row.names = F)

multi_culture_collection[multi_culture_collection == " "] <- NA

# store values from the "culture_collection" columns in a single vector
culture_collection_vec <- as.vector(as.matrix(multi_culture_collection[,c(9:18)]))

#remove NA values
culture_collection_vec <- culture_collection_vec[!is.na(culture_collection_vec)]

#convert to df
culture_collection_df <- data.frame(culture_collection_vec)

names(culture_collection_df)[1] <- "culture_collection"

# Remove duplicated rows based on the column "culture_collection" (function distinct() from the dplyr package):

culture_collection_df <- culture_collection_df %>% distinct(culture_collection, .keep_all = TRUE)

length(culture_collection_df$culture_collection)
# [1] 736 unique culture_collection identifiers

# Count the number of ':'s in each string of the column 'culture_collection' and create a new column
culture_collection_df$culture.no.of.colons <- str_count(culture_collection_df$culture_collection, ":")

length(which(culture_collection_df$culture.no.of.colons == 0))
# [1] 2 culture_collection identifiers have the format /culture_collection = culture_id

length(which(culture_collection_df$culture.no.of.colons == 1))
# [1] 734 culture_collection identifiers have the format /culture_collection = institution-code:culture_id


# CONCLUSIONS
# 592 accessions are linked to multiple culture_collection identifiers. In total we have 736 unique culture_collection identifiers with the formats 
# /culture_collection = institution-code:culture_id and /culture_collection = culture_id.


# Create a new data frame with culture_collection identifiers for later analysis.

culture_collection_df <- separate(data = culture_collection_df, col = culture_collection, into = c("institution_code", "culture_id"), sep = ":", remove = FALSE)

culture_collection_df$culture_id = coalesce(culture_collection_df$culture_id, culture_collection_df$institution_code)
# For rows where culture_id has a non NA value, the value would remain as is.

culture_collection_df[344,2] <- " "
culture_collection_df[671,2] <- " "

write.csv(culture_collection_df, file = "D:/.../mulitple_identifiers_analysisB/culture_collection_analysis.csv", row.names = F)


### multi_specimen_voucher data frame
=====================================

multi_specimen_voucher <- read.csv(file = "D:/.../multi_specimen_voucher.csv")

multi_specimen_voucher[multi_specimen_voucher == " "] <- NA

test <- 
  multi_specimen_voucher %>% 
  filter(!(is.na(bio_material)))

length(test$accession)
# [1] 221 accessions also have bio_material identifiers


test1 <- 
  multi_specimen_voucher %>% 
  filter(!(is.na(culture_collection)))

length(test1$accession)
# [1] 5 accessions also have culture_collection identifiers


# split the bio_material and culture_collection columns

str_count(test$bio_material, ":")

# [1] 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# [83] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# [165] 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

# bio_material identifiers have either the format /bio_material = institution_code:material_id (26 accessions) OR /bio_material = material_id (195 accessions)

str_count(test1$culture_collection, ":")

# [1] 1 1 1 1 1

#culture_collection identifiers have the format /culture_collection = institution_code:culture_id

# one accession has 3 bio_material identifiers so we delete them since they were analysed in a previous data frame
multi_specimen_voucher[2299,7] <- NA


multi_specimen_voucher <- multi_specimen_voucher %>% add_column(material_id = NA, .after = "bio_material") # add a new column 

multi_specimen_voucher$material_id[is.na(multi_specimen_voucher$material_id)] <- as.character(multi_specimen_voucher$bio_material[is.na(multi_specimen_voucher$material_id)]) 
# replace NA values in column "material_id" with the values in the adjacent column "bio_material"

multi_specimen_voucher$material_id = sub(".*:", "", as.character(multi_specimen_voucher$material_id)) # remove part of string before last ":"

multi_specimen_voucher <- multi_specimen_voucher %>% add_column(institution_codeMAT = NA, .after = "bio_material") # add a new column

multi_specimen_voucher$institution_codeMAT[is.na(multi_specimen_voucher$institution_codeMAT)] <- as.character(multi_specimen_voucher$bio_material[is.na(multi_specimen_voucher$institution_codeMAT)]) 
# replace NA values in column "institution_codeMAT" with the values in the column "bio_material"


# replace values that do not contain ":" with empty cells.

multi_specimen_voucher <- multi_specimen_voucher %>%
  mutate(institution_codeMAT = case_when(
    !str_detect(institution_codeMAT, ":") ~ "",
    TRUE ~ institution_codeMAT
  )
)

multi_specimen_voucher$institution_codeMAT = sub("\\:[^:]*$", "", as.character(multi_specimen_voucher$institution_codeMAT)) # remove part of string after last ":"


multi_specimen_voucher <- multi_specimen_voucher %>% add_column(culture_id = NA, .after = "culture_collection") # add a new column 

multi_specimen_voucher$culture_id[is.na(multi_specimen_voucher$culture_id)] <- as.character(multi_specimen_voucher$culture_collection[is.na(multi_specimen_voucher$culture_id)]) 
# replace NA values in column "culture_id" with the values in the adjacent column "culture_collection"

multi_specimen_voucher$culture_id = sub(".*:", "", as.character(multi_specimen_voucher$culture_id)) # remove part of string before last ":"

multi_specimen_voucher <- multi_specimen_voucher %>% add_column(institution_codeCULT = NA, .after = "culture_collection") # add a new column


multi_specimen_voucher$institution_codeCULT[is.na(multi_specimen_voucher$institution_codeCULT)] <- as.character(multi_specimen_voucher$culture_collection[is.na(multi_specimen_voucher$institution_codeCULT)]) 
# replace NA values in column "institution_codeCULT" with the values in the column "culture_collection"

multi_specimen_voucher$institution_codeCULT = sub("\\:[^:]*$", "", as.character(multi_specimen_voucher$institution_codeCULT)) # remove part of string after last ":"

multi_specimen_voucher[is.na(multi_specimen_voucher)] <- " "

write.csv(multi_specimen_voucher, file = "D:/.../mulitple_identifiers_analysisA/multi_specimen_voucherV2.csv", row.names = F)

multi_specimen_voucher[multi_specimen_voucher == " "] <- NA


# store values from the "specimen_voucher" columns in a single vector
specimen_voucher_vec <- as.vector(as.matrix(multi_specimen_voucher[,c(10:23)]))

#remove NA values
specimen_voucher_vec <- specimen_voucher_vec[!is.na(specimen_voucher_vec)]

#convert to df
specimen_voucher_df <- data.frame(specimen_voucher_vec)

names(specimen_voucher_df)[1] <- "specimen_voucher"

# Remove duplicated rows based on the column "specimen_voucher" (function distinct() from the dplyr package):

specimen_voucher_df <- specimen_voucher_df %>% distinct(specimen_voucher, .keep_all = TRUE)

length(specimen_voucher_df$specimen_voucher)
# [1] 27789 unique specimen_voucher identifiers

# Count the number of ':'s in each string of the column 'specimen_voucher' and create a new column
specimen_voucher_df$specimen.no.of.colons <- str_count(specimen_voucher_df$specimen_voucher, ":")

length(which(specimen_voucher_df$specimen.no.of.colons == 0))
# [1] 27120 specimen_voucher identifiers have the format /specimen_voucher = specimen_id

length(which(specimen_voucher_df$specimen.no.of.colons == 1))
# [1] 639 specimen_voucher identifiers have the format /specimen_voucher = institution-code:specimen_id

length(which(specimen_voucher_df$specimen.no.of.colons == 2))
# [1] 30 specimen_voucher identifiers have the format /specimen_voucher = institution-code:collection_code:specimen_id


# CONCLUSIONS
# 35392 accessions are linked to multiple specimen_voucher identifiers. In total we have 27789 unique specimen_voucher identifiers with the formats 
# /specimen_voucher = institution-code:collection_code:specimen_id, /specimen_voucher = institution-code:specimen_id and /specimen_voucher = specimen_id.


# Create a new data frame with specimen_voucher identifiers for later analysis.

specimen_voucher_df <- specimen_voucher_df %>% add_column(specimen_id = NA, .after = "specimen_voucher") # add a new column 

specimen_voucher_df$specimen_id[is.na(specimen_voucher_df$specimen_id)] <- as.character(specimen_voucher_df$specimen_voucher[is.na(specimen_voucher_df$specimen_id)]) 
# replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

specimen_voucher_df$specimen_id = sub(".*:", "", as.character(specimen_voucher_df$specimen_id)) # remove part of string before last ":"

specimen_voucher_df <- specimen_voucher_df %>% add_column(collection_code = NA, .after = "specimen_voucher") # add a new column

specimen_voucher_df$collection_code[is.na(specimen_voucher_df$collection_code)] <- as.character(specimen_voucher_df$specimen_voucher[is.na(specimen_voucher_df$collection_code)]) 
# replace NA values in column "collection_code" with the values in the column "specimen_voucher"

# replace values that do not contain "text:text:text" with empty cells.

specimen_voucher_df <- specimen_voucher_df %>%
  mutate(collection_code = case_when(
    !str_detect(collection_code, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_code
  )
)

specimen_voucher_df$collection_code = sub("^[^:]*:", "", as.character(specimen_voucher_df$collection_code)) # remove part of string before first ":"

specimen_voucher_df$collection_code = sub("\\:[^:]*$", "", as.character(specimen_voucher_df$collection_code)) # remove part of string after last ":"


specimen_voucher_df <- specimen_voucher_df %>% add_column(institution_code = NA, .after = "specimen_voucher") # add a new column

specimen_voucher_df$institution_code[is.na(specimen_voucher_df$institution_code)] <- as.character(specimen_voucher_df$specimen_voucher[is.na(specimen_voucher_df$institution_code)]) 
# replace NA values in column "institution_code" with the values in the column "specimen_voucher"

# replace values that do not contain ":" with empty cells.

specimen_voucher_df <- specimen_voucher_df %>%
  mutate(institution_code = case_when(
    !str_detect(institution_code, ":") ~ "",
    TRUE ~ institution_code
  )
)

specimen_voucher_df$institution_code = sub("\\:[^:]*", "", as.character(specimen_voucher_df$institution_code)) # remove part of string after ":"
specimen_voucher_df$institution_code = sub("\\:[^:]*", "", as.character(specimen_voucher_df$institution_code )) # remove part of string after ":"

write.csv(specimen_voucher_df, file = "D:/.../mulitple_identifiers_analysisB/specimen_voucher_analysis.csv", row.names = F)


#  Analyse the one_identifier data frame ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd() 

library(tidyverse)

one_identifier <- read.csv(file = "D:/.../one_identifier.csv")

one_identifier[one_identifier == " "] <- NA

dfA <- one_identifier %>%
  filter(!is.na(bio_material) & !is.na(culture_collection))
# 198 accessions have one bio_material AND one culture_collection identifier

dfB <- one_identifier %>%
  filter(!is.na(bio_material) & !is.na(specimen_voucher))
# 36103 accessions have one bio_material AND one specimen_voucher identifier

dfC <- one_identifier %>%
  filter(!is.na(culture_collection) & !is.na(specimen_voucher))
# 5566 accessions have one culture_collection AND one specimen_voucher identifier


dfD <- one_identifier %>%
  filter(!is.na(bio_material) & is.na(culture_collection) & is.na(specimen_voucher))

dfE <- one_identifier %>%
  filter(is.na(bio_material) & !is.na(culture_collection) & is.na(specimen_voucher))

dfF <- one_identifier %>%
  filter(is.na(bio_material) & is.na(culture_collection) & !is.na(specimen_voucher))

dfDEF <- rbind(dfD, dfE, dfF)
# 9433700 accessions have EITHER bio_material OR culture_collection OR specimen_voucher identifier

rm(one_identifier)


# CONCLUSIONS: 41,867 accessions had one identifier in >1 source qualifier fields (dfA, dfB & dfC) and 36,224 + 41,867 = 78,091 accessions had complex 
# annotation patterns, while 9,433,700 accessions had one identifier in 1 source qualifier field (dfDEF).This makes a total of 9,511,791 sequences with 
# source annotation.



# First, analyse the data frames with one identifier in >1 source qualifier fields

### data frame dfA
==================

# Count the number of ':'s in each string of the column 'bio_material' and create a new column
dfA$biomat.no.of.colons <- str_count(dfA$bio_material, ":")

length(which(dfA$biomat.no.of.colons == 0))
# [1] 183 accessions have bio_material identifiers with the format /bio_material = material_id

length(which(dfA$biomat.no.of.colons == 1))
# [1] 15 accessions have bio_material identifiers with the format /bio_material = institution-code:material_id

dfA <- dfA %>% add_column(material_id = NA, .after = "bio_material") # add a new column 

dfA$material_id[is.na(dfA$material_id)] <- as.character(dfA$bio_material[is.na(dfA$material_id)])
# replace NA values in column "material_id" with the values in the adjacent column "bio_material"

dfA$material_id = sub(".*:", "", as.character(dfA$material_id)) # remove part of string before last ":"

dfA <- dfA %>% add_column(institution_codeMAT = NA, .after = "bio_material") # add a new column

dfA$institution_codeMAT[is.na(dfA$institution_codeMAT)] <- as.character(dfA$bio_material[is.na(dfA$institution_codeMAT)])
# replace NA values in column "institution_codeMAT" with the values in the column "bio_material"

# replace values that do not contain ":" with empty cells.

dfA <- dfA %>%
  mutate(institution_codeMAT = case_when(
    !str_detect(institution_codeMAT, ":") ~ "",
    TRUE ~ institution_codeMAT
  )
)

dfA$institution_codeMAT = sub("\\:[^:]*", "", as.character(dfA$institution_codeMAT)) # remove part of string after ":"

# Count the number of ':'s in each string of the column 'culture_collection' and create a new column
dfA$culture.no.of.colons <- str_count(dfA$culture_collection, ":")

length(which(dfA$culture.no.of.colons == 0))
# [1] 0 accessions have culture_collection identifiers with the format /culture_collection = culture_id

length(which(dfA$culture.no.of.colons == 1))
# [1] 198 accessions have culture_collection identifiers with the format /culture_collection = institution-code:culture_id

dfA <- dfA %>% add_column(culture_id = NA, .after = "culture_collection") # add a new column

dfA$culture_id[is.na(dfA$culture_id)] <- as.character(dfA$culture_collection[is.na(dfA$culture_id)])
# replace NA values in column "culture_id" with the values in the adjacent column "culture_collection"

dfA$culture_id = sub(".*:", "", as.character(dfA$culture_id)) # remove part of string before last ":"

dfA <- dfA %>% add_column(institution_codeCULT = NA, .after = "culture_collection") # add a new column

dfA$institution_codeCULT[is.na(dfA$institution_codeCULT)] <- as.character(dfA$culture_collection[is.na(dfA$institution_codeCULT)])
# replace NA values in column "institution_codeCULT" with the values in the column "culture_collection"

dfA$institution_codeCULT = sub("\\:[^:]*", "", as.character(dfA$institution_codeCULT)) # remove part of string after ":"

# add new empty columns
dfA <- dfA %>% add_column(collection_codeMAT = NA, .after = "institution_codeMAT") 
dfA <- dfA %>% add_column(collection_codeCULT = NA, .after = "institution_codeCULT")
dfA <- dfA %>% add_column(institution_codeSPEC = NA, .after = "specimen_voucher")
dfA <- dfA %>% add_column(collection_codeSPEC = NA, .after = "institution_codeSPEC")
dfA <- dfA %>% add_column(specimen_id = NA, .after = "collection_codeSPEC")
dfA <- dfA %>% add_column(specimen.no.of.colons = NA, .after = "culture.no.of.colons") 


### data frame dfB
==================

# Count the number of ':'s in each string of the column 'bio_material' and create a new column
dfB$biomat.no.of.colons <- str_count(dfB$bio_material, ":")

length(which(dfB$biomat.no.of.colons == 0))
# [1] 26093 accessions have bio_material identifiers with the format /bio_material = material_id

length(which(dfB$biomat.no.of.colons == 1))
# [1] 5077 accessions have bio_material identifiers with the format /bio_material = institution-code:material_id

length(which(dfB$biomat.no.of.colons == 2))
# [1] 4933 accessions have bio_material identifiers with the format /bio_material = institution-code:collection_code:material_id

dfB <- dfB %>% add_column(material_id = NA, .after = "bio_material") # add a new column 

dfB$material_id[is.na(dfB$material_id)] <- as.character(dfB$bio_material[is.na(dfB$material_id)])
# replace NA values in column "material_id" with the values in the adjacent column "bio_material"

dfB$material_id = sub(".*:", "", as.character(dfB$material_id)) # remove part of string before last ":"

dfB <- dfB %>% add_column(collection_codeMAT = NA, .after = "bio_material") # add a new column

dfB$collection_codeMAT[is.na(dfB$collection_codeMAT)] <- as.character(dfB$bio_material[is.na(dfB$collection_codeMAT)])
# replace NA values in column "collection_codeMAT" with the values in the column "bio_material"

# replace values that do not contain "text:text:text" with empty cells.

dfB <- dfB %>%
  mutate(collection_codeMAT = case_when(
    !str_detect(collection_codeMAT, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeMAT
  )
)

dfB$collection_codeMAT = sub("^[^:]*:", "", as.character(dfB$collection_codeMAT)) # remove part of string before first ":"

dfB$collection_codeMAT = sub("\\:[^:]*$", "", as.character(dfB$collection_codeMAT)) # remove part of string after last ":"

dfB <- dfB %>% add_column(institution_codeMAT = NA, .after = "bio_material") # add a new column

dfB$institution_codeMAT[is.na(dfB$institution_codeMAT)] <- as.character(dfB$bio_material[is.na(dfB$institution_codeMAT)])
# replace NA values in column "institution_codeMAT" with the values in the column "bio_material"

# replace values that do not contain ":" with empty cells.

dfB <- dfB %>%
  mutate(institution_codeMAT = case_when(
    !str_detect(institution_codeMAT, ":") ~ "",
    TRUE ~ institution_codeMAT
  )
)

dfB$institution_codeMAT = sub("\\:[^:]*", "", as.character(dfB$institution_codeMAT)) # remove part of string after ":"
dfB$institution_codeMAT = sub("\\:[^:]*", "", as.character(dfB$institution_codeMAT)) # remove part of string after ":"


# Count the number of ':'s in each string of the column 'specimen_voucher' and create a new column
dfB$specimen.no.of.colons <- str_count(dfB$specimen_voucher, ":")

length(which(dfB$specimen.no.of.colons == 0))
# [1] 24876 accessions have specimen_voucher identifiers with the format /specimen_voucher = specimen_id

length(which(dfB$specimen.no.of.colons == 1))
# [1] 8928 accessions have specimen_voucher identifiers with the format /specimen_voucher = institution-code:specimen_id

length(which(dfB$specimen.no.of.colons == 2))
# [1] 2299 accessions have specimen_voucher identifiers with the format /specimen_voucher = institution-code:collection_code:specimen_id


dfB <- dfB %>% add_column(specimen_id = NA, .after = "specimen_voucher") # add a new column 

dfB$specimen_id[is.na(dfB$specimen_id)] <- as.character(dfB$specimen_voucher[is.na(dfB$specimen_id)]) 
# replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

dfB$specimen_id = sub(".*:", "", as.character(dfB$specimen_id)) # remove part of string before last ":"

dfB <- dfB %>% add_column(collection_codeSPEC = NA, .after = "specimen_voucher") # add a new column

dfB$collection_codeSPEC[is.na(dfB$collection_codeSPEC)] <- as.character(dfB$specimen_voucher[is.na(dfB$collection_codeSPEC)])
# replace NA values in column "collection_codeSPEC" with the values in the column "specimen_voucher"

# replace values that do not contain "text:text:text" with empty cells.

dfB <- dfB %>%
  mutate(collection_codeSPEC = case_when(
    !str_detect(collection_codeSPEC, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeSPEC
  )
)

dfB$collection_codeSPEC = sub("^[^:]*:", "", as.character(dfB$collection_codeSPEC)) # remove part of string before first ":"

dfB$collection_codeSPEC = sub("\\:[^:]*$", "", as.character(dfB$collection_codeSPEC)) # remove part of string after last ":"

dfB <- dfB %>% add_column(institution_codeSPEC = NA, .after = "specimen_voucher") # add a new column

dfB$institution_codeSPEC[is.na(dfB$institution_codeSPEC)] <- as.character(dfB$specimen_voucher[is.na(dfB$institution_codeSPEC)])
# replace NA values in column "institution_codeSPEC" with the values in the column "specimen_voucher"

# replace values that do not contain ":" with empty cells.

dfB <- dfB %>%
  mutate(institution_codeSPEC = case_when(
    !str_detect(institution_codeSPEC, ":") ~ "",
    TRUE ~ institution_codeSPEC
  )
)

dfB$institution_codeSPEC = sub("\\:[^:]*", "", as.character(dfB$institution_codeSPEC)) # remove part of string after ":"
dfB$institution_codeSPEC = sub("\\:[^:]*", "", as.character(dfB$institution_codeSPEC)) # remove part of string after ":"


# add new empty columns
dfB <- dfB %>% add_column(institution_codeCULT = NA, .after = "culture_collection")
dfB <- dfB %>% add_column(collection_codeCULT = NA, .after = "institution_codeCULT")
dfB <- dfB %>% add_column(culture_id = NA, .after = "collection_codeCULT")
dfB <- dfB %>% add_column(culture.no.of.colons = NA, .after = "biomat.no.of.colons")


### data frame dfC
==================
  
# Count the number of ':'s in each string of the column 'culture_collection' and create a new column
dfC$culture.no.of.colons <- str_count(dfC$culture_collection, ":")

length(which(dfC$culture.no.of.colons == 0))
# [1] 0 accessions have culture_collection identifiers with the format /culture_collection = culture_id

length(which(dfC$culture.no.of.colons == 1))
# [1] 5565 accessions have culture_collection identifiers with the format /culture_collection = institution-code:culture_id

length(which(dfC$culture.no.of.colons == 2))
# [1] 1 accession has culture_collection identifiers with the format /culture_collection = institution-code:collection_code:culture_id

dfC <- dfC %>% add_column(culture_id = NA, .after = "culture_collection") # add a new column 

dfC$culture_id[is.na(dfC$culture_id)] <- as.character(dfC$culture_collection[is.na(dfC$culture_id)]) 
# replace NA values in column "culture_id" with the values in the adjacent column "culture_collection"

dfC$culture_id = sub(".*:", "", as.character(dfC$culture_id)) # remove part of string before last ":"


dfC <- dfC %>% add_column(collection_codeCULT = NA, .after = "culture_collection") # add a new column

dfC$collection_codeCULT[is.na(dfC$collection_codeCULT)] <- as.character(dfC$culture_collection[is.na(dfC$collection_codeCULT)])
# replace NA values in column "collection_codeCULT" with the values in the column "culture_collection"

# replace values that do not contain "text:text:text" with empty cells.

dfC <- dfC %>%
  mutate(collection_codeCULT = case_when(
    !str_detect(collection_codeCULT, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeCULT
  )
)

dfC$collection_codeCULT = sub("^[^:]*:", "", as.character(dfC$collection_codeCULT)) # remove part of string before first ":"

dfC$collection_codeCULT = sub("\\:[^:]*$", "", as.character(dfC$collection_codeCULT)) # remove part of string after last ":"

dfC <- dfC %>% add_column(institution_codeCULT = NA, .after = "culture_collection") # add a new column

dfC$institution_codeCULT[is.na(dfC$institution_codeCULT)] <- as.character(dfC$culture_collection[is.na(dfC$institution_codeCULT)])
# replace NA values in column "institution_codeCULT" with the values in the column "culture_collection"

# replace values that do not contain ":" with empty cells.

dfC <- dfC %>%
  mutate(institution_codeCULT = case_when(
    !str_detect(institution_codeCULT, ":") ~ "",
    TRUE ~ institution_codeCULT
  )
)

dfC$institution_codeCULT = sub("\\:[^:]*", "", as.character(dfC$institution_codeCULT)) # remove part of string after ":"
dfC$institution_codeCULT = sub("\\:[^:]*", "", as.character(dfC$institution_codeCULT)) # remove part of string after ":"


# Count the number of ':'s in each string of the column 'specimen_voucher' and create a new column
dfC$specimen.no.of.colons <- str_count(dfC$specimen_voucher, ":")

length(which(dfC$specimen.no.of.colons == 0))
# [1] 2017 accessions have specimen_voucher identifiers with the format /specimen_voucher = specimen_id

length(which(dfC$specimen.no.of.colons == 1))
# [1] 3534 accessions have specimen_voucher identifiers with the format /specimen_voucher = institution-code:specimen_id

length(which(dfC$specimen.no.of.colons == 2))
# [1] 15 accessions have specimen_voucher identifiers with the format /specimen_voucher = institution-code:collection_code:specimen_id


dfC <- dfC %>% add_column(specimen_id = NA, .after = "specimen_voucher") # add a new column 

dfC$specimen_id[is.na(dfC$specimen_id)] <- as.character(dfC$specimen_voucher[is.na(dfC$specimen_id)]) 
# replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

dfC$specimen_id = sub(".*:", "", as.character(dfC$specimen_id)) # remove part of string before last ":"

dfC <- dfC %>% add_column(collection_codeSPEC = NA, .after = "specimen_voucher") # add a new column

dfC$collection_codeSPEC[is.na(dfC$collection_codeSPEC)] <- as.character(dfC$specimen_voucher[is.na(dfC$collection_codeSPEC)])
# replace NA values in column "collection_codeSPEC" with the values in the column "specimen_voucher"

# replace values that do not contain "text:text:text" with empty cells.

dfC <- dfC %>%
  mutate(collection_codeSPEC = case_when(
    !str_detect(collection_codeSPEC, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeSPEC
  )
)

dfC$collection_codeSPEC = sub("^[^:]*:", "", as.character(dfC$collection_codeSPEC)) # remove part of string before first ":"

dfC$collection_codeSPEC = sub("\\:[^:]*$", "", as.character(dfC$collection_codeSPEC)) # remove part of string after last ":"

dfC <- dfC %>% add_column(institution_codeSPEC = NA, .after = "specimen_voucher") # add a new column

dfC$institution_codeSPEC[is.na(dfC$institution_codeSPEC)] <- as.character(dfC$specimen_voucher[is.na(dfC$institution_codeSPEC)])
# replace NA values in column "institution_codeSPEC" with the values in the column "specimen_voucher"

# replace values that do not contain ":" with empty cells.

dfC <- dfC %>%
  mutate(institution_codeSPEC = case_when(
    !str_detect(institution_codeSPEC, ":") ~ "",
    TRUE ~ institution_codeSPEC
  )
)

dfC$institution_codeSPEC = sub("\\:[^:]*", "", as.character(dfC$institution_codeSPEC)) # remove part of string after ":"
dfC$institution_codeSPEC = sub("\\:[^:]*", "", as.character(dfC$institution_codeSPEC)) # remove part of string after ":"


# add new empty columns
dfC <- dfC %>% add_column(institution_codeMAT = NA, .after = "bio_material") 
dfC <- dfC %>% add_column(collection_codeMAT = NA, .after = "institution_codeMAT")
dfC <- dfC %>% add_column(material_id = NA, .after = "collection_codeMAT")
dfC <- dfC %>% add_column(biomat.no.of.colons = NA, .after = "study_accession")

dfABC <- rbind(dfA, dfB, dfC)
dfABC[is.na(dfABC)] <- " "

# Save this data frame with one identifier in >1 source qualifier columns
write.csv(dfABC, file = "D:/.../mulitple_identifiers_analysisA/one_identifier1.csv", row.names = F)


### data frame dfDEF
====================
 
rm(dfA, dfB, dfC, dfD, dfE, dfF)  

# bio_material column
=====================

# Count the number of ':'s in each string of the column 'bio_material' and create a new column
dfDEF$biomat.no.of.colons <- str_count(dfDEF$bio_material, ":")

length(which(dfDEF$biomat.no.of.colons == 0))
# [1] 248746 accessions have bio_material identifiers with the format /bio_material = material_id

length(which(dfDEF$biomat.no.of.colons == 1))
# [1] 314709 accessions have bio_material identifiers with the format /bio_material = institution-code:material_id

length(which(dfDEF$biomat.no.of.colons == 2))
# [1] 21724 accessions have bio_material identifiers with the format /bio_material = institution-code:collection_code:material_id

dfDEF <- dfDEF %>% add_column(material_id = NA, .after = "bio_material") # add a new column 

dfDEF$material_id[is.na(dfDEF$material_id)] <- as.character(dfDEF$bio_material[is.na(dfDEF$material_id)])
# replace NA values in column "material_id" with the values in the adjacent column "bio_material"

dfDEF$material_id = sub(".*:", "", as.character(dfDEF$material_id)) # remove part of string before last ":"

dfDEF <- dfDEF %>% add_column(collection_codeMAT = NA, .after = "bio_material") # add a new column

dfDEF$collection_codeMAT[is.na(dfDEF$collection_codeMAT)] <- as.character(dfDEF$bio_material[is.na(dfDEF$collection_codeMAT)])
# replace NA values in column "collection_codeMAT" with the values in the column "bio_material"

# replace values that do not contain "text:text:text" with empty cells.

dfDEF <- dfDEF %>%
  mutate(collection_codeMAT = case_when(
    !str_detect(collection_codeMAT, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeMAT
  )
)

dfDEF$collection_codeMAT = sub("^[^:]*:", "", as.character(dfDEF$collection_codeMAT)) # remove part of string before first ":"

dfDEF$collection_codeMAT = sub("\\:[^:]*$", "", as.character(dfDEF$collection_codeMAT)) # remove part of string after last ":"

dfDEF <- dfDEF %>% add_column(institution_codeMAT = NA, .after = "bio_material") # add a new column

dfDEF$institution_codeMAT[is.na(dfDEF$institution_codeMAT)] <- as.character(dfDEF$bio_material[is.na(dfDEF$institution_codeMAT)]) 
# replace NA values in column "institution_codeMAT" with the values in the column "bio_material"

# replace values that do not contain ":" with empty cells.

dfDEF <- dfDEF %>%
  mutate(institution_codeMAT = case_when(
    !str_detect(institution_codeMAT, ":") ~ "",
    TRUE ~ institution_codeMAT
  )
)

dfDEF$institution_codeMAT = sub("\\:[^:]*", "", as.character(dfDEF$institution_codeMAT)) # remove part of string after ":"
dfDEF$institution_codeMAT = sub("\\:[^:]*", "", as.character(dfDEF$institution_codeMAT)) # remove part of string after ":"


# culture_collection column
===========================

# Count the number of ':'s in each string of the column 'culture_collection' and create a new column
dfDEF$culture.no.of.colons <- str_count(dfDEF$culture_collection, ":")

length(which(dfDEF$culture.no.of.colons == 0))
# [1] 0 accessions have culture_collection identifiers with the format /culture_collection = culture_id

length(which(dfDEF$culture.no.of.colons == 1))
# [1] 766308 accessions have culture_collection identifiers with the format /culture_collection = institution-code:culture_id

length(which(dfDEF$culture.no.of.colons == 2))
# [1] 2625 accessions have culture_collection identifiers with the format /culture_collection = institution-code:collection_code:culture_id

dfDEF <- dfDEF %>% add_column(culture_id = NA, .after = "culture_collection") # add a new column 

dfDEF$culture_id[is.na(dfDEF$culture_id)] <- as.character(dfDEF$culture_collection[is.na(dfDEF$culture_id)]) 
# replace NA values in column "culture_id" with the values in the adjacent column "culture_collection"

dfDEF$culture_id = sub(".*:", "", as.character(dfDEF$culture_id)) # remove part of string before last ":"

dfDEF <- dfDEF %>% add_column(collection_codeCULT = NA, .after = "culture_collection") # add a new column

dfDEF$collection_codeCULT[is.na(dfDEF$collection_codeCULT)] <- as.character(dfDEF$culture_collection[is.na(dfDEF$collection_codeCULT)])
# replace NA values in column "collection_codeCULT" with the values in the column "culture_collection"

# replace values that do not contain "text:text:text" with empty cells.

dfDEF <- dfDEF %>%
  mutate(collection_codeCULT = case_when(
    !str_detect(collection_codeCULT, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeCULT
  )
)

dfDEF$collection_codeCULT = sub("^[^:]*:", "", as.character(dfDEF$collection_codeCULT)) # remove part of string before first ":"

dfDEF$collection_codeCULT = sub("\\:[^:]*$", "", as.character(dfDEF$collection_codeCULT)) # remove part of string after last ":"

dfDEF <- dfDEF %>% add_column(institution_codeCULT = NA, .after = "culture_collection") # add a new column

dfDEF$institution_codeCULT[is.na(dfDEF$institution_codeCULT)] <- as.character(dfDEF$culture_collection[is.na(dfDEF$institution_codeCULT)])
# replace NA values in column "institution_codeCULT" with the values in the column "culture_collection"

# replace values that do not contain ":" with empty cells.

dfDEF <- dfDEF %>%
  mutate(institution_codeCULT = case_when(
    !str_detect(institution_codeCULT, ":") ~ "",
    TRUE ~ institution_codeCULT
  )
)

dfDEF$institution_codeCULT = sub("\\:[^:]*", "", as.character(dfDEF$institution_codeCULT)) # remove part of string after ":"
dfDEF$institution_codeCULT = sub("\\:[^:]*", "", as.character(dfDEF$institution_codeCULT)) # remove part of string after ":"


# specimen_voucher column
=========================
  
# Count the number of ':'s in each string of the column 'specimen_voucher' and create a new column
dfDEF$specimen.no.of.colons <- str_count(dfDEF$specimen_voucher, ":")

length(which(dfDEF$specimen.no.of.colons == 0))
# [1] 6370570 accessions have specimen_voucher identifiers with the format /specimen_voucher = specimen_id

length(which(dfDEF$specimen.no.of.colons == 1))
# [1] 1267608 accessions have specimen_voucher identifiers with the format /specimen_voucher = institution-code:specimen_id

length(which(dfDEF$specimen.no.of.colons == 2))
# [1] 441409 accessions have specimen_voucher identifiers with the format /specimen_voucher = institution-code:collection_code:specimen_id

dfDEF <- dfDEF %>% add_column(specimen_id = NA, .after = "specimen_voucher") # add a new column 

dfDEF$specimen_id[is.na(dfDEF$specimen_id)] <- as.character(dfDEF$specimen_voucher[is.na(dfDEF$specimen_id)]) 
# replace NA values in column "specimen_id" with the values in the adjacent column "specimen_voucher"

dfDEF$specimen_id = sub(".*:", "", as.character(dfDEF$specimen_id)) # remove part of string before last ":"

dfDEF <- dfDEF %>% add_column(collection_codeSPEC = NA, .after = "specimen_voucher") # add a new column

dfDEF$collection_codeSPEC[is.na(dfDEF$collection_codeSPEC)] <- as.character(dfDEF$specimen_voucher[is.na(dfDEF$collection_codeSPEC)])
# replace NA values in column "collection_codeSPEC" with the values in the column "specimen_voucher"

# replace values that do not contain "text:text:text" with empty cells.

dfDEF <- dfDEF %>%
  mutate(collection_codeSPEC = case_when(
    !str_detect(collection_codeSPEC, ".*:(.*)\\:.*") ~ "",
    TRUE ~ collection_codeSPEC
  )
)

dfDEF$collection_codeSPEC = sub("^[^:]*:", "", as.character(dfDEF$collection_codeSPEC)) # remove part of string before first ":"

dfDEF$collection_codeSPEC = sub("\\:[^:]*$", "", as.character(dfDEF$collection_codeSPEC)) # remove part of string after last ":"

dfDEF <- dfDEF %>% add_column(institution_codeSPEC = NA, .after = "specimen_voucher") # add a new column

dfDEF$institution_codeSPEC[is.na(dfDEF$institution_codeSPEC)] <- as.character(dfDEF$specimen_voucher[is.na(dfDEF$institution_codeSPEC)])
# replace NA values in column "institution_codeSPEC" with the values in the column "specimen_voucher"

# replace values that do not contain ":" with empty cells.

dfDEF <- dfDEF %>%
  mutate(institution_codeSPEC = case_when(
    !str_detect(institution_codeSPEC, ":") ~ "",
    TRUE ~ institution_codeSPEC
  )
)

dfDEF$institution_codeSPEC = sub("\\:[^:]*", "", as.character(dfDEF$institution_codeSPEC)) # remove part of string after ":"
dfDEF$institution_codeSPEC = sub("\\:[^:]*", "", as.character(dfDEF$institution_codeSPEC)) # remove part of string after ":"

dfDEF[is.na(dfDEF)] <- " "

# Save this data frame with one identifier in one of the source qualifier columns
write.csv(dfDEF, file = "D:/.../mulitple_identifiers_analysisA/one_identifier2.csv", row.names = F)


#  Detect time trends in the use of source identifiers during the registration and publication of nucleotide sequences ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(tidyverse)

# Read the data frame that has one identifier in one of the source qualifier columns

source_file <- read.csv(file = "D:/.../one_identifier2.csv")


# Split the column "first_public" into three new columns.

source_analysis <- separate(data = source_file, col = first_public, into = c("first_publicYear", "first_publicMonth", "first_publicDay"), sep = "-", remove = FALSE)
rm(source_file)

source_analysis$biomat.no.of.colons[source_analysis$biomat.no.of.colons==0] <- "catalogNumber"
source_analysis$biomat.no.of.colons[source_analysis$biomat.no.of.colons==1] <- "DwCD"
source_analysis$biomat.no.of.colons[source_analysis$biomat.no.of.colons==2] <- "DwCT"

source_analysis$culture.no.of.colons[source_analysis$culture.no.of.colons==0] <- "catalogNumber"
source_analysis$culture.no.of.colons[source_analysis$culture.no.of.colons==1] <- "DwCD"
source_analysis$culture.no.of.colons[source_analysis$culture.no.of.colons==2] <- "DwCT"

source_analysis$specimen.no.of.colons[source_analysis$specimen.no.of.colons==0] <- "catalogNumber"
source_analysis$specimen.no.of.colons[source_analysis$specimen.no.of.colons==1] <- "DwCD"
source_analysis$specimen.no.of.colons[source_analysis$specimen.no.of.colons==2] <- "DwCT"

names(source_analysis)[24] <- "identifier_formatMAT"
names(source_analysis)[25] <- "identifier_formatCULT"
names(source_analysis)[26] <- "identifier_formatSPEC"

source_analysis <- source_analysis %>% select("first_public", "first_publicYear", "identifier_formatMAT", "identifier_formatCULT", "identifier_formatSPEC")


# The column "first_public" has the YYYY-MM-DD timestamp format. We need to chronologically sort the dates with precision down to the level of day.

source_analysis <- source_analysis %>% arrange(first_public)

source_analysis[is.na(source_analysis)] <- ""

write.csv(source_analysis, file = "D:/.../dates_identifiers_analysis/dates_identifiers.csv", row.names = F)


# Create a data frame with the number of rows (records) per year.
year_count <- source_analysis %>% 
  count(first_publicYear)

# add new columns
year_count[ ,c("material_id","DwCDmat","DwCTmat", "culture_id", "DwCDcult", "DwCTcult", "specimen_id", "DwCDspec", "DwCTspec")] <- NA


# Count the number of records per year that have one, two and three parts of the DwC triplet.

df_biomaterial_catalogNumber <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(material_id = sum(str_count(identifier_formatMAT,  "catalogNumber")))

year_count$material_id[is.na(year_count$material_id)] <- as.character(df_biomaterial_catalogNumber$material_id[is.na(year_count$material_id)])

df_biomaterial_DwCD <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(DwCDmat = sum(str_count(identifier_formatMAT,  "DwCD")))

year_count$DwCDmat[is.na(year_count$DwCDmat)] <- as.character(df_biomaterial_DwCD$DwCDmat[is.na(year_count$DwCDmat)])

df_biomaterial_DwCT <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(DwCTmat = sum(str_count(identifier_formatMAT,  "DwCT")))

year_count$DwCTmat[is.na(year_count$DwCTmat)] <- as.character(df_biomaterial_DwCT$DwCTmat[is.na(year_count$DwCTmat)])

df_culture_catalogNumber <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(culture_id = sum(str_count(identifier_formatCULT,  "catalogNumber")))

year_count$culture_id[is.na(year_count$culture_id)] <- as.character(df_culture_catalogNumber$culture_id[is.na(year_count$culture_id)])

df_culture_DwCD <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(DwCDcult = sum(str_count(identifier_formatCULT,  "DwCD")))

year_count$DwCDcult[is.na(year_count$DwCDcult)] <- as.character(df_culture_DwCD$DwCDcult[is.na(year_count$DwCDcult)])

df_culture_DwCT <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(DwCTcult = sum(str_count(identifier_formatCULT,  "DwCT")))

year_count$DwCTcult[is.na(year_count$DwCTcult)] <- as.character(df_culture_DwCT$DwCTcult[is.na(year_count$DwCTcult)])

df_specimen_catalogNumber <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(specimen_id = sum(str_count(identifier_formatSPEC,  "catalogNumber")))

year_count$specimen_id[is.na(year_count$specimen_id)] <- as.character(df_specimen_catalogNumber$specimen_id[is.na(year_count$specimen_id)])

df_specimen_DwCD <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(DwCDspec = sum(str_count(identifier_formatSPEC,  "DwCD")))

year_count$DwCDspec[is.na(year_count$DwCDspec)] <- as.character(df_specimen_DwCD$DwCDspec[is.na(year_count$DwCDspec)])

df_specimen_DwCT <- source_analysis %>%
  group_by(first_publicYear) %>%
  summarize(DwCTspec = sum(str_count(identifier_formatSPEC,  "DwCT")))

year_count$DwCTspec[is.na(year_count$DwCTspec)] <- as.character(df_specimen_DwCT$DwCTspec[is.na(year_count$DwCTspec)])

lapply(year_count, class)

# $first_publicYear    
# [1] "character"

# $material_id
# [1] "character"

# $DwCDmat
# [1] "character"

# $DwCTmat
# [1] "character"

# $culture_id
# [1] "character"

# $DwCDcult
# [1] "character"

# $DwCTcult
# [1] "character"

# $specimen_id
# [1] "character"

# $DwCDspec
# [1] "character"

# $DwCTspec
# [1] "character"

# Convert all columns to numeric, ensuring that "year_count" stays data frame.

year_count[] <- sapply(year_count, as.numeric)

lapply(year_count, class)

# $first_publicYear
# [1] "numeric"

# $n
# [1] "numeric"

# $material_id
# [1] "numeric"

# $DwCDmat
# [1] "numeric"

# $DwCTmat
# [1] "numeric"

# $culture_id
# [1] "numeric"

# $DwCDcult
# [1] "numeric"

# $DwCTcult
# [1] "numeric"

# $specimen_id
# [1] "numeric"

# $DwCDspec
# [1] "numeric"

# $DwCTspec
# [1] "numeric"

# Create three new columns "n_catalogNumber", "n_DwCD" and "n_DwCT" by adding the numbers of records with a specific identifier format. 

year_count <- year_count %>%
  mutate(n_catalogNumber = select(., material_id, culture_id, specimen_id) %>% rowSums(na.rm = TRUE))

year_count <- year_count %>%
  mutate(n_DwCD = select(., DwCDmat, DwCDcult, DwCDspec) %>% rowSums(na.rm = TRUE))

year_count <- year_count %>%
  mutate(n_DwCT = select(., DwCTmat, DwCTcult, DwCTspec) %>% rowSums(na.rm = TRUE))


write.csv(year_count, file = "D:/.../dates_identifiers_analysis/year_countIDENT.csv", row.names = F)


# Read the data frames that do not have any identifiers in either of the source qualifier columns (create two folders, one for sequences extracted from model
# organisms, humans, other organisms containing the terms we used, and one with the remaining sequences)

patterns_analysis_files <- list.files(path = "D:/.../patterns_analysis", pattern = ".csv", full.names = TRUE)


for (i in 1:length(patterns_analysis_files)){
  # load the csv that corresponds to the i element in the list of files
  patterns_df <- read.csv(patterns_analysis_files[[i]])
  patterns_analysis <- separate(data = patterns_df, col = first_public, into = c("first_publicYear", "first_publicMonth", "first_publicDay"), sep = "-", remove = FALSE)
  patterns_analysis <- patterns_analysis %>% add_column(source_material_identifier = "", .after = "first_public") # add a new column
  patterns_analysis <- patterns_analysis %>% select("first_public", "first_publicYear", "source_material_identifier")
  patterns_analysis <- patterns_analysis %>% arrange(first_public)
  rm(patterns_df)
  write.csv(patterns_analysis, file = paste0("D:/.../dates_identifiers_analysis/dates_noidentifiers/patterns/","patterns_", i,".csv"), row.names = F)
}


nopatterns_analysis_files <- list.files(path = "D:/.../nopatterns_analysis", pattern = ".csv", full.names = TRUE)

for (i in 1:length(nopatterns_analysis_files)){
  # load the csv that corresponds to the i element in the list of files
  nopatterns_df <- read.csv(nopatterns_analysis_files[[i]])
  nopatterns_analysis <- separate(data = nopatterns_df, col = first_public, into = c("first_publicYear", "first_publicMonth", "first_publicDay"), sep = "-", remove = FALSE)
  nopatterns_analysis <- nopatterns_analysis %>% add_column(source_material_identifier = "", .after = "first_public") # add a new column
  nopatterns_analysis <- nopatterns_analysis %>% select("first_public", "first_publicYear", "source_material_identifier")
  nopatterns_analysis <- nopatterns_analysis %>% arrange(first_public)
  rm(nopatterns_df)
  write.csv(nopatterns_analysis, file = paste0("D:/.../dates_identifiers_analysis/dates_noidentifiers/nopatterns/","nopatterns_", i,".csv"), row.names = F)
}


# Create again two folders, where the files will contain the total number of sequences first public in each year

file.list1 <- list.files(path = "D:/.../dates_noidentifiers/patterns", pattern = ".csv", full.names = TRUE)

for (i in 1:length(file.list1)){
  # load the csv that corresponds to the i element in the list of files
  dates_nosource_patterns <- read.csv(file.list1[[i]])
  #count the number of sequences published in each year
  year_count <- dates_nosource_patterns %>% count(first_publicYear) 
  rm(df1)
  write.csv(year_count, file = paste0("D:/.../dates_identifiers_analysis/year_countNOIDENT/patterns/","patterns_", i,".csv"), row.names = F)
}


year_count1 <- list.files(path = "D:/.../year_countNOIDENT/patterns", pattern = ".csv", full.names = TRUE)

# Summarise all files in the folder "patterns" into one csv file

# read files into a list
tables1 <- lapply(year_count1, read.csv, header = TRUE)

year_count.df1 <- do.call(rbind , tables1)

year_count.df1[] <- sapply(year_count.df1, as.numeric)

# Create a data frame with the "first_publicYear" values and "total_n" as the sum of records registered at that year.
year_count.df1 <- year_count.df1 %>%
  group_by(first_publicYear) %>% 
  summarise(total_n = sum(n))

write.csv(year_count.df1, file = "D:/.../year_countNOIDENT/patterns/year_countSUM.csv", row.names = F)


file.list2 <- list.files(path = "D:/.../dates_noidentifiers/nopatterns", pattern = ".csv", full.names = TRUE)

for (i in 1:length(file.list2)){
  # load the csv that corresponds to the i element in the list of files
  df2 <- read.csv(file.list2[[i]])
  #count the number of sequences published in each year
  year_count <- df2 %>% count(first_publicYear) 
  rm(df2)
  write.csv(year_count, file = paste0("D:/.../dates_identifiers_analysis/year_countNOIDENT/nopatterns/","nopatterns_", i,".csv"), row.names = F)
}

year_count2 <- list.files(path = "D:/.../year_countNOIDENT/nopatterns", pattern = ".csv", full.names = TRUE)

# Summarise all files in the folder "nopatterns" into one csv file

# read files into a list
tables2 <- lapply(year_count2, read.csv, header = TRUE)

year_count.df2 <- do.call(rbind , tables2)

year_count.df2[] <- sapply(year_count.df2, as.numeric)

# Create a data frame with the "first_publicYear" values and "total_n" as the sum of records registered at that year.
year_count.df2 <- year_count.df2 %>%
  group_by(first_publicYear) %>% 
  summarise(total_n = sum(n))

write.csv(year_count.df2, file = "D:/.../year_countNOIDENT/nopatterns/year_countSUM.csv", row.names = F)

# The .csv files year_countIDENT and year_count summaries of the sequences lacking source annotation were put together, creating a new .csv file, year_countTOT.


# Calculate 3-year rolling averages and standard deviations.

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(tidyverse)
library(zoo)
library(roll)
library(ggplot2)
library(scales)

loadfonts(); windowsFonts()

# $serif
# [1] "TT Times New Roman"

# $sans
# [1] "TT Arial"

# $mono
# [1] "TT Courier New"

# $Times
# [1] "Helvetica"


# Rolling (or moving) averages is a type of smoothing method that averages values over periods, thereby generating a series of averages in order to 
# reduce the noise and uncover patterns/trends in a time series dataset. The values averaged come from a window of consecutive time periods 
# (for example, every 3 years). By varying the window (the number of observations included in the rolling calculation), we can vary the 
# sensitivity of the window calculation. 

year_count_analysis <- read.csv(file = "D:/.../dates_identifiers_analysis/year_countTOT.csv", stringsAsFactors = F)


# n_ident = number of accessions/records associated with source identifiers

source <- select(year_count_analysis, first_publicYear, n_ident)
names(source) <- c("year", "count")

source$RA3_source = rollmean(source$count, k = 3, fill = NA)
source$SD3_source = roll_sd(source$count, width = 3)


# n_catalogNumber = number of accessions/records associated only with catalogNumbers

catalogNumber <- select(year_count_analysis, first_publicYear, n_catalogNumber)
names(catalogNumber) <- c("year", "count")

catalogNumber$RA3_catalogNumber = rollmean(catalogNumber$count, k = 3, fill = NA)
catalogNumber$SD3_catalogNumber = roll_sd(catalogNumber$count, width = 3)


# n_DwCD = number of accessions/records associated with institution_code:catalogNumber

DwCD <- select(year_count_analysis, first_publicYear, n_DwCD)
names(DwCD) <- c("year", "count")

DwCD$RA3_DwCD = rollmean(DwCD$count, k = 3, fill = NA)
DwCD$SD3_DwCD = roll_sd(DwCD$count, width = 3)


# n_DwCT = number of accessions/records associated with institution_code:collection_code:catalogNumber

DwCT <- select(year_count_analysis, first_publicYear, n_DwCT)
names(DwCT) <- c("year", "count")

DwCT$RA3_DwCT = rollmean(DwCT$count, k = 3, fill = NA)
DwCT$SD3_DwCT = roll_sd(DwCT$count, width = 3)


# n_noident_patterns = number of accessions/records not associated with source identifiers extracted from model organisms, humans etc.

noSource_patterns <- select(year_count_analysis, first_publicYear, n_noident_patterns)
names(noSource_patterns) <- c("year", "count")

noSource_patterns$RA3_noSource_patterns = rollmean(noSource_patterns$count, k = 3, fill = NA)
noSource_patterns$SD3_noSource_patterns = roll_sd(noSource_patterns$count, width = 3)


# n_noident_nopatterns = number of accessions/records not associated with source identifiers but not extracted from model organisms, humans etc.

noSource_nopatterns <- select(year_count_analysis, first_publicYear, n_noident_nopatterns)
names(noSource_nopatterns) <- c("year", "count")

noSource_nopatterns$RA3_noSource_nopatterns = rollmean(noSource_nopatterns$count, k = 3, fill = NA)
noSource_nopatterns$SD3_noSource_nopatterns = roll_sd(noSource_nopatterns$count, width = 3)


# n_noidentSUM = total number of accessions/records not associated with source identifiers

noSourceSUM <- select(year_count_analysis, first_publicYear, n_noidentSUM)
names(noSourceSUM) <- c("year", "count")

noSourceSUM$RA3_noSourceSUM = rollmean(noSourceSUM$count, k = 3, fill = NA)
noSourceSUM$SD3_noSourceSUM = roll_sd(noSourceSUM$count, width = 3)


year_countTOT_ROLL <- cbind(source, catalogNumber, DwCD, DwCT, noSource_patterns, noSource_nopatterns, noSourceSUM)
year_countTOT_ROLL <- year_countTOT_ROLL[, -c(5, 9, 13, 17, 21, 25)]


names(year_countTOT_ROLL)[2] <- "source"
names(year_countTOT_ROLL)[5] <- "catalogNumber"
names(year_countTOT_ROLL)[8] <- "DwCD"
names(year_countTOT_ROLL)[11] <- "DwCT"
names(year_countTOT_ROLL)[14] <- "noSource_patterns"
names(year_countTOT_ROLL)[17] <- "noSource_nopatterns"
names(year_countTOT_ROLL)[20] <- "noSourceSUM"

write.csv(year_countTOT_ROLL, file = "D:/.../dates_identifiers_analysis/year_countTOT_ROLL.csv", row.names = F)


# Plot 3 year rolling averages and standard deviations

# Subset data frame in order to exclude the years 1982-1989 and 2021 in the plot (too few records were registered at that period)

year_countTOT_ROLL <- year_countTOT_ROLL[9:39, ]


# Nucleotide sequences with source annotation, sequences extracted from model organisms, humans etc. that lack source annotation and the remaining sequences lacking
# source annotation


p1 <- ggplot(year_countTOT_ROLL, aes(x = year))
p1 <- p1 + geom_point(aes(y = source, color = "with source annotation"), alpha = 1, size = 2, group = 1)
p1 <- p1 + geom_point(aes(y = noSource_patterns, color = "lacking source annotation, model org., humans etc."), alpha = 1, size = 2, group = 1)
p1 <- p1 + geom_point(aes(y = noSource_nopatterns, color = "lacking source annotation, rest"), alpha = 1, size = 2, group = 1)
p1 <- p1 + geom_line(aes(y = RA3_source, color = "with source annotation"), size = 1.2, group = 1)
p1 <- p1 + geom_line(aes(y = RA3_noSource_patterns, color = "lacking source annotation, model org., humans etc."), size = 1.2, group = 1)
p1 <- p1 + geom_line(aes(y = RA3_noSource_nopatterns, color = "lacking source annotation, rest"), size = 1.2, group = 1)
p1 <- p1 + geom_ribbon(aes(ymin = RA3_source - SD3_source/2, ymax = RA3_source + SD3_source/2), fill = "lightgray", color = "lightgray", alpha = 0.5)
p1 <- p1 + geom_ribbon(aes(ymin = RA3_noSource_patterns - SD3_noSource_patterns/2, ymax = RA3_noSource_patterns + SD3_noSource_patterns/2), fill = "lightgray", color = "lightgray", alpha = 0.5)
p1 <- p1 + geom_ribbon(aes(ymin = RA3_noSource_nopatterns - SD3_noSource_nopatterns/2, ymax = RA3_noSource_nopatterns + SD3_noSource_nopatterns/2), fill = "lightgray", color = "lightgray", alpha = 0.5)
# Change the name of each axis label
p1 <- p1 + labs(x = "Year when made public", y = "Number of sequences")
p1 <- p1 + scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
p1 <- p1 + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-7))
p1 <- p1 + theme_minimal()
p1 <- p1 + theme(legend.position = "top")
p1 <- p1 + labs(title = "3-year Rolling Average with 95% Confidence Interval Bands (+/-2 Standard Deviations)")
p1 <- p1 + labs(color = "")  
# Change the font size of the title, legend, axis text and title
p1 <- p1 + theme(plot.title = element_text(size = 30, face = "bold", family = "Times"),
                 legend.text = element_text(size = 30, family = "Times"),
                 legend.direction = "vertical",
                 legend.box = "vertical",
                 legend.position = "top",
                 axis.text.x = element_text(size = 30, family = "Times"),
                 axis.text.y = element_text(size = 30, family = "Times"),
                 axis.title.x = element_text(size = 30, vjust = -0.5, family = "Times"),
                 axis.title.y = element_text(size = 30, vjust = 2, family = "Times"))
p1


# Plot cumulative sum data

year_countTOT_ROLL <-  read.csv(file = "D:/.../year_countTOT_ROLL.csv", stringsAsFactors = F)

# Calculate and plot cumulative sums. Cumulative sums or running totals, are used to display the total sum of data as it grows with time.

# Calculate the cumulative sum of the column "source" and add it as a new column
year_countTOT_ROLL$source_cumsum <- cumsum(year_countTOT_ROLL[, c('source')])

# Calculate the cumulative sum of the column "noSourceSUM" and add it as a new column
year_countTOT_ROLL$noSourceSUM_cumsum <- cumsum(year_countTOT_ROLL[, c('noSourceSUM')])

# Calculate the cumulative sum of the column "catalogNumber" and add it as a new column
year_countTOT_ROLL$catalogNumber_cumsum <- cumsum(year_countTOT_ROLL[, c('catalogNumber')])

# Calculate the cumulative sum of the column "DwCD" and add it as a new column
year_countTOT_ROLL$DwCD_cumsum <- cumsum(year_countTOT_ROLL[, c('DwCD')])

# Calculate the cumulative sum of the column "DwCT" and add it as a new column
year_countTOT_ROLL$DwCT_cumsum <- cumsum(year_countTOT_ROLL[, c('DwCT')])

# Calculate the cumulative sum of the column "noSource_patterns" and add it as a new column
year_countTOT_ROLL$noSource_patterns_cumsum <- cumsum(year_countTOT_ROLL[, c('noSource_patterns')])

# Calculate the cumulative sum of the column "noSource_nopatterns" and add it as a new column
year_countTOT_ROLL$noSource_nopatterns_cumsum <- cumsum(year_countTOT_ROLL[, c('noSource_nopatterns')])

write.csv(year_countTOT_ROLL, file = "D:/.../year_countTOT_ROLL.csv", row.names = F) # Supplementary File 2


# Subset data frame in order to exclude the years 1982-1999 and 2021 in the plot (cumulative sum lines are close to 0)

year_countTOT_ROLL <- year_countTOT_ROLL[19:39, ]


options(scipen=999)

p2 <- ggplot(year_countTOT_ROLL, aes(x = year))
p2 <- p2 + geom_line(aes(y = catalogNumber_cumsum, color = "catalogNumber"), size = 1.2, group = 1)
p2 <- p2 + geom_line(aes(y = DwCD_cumsum, color = "institutionCode:catalogNumber"), size = 1.2, group = 1)
p2 <- p2 + geom_line(aes(y = DwCT_cumsum, color = "DwCT"), size = 1.2, group = 1)
p2 <- p2 + scale_color_manual(values = c("catalogNumber"="#663399", "institutionCode:catalogNumber"="#FF9933", "DwCT"="#339999"))
p2 <- p2 +  theme_minimal()
# Change the labels of the axes
p2 <- p2 + labs(x = "Year when made public", y = "Cumulative sums")
p2 <- p2 + scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 5))
p2 <- p2 + theme(legend.position = "top")
p2 <- p2 + labs(title = "Cumulative sum chart")
p2 <- p2 + labs(color="")
# Change the font size of the title, legend, axis text and title
p2 <- p2 + theme(plot.title = element_text(size = 30, face = "bold", family = "Times"),
                 legend.text = element_text(size = 30, family = "Times"),
                 axis.text.x = element_text(size = 30, family = "Times"),
                 axis.text.y = element_text(size = 30, family = "Times"),
                 axis.title.x = element_text(size = 30, vjust = -0.5, family = "Times"),
                 axis.title.y = element_text(size = 30, vjust = 2, family = "Times"))
p2 <- p2 + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, accuracy = 1))
p2


#  Search institution codes against ROR API using /organizations?query= (https://github.com/ror-community/ror-api) ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(httr)
library(jsonlite)
library(tidyverse)

# We will extract the institution codes from all sequences with source annotation.


one_identifier1 <- read.csv(file = "D:/.../one_identifier1.csv")

one_identifier1$institution_codeMAT[one_identifier1$institution_codeMAT==" "] <- NA
one_identifier1$institution_codeMAT[one_identifier1$institution_codeMAT==""] <- NA

one_identifier1$institution_codeCULT[one_identifier1$institution_codeCULT==" "] <- NA
one_identifier1$institution_codeCULT[one_identifier1$institution_codeCULT==""] <- NA

one_identifier1$institution_codeSPEC[one_identifier1$institution_codeSPEC==" "] <- NA
one_identifier1$institution_codeSPEC[one_identifier1$institution_codeSPEC==""] <- NA

vec1 <- c(one_identifier1$institution_codeMAT, one_identifier1$institution_codeCULT, one_identifier1$institution_codeSPEC)
vec1 <- na.omit(vec1)
vec1 <- unique(vec1)
length(vec1)
# [1] 271 unique institution codes



one_identifier2 <- read.csv(file = "D:/.../one_identifier2.csv")

one_identifier2$institution_codeMAT[one_identifier2$institution_codeMAT==" "] <- NA
one_identifier2$institution_codeMAT[one_identifier2$institution_codeMAT==""] <- NA

one_identifier2$institution_codeCULT[one_identifier2$institution_codeCULT==" "] <- NA
one_identifier2$institution_codeCULT[one_identifier2$institution_codeCULT==""] <- NA

one_identifier2$institution_codeSPEC[one_identifier2$institution_codeSPEC==" "] <- NA
one_identifier2$institution_codeSPEC[one_identifier2$institution_codeSPEC==""] <- NA

vec2 <- c(one_identifier2$institution_codeMAT, one_identifier2$institution_codeCULT, one_identifier2$institution_codeSPEC)
vec2 <- na.omit(vec2)
vec2 <- unique(vec2)
length(vec2)
# [1] 2946 unique institution codes



specimen_voucher_analysis <- read.csv(file = "D:/.../specimen_voucher_analysis.csv")

specimen_voucher_analysis$institution_code[specimen_voucher_analysis$institution_code==" "] <- NA
specimen_voucher_analysis$institution_code[specimen_voucher_analysis$institution_code==""] <- NA
vec3 <- specimen_voucher_analysis$institution_code
vec3 <- na.omit(vec3)
vec3 <- unique(vec3)
length(vec3)
# [1] 87 unique institution codes



culture_collection_analysis <- read.csv(file = "D:/.../culture_collection_analysis.csv")

culture_collection_analysis$institution_code[culture_collection_analysis$institution_code==" "] <- NA
vec4 <- culture_collection_analysis$institution_code
vec4 <- na.omit(vec4)
vec4 <- unique(vec4)
length(vec4)
# [1] 50 unique institution codes



multi_culture_collectionV2 <- read.csv(file = "D:/.../multi_culture_collectionV2.csv")

multi_culture_collectionV2$institution_code[multi_culture_collectionV2$institution_code==" "] <- NA
multi_culture_collectionV2$institution_code[multi_culture_collectionV2$institution_code==""] <- NA
vec5 <- multi_culture_collectionV2$institution_code
vec5 <- na.omit(vec5)
vec5 <- unique(vec5)
length(vec5)
# [1] 3 unique institution codes



multi_specimen_voucherV2 <- read.csv(file = "D:/.../multi_specimen_voucherV2.csv")

multi_specimen_voucherV2$institution_codeMAT[multi_specimen_voucherV2$institution_codeMAT==" "] <- NA
multi_specimen_voucherV2$institution_codeMAT[multi_specimen_voucherV2$institution_codeMAT==""] <- NA
multi_specimen_voucherV2$institution_codeCULT[multi_specimen_voucherV2$institution_codeCULT==" "] <- NA
multi_specimen_voucherV2$institution_codeCULT[multi_specimen_voucherV2$institution_codeCULT==""] <- NA

vec6 <- c(multi_specimen_voucherV2$institution_codeMAT, multi_specimen_voucherV2$institution_codeCULT)
vec6 <- na.omit(vec6)
vec6 <- unique(vec6)
length(vec6)
# [1] 3 unique institution codes



# Create a character vector of institution codes from all data frames.

vecALL <- c(vec1, vec2, vec3, vec4, vec5, vec6)
vecALL <- unique(vecALL)
length(vecALL)
# [1] 2993 unique institution codes in total

vecALL

# [1] "CIRAD"                                       "WPC"                                         "INRA"                                       
# [4] "AM"                                          "personal"                                    "ZFMK"      
# ....

base_ROR = "https://api.ror.org/organizations?query=%s"

RORurls <- sprintf(base_ROR, vecALL) # list of URLs

RORurls[1]

# [1] "https://api.ror.org/organizations?query=CIRAD"

RORresponses <- lapply(RORurls, GET) #list of responses

RORresponses[[1]]
# Response [https://api.ror.org/organizations?query=CIRAD]
# Date: XXXX-XX-XX XX:XX
# Status: 200
# Content-Type: application/json
# Size: 795 B

RORresponses_400 <- RORresponses[lapply(RORresponses, '[[',"status_code")=='400']
length(RORresponses_400)
# [1] 262 responses with status_code = 400

262/2993*100
# [1] 8.753759% of the responses had status_code = 400


# Create a vector with the 262 urls with status_code 400 responses
codes_400 <- lapply(RORresponses_400, '[', "url")
codes_400 <- unlist(codes_400, use.names=FALSE)
# Extract the institution codes that correspond to the aforementioned urls
codes_400 <- sapply(strsplit(codes_400, "="), "[", 2)


RORresponses_200 <- RORresponses[lapply(RORresponses, '[[',"status_code")=='200']
length(RORresponses_200)
# [1] 2731 responses with status_code = 200

2731/2993*100
# [1] 91.24624% of the responses had status_code = 200


# Create a vector with the 2731 urls with status_code 200 responses
codes_200 <- lapply(RORresponses_200, '[', "url")
codes_200 <- unlist(codes_200, use.names=FALSE)
# Extract the institution codes that correspond to the aforementioned urls
codes_200 <- sapply(strsplit(codes_200, "="), "[", 2)


# Extract the contents of the responses with status_code 200
RORcontents <- lapply(RORresponses_200, content, "text")

RORcontents_list <- lapply(RORcontents, fromJSON)
# Error: lexical error: invalid char in json text.
# <h1>Server Error (500)</h1>
#   (right here) ------^

length(RORcontents[str_detect(RORcontents, "Server Error")])
# 15 institution code queries resulted in a Server Error (500)

15/2731*100
# [1] 0.5492494% of the responses that had status_code = 200, ultimately resulted in Server Error (500)


# get the positions of responses with Server Error
grep("Server Error", RORcontents)
# [1]  114  168  173  357  400  469  522  523  545  756 1729 1858 2116 2225 2431

codes_errors <- codes_200[c(114,  168,  173,  357,  400,  469,  522,  523,  545,  756, 1729, 1858, 2116, 2225, 2431)]
codes_errors
# [1] "CIP<PER>"        "UOA/HCPF<GRC>"   "CPC"             "BCCM/IHEM"       "TCC/USP"         "BCCM/ULC"        "BCCM/MUCL"       "BCCM/LMG"       
# [9] "Fiocruz/COLPROT" "NE"              "USM<PER>"        "UBT"             "OR"              "SMF<PER>"        "HUT<PER>"       


RORcontents <- RORcontents[lapply(RORcontents, '[') != '<h1>Server Error (500)</h1>']
length(RORcontents)
# [1] 2716 responses without errors

RORcontents_list <- lapply(RORcontents, fromJSON)

# Create vectors of the institution codes that had ROR results as well as the numbers of results and put them together as a data frame 
ROR_no.results <- sapply(RORcontents_list, "[[", "number_of_results")
codes_results <- setdiff(codes_200, codes_errors)

RORresults <- data.frame(codes_results, ROR_no.results)

names(RORresults)[1] <- "institution codes"
names(RORresults)[2] <- "number of results"

RORresults[nrow(RORresults) + 15,] <- NA

RORresults$`institution codes`[is.na(RORresults$`institution codes`)] <- codes_errors

RORresults$`number of results`[is.na(RORresults$`number of results`)] <- "Server Error (500)"

RORresults[nrow(RORresults) + 262,] <- NA

RORresults$`institution codes`[is.na(RORresults$`institution codes`)] <- codes_400

RORresults$`number of results`[is.na(RORresults$`number of results`)] <- "Status code 400"

write.csv(RORresults, file = "D:/.../database_queries/RORresults.csv", row.names = F) # Supplementary File 3


length(RORresults$`number of results`[RORresults$`number of results`==0])
# 1115 institution codes had 0 results

length(RORresults$`number of results`[RORresults$`number of results`==1])
# 450 institution codes had 1 result

length(RORresults$`number of results`[RORresults$`number of results`==2])
# 200 institution codes had 2 results

length(RORresults$`number of results`[RORresults$`number of results` == "Server Error (500)"])
# 15 institution codes

length(RORresults$`number of results`[RORresults$`number of results` == "Status code 400"])
# 262 institution codes


#Pie chart: percentages of institution codes that had the following results when queried in ROR API

slices <- c(1115, 450, 200, 951, 262, 15)
lbls <- c("0", "1", "2", ">2", "Status code 400", "Server Error (500)")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="ROR API")


#  Search institution codes against GRID database ---------------------------------

# Filter GRID.json data frame. Select rows based on the values in "acronyms" variable matching to the values in
# the "institution_codeALL" character vector.

GRID.json <- read_json("D:/.../grid-2020-12-09/grid.json", simplifyVector = TRUE)

class(GRID.json)
# "list"

class(GRID.json$institutes)
# "data.frame"

GRID.json.match <- filter(GRID.json$institutes, acronyms %in% vecALL) # 2912 obs.

class(GRID.json.match$acronyms)
# "list"

GRID.json.match$acronyms <- unlist(GRID.json.match$acronyms) # unlist column "acronyms"

class(GRID.json.match$acronyms)
# "character"

nores_GRID_json <- GRID.json.match %>% count(acronyms)
# data frame with institution codes queried in GRID.json and number of results per code

length(nores_GRID_json$acronyms)
# 923 from a total of 2993 institution codes that were queried in GRID.json gave back results

(length(nores_GRID_json$acronyms)/length(vecALL))*100
#[1] 30.83862% of the 2993 institution codes queried in GRID.json gave back results


# 2,070 institution codes had 0 results

length(nores_GRID_json$n[nores_GRID_json$n==1])
# 431 institution codes had 1 result

length(nores_GRID_json$n[nores_GRID_json$n==2])
# 157 institution codes had 2 results

length(nores_GRID_json$n[nores_GRID_json$n>2])
# 335 institution codes had more than 2 results

# Pie chart with percentages of institution codes that have 0, 1, 2 or more than 2 matches in GRID database

slices1 <- c(2070, 431, 157, 335)
lbls1 <- c("0", "1", "2", ">2")
pct1 <- round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1, pct1) # add percents to labels
lbls1 <- paste(lbls1,"%",sep="") # add % to labels
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="GRID")

# create a character vector of institution codes with 0 results
institution_codes_zeroresults <- setdiff(vecALL, nores_GRID_json$acronyms)

nores_GRID_json[nrow(nores_GRID_json) + 2070,] <- NA

nores_GRID_json$acronyms[is.na(nores_GRID_json$acronyms)] <- institution_codes_zeroresults

nores_GRID_json$n[is.na(nores_GRID_json$n)] <- "no results" 

names(nores_GRID_json)[1] <- "institution codes" 
names(nores_GRID_json)[2] <- "number of results" 

write.csv(nores_GRID_json, file = "D:/.../database_queries/GRIDjson_results.csv", row.names = F) # Supplementary File 3


#  Search institution codes against GBIF Registry of Scientific Collections (GRSciColl) API (https://www.gbif.org/developer/registry) ---------------------------------

base_GRSciColl = "https://api.gbif.org/v1/grscicoll/institution?q=%s"

GBIFurls <- sprintf(base_GRSciColl, vecALL) # list of URLS

GRSciCollurls[[1]] 

# [1] "https://api.gbif.org/v1/grscicoll/institution?q=CIRAD"

GRSciCollresponses <- lapply(GRSciCollurls, GET) # list of responses

GRSciCollresponses[[1]]
# Response [https://api.gbif.org/v1/grscicoll/institution?q=CIRAD]
# Date: XXXX-XX-XX XX:XX
# Status: 200
# Content-Type: application/json
# Size: 6.21 kB


GRSciCollresponses_400 <- GRSciCollresponses[lapply(GRSciCollresponses, '[[',"status_code")=='400']
length(GRSciCollresponses_400)
# [1] 634 responses with status_code = 400

634/2993*100
# [1] 21.18276% of the responses had status_code = 400


# Create a vector with the 634 urls with status_code 400 responses
codes_400 <- lapply(GRSciCollresponses_400, '[', "url")
codes_400 <- unlist(codes_400, use.names=FALSE)
# Extract the institution codes that correspond to the aforementioned urls
codes_400 <- sapply(strsplit(codes_400, "="), "[", 2)


GRSciCollresponses_200 <- GRSciCollresponses[lapply(GRSciCollresponses, '[[',"status_code")=='200']
length(GRSciCollresponses_200)
# [1] 2359 responses with status_code = 200

2359/2993*100
# [1] 78.81724% of the responses had status_code = 200


# Create a vector with the 2359 urls with status_code 200 responses
codes_200 <- lapply(GRSciCollresponses_200, '[', "url")
codes_200 <- unlist(codes_200, use.names=FALSE)
# Extract the institution codes that correspond to the aforementioned urls
codes_200 <- sapply(strsplit(codes_200, "="), "[", 2)


# Extract the contents of the responses with status_code 200
GRSciCollcontents <- lapply(GRSciCollresponses_200, content, "text")

GRSciCollcontents_list <- lapply(GRSciCollcontents, fromJSON)

# Create vectors of the institution codes that had GRSciColl results as well as the numbers of results and put them together as a data frame 
GRSciColl_no.results <- sapply(GRSciCollcontents_list, "[[", "count")

GRSciCollresults <- data.frame(codes_200, GRSciColl_no.results)

names(GRSciCollresults)[1] <- "institution codes"
names(GRSciCollresults)[2] <- "number of results"

GRSciCollresults[nrow(GRSciCollresults) + 634,] <- NA

GRSciCollresults$`institution codes`[is.na(GRSciCollresults$`institution codes`)] <- codes_400

GRSciCollresults$`number of results`[is.na(GRSciCollresults$`number of results`)] <- "Status code 400"

write.csv(GRSciCollresults, file = "D:/.../database_queries/GRSciCollresults_inst.csv", row.names = F) # Supplementary File 3


length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results`==0])
# 356 institution codes had 0 results

length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results`==1])
# 884 institution codes had 1 result

length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results`==2])
# 262 institution codes had 2 results

length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results` == "Status code 400"])
# 634 institution codes


#Pie chart: percentages of institution codes that had the following results when queried in GRSciColl API

slices <- c(356, 884, 262, 857, 634)
lbls <- c("0", "1", "2", ">2", "Status code 400")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="GRSciColl/institutionCodes")


#  Search collection codes against GBIF Registry of Scientific Collections (GRSciColl) API (https://www.gbif.org/developer/registry) ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(httr)
library(jsonlite)
library(tidyverse)

# We will extract the collection codes from all sequences with source annotation.


one_identifier1 <- read.csv(file = "D:/.../one_identifier1.csv")

one_identifier1$collection_codeMAT[one_identifier1$collection_codeMAT==" "] <- NA
one_identifier1$collection_codeMAT[one_identifier1$collection_codeMAT==""] <- NA

one_identifier1$collection_codeCULT[one_identifier1$collection_codeCULT==" "] <- NA
one_identifier1$collection_codeCULT[one_identifier1$collection_codeCULT==""] <- NA

one_identifier1$collection_codeSPEC[one_identifier1$collection_codeSPEC==" "] <- NA
one_identifier1$collection_codeSPEC[one_identifier1$collection_codeSPEC==""] <- NA

vec1 <- c(one_identifier1$collection_codeMAT, one_identifier1$collection_codeCULT, one_identifier1$collection_codeSPEC)
vec1 <- na.omit(vec1)
vec1 <- unique(vec1)
length(vec1)
# [1] 43 unique collection codes



one_identifier2 <- read.csv(file = "D:/.../one_identifier2.csv")

one_identifier2$collection_codeMAT[one_identifier2$collection_codeMAT==" "] <- NA
one_identifier2$collection_codeMAT[one_identifier2$collection_codeMAT==""] <- NA

one_identifier2$collection_codeCULT[one_identifier2$collection_codeCULT==" "] <- NA
one_identifier2$collection_codeCULT[one_identifier2$collection_codeCULT==""] <- NA

one_identifier2$collection_codeSPEC[one_identifier2$collection_codeSPEC==" "] <- NA
one_identifier2$collection_codeSPEC[one_identifier2$collection_codeSPEC==""] <- NA

vec2 <- c(one_identifier2$collection_codeMAT, one_identifier2$collection_codeCULT, one_identifier2$collection_codeSPEC)
vec2 <- na.omit(vec2)
vec2 <- unique(vec2)
length(vec2)
# [1] 1410 unique collection codes



specimen_voucher_analysis <- read.csv(file = "D:/.../specimen_voucher_analysis.csv")

specimen_voucher_analysis$collection_code[specimen_voucher_analysis$collection_code==" "] <- NA
specimen_voucher_analysis$collection_code[specimen_voucher_analysis$collection_code==""] <- NA
vec3 <- specimen_voucher_analysis$collection_code
vec3 <- na.omit(vec3)
vec3 <- unique(vec3)
length(vec3)
# [1] 6 unique collection codes



# Create a character vector of collection codes from all data frames.

vecALL <- c(vec1, vec2, vec3)
vecALL <- unique(vecALL)
length(vecALL)
# [1] 1425 unique collection codes in total

vecALL

# [1] "CIBE"                                                               "DNA"                                                               
# [3] "IT"                                                                 "GRIN"       

base_GRSciColl = "https://api.gbif.org/v1/grscicoll/collection?q=%s"

GRSciCollurls <- sprintf(base_GRSciColl, vecALL) # list of URLS

GRSciCollurls[[1]] 

# [1] "https://api.gbif.org/v1/grscicoll/collection?q=CIBE"

GRSciCollresponses <- lapply(GRSciCollurls, GET) # list of responses

GRSciCollresponses[[1]]
# Response [https://api.gbif.org/v1/grscicoll/collection?q=CIBE]
# Date: XXXX-XX-XX XX:XX
# Status: 200
# Content-Type: application/json
# Size: 66 B

GRSciCollresponses_400 <- GRSciCollresponses[lapply(GRSciCollresponses, '[[',"status_code")=='400']
length(GRSciCollresponses_400)
# [1] 770 responses with status_code = 400

770/1425*100
# [1] 54.03509% of the responses had status_code = 400


# Create a vector with the 770 urls with status_code 400 responses
codes_400 <- lapply(GRSciCollresponses_400, '[', "url")
codes_400 <- unlist(codes_400, use.names=FALSE)
# Extract the collection codes that correspond to the aforementioned urls
codes_400 <- sapply(strsplit(codes_400, "="), "[", 2)


GRSciCollresponses_200 <- GRSciCollresponses[lapply(GRSciCollresponses, '[[',"status_code")=='200']
length(GRSciCollresponses_200)
# [1] 655 responses with status_code = 200

655/1425*100
# [1] 45.96491% of the responses had status_code = 200


# Create a vector with the 655 urls with status_code 200 responses
codes_200 <- lapply(GRSciCollresponses_200, '[', "url")
codes_200 <- unlist(codes_200, use.names=FALSE)
# Extract the collection codes that correspond to the aforementioned urls
codes_200 <- sapply(strsplit(codes_200, "="), "[", 2)


# Extract the contents of the responses with status_code 200
GRSciCollcontents <- lapply(GRSciCollresponses_200, content, "text")

GRSciCollcontents_list <- lapply(GRSciCollcontents, fromJSON)

# Create vectors of the collection codes that had GRSciColl results as well as the numbers of results and put them together as a data frame 
GRSciColl_no.results <- sapply(GRSciCollcontents_list, "[[", "count")

GRSciCollresults <- data.frame(codes_200, GRSciColl_no.results)

names(GRSciCollresults)[1] <- "collection codes"
names(GRSciCollresults)[2] <- "number of results"

GRSciCollresults[nrow(GRSciCollresults) + 770,] <- NA

GRSciCollresults$`collection codes`[is.na(GRSciCollresults$`collection codes`)] <- codes_400

GRSciCollresults$`number of results`[is.na(GRSciCollresults$`number of results`)] <- "Status code 400"

write.csv(GRSciCollresults, file = "D:/.../database_queries/GRSciCollresults_coll.csv", row.names = F) # Supplementary File 3


length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results`==0])
# 425 collection codes had 0 results

length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results`==1])
# 46 collection codes had 1 result

length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results`==2])
# 11 collection codes had 2 results

length(GRSciCollresults$`number of results`[GRSciCollresults$`number of results` == "Status code 400"])
# 770 collection codes


#Pie chart: percentages of collection codes that had the following results when queried in GRSciColl API

slices <- c(425, 46, 11, 173, 770)
lbls <- c("0", "1", "2", ">2", "Status code 400")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="GRSciColl/collectionCodes")


#  USE CASES ---------------------------------

#  Use case 1: Paper with title "Bat coronavirus phylogeography in the Western Indian Ocean" (Joffrin et al., 2020)  ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

# Query ENA for studied sequences

# The DNA sequences used in this paper are stored in GenBank and mirrored in ENA, under the accession numbers MN183146 to MN183273 

library(tidyverse)
library(httr)
library(jsonlite)

batsAccessions_df <- read.csv(file = "D:/.../use_cases/bats.accessions.csv")

names(batsAccessions_df)[1] <- "accessions"

# create accession vector
batsAccessions <- batsAccessions_df$accessions
class(batsAccessions) 
# character vector
length(batsAccessions) 
# 128 accessions
batsAccessions

# "MN183146" "MN183147" "MN183148" "MN183149" "MN183150" "MN183151" "MN183152" "MN183153" "MN183154" "MN183155" "MN183156" "MN183157" "MN183158"
# "MN183159" "MN183160" "MN183161" "MN183162" "MN183163" "MN183164" "MN183165" "MN183166" "MN183167" "MN183168" "MN183169" "MN183170" "MN183171"
# ...

base_ENA = "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,country,first_public,bio_material,culture_collection,specimen_voucher,host,isolate,isolation_source&includeAccessionType=sequence&includeAccessions="

# Create list of urls

ENAurls <- paste0(base_ENA, bats¡ccessions, "&format=json")

# View the first 5 urls

ENAurls[1:5]

# [1] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,country,first_public,bio_material,culture_collection,specimen_voucher,host,isolate,isolation_source&includeAccessionType=sequence&includeAccessions=MN183146&format=json"
# [2] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,country,first_public,bio_material,culture_collection,specimen_voucher,host,isolate,isolation_source&includeAccessionType=sequence&includeAccessions=MN183147&format=json"
# [3] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,country,first_public,bio_material,culture_collection,specimen_voucher,host,isolate,isolation_source&includeAccessionType=sequence&includeAccessions=MN183148&format=json"
# [4] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,country,first_public,bio_material,culture_collection,specimen_voucher,host,isolate,isolation_source&includeAccessionType=sequence&includeAccessions=MN183149&format=json"
# [5] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,country,first_public,bio_material,culture_collection,specimen_voucher,host,isolate,isolation_source&includeAccessionType=sequence&includeAccessions=MN183150&format=json"

ENAresponses <- lapply(ENAurls, GET) # list of responses

ENAresponses_200 <- ENAresponses[lapply(ENAresponses, '[[',"status_code")=='200']
length(ENAresponses_200)
# [1] 128 (all responses had status code 200)

# Extract the contents of the responses with status_code 200
ENAcontents <- lapply(ENAresponses_200, content, "text")

# Create list of dataframes
ENAcontents_list <- lapply(ENAcontents, fromJSON)

ENAcontents_df <- bind_rows(ENAcontents_list)

write.csv(ENAcontents_df, file = "D:/.../use_cases/ENAresults_bats.csv", row.names = F)

ENAcontents_df[1:5,]

#    accession  description                                                                                          scientific_name   country
# 1  MN183146   Bat coronavirus isolate 19020 polyprotein, RNA-dependent RNA polymerase region, gene, partial cds.   Bat coronavirus   Mozambique:Inhassoro
# 2  MN183147   Bat coronavirus isolate 19037 polyprotein, RNA-dependent RNA polymerase region, gene, partial cds.   Bat coronavirus   Mozambique:Inhassoro
# 3  MN183148   Bat coronavirus isolate 19028 polyprotein, RNA-dependent RNA polymerase region, gene, partial cds.   Bat coronavirus   Mozambique:Inhassoro
# 4  MN183149   Bat coronavirus isolate 19031 polyprotein, RNA-dependent RNA polymerase region, gene, partial cds.   Bat coronavirus   Mozambique:Inhassoro
# 5  MN183150   Bat coronavirus isolate 19032 polyprotein, RNA-dependent RNA polymerase region, gene, partial cds.   Bat coronavirus   Mozambique:Inhassoro

#    first_public   bio_material   culture_collection   specimen_voucher  host                                        isolate    isolation_source
# 1  2020-03-03                                                           Rhinolophus rhodesiae; male; FMNH 228945    19020      pooled oral and rectal swabs
# 2  2020-03-03                                                           Rhinolophus rhodesiae; male; FMNH 228962    19037      pooled oral and rectal swabs
# 3  2020-03-03                                                           Rhinolophus rhodesiae; male; FMNH 228953    19028      pooled oral and rectal swabs
# 4  2020-03-03                                                           Rhinolophus rhodesiae; female; FMNH 228956  19031      pooled oral and rectal swabs
# 5  2020-03-03                                                           Rhinolophus rhodesiae; female; FMNH 228957  19032      pooled oral and rectal swabs



# Extract and process the isolates and hosts

ENAcontents_df$isolate

# "19020"  "19037"  "19028"  "19031"  "19032"  "19030"  "19026"  "19027"  "18990"  "18991"  "18992"  "19013"  "19004"  "19024"  "19015"  "19006" 
# "18958"  "18996"  "19154"  "19174"  "19183"  "18997"  "18993"  "19185"  "19002"  "18951"  "18987"  "015"    "051"    "025"    "027"    "004"   
# ...

# Analyse the host fields.

ENAcontents_df <- ENAcontents_df %>% add_column(host_species = NA, .after = "host") # add a new column

ENAcontents_df$host_species[is.na(ENAcontents_df$host_species)] <- as.character(ENAcontents_df$host[is.na(ENAcontents_df$host_species)])

ENAcontents_df$host_species = sub("\\;[^;]*$", "", as.character(ENAcontents_df$host_species))

ENAcontents_df$host_species = sub("\\;[^;]*$", "", as.character(ENAcontents_df$host_species))

ENAcontents_df <- ENAcontents_df %>% add_column(host_identifier = NA, .after = "host_species") # add a new column

ENAcontents_df$host_identifier[is.na(ENAcontents_df$host_identifier)] <- as.character(ENAcontents_df$host[is.na(ENAcontents_df$host_identifier)])

# replace values that do not contain "text;text;text" with empty cells.

ENAcontents_df <- ENAcontents_df %>%
  mutate(host_identifier = case_when(
    !str_detect(host_identifier, ".*;(.*)\\;.*") ~ "",
    TRUE ~ host_identifier
  )
)

ENAcontents_df$host_identifier = sub("^[^;]*;", "", as.character(ENAcontents_df$host_identifier))

ENAcontents_df$host_identifier = sub("^[^;]*;", "", as.character(ENAcontents_df$host_identifier))

write.csv(ENAcontents_df, file = "D:/.../use_cases/ENAresults_Joffrin_et_al.csv", row.names = F) # Supplementary File 4


# Examine the format of host identifiers

length(ENAcontents_df$host_identifier[ENAcontents_df$host_identifier==""])
# 17 sequence accessions do not have a host identifier, probably they come from non-vouchered specimens

(length(ENAcontents_df$host_identifier[ENAcontents_df$host_identifier==""])/length(bats¡ccessions))*100
# 13.28125%

length(ENAcontents_df$host_identifier[ENAcontents_df$host_identifier!=""])
# 111 sequence accessions have a host identifier

(length(ENAcontents_df$host_identifier[ENAcontents_df$host_identifier!=""])/length(bats¡ccessions))*100
# 86.71875%

# These identifiers have the INSDC specimen voucher format =specimen_id and point to samples taken from vouchered specimens.


#  Use case 2: Kew Tree of Life Explorer (Baker et al., A Comprehensive Phylogenomic Platform for Exploring the Angiosperm Tree of Life.  ---------------------------------

# Tree of Life data are periodically released via this site: sftp.kew.org/pub/treeoflife. The latest
# release is Release 1.0 (February 2021) found here: http://sftp.kew.org/pub/treeoflife/current_release/.

rm(list = ls(all=T))
setwd("D:/")
getwd()

# Read the sequence_manifest.txt. A document listing the accession numbers (in public repositories)
# of all nucleotide sequence data used in the release. The "Sequence ID" field contains raw sequence 
# reads generated by PAFTOL and submitted to ENA, with the format (E|D|S)RR[0-9]{6,}, assembled 
# transcripts in the oneKP repository (https://db.cngb.org/onekp/) and assembled genomes downloaded 
# from ENA, with the study accession format PRJ(E|D|N)[A-Z][0-9]+.

sequence_manifest <- read.csv("D:/.../use_cases/Kew_treeoflife/sequence_manifest.csv", header = TRUE)


# Read the specimen_manifest.txt. A document listing the scientific name of all species included 
# in this release, with additional information about the specimens which have been sampled.
# In the specimen_manifest.txt, there are source information regarding the specimens used to 
# generate the raw reads in the column "Collection_ID".

specimen_manifest <- read.csv("D:/.../use_cases/Kew_treeoflife/specimen_manifest.csv", header = TRUE)


# How many angiosperm taxa were examined?

species <- sequence_manifest$Species_name

species1 <- specimen_manifest$Species_name

# Check if the two vectors of the species from the files sequence_manifest and specimen_manifest are composed of the same elements

setequal(species, species1)
# [1] TRUE

species <- unique(species)
length(species)
# [1] 2966 unique taxa


# How many vouchers were sequenced?

# After a first look in the specimen_manifest.txt, some species are not linked to any Collection_ID, Specimen_ID and Voucher_information.

subset_df <- specimen_manifest %>% filter(Collection_ID != "-" | Specimen_ID != "-" | Voucher_information != "-")

length(subset_df$Voucher_information)
# [1] 2374 specimen vouchers sampled by PAFTOL

species_subset_df <- subset_df$Species_name

# What types of nucleotide sequence data were used in this project?

unique(sequence_manifest$Sequence_type)
# [1] "Read"  "Transcript"  "Genome"  


# Analyse the three types of nucleotide sequence data

# A. READS

reads <- subset(sequence_manifest, grepl("Read", sequence_manifest$Sequence_type))

species_reads <- reads$Species_name

# Check if the two vectors of the species linked to voucher information and species associated with raw reads are composed of the same elements

setequal(species_subset_df, species_reads)
# [1] TRUE                                  

# 2374 angiosperm vouchers were sequenced and raw reads were generated by PAFTOL


# Create a vector of the "raw sequence read" accessions (run accessions) generated by PAFTOL
run_accessionsIDs <- unique(reads$Sequence_ID)

length(run_accessionsIDs)
# [1] 2374 run accessions associated with 2374 voucher specimens

run_accessionsIDs

# Download all raw reads stored in the ENA database
# The URL was dowloaded using curl and Git CMD
# curl -o read_run-all.json "https://www.ebi.ac.uk/ena/portal/api/search?result=read_run&fields=run_accession,scientific_name,bio_material,specimen_voucher,sample_accession,study_accession&limit=0&format=json"

# Using Cygwin Terminal, the json file was split into multiple pieces using the following:
# split -l 1000000 --additional-suffix=.json C:/.../read_run-all.json C:/.../read_run-all/

library(tidyverse)
library(jsonlite)

read_run_all <- list.files(path = "C:/.../read_run-all/", pattern = ".json", full.names = TRUE)

# Subset all raw read json files from the ENA database by filtering them based on matches with the vector "run_accessionsIDs"

for (i in 1:length(read_run_all)){
  # load the json that corresponds to the i element in the list of files
  readRun_df <- fromJSON(read_run_all[[i]], flatten = TRUE)
  readRun_df <- filter(readRun_df, run_accession %in% run_accessionsIDs)
  write.csv(readRun_df, file = paste0("D:/.../use_cases/Kew_treeoflife/ENArawReads_matches/","match_", i,".csv"), row.names = F)
}

rm(readRun_df, i, read_run_all, tables)


ENArawReads_matches <- list.files(path = "D:/.../ENArawReads_matches", pattern = ".csv", full.names = TRUE)


# read files into a list
tables <- lapply(ENArawReads_matches, read.csv, header = TRUE)

ENArawReads_matches.df <- do.call(rbind , tables)

ENArawReads_matches.df[is.na(ENArawReads_matches.df)] <- ""

write.csv(ENArawReads_matches.df, file = "D:/.../ENArawReads_matches/matchesTOT.csv", row.names = F) # Supplementary File 5


length(ENArawReads_matches.df$run_accession)
# [1] 2359

2359/2374*100
# [1] 99.36816% of the run accessions used in the project were matched in the ENA database


# How many run accessions are not associated with source information?

rawReads_nosource <- ENArawReads_matches.df %>% filter(bio_material == "" & specimen_voucher == "")

length(rawReads_nosource$run_accession)
# [1] 101

101/2359*100
# [1] 4.281475% of the run accessions used in the project and matched in the ENA database, lack source annotation


# For a fraction of the run accessions used in the project, ENA didn't provide source annotation (which in contrast, is available in the specimen_manifest.txt)


# How many run accessions are associated with source information?

rawReads_source <- ENArawReads_matches.df %>% filter(!(bio_material == "" & specimen_voucher == ""))

length(rawReads_source$run_accession)
# [1] 2258

2258/2359*100
# [1] 95.71852% of the run accessions used in the project and matched in the ENA database, have source annotation


# Analyse the source identifiers linked to raw reads and detect the type of identifier format.

rawReads_source[rawReads_source == ""] <- NA

biomat_ident <- rawReads_source$bio_material
biomat_ident <- na.omit(biomat_ident)
biomat_ident <- unique(biomat_ident)

biomat_ident

# [1] "EA, K"         "K"             "GENT"          "EA"            "K000468216"    "K001275013"    "K001275012"    "K000326637"    "MO"           
# [10] "NBG"           "K000618241"    "K000618243"    "K001257762"    "K000798319"    "K000576177"    "K000618256"    "K000618261"    "K000618252"
# [19] "K000618247"    "K000618255"    "K000214340"    "K000618271"    "K000618263"    "K000946661"    "K000946832"    "K000743352"    "K000618265" ...


# Check if any of the bio_material identifiers has a DwC Triplet format

grep(":", biomat_ident, value = TRUE)
# character(0)

specimen_manifest$Collection_ID

# [1] "NY"    "K"     "NY"    "NY"    "K"     "USM"   "NY"    "NY"    "NY"    "NY"    "K"     "UPCB"  "UPCB"  "NY"    "K"     "USM"   "NY"    "NY"   
# [19] "K"     "K"     "K"     "NY"    "USM"   "K"     "NY"    "MO"    "K"     "UPCB"  "K"     "K"     "K"     "NY"    "NY"    "K"     "HTW"   "K" ...  


specimen_manifest$Specimen_ID[!grepl("-", specimen_manifest$Specimen_ID)]

#  [1] "K001275241"    "K000696937"    "K001275240"    "K000493461"    "K000576284"    "K000746029"    "NY02844207"    "K (spirit)"    "K000460414"   
# [10] "K001275243"    "K001243667"    "64256"         "K000396184"    "K001061989"    "K000460187"    "K000696901"    "K001275058"    "K000696972"   
# [19] "K000926543"    "K000939270"    "K"             "K000738913"    "K000703312"    "K000696961"    "K001275094"    "K001275041"    "K001061783" ... 

# All bio_material identifiers have the format /catalogNumber. Bio_material identifiers have the format of "Specimen_ID" or "Collection_ID" from the specimen_manifest.txt


specimen_ident <- rawReads_source$specimen_voucher
specimen_ident <- na.omit(specimen_ident)
specimen_ident <- unique(specimen_ident)

specimen_ident

# [1] "FRI 52109 (K)"
# [2] "COLT 69 (K)"                                                                                                                                   
# [3] "Pennington 17749 (K)"                                                                                                                          
# [4] "Castillo 2787 (K)"                                                                                                                             
# [5] "Strijk 203 (UPS)"                                                                                                                              
# [6] "REU 10015 (REU)" ...


# Check if any of the specimen_voucher identifiers has a DwC Triplet format and if a semicolon (";") exists

grep(":", specimen_ident, value = TRUE)
# [1] "Whaite, T.: Whaite, J., 3471 (K)"    "Andriamahay, M. : SNGF; Rakotoarisoa, S.E., 3411 (K)"

grep(";", specimen_ident, value = TRUE)

# [1] "M&apos;Boungou 144 (K)"                                                                                                                                 
# [2] "Morawetz, W.; Wallnofer, B. - M13-29985 (K)"                                                                                                            
# [3] "Greenway, P.J.;  Kanuri, 11371 (K)"                                                                                                                     
# [4] "Goes, S.P.; et al., 20285 (K)"                                                                                                                          
# [5] "Prance, G.T.; Maas, P.J.M.; Atchley, A.A.; Steward, W.C.; Woolcott, D.B.; Coelho, D.F.; Monteiro, O.P.; Pinheiro, W.S.; Ramos, J.F., 13461 (K)"         
# [6] "Weiblen, G.D.; Montgomery, R.; Isua, B.; Molem, K. - GW1865 (K)"                                                                                        
# [7] "Davis &amp; Rakotonasolo, 3117 (K)"                                                                                                                     
# [8] "Cloclet, F.A.; Souza, L.R.M.; Alves, J., 21.236 (K)"                                                                                                    
# [9] "Acevedo.R. P.; Bell, D.; Rankin, K.; Smith, S.F., 8833 (K)"                                                                                             
# [10] "Berg, C.C.; Steward, W.C.; Ramos, J.F.; Monteiro, O.P.; Lima, J.F., 18524 (K)" ...



grep("Whaite, T.", specimen_manifest$Voucher_information, value = TRUE)
# [1] "Whaite, T.: Whaite, J. 3471"

grep("Andriamahay, M.", specimen_manifest$Voucher_information, value = TRUE)
# [1] "Andriamahay, M. : SNGF; Rakotoarisoa, S.E. 3411"

grep(";", specimen_manifest$Voucher_information, value = TRUE)

# [1] "van Andel, T.R.; Behari-Ramdass, J.; Ramharakh, S.;  Wewe, K. 4359"                                                                       
# [2] "Balkwill, K.;  Balkwill, M.J. 10715"                                                                                                      
# [3] "Bodine, S.A.; Dietrich, C.; Schmidt, H.H. 5"                                                                                              
# [4] "Forest, F. ;  Manning, J.C. 527"                                                                                                          
# [5] "Cavalcanti; Pereira-Siva 3786"                                                                                                            
# [6] "Paul, M.; Seyda, L.; Durant, C.; Jaeger, G.; Styer, D.  CA190-65"                                                                         
# [7] "Nee, M.H.; Mendoza, M. 57763"                                                                                                             
# [8] "Smith, S.A.L., Beentje, H.J.;  Muasya, A.M. 288"                                                                                          
# [9] "Civeyrel, Chase;  Fay s.n."                                                                                                               
# [10] "Nusbaumer, L.;  Ranirison, P. 1573" ...       



# All specimen_voucher identifiers have the format /catalogNumber. The institution codes are in parentheses. The use of semicolon (";") in ENA indicates 
# multiple specimen_vouchers linked to an accession. In our case, specimen vouchers have the same format as the specimen_manifest.txt and ";" symbolises
# different collectors.


# B. TRANSCRIPTS AND GENOMES

transcripts <- subset(sequence_manifest, grepl("Transcript", sequence_manifest$Sequence_type))

length(transcripts$Sequence_ID)
# [1] 674 transcriptomes stored in the oneKP repository

species_transcripts <- transcripts$Species_name


genomes <- subset(sequence_manifest, grepl("Genome", sequence_manifest$Sequence_type))

length(genomes$Sequence_ID)
# [1] 61 genomes stored in ENA

species_genomes <- genomes$Species_name

species_transcripts_genomes <- c(species_transcripts, species_genomes)


# As we mentioned earlier, some species in the specimen_manifest.txt are not linked to any Collection_ID, Specimen_ID and Voucher_information.

subset_df1 <- specimen_manifest %>% filter(Collection_ID == "-" & Specimen_ID == "-" & Voucher_information == "-")

length(subset_df1$Voucher_information)
# [1] 735 specimens missing source annotation

species_subset_df1 <- subset_df1$Species_name


# Check if the two vectors of the species not linked to voucher information and the species associated with transcriptomes and genomes are composed
# of the same elements.

setequal(species_subset_df1, species_transcripts_genomes)
# [1] TRUE                                  


# Create a vector of the project accessions linked to the assembled genomes

project_accessionsIDs <- genomes$Sequence_ID

project_accessionsIDs

# [1] "PRJNA391506" "PRJNA341983" "PRJNA212863" "PRJNA33681"  "PRJEB12914"  "PRJNA41137"  "PRJNA10719"  "PRJNA41497"  "PRJNA32607" 
# [10] "PRJEB5043"   "PRJNA217459" "PRJNA59981"  "PRJNA223222" "PRJNA223006" "PRJEB4211"   "PRJNA33619"  "PRJNA238069" "PRJNA268187"
# [19] "PRJDB3383"   "PRJNA253673" "PRJNA13880"  "PRJNA49047"  "PRJNA19861"  "PRJNA171262" "PRJNA345532" "PRJEB32488"  "PRJNA428241"
# [28] "PRJNA163065" "PRJNA299755" "PRJNA234389" "PRJNA10791"  "PRJEA82777"  "PRJNA316810" "PRJNA30379"  "PRJNA70533"  "PRJNA13765" 
# [37] "PRJNA48429"  "PRJNA169314" "PRJNA48433"  "PRJNA48107"  "PRJNA13770"  "PRJEB4137"   "PRJDB1747"   "PRJNA41439"  "PRJNA10772" 
# [46] "PRJDB4877"   "PRJNA31227"  "PRJNA483885" "PRJNA32913"  "PRJNA119"    "PRJNA63145"  "PRJNA13876"  "PRJEB14326"  "PRJEB9186"  
# [55] "PRJEB27788"  "PRJNA310175" "PRJEB22687"  "PRJNA261643" "PRJNA243847" "PRJEA18785"  "PRJNA10769" 


# The vector "project_accessionsIDs" was searched against the ENA dataset "Genome assemblies".

library(httr)

base_ENA = "https://www.ebi.ac.uk/ena/portal/api/search?result=assembly&query=study_accession="

# Create list of urls (the fields "bio_material", "specimen_voucher" are searchable in this dataset)

ENAurls <- paste0(base_ENA, project_accessionsIDs, "&fields=accession,assembly_title,study_title,scientific_name,strain,sample_accession,study_accession&format=json")

# View the first 5 urls

ENAurls[1:5]

# [1] "https://www.ebi.ac.uk/ena/portal/api/search?result=assembly&query=study_accession=PRJNA391506&fields=accession,assembly_title,study_title,scientific_name,strain,sample_accession,study_accession&format=json"
# [2] "https://www.ebi.ac.uk/ena/portal/api/search?result=assembly&query=study_accession=PRJNA341983&fields=accession,assembly_title,study_title,scientific_name,strain,sample_accession,study_accession&format=json"
# [3] "https://www.ebi.ac.uk/ena/portal/api/search?result=assembly&query=study_accession=PRJNA212863&fields=accession,assembly_title,study_title,scientific_name,strain,sample_accession,study_accession&format=json"
# [4] "https://www.ebi.ac.uk/ena/portal/api/search?result=assembly&query=study_accession=PRJNA33681&fields=accession,assembly_title,study_title,scientific_name,strain,sample_accession,study_accession&format=json" 
# [5] "https://www.ebi.ac.uk/ena/portal/api/search?result=assembly&query=study_accession=PRJEB12914&fields=accession,assembly_title,study_title,scientific_name,strain,sample_accession,study_accession&format=json" 

ENAresponses <- lapply(ENAurls, GET) # list of responses

ENAresponses_200 <- ENAresponses[lapply(ENAresponses, '[[',"status_code")=='200']
length(ENAresponses_200)
# [1] 61 (all responses had status code 200)

# Extract the contents of the responses with status_code 200
ENAcontents <- lapply(ENAresponses_200, content, "text")

# Create list of dataframes
ENAcontents_list <- lapply(ENAcontents, fromJSON)

ENAcontents_df <- bind_rows(ENAcontents_list)

write.csv(ENAcontents_df, file = "D:/.../Kew_treeoflife/ENAresults_genomes.csv", row.names = F) # Supplementary File 5

ENAcontents_df[ENAcontents_df == ""] <- NA

na.omit(ENAcontents_df$strain)

# [1] "MN47"                               "Bd21"                               "Bd21"                               "Bd21"                              
# [5] "TO1000"                             "DH200=94"                           "BRASUZ1"                            "A17"                               
# [9] "A17"                                "Doubled-haploid Pahang (DH-Pahang)" "Doubled-haploid Pahang (DH-Pahang)" "Doubled-haploid Pahang (DH-Pahang)"
# [13] "UT"                                 "IRGC 105608"                        "IRGC 105608"                        "IRGC 105608"                       
# [17] "IRGC 105608"                        "IRGC: 96717"                        "IRGC: 96717"                        "W1943"                             
# [21] "Nipponbare"                         "Yugu1"                              "Yugu1"                              "DM1-3 516 R44"                     
# [25] "PN40024"                            "PN40024"                           


grep(":", na.omit(ENAcontents_df$strain), value = TRUE)
# [1] "IRGC: 96717" "IRGC: 96717"

# the institution code IRGC has been searched in ROR, GRID and GRSciColl API in the analysis of the main ENA dataset

# From a first look at the data frame, only the field "strain" might contain source information but it is not a link to any
# Museum/Reference Collection source material (https://www.ncbi.nlm.nih.gov/books/NBK566990/#qkstrt_Srce_Info.what_kind_of_source_inf_2).


# Can we extract any source information from the samples of the genomes?

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(tidyverse)
library(jsonlite)

ENAcontents_df <- read.csv(file = "D:/.../Kew_treeoflife/ENAresults_genomes.csv")

ENAcontents_df$sample_accession[ENAcontents_df$sample_accession == ""] <- NA

sample_accessionIDs <- unique(ENAcontents_df$sample_accession)
sample_accessionIDs <- na.omit(sample_accessionIDs)
# 77 unique sample accessions

# Download information regarding these sample accession IDs from the ENA dataset "sample".
# The URL was dowloaded using curl and Git CMD
# curl -o sample-all.json "https://www.ebi.ac.uk/ena/portal/api/search?result=sample&fields=accession,sample_title,description,scientific_name,first_public,bio_material,culture_collection,specimen_voucher&limit=0&format=json"

# Using Cygwin Terminal, the json file was split into multiple pieces using the following:
# split -l 1000000 --additional-suffix=.json C:/.../sample-all.json C:/.../sample-all/

sample_all <- list.files(path = "C:/.../sample-all", pattern = ".json", full.names = TRUE)

# Subset all sample json files from the ENA database by filtering them based on matches with the vector "sample_accessionIDs"

for (i in 1:length(sample_all)){
  # load the json that corresponds to the i element in the list of files
  sample_df <- fromJSON(sample_all[[i]], flatten = TRUE)
  sample_df <- filter(sample_df, accession %in% sample_accessionIDs)
  write.csv(sample_df, file = paste0("D:/.../use_cases/Kew_treeoflife/ENAsamples_matches/","ENAsample_match_", i,".csv"), row.names = F)
}

rm(sample_df, i, sample_all)

ENAsamples_matches <- list.files(path = "D:/.../ENAsamples_matches", pattern = ".csv", full.names = TRUE)

# read files into a list
tables <- lapply(ENAsamples_matches, read.csv, header = TRUE)

ENAsamples_matches.df <- do.call(rbind , tables)

ENAsamples_matches.df[is.na(ENAsamples_matches.df)] <- ""

write.csv(ENAsamples_matches.df, file = "D:/.../ENAsamples_matches/ENAsample_matchTOT.csv", row.names = F) # Supplementary File 5


length(ENAsamples_matches.df$accession)
# [1] 47 sample accessions stored in the ENA dataset "sample", were matched to the "sample_accessionIDs" vector

47/77*100
# [1] 61.03896%


# How many sample accessions are not associated with source information?

samples_nosource <- ENAsamples_matches.df %>% filter(bio_material == "" & specimen_voucher == "" & culture_collection == "")

length(samples_nosource$accession)
# [1] 47 (all sample accessions lack source annotation)


#  Use case 3: Paper with title "A complete time-calibrated multi-gene phylogeny of the European butterflies" (Wiemers et al., 2020)  ---------------------------------

rm(list = ls(all=T))
setwd("D:/")
getwd()

library(tidyverse)
library(httr)
library(jsonlite)

# The vouchers are stored in ENA and GenBank databases and the original dataset is available here: https://zookeys.pensoft.net/article/50878/download/suppl/31/.

butterflies_vouchers <- read.csv(file = "D:/.../use_cases/zookeys-938-097-s001.csv")

names(butterflies_vouchers)[1] <- "Species.name"

# create voucher vector
vouchers <- butterflies_vouchers$Voucher
vouchers <- unique(vouchers)
class(vouchers) 
# character vector
length(vouchers) 
# 496 vouchers associated with 496 extant butterfly species in Europe

vouchers

# "RV12O525"              "NW63-16"               "NW63-3"                "NHMO-06179"            "AD00P259"              "VL05Z994"              "VL01B424"             
# "AD03B064"              "MW99018"               "RV09V415"              "NW151-2"               "WMB1125-13"            "NW151-1"               "BC_SB_Lep_0030"    
# ...

# Examine the format used for the specimen vouchers.

# Detect the presence or absence of ":".

presence <- str_detect(butterflies_vouchers$Voucher, ":")

length(presence[presence == "FALSE"])
# 496 vouchers do not contain ":".

# 100% of the vouchers have the INSDC format /specimen_voucher=catalogNumber


# Query ENA for the studied sequences and get the vouchers associated with them

# Create a vector of sequence accessions

butterflies_accessions <- c(butterflies_vouchers$COI.barcode, butterflies_vouchers$COI.3., butterflies_vouchers$ArgKin, butterflies_vouchers$CAD, butterflies_vouchers$DDC, butterflies_vouchers$EF.1a, butterflies_vouchers$GAPDH, butterflies_vouchers$H3, butterflies_vouchers$IDH, butterflies_vouchers$MDH, butterflies_vouchers$RpS2, butterflies_vouchers$RpS5, butterflies_vouchers$wingless)

butterflies_accessions[butterflies_accessions == ""] <- NA
butterflies_accessions <- na.omit(butterflies_accessions)
butterflies_accessions <- unique(butterflies_accessions)

length(butterflies_accessions)
# [1] 1691 unique accessions linked to 496 butterfly vouchers


base_ENA = "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,bio_material,specimen_voucher,study_accession&includeAccessionType=sequence&includeAccessions="

# Create list of urls

ENAurls <- paste0(base_ENA, butterflies_accessions, "&format=json")

# View the first 5 urls

ENAurls[1:5]

# [1] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,bio_material,specimen_voucher,study_accession&includeAccessionType=sequence&includeAccessions=MH419480&format=json"
# [2] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,bio_material,specimen_voucher,study_accession&includeAccessionType=sequence&includeAccessions=AY248785&format=json"
# [3] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,bio_material,specimen_voucher,study_accession&includeAccessionType=sequence&includeAccessions=AY248786&format=json"
# [4] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,bio_material,specimen_voucher,study_accession&includeAccessionType=sequence&includeAccessions=KX049851&format=json"
# [5] "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&fields=accession,description,scientific_name,bio_material,specimen_voucher,study_accession&includeAccessionType=sequence&includeAccessions=GQ128944&format=json"


ENAresponses <- lapply(ENAurls, GET) # list of responses

# Check the status of the responses
ENAresponses_200 <- ENAresponses[lapply(ENAresponses, '[[',"status_code")=='200']
length(ENAresponses_200)
# [1] 1662 responses had status code 200

# Extract the contents of the responses with status_code 200
ENAcontents_200 <- lapply(ENAresponses_200, content, "text")

# Create list of dataframes
ENAcontents_200_list <- lapply(ENAcontents_200, fromJSON)

# Bind the rows of the dataframes together
ENAcontents_200_df <- bind_rows(ENAcontents_200_list)


write.csv(ENAcontents_200_df, file = "D:/.../use_cases/ENAresults_butterflies.csv", row.names = F)


# Check the status of the remaining responses
ENAresponses_rest <- ENAresponses[lapply(ENAresponses, '[[',"status_code")!='200']
length(ENAresponses_rest)
# [1] 29 responses didn't have status code 200

# Extract the contents of the remaining responses
ENAcontents_rest <- lapply(ENAresponses_rest, content, "text")

ENAcontents_rest

# [[1]]
# [1] ""

# [[2]]
# [1] ""

# [[3]]
# [1] "{\"message\":\"Invalid accession 'WMB1350-13' included for the accession type 'sequence'\"}\n"

# [[4]]
# [1] ""

# [[5]]
# [1] "{\"message\":\"Invalid accession 'IRANB272-08' included for the accession type 'sequence'\"}\n"

# [[6]]
# [1] "{\"message\":\"Invalid accession 'TBBUT1686-12' included for the accession type 'sequence'\"}\n"

# [[7]]
# [1] "{\"message\":\"Invalid accession 'PRJNA432311' included for the accession type 'sequence'\"}\n"
# ...

# 29 accessions resulted in "bad responses"

setdiff(butterflies_accessions, ENAcontents_200_df$accession)

# [1] "HM888370"     "HM404207"     "WMB1350-13"   "HM404203"     "IRANB272-08"  "TBBUT1686-12" "PRJNA432311"  "JF851939"     "EULEP674-15" 
# [10] "JF850406"     "HM404734"     "BPAL1670-12"  "HM404669"     "EULEP451-14"  "EULEP5112-17" "EULEP3136-15" "IRANB192-08"  "HM385171"    
# [19] "EULEP1414-15" "KP008036"     "KP008079"     "NK00P719"     "AD00P319"     "NK00P605"     "RDJ0207252"   "KVDphl2"      "VL01L402"    
# [28] "MAT99Q934"    "VL99Q072"  


# Examine the data frame that contains the status code 200 responses, and more specifically, the "bio_material" and "specimen_voucher" columns.

# How many accessions lack "bio_material" and "specimen_voucher" annotation?

ENAcontents_nosource <- ENAcontents_200_df %>% filter(bio_material == "" & specimen_voucher == "")

length(ENAcontents_nosource$accession)
# [1] 114

114/1662*100
# [1] 6.859206% out of 1,662 accessions with status code 200 responses, lack source annotation in both "bio_material" and "specimen_voucher" fields.


# How many accessions are associated with source information?

ENAcontents_source <- ENAcontents_200_df %>% filter(!(bio_material == "" & specimen_voucher == ""))

length(ENAcontents_source$accession)
# [1] 1548

1548/1662*100
# [1] 93.14079% out of 1,662 accessions with status code 200 responses, have source annotation in "bio_material" or "specimen_voucher" fields


# Analyse the source identifiers linked to the accessions and detect the type of identifier format.

ENAcontents_source[ENAcontents_source == ""] <- NA

biomat_ident <- ENAcontents_source$bio_material
biomat_ident <- na.omit(biomat_ident)
biomat_ident

# [1] "DNA 2245"

# 1 bio_material identifier with the format /catalogNumber

specimen_ident <- ENAcontents_source$specimen_voucher
specimen_ident <- na.omit(specimen_ident)
specimen_ident <- unique(specimen_ident)
length(specimen_ident)

# [1] 582 unique specimen voucher identifiers associated with 1,548 accessions.


# Check if any of the specimen_voucher identifiers has a DwC Triplet format

grep(":", specimen_ident, value = TRUE)

# [1] "MCZ:AD00P259"   "MCZ:VL05Z994"   "MCZ:VL01B424"   "MCZ:AD03B064"   "MCZ:AD03B041"   "MCZ:AH-95-Y685" "MCZ:AD00P540"   "MCZ:DL-12-A049"
# [9] "MCZ:AD03B062"   "MCZ:VL01L462"   "MCZ:AD00P066"   "MCZ:MWT93E012"  "MCZ:RV03H454"   "MCZ:AD00P266"   "MCZ:NGK02C411"  "MCZ:NK00P594"  
# [17] "MCZ:KD94Q064"   "MCZ:NK00P712"   "MCZ:NK00P135"   "MCZ:AD00P560"   "MCZ:MAT99Q841"  "MCZ:NK00P562"   "MCZ:AH95Y685"   "MCZ:RV03N585"  


# 24 specimen_voucher identifiers have the format /institutionCode:catalogNumber. The remaining have the format /catalogNumber.
# One institutionCode "MCZ", which has been searched against ROR, GRID and GRSciColl API in the analysis of the main ENA dataset


# Check if any of the specimen_voucher identifiers has the pattern ";" (more than one specimen_voucher identifiers)

grep(";", specimen_ident, value = TRUE)

# [1] "JD03A505; J. Costa (MCZ DNA and TissueCollection; Harvard)"      "JC96Q003; J. Costa (MCZ DNA and TissueCollection; Harvard)"     
# [3] "570-ADW; (DM02-086)"                                             "MAT99Q829; M. Travassos (MCZ DNA andTissue Collection; Harvard)"
# [5] "AD00P155; A. Dantchenko (MCZ DNA andTissue Collection; Harvard)" "NK00P753; N. Kandul (MCZ DNA and TissueCollection; Harvard)"    
# [7] "VL02L518; V. Lukhtanov (MCZ DNA andTissue Collection; Harvard)"  "RV03N585; R. Vila (MCZ DNA and TissueCollection; Harvard)"      
# [9] "AD00P068; A. Dantchenko (MCZ DNA andTissue Collection; Harvard)" "MG02N009; M. Goia (MCZ DNA and TissueCollection; Harvard)" 

# 10 specimen_vouchers are constructed with more than one identifier


# Join the supplementary file from the paper with the results from ENA (the accessions that were successfully queried)

butterflies_vouchers <- read.csv(file = "D:/.../zookeys-938-097-s001.csv")

# Subset the "butterflies_vouchers" data frame

names(butterflies_vouchers)[1] <- "Species.name"

wide <- butterflies_vouchers %>% select("Voucher", "COI.barcode", "COI.3.", "ArgKin", "CAD", "DDC", "EF.1a", "GAPDH", "H3", "IDH", "MDH", "RpS2", "RpS5", "wingless")

# Reshape the "wide" data frame from wide to long format

long <- wide %>%
  pivot_longer(-Voucher)

long <- subset(long, select = -name)

names(long)[2] <- "Accessions"

long[long == ""] <- NA
long <- na.omit(long)

# Remove rows that have duplicate values in the column "Accessions"
long <- long[!duplicated(long$Accessions),]

long <- long[,c(2, 1)]

length(long$Accessions)
# [1] 1691 accessions studied

write.csv(long, file = "D:/.../use_cases/zookeys_subset.csv", row.names = F)

ENAcontents_200_df <- read.csv(file = "D:/.../ENAresults_butterflies.csv")

names(ENAcontents_200_df)[1] <- "Accessions"


# Partial matching, keep the rows with no matching rows in the other data frame ("ENAcontents_200_df"). These rows will have NA in those columns.

final <- merge(long, ENAcontents_200_df, by.x = "Accessions", all.x = TRUE)

names(final)[3] <- "Description"
names(final)[4] <- "Scientific name"
names(final)[7] <- "Study accession"

final[is.na(final)] <- ""

write.csv(final, file = "D:/.../use_cases/resultsFIN_Wiemers_et_al.csv", row.names = F) # Supplementary File 6

# The first two columns contain information from the supplementary file, the rest are the results from ENA.
