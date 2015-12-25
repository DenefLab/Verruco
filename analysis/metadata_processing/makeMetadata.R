source("global.R")
source("metadataFunctions.R")


# Read in data
dat <- read.csv("sample_studies.csv")
inl.dat <- read.csv("inland_metadata.txt", sep = "\t")

## Make Verruco Sample data 
Mich <- makeMichiganData(dat)
Musk <- makeMuskegonData(dat)
Inl <- makeInlandData(dat, inl.dat)
TB <- makeTbData(dat)
None <- makeNonData(dat)

# Bind all subsetted datasets
metadata <- rbind(Mich, Musk, Inl, TB, None)

# Write to output
write.csv(metadata, "~/git_repos/Verruco/analysis/verruco_metadata_duplicates.csv")







