## Script which creates sample metadata for samples in verruco-analysis.Rmd
source("metadataFunctions.R")


# Read in data
dat <- read.csv("sample_studies.csv")
inl.dat <- read.csv("inland_lakes_metadata.csv")

# Replace "-" with "." in sample names
inl.dat$Sample <- gsub(
    x = inl.dat$Sample, 
    pattern = "-",
    replacement = "."
  )

## Make Verruco Sample data 
Mich <- makeMichiganData(dat)
Musk <- makeMuskegonData(dat)
Inl <- makeInlandData(dat, inl.dat)
TB <- makeTbData(dat)
None <- makeNonData(dat)

# Bind all subsetted datasets
metadata <- rbind(Mich, Musk, Inl, TB, None)

# Write to output
write.csv(
  x = metadata, 
  file = "~/git_repos/Verruco/analysis/verruco_metadata_duplicates.csv", 
  row.names = FALSE
)







