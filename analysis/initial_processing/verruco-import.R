library(phyloseq)
library(dplyr)

#setwd("~/git_repos/Verruco/analysis/")

# files
sharedfile = "/initial_processing/mothur_output/verruco.shared"
taxfile = "/initial_processing/mothur_output/verruco.cons.taxonomy"
mapfile = "/initial_processing/verruco_metadata_duplicates.csv"

# Import mothur data
mothurdata = import_mothur(
  mothur_shared_file = sharedfile, 
  mothur_constaxonomy_file = taxfile
)

# Add the OTU number as a column in the taxonomy file
tax_table(mothurdata) <- cbind(tax_table(mothurdata), 
                               row.names(tax_table(mothurdata)))

# Rename the taxonomy columns
colnames(tax_table(mothurdata)) <- 
  c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# Import sample metadata and transform into sample_data class
map <- read.csv(mapfile)

# Convert map to sample_data class
# Merge mothurdata object with sample_data
map <- sample_data(map)
rownames(map) <- map$SampleID
mothur.merge <- merge_phyloseq(mothurdata, map)



# Names in original mothur file vs. VerrucoData file
# To be used to created phylogenetic tree in mothur
orig.names <- sample_names(mothur.merge)
ver.names <- sample_names(mothurdata)
dif.names <- setdiff(orig.names, ver.names)
#sub.names <- gsub(pattern = "[.]", replacement = "-", x = dif.names)
#sub.names <- data.frame(sub.names)
writeLines(text = dif.names,con = "mothur.remove.groups")


# Filter out non-samples (i.e. water, mock) and non-bacteria
verruco <-
  mothur.merge %>%
  subset_taxa(
    Kingdom == "Bacteria" &
      Family != "mitochondria" &
      Class != "Chloroplast"
  ) %>%
  subset_samples(
      Blank == "no" &
      !Fraction %in% c("cFree", "cParticle") & 
      Lake != "Huron" &
        Depth != "DCM"
  ) 

# Also prune out taxa which were only present in removed samples
verruco <- prune_taxa(taxa_sums(verruco) > 0, verruco)

#save(verruco, file = "VerrucoData.RData")

