# Functions to make metadata for Verruco project
library(dplyr)
library(qdap)

#########################################################################
########################## Lake Michigan ################################
#########################################################################

makeLMGroup <- function(x) {
	ifelse(grepl("ML", x), "Estuary", 
	ifelse(grepl("MM", x), "Laurentian",
		NA))
}

makeLMnames <- function(x) {
	beg2char(x, ".", 4)
}


makeLMLake <- function(x) {
	ifelse(grepl("MM", x), "Michigan", 
	ifelse(grepl("ML", x), "Muskegon",
		NA))
}

makeLMStation <- function(x) {
	ifelse(grepl("MLB", x), "Buoy", 
	ifelse(grepl("M110", x), "Far",
	ifelse(grepl("M15", x), "Shore",
		NA)))
}

makeLMSeason <- function(x) {
  ifelse(grepl("Su", x), "Summer", 
  ifelse(grepl("Sp", x), "Spring",
  ifelse(grepl("Fa", x), "Fall",
    NA)))
}

makeLMFraction <- function(x) {
	ifelse(grepl("BD", x), "Free", 
	ifelse(grepl("ED", x), "Particle",
	ifelse(grepl("Bc", x), "cFree",
	ifelse(grepl("Ec", x), "cParticle",
		NA))))
}

makeLMDepth <- function(x) {
	ifelse(grepl("SD", x), "Top", 
	ifelse(grepl("SN", x), "Top",
	ifelse(grepl("DN", x), "Deep",
	ifelse(grepl("DD", x), "Deep",
	ifelse(grepl("DCM", x), "DCM",
		NA)))))
}

makeMichiganData <- function(df) {
	
	df.lm <- 
		df %>%
		filter(Study == "LakeMichigan") %>%
		mutate(Duplicates = makeLMnames(SampleID)) %>% 		
		mutate(Group = makeLMGroup(SampleID)) %>%	
		mutate(Station = makeLMStation(SampleID))  %>%	
	  mutate(Season = makeLMSeason(SampleID)) %>%					
	  mutate(Year = "2013") %>%
		mutate(Lake = makeLMLake(SampleID)) %>%
		mutate(Depth = makeLMDepth(SampleID)) %>%
		mutate(Fraction = makeLMFraction(SampleID))
}
			
			

#########################################################################
########################## Inland Lakes #################################
#########################################################################


#Add Depth column
makeILDepth <- function(df) {
	ifelse(df$Station == "Epi" | df$Station == "Inlet", "Top",
	ifelse(df$Station == "Hypo" | df$Station == "Outlet", "Bottom",
		NA))
}


#Add Fraction column
makeILFraction <- function(df) {
	ifelse(grepl("F", df$SampleID), "Free",
	ifelse(grepl("P", df$SampleID) & df$Blank == "no", "Particle",  # Gets rid of collision with PBS samples
		NA))
}


makeInlandData <- function(full.df, inl.df) {

	df.subset <-
		full.df %>%	
		filter(Study == "Inland" & Blank == "no") 

	# Grap substring of SampleID, removing fraction
	df.subset$sampnum <- substr(df.subset$SampleID, 1, 7)

	joined.df <- left_join(df.subset, inl.df, by = c("sampnum" = "Sample"))

	newdf.inland <- 
		df.subset %>%	 	
		mutate(Duplicates = SampleID) %>% 		# No duplicates in this study
		mutate(Group = "Inland") %>%					
		mutate(Station = joined.df$Station) %>%					
		mutate(Season = joined.df$Season) %>%					
		mutate(Year = joined.df$Year) %>%
		mutate(Lake = joined.df$Lake) %>%					
		mutate(Depth = makeILDepth(joined.df)) %>%		
		mutate(Fraction = makeILFraction(.)) %>%
	  select(-sampnum)
}


# Note: there appears to be only 1 sample with a replicate? Z14.015.F1 and Z14.015.F2
# inalnd_metadata does not contain all samples . . . 
#########################################################################
########################## Muskegon #####################################
#########################################################################


# Add Season column
makeMuskSeason <- function(x) {
	ifelse(grepl("514", x), "Spring",
	ifelse(grepl("714", x), "Summer",
	ifelse(grepl("914", x), "Fall",
		NA)))
}



#Add Site column
makeMuskSite <- function(x) {
	ifelse(grepl("IN", x), "Inlet",
	ifelse(grepl("BR", x), "Bear",
	ifelse(grepl("OT", x), "Outlet",
	ifelse(grepl("DP", x), "Deep",
		NA))))	
}


# Add Depth column
makeMuskDepth <- function(x) {
	ifelse(grepl("E", x), "Top",
	ifelse(grepl("H", x), "Bottom",
	"Sediment"))
}


#Add Fraction column
makeMuskFraction <- function(x) {
	# Extract the letter between the two periods (i.e the 2nd column)
	x <- as.character(x)
	fract <- matrix(unlist(strsplit(x, "[.]")), ncol = 3, byrow = TRUE)[ ,2]
	
	# Conditional
	ifelse(fract == "F", "Free",
	ifelse(fract == "P", "Particle",
	ifelse(fract == "S", "Sediment",
		NA)))
}


makeMuskegonData <- function(df) {

	df.Muskegon <-
		df %>%
		filter(Study == "Muskegon") %>%
		mutate(Duplicates = SampleID) %>%
		mutate(Group = "Estuary") %>%
		mutate(Station = makeMuskSite(SampleID)) %>%
		mutate(Season = makeMuskSeason(SampleID)) %>%
		mutate(Year = "2014") %>%
		mutate(Lake = "Muskegon") %>%
		mutate(Depth = makeMuskDepth(SampleID)) %>%
		mutate(Fraction = makeMuskFraction(SampleID))

}



#########################################################################
########################## Thunder Bay ##################################
#########################################################################


# Add Depth column
makeTBDepth <- function(x) {
	ifelse(grepl("SD", x), "Top", 
	ifelse(grepl("SN", x), "Top",
	ifelse(grepl("DN", x), "Deep",
	ifelse(grepl("DD", x), "Deep",
		NA))))
}

# Add Fraction column
makeTBFraction <- function(x) {
	ifelse(grepl("BD.T", x), "Free",
	ifelse(grepl("BcD", x), "cFree",	
	ifelse(grepl("ED", x), "Particle",
		NA)))	
}

# Add Season column
makeTBSeason <- function(x) {
	ifelse(grepl("Sp", x), "Spring",
	ifelse(grepl("Su", x), "Summer",
		NA))
}


makeTbData <- function(df) {

	df.TB <-
		df %>% 
		filter(Study == "ThunderBay") %>%
		mutate(Duplicates = SampleID) %>%		# No duplicates in this study
		mutate(Group = "Laurentian") %>%
		mutate(Station = char2end(SampleID, ".", 2)) %>%
		mutate(Season = makeTBSeason(SampleID)) %>%
		mutate(Year = "2012") %>%
		mutate(Lake = "Huron") %>%
		mutate(Depth = makeTBDepth(SampleID)) %>%
		mutate(Fraction = makeTBFraction(SampleID))		

}


#########################################################################
########################## No study ##################################
#########################################################################


makeNonData <- function(df) {
	df.none <-
		df %>%
		filter(is.na(Study) | Blank == "yes") %>%
		mutate(Duplicates = NA) %>%		
		mutate(Group = NA) %>%
		mutate(Station = NA) %>%
		mutate(Season = NA) %>%
		mutate(Year = NA) %>%
		mutate(Lake = NA) %>%
		mutate(Depth = NA) %>%
		mutate(Fraction = NA)
}

 