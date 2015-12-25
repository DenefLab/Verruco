# Functions to make metadata for Verruco project
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

makeLMSite <- function(x) {
	ifelse(grepl("MLB", x), "Buoy", 
	ifelse(grepl("M110", x), "Far",
	ifelse(grepl("M15", x), "Shore",
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
		mutate(Site = "Michigan")  %>%				
		mutate(Date = substr(SampleID, 1, 4)) %>%	
		mutate(Lake = makeLMLake(SampleID)) %>%
		mutate(Depth = makeLMDepth(SampleID)) %>%
		mutate(Fraction = makeLMFraction(SampleID))
}
			
			

#########################################################################
########################## Inland Lakes #################################
#########################################################################


#Add Depth column
makeILDepth <- function(df) {
	ifelse(df$Site == "Epi" | df$Site == "Inlet", "Top",
	ifelse(df$Site == "Hypo" | df$Site == "Outlet", "Bottom",
		NA))
}


#Add Fraction column
makeILFraction <- function(df) {
	ifelse(grepl("F", df$SampleID), "Free",
	ifelse(grepl("P", df$SampleID) & df$Blank == "no", "Particle",  # Gets rid of collision with PBS samples
		NA))
}


makeInlandData <- function(full.df, inl.df) {

	inl.df <- inl.df	# placeholder

	df.inland <-
		full.df %>%	
		filter(Study == "Inland") %>%	
		#left_join(inl.df, by = c("SampleID" = "Sample")) %>%	# To be fixed	 	
		mutate(Duplicates = SampleID) %>% 		# No duplicates in this study
		mutate(Group = "Inland") %>%					
		mutate(Site = NA) %>%					# To be fixed
		mutate(Date = NA) %>%					# To be fixed
		mutate(Lake = NA) %>%					# To be fixed
		mutate(Depth = NA) %>%		# To be fixed
		mutate(Fraction = makeILFraction(.))
}


# Note: there appears to be only 1 sample with a replicate? Z14.015.F1 and Z14.015.F2
# inalnd_metadata does not contain all samples . . . 
#########################################################################
########################## Muskegon #####################################
#########################################################################


# Add Date column
makeMuskDate <- function(x) {
	ifelse(grepl("514", x), "Sp14",
	ifelse(grepl("714", x), "Su14",
	ifelse(grepl("914", x), "Fa14",
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
	fract <- matrix(unlist(strsplit(x, "[.]")), ncol = 3, byrow = TRUE)[,2]
	
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
		mutate(Site = makeMuskSite(SampleID)) %>%
		mutate(Date = makeMuskDate(SampleID)) %>%
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


makeTbData <- function(df) {

	df.TB <-
		df %>% 
		filter(Study == "ThunderBay")%>%
		mutate(Duplicates = SampleID) %>%		# No duplicates in this study
		mutate(Group = "Laurentian") %>%
		mutate(Site = char2end(id, ".", 2)) %>%
		mutate(Date = substr(SampleID, 1, 4)) %>%
		mutate(Lake = "Huron") %>%
		mutate(Depth = makeTBDepth(id)) %>%
		mutate(Fraction = makeTBFraction(id))		

}


#########################################################################
########################## No study ##################################
#########################################################################


makeNonData <- function(df) {
	df.none <-
		df %>%
		filter(is.na(Study)) %>%
		mutate(Duplicates = NA) %>%		
		mutate(Group = NA) %>%
		mutate(Site = NA) %>%
		mutate(Date = NA) %>%
		mutate(Lake = NA) %>%
		mutate(Depth = NA) %>%
		mutate(Fraction = NA)
}

 