# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
library(BIRDIE)
library(CWAC)
library(ABAP)
library(dplyr)
library(ggplot2)
library(stringr)
library(sf)
library(futile.logger)
library(NicheMapR)

# --- STEPS --- #

# Build and calibrate model
# fit random walk
# add covariates

# Forecast
  # JAGS + NAs
  # propogation in R

# validate against 1st chunkof held out data

# Iterate
  # Refine JAGS
  # Data assimilation

# Automate (exercise 4)

spp_list <- listCwacSpp()

# Download all counts for bird island
bird_counts <- getCwacSiteCounts("32041818")

# Get list of all CWAC species
spp_list <- listCwacSpp()

# Get ID data for Cape Gannet
genus <- "Morus"
sp_epithet <- "capensis"
sp_counts_gannets <- bird_counts %>% 
  filter(Genus == genus & Species == sp_epithet) %>% 
  filter(Season == "S")

colnames(sp_counts_gannets)
summary(sp_counts_gannets)

sp_counts_gannets_filtered <- sp_counts_gannets %>% 
  select(StartDate, TimeStart, TimeEnd, Count, Pairs, X, Y)

write.csv(sp_counts_gannets_filtered, 
          "./sp_counts_gannets_filtered.csv")

sp_counts_gannets_filtered %>%
  ggplot(aes(x = StartDate, y = Count)) +
  geom_line() +
  geom_point() +
  # labs(title = "Hourly Data",
  #      x = "Date",
  #      y = "Value") +
  theme_minimal()

# Get list of all CWAC sites in the Western Cape
sites <- listCwacSites(.region_type = "province", 
                       .region = "Western Cape")

# Select data for the Eerste River Estuary only
bird_island <- sites %>% filter(LocationName == "Lamberts Bay: Bird Island")
bird_island

# # Get pentad for Bird Island (this is just an easy way to get a "polygon" for our study site so that we can get environmental covariate data later)
# pentads_wc <- getRegionPentads(.region_type = "province", 
#                                .region = "Western Cape") #Get all WC pentads
# pentad_id <- findPentadFromCoords(as.numeric(bird_island$Y), 
#                                  as.numeric(bird_island$X)) #Get ID for Eerste River Estuary
# pentad_site <- pentads_wc %>%
#   filter(pentad == pentad_id)
