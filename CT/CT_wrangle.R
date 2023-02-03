source("_dependencies/dependencies.R")

#---------
# Get and clean data
#---------

dat <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1Ilr6aq3ilhYbF8KnQQ5xoFGgEs1eX6bPgKqDKylzPNk/edit?usp=sharing",
    sheet = "AllOutcomesRR copy2")
glimpse(dat)

# Rename and prepare the data
# The ifelse statements with unlisting are there b/c:
# - there is a mix of strings, null values, and numeric values in these variables
# - when unlisted, null values disappear instead of being coerced into NA,
#   creating column size issues
dat %<>% mutate(
    CTvalAsLumPPP = as.numeric(unlist(CTvalAsLumPPP))
    , CT_PPP = as.numeric(unlist(CT_PPP))
    , CT_lumpMER = as.numeric(unlist(CT_lumpMER))
    , CT_MER = as.numeric(unlist(CT_MER))
    , yearsSince = as.numeric(unlist(yearsSince))
    , ScaleLowerBound = as.numeric(na_if(unlist(ScaleLowerBound), "NA"))
    , ScaleUpperBound = as.numeric(na_if(unlist(ScaleUpperBound), "NA"))
)

# Add dosage and identity
dat %<>% mutate(
    PPP36  = if_else(CTisLump == 1, CTvalAsLumPPP / 36, CT_PPP),
                 PPP12  = if_else(CTisLump == 1, CTvalAsLumPPP / 12, CT_PPP),
                 PPP24k = PPP24 / 1000,
                 PPP12k = (PPP12) / 1000,
                 PPP36k = (PPP36) / 1000,
                 LumpValPPPk = CTvalAsLumPPP / 1000 ,
                 isGD        = if_else(Program == "GiveDirectly", 1,0))

# Add SDp
dat %<>% mutate(
    scale_length = ScaleUpperBound - ScaleLowerBound
    , SDp = abs(ES / dt)
) %>% mutate(
    # Fix certain elements of the cash transfer studies based on the details
    # found in the studies.
    MeasureDescript = if_else(Study_ID == 52 & MeasureType == "MH", "GDS-15", MeasureDescript),
    SDp =          if_else(InLineCitation == "Powell-Jackson et al, 2016",
                           getPooledSD(1589, 5, 106, 6.2), SDp), # Table 1 page 213
    scale_length = if_else(InLineCitation == "Powell-Jackson et al, 2016",
                           40                            , scale_length),
    SDp =          if_else(InLineCitation == "Ozer et al, 2011", # see table 2 p. 1572
                           getPooledSD(1087, 10.2, 3539, 9.8), SDp),
    SDp =          if_else(InLineCitation == "Chen  et al., 2019", # see table 1 p. 29
                           8.622, SDp),
    scale_length = if_else(InLineCitation == "Berhane et al., 2016",
                           20 , scale_length)
)

#---------
# Finishing touches
#---------

# Give ids
dat$dataPointId <- 1:nrow(dat)
dat$dataPointId <- paste0("id_",dat$dataPointId)

# Rename d
dat <- dat %>% rename(d=dt)

# Filter data
dat %<>%
    filter(!is.na(d) & !is.na(yearsSince))
