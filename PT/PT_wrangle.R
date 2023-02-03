source("_dependencies/dependencies.R")

#---------
# Get and clean data
#---------

dat <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1jyi44_LxAxxuHoqWRDYjfG4I3qVabILDYOvRrjXol-k/edit?usp=sharing",
    sheet = "Before 23.02.2022 Edits Data", na = "")
glimpse(dat)

# Rename and prepare the data
# The ifelse statements with unlisting are there b/c:
# - there is a mix of strings, null values, and numeric values in these variables
# - when unlisted, null values disappear instead of being coerced into NA,
#   creating column size issues
dat <- dat %>%
    rename(
        AvgSessionTime = `Avg. Session Time Min`
        , CostPP = `$Cost Per Person`
    ) %>%
    rowwise() %>%
    mutate(FollowUp           = as.numeric(FollowUp),
           SE                 = as.numeric(SE),
           TrainingLengthDays = ifelse(
               is.null(unlist(TrainingLengthDays)),
               NA,
               as.numeric(unlist(TrainingLengthDays))
               ),
           AvgSessionTime     = ifelse(
               is.null(unlist(AvgSessionTime)),
               NA,
               as.numeric(unlist(AvgSessionTime))
           ),
           CostPP             = ifelse(
               is.null(unlist(CostPP)),
               NA,
               as.numeric(unlist(CostPP))
           ),
           TotalTimeInSession = Sessions * AvgSessionTime,
           FollowUp           = FollowUp / 12, #convert from months to years
           d                  = abs(d),
           d                  = if_else(
               Authors == "Haushofer et al., 2020",
               -d,
               d), #only w/ neg. effects
           period             = if_else(
               FollowUp < 2,
               "shortrun",
               "longrun"  ),
           Layness            = if_else(
               Authors == "Hamdani et al., 2021",
               1.5,
               Layness),
           Expertise          = as.factor(if_else(
               Layness >= 1, "non-specialist", "specialist"))
           ) %>% ungroup()

# Add dosage
dat %<>% mutate(
    TotalHoursInSession = if_else(is.na(AvgSessionTime),
                                  Sessions, TotalTimeInSession / 60))

# Add SDp
dat %<>% mutate(
    scale_length = upperScaleRange - lowerScaleRange
    , SDp = getPooledSD(n.t, sd.t, n.c, sd.c)
) %>% mutate(
    # If can't get SD pooled via SDs, use mean diff and d (usually obtained via t)
    SDp = ifelse(is.na(SDp), abs(b/d), SDp)
)

#---------
# Finishing touches
#---------

# Filter data
dat %<>%
    filter(!is.na(d) & !is.na(FollowUp))

# Add an effect size index
dat$dataPointId <- 1:nrow(dat)
dat$dataPointId <- paste0("id_",dat$dataPointId)
