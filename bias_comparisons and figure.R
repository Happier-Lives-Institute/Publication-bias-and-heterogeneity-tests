################################################################################
#                                   Load the data                              #
################################################################################

source("_dependencies/dependencies.R")

# Get the data
dat <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1K7AafL6qWWW6lhR7EaUeYoMeTaLaq-rrw3Y0D54LpD8/",
    sheet = "comparison of publication bias corrections", skip = 2)
glimpse(dat)

# Clean up some names
dat <- dat %>%  mutate(
    PublicationBiasMethod =
        case_when(
            PublicationBiasMethod == "PET (using rma for additive error)" ~ "PET",
            PublicationBiasMethod == "PEESE (using rma for additive error)" ~ "PEESE",
            PublicationBiasMethod == "Selection model: Three-Parameter Selection Model" ~ "Selection Model",
            TRUE ~ PublicationBiasMethod
        )
)

# Take out tests that don't really work
dat <- dat %>% filter(ID %ni% c(28,29,30,34,35,36))

################################################################################
#                                      analysis                                #
################################################################################

# Only get the changes
dat.analysis <- dat %>% filter(PublicationBiasMethod != "no method")

# Average reduction of PT effect
mean(dat.analysis$perc_ref_PT)
sd(dat.analysis$perc_ref_PT)
median(dat.analysis$perc_ref_PT)

# Average reduction of all CT effect
mean(dat.analysis$perc_ref_allGD)
sd(dat.analysis$perc_ref_allGD)
median(dat.analysis$perc_ref_allGD)

# Average c-e ratio
mean(dat.analysis$ce_change_allCTs)
sd(dat.analysis$ce_change_allCTs)
median(dat.analysis$ce_change_allCTs)

# More general information
min(dat.analysis$ce_change_allCTs)
max(dat.analysis$ce_change_allCTs)

dat.analysis <- dat.analysis %>% mutate(category = case_when(
    ce_change_allCTs > 9.4 ~ "9.4+",
    ce_change_allCTs < 9.4 & ce_change_allCTs > 7 ~ "7-9.4",
    ce_change_allCTs < 7 & ce_change_allCTs > 5 ~ "5-7",
    ce_change_allCTs < 5 ~ "3-5",
))

table(dat.analysis$category)

################################################################################
#                                      Graph                                   #
################################################################################

# Prepare graph
p <-
    # Prepare the data #
    dat %>% arrange(ce_change_allCTs) %>%

    # Graph Data #
    ggplot(aes(
        x    = ce_change_allCTs,
        y    = reorder(ID, -ce_change_allCTs),
        fill = PublicationBiasMethod,
        pattern = RandomOrFixed
    )) +

    geom_col_pattern(position = position_dodge(preserve = "single"),
                     pattern_fill = "black",
                     pattern_angle = 45,
                     pattern_density = 0.1,
                     pattern_spacing = 0.025,
                     pattern_size = 0.1,
                     pattern_key_scale_factor = 0.6) +

    # geom_col() +
    geom_vline(xintercept = 9.4) +

    # Graph graphics #
    theme_classic() +

    xlab("Psychotherapy's cost-effectiveness in multiples of cash (adjusted for publication bias)") + ylab("ID") +

    # THEME() Specifies details of the plot such as text size and alignment.
    # Element blank used to delete the plot element.
    theme(
        text                 = element_text(size = 12),
        #axis.title.x         = element_blank(),
        axis.text.x          = element_text(size = 12),
        axis.text.y          = element_text(hjust = 0, size = 12),
        axis.ticks.y         = element_blank(),
        axis.line.y          = element_blank(),
        panel.grid.major.y   = element_blank(),
        panel.grid.major     = element_line(colour = "white"),
        panel.grid.minor     = element_line(colour = "white"),
        strip.background     = element_blank(),
        strip.text.y         = element_text(size = 12,
                                            colour = "black",
                                            face="bold"),

        # Below specifies details of legends appearance
        legend.position= c(0.7,0.5),
        legend.justification = "center",
        legend.margin        = margin(6, 6, 6, 6),
        legend.text          = element_text(size = 10),
        legend.text.align    = 0,
        legend.title         = element_text(size=11, face = "bold"),
        legend.key           = element_rect(size = 0.4),
        legend.background    = element_blank(),
        legend.box.just      = "left",
        legend.box.background= element_rect(colour = "black")
    ) +
    scale_fill_brewer(palette="Set2") +
    scale_pattern_manual(values = c(Fixed = "none", Random = "stripe")) +
    guides(pattern = guide_legend(override.aes = list(fill = "white")),
           fill = guide_legend(override.aes = list(pattern = "none"))); p

# Save plot
ggsave(filename = "bias_comparisons.png",
       plot = p,
       width = 8,
       height = 8,
       dpi = 1200)

# wrap the plot
wrap.p <- p + facet_wrap(~DataSelection) + theme(legend.position = "none")

ggsave(filename = "bias_comparisons_wrap.png",
       plot = wrap.p,
       width = 8,
       height = 8,
       dpi = 1200)
