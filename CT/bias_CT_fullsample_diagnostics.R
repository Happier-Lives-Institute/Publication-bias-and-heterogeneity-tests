################################################################################
#                                preparation                                   #
################################################################################

source("CT/bias_CT_fullsample_preparation.R")

# Remake the simple RE model in rma
# Need for some graphs
mymodel <- rma(yi = d, sei = d_se,  slab = InLineCitation,
               data = dat, test = "t", method="REML")

################################################################################
#                                   tests                                      #
################################################################################

# Egger's test
metabias(m.RE.full, method.bias = "linreg")

# Pustejovsky-Rodgers Approach
m.RE.full$n.e = dat$T_n; m.RE.full$n.c = dat$C_n
metabias(m.RE.full, method.bias = "Pustejovsky")

# pcurve
pcurve(m.RE.full)

################################################################################
#                                  graphs                                      #
################################################################################

# ------------
# Forest plot
# ------------

forest(
    mymodel,
    order = dat$d,
    xlim=c(-1.5,3),
    cex=.7, header="Author(s) and Year", mlab="")

# Need to use the export to image option from R to save
# I set the height to 1000px, preserving ratio

# ------------
# Funnel plot
# ------------

the_funnel <- metaviz::viz_funnel(mymodel, egger = TRUE,
                                  xlab = "Cohen's d", text_size = 5,
                                  contours_col = "Greys")
the_funnel

# Convert funnel plot into ggplot object so it can be ggarranged.
fun1 <- the_funnel + ggtitle("(a) Funnel Plot") + theme_classic() +
    theme(plot.title  = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12))

ggsave(filename = "CT/CT_fullsample_funnel.png",
       plot = fun1,
       width = 8,
       height = 5,
       dpi = 1200)

# ------------
# P curve
# ------------

p_obj <- tibble("studlab" = dat$InLineCitation,
                "seTE"    = dat$d_se,
                "TE"      = dat$d)

the_p <- dmetar::pcurve(p_obj)
summary(the_p) # no significant evidence of p-hacking.

p_data <- the_p$PlotData # Extract data to put into ggplot.

# long data works better in ggplot
melted_p <- pivot_longer(p_data, cols = - `p-value`) %>%
    mutate(value = round(value, digits =1))

# Generates new p_curve plot from p_curve model results
the_curve <- ggplot(melted_p, aes(x = `p-value`, y = value, color = name)) +
    geom_line(aes(linetype = name), size = 1.5) + theme_classic() +
    geom_point(data = filter(melted_p,  name == "Observed (blue)")) +
    geom_text( data = filter(melted_p,  name == "Observed (blue)"),
               aes(label = paste(value, "%", sep = "")),
               size = 3.25, color = "Black",    vjust = -1.25 ) +
    ylab("Percentage of test results") +
    scale_color_manual(values=c('grey40', 'indianred3', "grey10"),
                       name = "Curve",
                       labels = c("Curve under null of 0% power",
                                  "Observed p-curve",
                                  "Curve under null of 30% power")) +
    ggtitle("(b) P-curve")  +
    theme(legend.position = c(.90, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12)) +
    scale_linetype_manual(values=c("dotted", "solid", "twodash")) +
    guides(linetype = FALSE) +ylim(NA, 80) + xlim(0.009,NA)

the_curve

ggsave(filename = "CT/CT_fullsample_pcurve.png",
       plot = the_curve,
       width = 8,
       height = 5,
       dpi = 1200)
