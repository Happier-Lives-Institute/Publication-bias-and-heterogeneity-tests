
# Get the wrangled data
source("CT/CT_wrangle.R")

################################################################################
#                               Data fiddling                                  #
################################################################################

# We need a variable that has more legible output for sample size.
# So let's make a new "n" to be per 100 people included.
dat %<>% mutate(n_100 = N / 100)

# SE is artificially correlated to d / SMD, which effects several tests.
# So it's best to include the corrected SE as well.

dat %<>% mutate(
    SE_corrected = if_else(is.na(T_n), sqrt(((N/2)*(N/2)) / ((N/2)*(N/2))),
                           sqrt((T_n   + C_n) / (T_n  * C_n))),
    SE_corrected_sq = SE_corrected^2
) #%>%
#select(d_se, SE_corrected) ##They seem in the ballpark of one another.

# I'm concerned that some of these tests for publication bias will not work appropriately
# When given nested data. As sanity check, let's also run these with the only the first
# Follow-up included -- an approach more in line with typical meta-analyses.
# First this means making a variable to indicate which follow-up is first.
dat %<>% group_by(InLineCitation) %>%
    mutate(AVGsinceCTbegan.mo = as.double(AVGsinceCTbegan.mo),
           FUPisFirst = if_else(AVGsinceCTbegan.mo == min(AVGsinceCTbegan.mo), 1 ,0)) %>% ungroup()

# Does this look right?
dat %>% group_by(InLineCitation,AVGsinceCTbegan.mo, FUPisFirst) %>% summarise(n = n()) %>% ungroup()
# Looks like it's working as it should.

# So let's create a simple non-nested version of the dataset.
dat_noFU <- dat %>% filter(FUPisFirst == 1)

################################################################################
#                               model preparation                              #
################################################################################

# We have 2 models (random, fixed)
# x 3 data selections (full, without outliers, without follow-ups)

# ------------
# Random Effects
# ------------
m.RE.full <- metagen(
    TE = d,
    seTE = d_se,
    studlab = InLineCitation,
    data = dat,
    fixed = F,
    random = T,
    prediction = T,
    method.ci = "t",
    method.tau = "REML"
); m.RE.full

# getting the outliers with the CI method #
m.RE.outliers <- dmetar::find.outliers(m.RE.full); m.RE.outliers
m.RE.outliers$out.study.random
dat_RE_noout <- dat %>% filter(InLineCitation %ni% m.RE.outliers$out.study.random)

m.RE.noout <- metagen(
    TE = d,
    seTE = d_se,
    studlab = InLineCitation,
    data = dat_RE_noout,
    fixed = F,
    random = T,
    prediction = T,
    method.ci = "t",
    method.tau = "REML"
); m.RE.noout

# Without the follow-up data
m.RE.noFU <- metagen(
    TE = d,
    seTE = d_se,
    studlab = InLineCitation,
    data = dat_noFU,
    fixed = F,
    random = T,
    prediction = T,
    method.ci = "t",
    method.tau = "REML"
); m.RE.noFU

# ------------
# Fixed effects
# ------------

m.FE.full <- metagen(
    TE = d,
    seTE = d_se,
    studlab = InLineCitation,
    data = dat,
    fixed = T,
    random = F,
    method.ci = "t"
); m.FE.full

# getting the outliers with the CI method #
m.FE.outliers <- dmetar::find.outliers(m.FE.full); m.FE.outliers
m.FE.outliers$out.study.fixed
# the list of outliers is different...
dat_FE_noout <- dat %>% filter(InLineCitation %ni% m.FE.outliers$out.study.fixed)

m.FE.noout <- metagen(
    TE = d,
    seTE = d_se,
    studlab = InLineCitation,
    data = dat_FE_noout,
    fixed = T,
    random = F,
    method.ci = "t"
); m.FE.noout

# Without the follow-up data
m.FE.noFU <- metagen(
    TE = d,
    seTE = d_se,
    studlab = InLineCitation,
    data = dat_noFU,
    fixed = T,
    random = F,
    method.ci = "t"
); m.FE.noFU

