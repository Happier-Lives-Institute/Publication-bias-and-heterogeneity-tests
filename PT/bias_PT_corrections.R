source("PT/bias_PT_preparation.R")

############################
#      Trim-and-fill
############################

# ------------
# Random effects
# ------------
tf.m.RE.full  <- trimfill(m.RE.full); tf.m.RE.full
tf.m.RE.noout <- trimfill(m.RE.noout); tf.m.RE.noout
tf.m.RE.noFU  <- trimfill(m.RE.noFU); tf.m.RE.noFU

# ------------
# Fixed effects
# ------------
tf.m.FE.full  <- trimfill(m.FE.full); tf.m.FE.full
tf.m.FE.noout <- trimfill(m.FE.noout); tf.m.FE.noout
tf.m.FE.noFU  <- trimfill(m.FE.noFU); tf.m.FE.noFU

############################
#       PET
############################

# ------------
# Random effects
# ------------
# trying something, not how this is intended to be used

PET.RE.full <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected,
                       data = dat, method = "REML"); PET.RE.full

PET.RE.noout <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected,
                        data = dat_RE_noout, method = "REML"); PET.RE.noout

PET.RE.noFU <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected,
                       data = dat_noFU, method = "REML"); PET.RE.noFU

# ------------
# Fixed effects
# ------------
# This is how PET-PEESE is intended to be used
PET.FE.full <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected,
                        data = dat, method = "FE"); PET.FE.full

PET.FE.noout <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected,
                        data = dat_FE_noout, method = "FE"); PET.FE.noout

PET.FE.noFU <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected,
                         data = dat_noFU, method = "FE"); PET.FE.noFU

############################
#       PEESE
############################

# ------------
# Random effects
# ------------
# trying something, not how this is intended to be used

PEESE.RE.full <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected_sq,
                         data = dat, method = "REML"); PEESE.RE.full

PEESE.RE.noout <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected_sq,
                          data = dat_RE_noout, method = "REML"); PEESE.RE.noout

PEESE.RE.noFU <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected_sq,
                         data = dat_noFU, method = "REML"); PEESE.RE.noFU

# ------------
# Fixed effects
# ------------
# This is how PET-PEESE is intended to be used

PEESE.FE.full <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected_sq,
                        data = dat, method = "FE"); PEESE.FE.full

PEESE.FE.noout <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected_sq,
                         data = dat_FE_noout, method = "FE"); PEESE.FE.noout

PEESE.FE.noFU <- rma.uni(yi = d, vi = SE^2, mods = ~SE_corrected_sq,
                        data = dat_noFU, method = "FE"); PEESE.FE.noFU

############################
#       Rucker's limit
############################

# ------------
# Random effect
# ------------
rl.m.RE.full  <- metasens::limitmeta(m.RE.full); rl.m.RE.full
rl.m.RE.noout <- metasens::limitmeta(m.RE.noout); rl.m.RE.noout
rl.m.RE.noFU  <- metasens::limitmeta(m.RE.noFU); rl.m.RE.noFU

# ------------
# Fixed effect
# ------------
rl.m.FE.full  <- metasens::limitmeta(m.FE.full); rl.m.FE.full
rl.m.FE.noout <- metasens::limitmeta(m.FE.noout); rl.m.FE.noout
rl.m.FE.noFU  <- metasens::limitmeta(m.FE.noFU); rl.m.FE.noFU

############################
#         P-curve
############################

# ------------
# Random effect
# ------------
pc.m.RE.full  <- pcurve(m.RE.full,
                         N=dat$n,
                         effect.estimation = T); pc.m.RE.full
pc.m.RE.noout <- pcurve(m.RE.noout,
                         N=dat_RE_noout$n,
                         effect.estimation = T); pc.m.RE.noout
pc.m.RE.noFU  <- pcurve(m.RE.noFU,
                         N=dat_noFU$n,
                         effect.estimation = T); pc.m.RE.noFU

# ------------
# Fixed effect
# ------------
pc.m.FE.full  <- pcurve(m.FE.full,
                        N=dat$n,
                        effect.estimation = T); pc.m.FE.full
pc.m.FE.noout <- pcurve(m.FE.noout,
                        N=dat_FE_noout$n,
                        effect.estimation = T); pc.m.FE.noout
pc.m.FE.noFU  <- pcurve(m.FE.noFU,
                        N=dat_noFU$n,
                        effect.estimation = T); pc.m.FE.noFU

############################
#      Selection models
############################

################################################################################
#                                  preparation                                 #
################################################################################
# Need to change all the m.gen models to their rma equivalent

# ------------
# random effects
# ------------
m.RE.full <- rma(yi = d,
                  sei = SE,
                  data = dat,
                  slab = Authors,
                  method = "REML",
                  test = "t"
)
m.RE.noout <- rma(yi = d,
                   sei = SE,
                   data = dat_RE_noout,
                   slab = Authors,
                   method = "REML",
                   test = "t"
)
m.RE.noFU <- rma(yi = d,
                  sei = SE,
                  data = dat_noFU,
                  slab = Authors,
                  method = "REML",
                  test = "t"
)


# ------------
# fixed effects
# ------------
m.FE.full <- rma(yi = d,
                 sei = SE,
                 data = dat,
                 slab = Authors,
                 method = "FE",
                 test = "t"
)
m.FE.noout <- rma(yi = d,
                  sei = SE,
                  data = dat_FE_noout,
                  slab = Authors,
                  method = "FE",
                  test = "t"
)
m.FE.noFU <- rma(yi = d,
                 sei = SE,
                 data = dat_noFU,
                 slab = Authors,
                 method = "FE",
                 test = "t"
)

################################################################################
#                      three-parameter selection model                         #
################################################################################

# ------------
# random effects
# ------------

sm3.m.RE.full  <- selmodel(m.RE.full,
                            type = "stepfun",
                            steps = 0.025); sm3.m.RE.full
sm3.m.RE.noout <- selmodel(m.RE.noout,
                            type = "stepfun",
                            steps = 0.025); sm3.m.RE.noout
sm3.m.RE.noFU  <- selmodel(m.RE.noFU,
                            type = "stepfun",
                            steps = 0.025); sm3.m.RE.noFU

# ------------
# fixed effects
# ------------

sm3.m.FE.full  <- selmodel(m.FE.full,
                           type = "stepfun",
                           steps = 0.025); sm3.m.FE.full
sm3.m.FE.noout <- selmodel(m.FE.noout,
                           type = "stepfun",
                           steps = 0.025); sm3.m.FE.noout
sm3.m.FE.noFU  <- selmodel(m.FE.noFU,
                           type = "stepfun",
                           steps = 0.025); sm3.m.FE.noFU

