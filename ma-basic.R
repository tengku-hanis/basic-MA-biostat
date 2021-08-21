# Install packages
if (!require("meta")) {
  install.packages("meta")
}
if (!require("devtools")) {
  install.packages("devtools")
}
if (!require("dmetar")) {
  devtools::install_github("MathiasHarrer/dmetar", upgrade = "never")
}

# Load packages
library(meta)
library(dmetar)

# Data
?DepressionMortality
DepressionMortality

# Fixed and random effect model ----
ma <- metabin(event.e = event.e, 
              n.e = n.e,
              event.c = event.c,
              n.c = n.c, 
              studlab = author,
              data = DepressionMortality,
              method.tau = "PM", #estimator
              comb.fixed = T, 
              comb.random = T,
              prediction = T, 
              hakn = T, #reduce false positive
              adhoc.hakn = "iqwig6") #adjust the possible narrow ci caused by hakn
ma

# Forest plot ----
forest(ma, sortvar = TE)

# Publication bias
ma2 <- update.meta(ma, comb.fixed = F)

metabias(ma2, plotit = T, method.bias = "Egger")
metabias(ma2, plotit = T, method.bias = "Begg")
metabias(ma2, plotit = T, method.bias = "peters")

# Funnel plot ----
funnel(ma2, studlab = T)

# Outlier ----
find.outliers(ma2)

# Influential diagnostic ----
ma_inf <- InfluenceAnalysis(ma2, random = T)
ma_inf

plot(ma_inf, "baujat")
plot(ma_inf, "influence")

# Extra ----
## metagen ----

# data for metagen
dat_metagen <- data.frame(
  studlab = DepressionMortality$author,
  logrr = ma2$TE,
  se = ma2$seTE,
  ci_lower = ma2$lower,
  ci_upper = ma2$upper
)

# data witth NAs
dat_metagen$se[c(3, 8)] <- NA
dat_metagen

# random effect model
ma_metagen <- metagen(TE = logrr,
                      seTE = se,
                      lower = ci_lower,
                      upper = ci_upper,
                      studlab = studlab,
                      data = dat_metagen,
                      sm = "RR",
                      method.tau = "PM",
                      comb.fixed = F,
                      comb.random = T,
                      prediction = T,
                      hakn = T,
                      adhoc.hakn = "iqwig6") 
summary(ma_metagen)
summary(ma2)

## use metafor ----
library(metafor)

# random effect model
ma_metafor <- rma(yi = ma2$TE,
                  sei = ma2$seTE,
                  measure = "RR",
                  method = "PM",
                  test = "knha")
ma_metafor
predict(ma_metafor, transf = exp, digits = 2)

# baujat plot from metafor
baujat(ma_metafor)
ma2$studlab[c(13,14)]

TESTING2