# Packages
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
              hakn = T)
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
