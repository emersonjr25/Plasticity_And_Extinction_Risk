#### VERIFY CORRELATION BETWEEN PLASTICITY AND EXTINCTION RISK ####
library(tidyverse)

hedges <- read.csv2('data/hedges.csv')
extinction_risk<- read.csv2('data/base_de_dados_risco_de_extincao.csv')

hedges <- hedges %>%
  select(-X) %>%
  mutate(risk = NA, risk_two = NA)

for(i in seq_along(extinction_risk$Species.Genus)){
  temporary <- extinction_risk$Species.Genus[i] == hedges$species_complete
  if(sum(temporary) >= 1){
    hedges$risk[temporary] <- extinction_risk$Extinction.risk[i]
    hedges$risk_two[temporary] <- extinction_risk$Resume[i]
  }
}

### WITHOUT ABS ###
hedges <- hedges %>%
  as.tibble() %>%
  filter(!is.na(risk), !is.na(risk_two)) %>%
  mutate(risk = as.factor(risk), 
         risk_two = as.factor(risk_two))

plot(hedges$risk, hedges$hedgesg)
plot(hedges$risk_two, hedges$hedgesg)

hedges_filter <- hedges %>%
  filter(hedges$hedgesg > as.numeric(summary(hedges$hedgesg)[2]),
         hedges$hedgesg < as.numeric(summary(hedges$hedgesg)[5]))

plot(hedges_filter$risk, hedges_filter$hedgesg)
plot(hedges_filter$risk_two, hedges_filter$hedgesg)

### WITH ABS ###
hedges_abs <- hedges %>%
  mutate(hedgesg = abs(hedgesg))

hedges_filter2 <- hedges_abs %>%
  filter(hedges_abs$hedgesg > as.numeric(summary(hedges_abs$hedgesg)[2]),
         hedges_abs$hedgesg < as.numeric(summary(hedges_abs$hedgesg)[5]))

plot(hedges_filter2$risk, hedges_filter2$hedgesg)
plot(hedges_filter2$risk_two, hedges_filter2$hedgesg)
