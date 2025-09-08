library(corrr)
################################################################
##### Condition Factor
################################################################
cf <- function(x,y){
  up = x
  down = y^3
  r = up/down
  return(r * 100)
}

crab_condition <- crab%>%
  mutate(K = cf(`Weight (g)`, `length (cm)`))
####################################################################
crab_corr <- crab%>%
  select(-sn)%>%
  correlate(method = "spearman")%>%
  column_to_rownames(., var = "term")%>%
  round(2)
write.csv(crab_corr, "crab_correlation_table.csv")


ala_corr <- crab%>%
  select(-sn)%>%
  filter(Site == "Ala")%>%
  correlate(method = "spearman")%>%
  column_to_rownames(., var = "term")%>%
  round(2)
write.csv(ala_corr, "ala_correlation_table.csv")  


arugu_corr <- crab%>%
  select(-sn)%>%
  filter(Site == "Futa")%>%
  correlate(method = "spearman")%>%
  column_to_rownames(., var = "term")%>%
  round(2)
write.csv(arugu_corr, "arugu_correlation_table.csv")  

elegbin_corr <- crab%>%
  select(-sn)%>%
  filter(Site == "Elegbin")%>%
  correlate(method = "spearman")%>%
  column_to_rownames(., var = "term")%>%
  round(2)
write.csv(elegbin_corr, "elegbin_correlation_table.csv")  

male_corr <- crab%>%
  select(-sn)%>%
  filter(sex == "M")%>%
  correlate(method = "spearman")%>%
  column_to_rownames(., var = "term")%>%
  round(2)
write.csv(male_corr, "male_correlation_table.csv")  

female_corr <- crab%>%
  select(-sn)%>%
  filter(sex == "F")%>%
  correlate(method = "spearman")%>%
  column_to_rownames(., var = "term")%>%
  round(2)
write.csv(female_corr, "female_correlation_table.csv")  

###############################################################
##### ANOVA
###############################################################
df <- crab_condition%>%
  select(-c(sn, Month, sex))%>%
  rename_with(~ gsub(" ", "", .x))%>%
  rename_with(~gsub("[()]", "", .x))

formulae <- lapply(colnames(df)[2:ncol(df)],
                   function(x) as.formula(paste0(x, "~Site")))
library(broom)

res <- lapply(formulae, function(x) tidy(aov(x, data = df)))

names(res) <- format(formulae)
res

df_sg <- df%>%
  select(c(Site,Weightg, lengthcm, largechelaecm, smallchelaecm))

formulae <- lapply(colnames(df_sg)[2:ncol(df_sg)],
                   function(x) as.formula(paste0(x, "~Site")))
library(agricolae)
ress <- lapply(formulae, function(x) duncan.test(aov(x, data = df_sg), "Site"))

names(ress) <- format(formulae)
ress
plot(ress$`Weightg ~ Site`, variation = "SE", main = "Weight (g)")
plot(ress$`lengthcm ~ Site`, variation = "SE", main = "Length (cm)")
plot(ress$`largechelaecm ~ Site`,variation = "SE", main = "Large Chelae (cm)")
plot(ress$`smallchelaecm ~ Site`,variation = "SE", main = "Small Chelae (cm) ")
