df <- crab_condition%>%
  select(-c(sn, sex,Site))%>%
  rename_with(~ gsub(" ", "", .x))%>%
  rename_with(~gsub("[()]", "", .x))

formulae <- lapply(colnames(df)[-1],
                   function(x) as.formula(paste0(x, "~Month")))
library(broom)

res <- lapply(formulae, function(x) summary.aov(aov(x, data = df)))

names(res) <- format(formulae)
res

df_sg <- df%>%
  select(-K)

formulae <- lapply(colnames(df_sg)[-1],
                   function(x) as.formula(paste0(x, "~Month")))
library(agricolae)
ress <- lapply(formulae, function(x) duncan.test(aov(x, data = df_sg), "Month"))

names(ress) <- format(formulae)
ress
plot(ress$`Weightg ~ Month`, variation = "SE", main = "Weight (g)")
plot(ress$`lengthcm ~ Site`, variation = "SE", main = "Length (cm)")
plot(ress$`largechelaecm ~ Site`,variation = "SE", main = "Large Chelae (cm)")
plot(ress$`smallchelaecm ~ Site`,variation = "SE", main = "Small Chelae (cm) ")
