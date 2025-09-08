df <- crab_condition%>%
  select(-c(sn, Month,Site))%>%
  rename_with(~ gsub(" ", "", .x))%>%
  rename_with(~gsub("[()]", "", .x))
columns <- colnames(df)
columns <- columns[-7]

formulae <- lapply(colnames(df)[-7],
                   function(x) as.formula(paste0(x, "~sex")))
library(broom)

res <- lapply(formulae, function(x) summary.aov(aov(x, data = df)))

names(res) <- format(formulae)
res


#### No significant difference
