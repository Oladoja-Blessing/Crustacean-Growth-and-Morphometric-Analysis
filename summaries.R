crab_summary <- crab_summ%>%
  mutate_if(is.numeric, round, 2)%>%
  unite(col = `Weight (g)`,`Weight (g)_mean`:`Weight (g)_error`, sep = "\u00B1")%>%
  unite(col = `length (cm)`,`length (cm)_mean`:`length (cm)_error`, sep = "\u00B1")%>%
  unite(col = `breadth (cm)`,`breadth (cm)_mean`:`breadth (cm)_error`, sep = "\u00B1")%>%
  unite(col = `height (cm)`,`height (cm)_mean`:`height (cm)_error`, sep = "\u00B1")%>%
  unite(col = `large chelae (cm)`,`large chelae (cm)_mean`:`large chelae (cm)_error`, sep = "\u00B1")%>%
  unite(col = `small chelae (cm)`,`small chelae (cm)_mean`:`small chelae (cm)_error`, sep = "\u00B1")%>%
  unite(col = `Condition factor K`,`K_mean`:`K_error`, sep = "\u00B1")


crab_summary_sex <- crab_summ_sex%>%
  mutate_if(is.numeric, round, 2)%>%
  unite(col = `Weight (g)`,`Weight (g)_mean`:`Weight (g)_error`, sep = "\u00B1")%>%
  unite(col = `length (cm)`,`length (cm)_mean`:`length (cm)_error`, sep = "\u00B1")%>%
  unite(col = `breadth (cm)`,`breadth (cm)_mean`:`breadth (cm)_error`, sep = "\u00B1")%>%
  unite(col = `height (cm)`,`height (cm)_mean`:`height (cm)_error`, sep = "\u00B1")%>%
  unite(col = `large chelae (cm)`,`large chelae (cm)_mean`:`large chelae (cm)_error`, sep = "\u00B1")%>%
  unite(col = `small chelae (cm)`,`small chelae (cm)_mean`:`small chelae (cm)_error`, sep = "\u00B1")%>%
  unite(col = `Condition factor K`,`K_mean`:`K_error`, sep = "\u00B1")

write.csv(crab_summary_sex,"crab_summary_sex.csv")


crab_summary_month <- crab_summ_month%>%
  mutate_if(is.numeric, round, 2)%>%
  unite(col = `Weight (g)`,`Weight (g)_mean`:`Weight (g)_error`, sep = "\u00B1")%>%
  unite(col = `length (cm)`,`length (cm)_mean`:`length (cm)_error`, sep = "\u00B1")%>%
  unite(col = `breadth (cm)`,`breadth (cm)_mean`:`breadth (cm)_error`, sep = "\u00B1")%>%
  unite(col = `height (cm)`,`height (cm)_mean`:`height (cm)_error`, sep = "\u00B1")%>%
  unite(col = `large chelae (cm)`,`large chelae (cm)_mean`:`large chelae (cm)_error`, sep = "\u00B1")%>%
  unite(col = `small chelae (cm)`,`small chelae (cm)_mean`:`small chelae (cm)_error`, sep = "\u00B1")%>%
  unite(col = `Condition factor K`,`K_mean`:`K_error`, sep = "\u00B1")

write.csv(crab_summary_month,"crab_summary_month.csv")



weight_grp_st <- ress$`Weightg ~ Site`$groups
weight_grp_st <- rownames_to_column(weight_grp_st, var = "Site") %>% as_tibble()
crab_summary <- left_join(by = "Site", x = crab_summary, y = weight_grp_st)

length_grp_st <- ress$`lengthcm ~ Site`$groups
length_grp_st <- rownames_to_column(length_grp_st, var = "Site") %>% as_tibble()
crab_summary <- left_join(by = "Site", x = crab_summary, y = length_grp_st)


largechelae_grp_st <- ress$`largechelaecm ~ Site`$groups
largechelae_grp_st <- rownames_to_column(largechelae_grp_st, var = "Site") %>% as_tibble()
crab_summary <- left_join(by = "Site", x = crab_summary, y = largechelae_grp_st)


smallchelae_grp_st <- ress$`smallchelaecm ~ Site`$groups
smallchelae_grp_st <- rownames_to_column(smallchelae_grp_st, var = "Site") %>% as_tibble()
crab_summary <- left_join(by = "Site", x = crab_summary, y = smallchelae_grp_st)

write.csv(crab_summary,"crab_summary_site.csv")


###################################
#### Month
####################################

weight_grp_mt <- ress$`Weightg ~ Month`$groups
weight_grp_mt <- rownames_to_column(weight_grp_mt, var = "Month") %>% as_tibble()
crab_summary_month <- left_join(by = "Month", x = crab_summary_month, y = weight_grp_mt)

length_grp_mt <- ress$`lengthcm ~ Month`$groups
length_grp_mt <- rownames_to_column(length_grp_mt, var = "Month") %>% as_tibble()
crab_summary_month <- left_join(by = "Month", x = crab_summary_month, y = length_grp_mt)

breadth_grp_mt <- ress$`breadthcm ~ Month`$groups
breadth_grp_mt <- rownames_to_column(breadth_grp_mt, var = "Month") %>% as_tibble()
crab_summary_month <- left_join(by = "Month", x = crab_summary_month, y = breadth_grp_mt)


height_grp_mt <- ress$`heightcm ~ Month`$groups
height_grp_mt <- rownames_to_column(height_grp_mt, var = "Month") %>% as_tibble()
crab_summary_month <- left_join(by = "Month", x = crab_summary_month, y = height_grp_mt)

largechelae_grp_mt <- ress$`largechelaecm ~ Month`$groups
largechelae_grp_mt <- rownames_to_column(largechelae_grp_mt, var = "Month") %>% as_tibble()
crab_summary_month <- left_join(by = "Month", x = crab_summary_month, y = largechelae_grp_mt)

smallchelae_grp_mt <- ress$`smallchelaecm ~ Month`$groups
smallchelae_grp_mt <- rownames_to_column(smallchelae_grp_mt, var = "Month") %>% as_tibble()
crab_summary_month <- left_join(by = "Month", x = crab_summary_month, y = smallchelae_grp_mt)

numberofteeth_grp_mt <- ress$`numberofteeth ~ Month`$groups
numberofteeth_grp_mt <- rownames_to_column(numberofteeth_grp_mt, var = "Month") %>% as_tibble()
crab_summary_month <- left_join(by = "Month", x = crab_summary_month, y = numberofteeth_grp_mt)

write.csv(crab_summary_month, "crab_summary_month.csv")
