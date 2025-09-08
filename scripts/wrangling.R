#### log transformation
crab_log <- crab%>%
  mutate_if(is.double, log10)

### mean summarisation without log
crab_mean <- crab%>%
  group_by(Site, Month,sex)%>%
  summarise(across(c(`Weight (g)`: `small chelae (cm)`, `number of teeth`), ~ round(mean(.x, na.rm = TRUE),2)))

#### mean summarisation of log
crab_mean_log <- crab_log%>%
  group_by(Site, Month, sex)%>%
  summarise(across(c(`Weight (g)`: `small chelae (cm)`, `number of teeth`), ~ round(mean(.x, na.rm = TRUE),2)))%>%
  select(-`number of teeth`)

crab_sex <- crab%>%
  group_by(Site, sex)%>%
  summarise(
    `Sex Ratio` = n()
  )

se <- function(x){
  round(sd(x)/(sqrt(length(x))),3)
}


crab_summ <- crab_condition%>%
  select(-c(sn, Month, sex, `number of teeth`))%>%
  group_by(Site)%>%
  summarise(across(`Weight (g)`:K, list(mean = mean, error = se)))
# write.csv(crab_summ, "summary table.csv")
# 
crab_summ_sex <- crab_condition%>%
  select(-c(sn, Month, Site, `number of teeth`))%>%
  group_by(sex)%>%
  summarise(across(everything(),list(mean = mean, error = se)))
# write.csv(crab_summ_sex, "summary sex table.csv")

crab_summ_month <- crab_condition%>%
  select(-c(sn, Site, `number of teeth`,sex))%>%
  group_by(Month)%>%
  summarise(across(everything(),list(mean = mean, error = se)))       
