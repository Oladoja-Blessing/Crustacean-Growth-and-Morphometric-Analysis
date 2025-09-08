library(tidyverse)
library(magrittr)

library(readr)
crab <- read_csv("crab.csv", 
                 col_types = cols(Site = col_factor(levels = c("Ala", 
                                                                           "Futa", "Elegbin")), 
                                              Month = col_factor(levels = c("November", 
                                                                                                                              "December", "January", "February", "March", 
                                                                                                                              "April", "May", "June")), 
                                              sex = col_factor(levels = c("F", 
                                                                                                                                                                                    "M"))))

crab <- crab%>%
  mutate(`length (mm)` = `length (mm)` * 0.1,
         `breadth (mm)` = `breadth (mm)` * 0.1,
         `height (mm)` = `height (mm)` * 0.1,
         `small chelae (mm)` = `small chelae (mm)` * 0.1,
         `large chelae (mm)` = `large chelae (mm)` * 0.1)%>%
  rename(`length (cm)` = `length (mm)`,
         `breadth (cm)` = `breadth (mm)`,
         `height (cm)` = `height (mm)`,
         `small chelae (cm)` = `small chelae (mm)`,
         `large chelae (cm)` = `large chelae (mm)`)



write.csv(crab, "crab(cm).csv")

