library(tidyverse)
library(ggpubr)
library(ggthemes)

label <- as_labeller(c("Ala" = "Ala", "Futa" = "Arugu", "Elegbin" = "Elegbin"))

#### weifht over month general 
crab_mean%>%
  ggplot(aes(Month, `Weight (g)`, group = Site, col = Site))+
  geom_point(size = 2)+
  geom_line()+
  theme_classic2()+
  theme(legend.position = "top",
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"))+
  scale_color_discrete(labels = c("Ala", "Arugu","Elegbin"))

#### weight over month male
crab_mean%>%
  filter(sex == "M")%>%
  ggplot(aes(Month, `Weight (g)`, group = Site, col = Site))+
  geom_point(size = 2)+
  geom_line()+
  theme_classic2()+
  theme(legend.position = "top",
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"))+
  scale_color_discrete(labels = c("Ala", "Arugu","Elegbin"))


##### weight over month female

crab_mean%>%
  filter(sex == "F")%>%
  ggplot(aes(Month, `Weight (g)`, group = Site, col = Site))+
  geom_point(size = 2)+
  geom_line()+
  theme_classic2()+
  theme(legend.position = "top",
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"))+
  scale_color_discrete(labels = c("Ala", "Arugu","Elegbin"))

### all months (weight)
crab_mean%>%
  group_by(Month)%>%
  summarise(across(c(`Weight (g)`: `small chelae (cm)`, `number of teeth`), ~ round(mean(.x, na.rm = TRUE),2)))%>%
  ggplot(aes(Month, `Weight (g)`, group = 1))+
  geom_point(size = 2)+
  geom_line()+
  #facet_wrap(~Site, nrow = 3, scales = "free")+
  ggthemes::theme_tufte()
crab_mean_log%>%
  ggplot(aes(`length (cm)`, `Weight (g)`, group = 1))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = F, col = "red")+
  theme_classic2()+
  labs(x = "Log (Length)", y = "Log (Weight)")+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 12, face = "bold"),
        axis.line.y.right = element_blank())+
  stat_regline_equation(label.y = 1.8, 
                        aes(label = ..eq.label..),show.legend = F)+
  stat_regline_equation(label.y = 1.76, 
                        aes(label = ..rr.label..), show.legend = F)


crab_mean_log%>%
  filter(sex == "M")%>%
  ggplot(aes(`length (cm)`, `Weight (g)`, group = 1))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = F,col = "red")+
  theme_classic2()+
  labs(x = "Log (Length)", y = "Log (Weight)")+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 9))+
  stat_regline_equation(label.y = 1.8, 
                        aes(label = ..eq.label..),show.legend = F)+
  stat_regline_equation(label.y = 1.7, 
                        aes(label = ..rr.label..), show.legend = F)+
  scale_color_discrete(labels = c("Ala", "Arugu","Elegbin"))

crab_mean_log%>%
  filter(sex == "F")%>%
  ggplot(aes(`length (cm)`, `Weight (g)`, group = 1))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = F,col = "red")+
  theme_classic2()+
  labs(x = "Log (Length)", y = "Log (Weight)")+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 9))+
  stat_regline_equation(label.y = 1.8, 
                        aes(label = ..eq.label..),show.legend = F)+
  stat_regline_equation(label.y = 1.7, 
                        aes(label = ..rr.label..), show.legend = F)+
  scale_color_discrete(labels = c("Ala", "Arugu","Elegbin"))



crab_mean%>%
  ggplot(aes(Site, `Weight (g)`, fill = Site))+
  geom_col()+
  guides(fill = "none")+
  facet_wrap(~Month, nrow = 3)+
  theme_classic2()


crab%>%
  group_by(Site, sex)%>%
  summarise(weight = mean(`Weight (g)`, na.rm = T))%>%
  ggplot(aes(sex, weight, fill = sex))+
  geom_col(width = 0.6)+
  labs(x = "Sex", y = "Average weight (g)")+
  facet_wrap(~Site, labeller = label)+
  theme_classic2()+
  theme(strip.text.x = element_text(face = "bold", size = 10))

crab%>%
  ggplot(aes(`Weight (g)`))+
  geom_histogram(bins = 40, col = "wheat", fill = "burlywood4")+
  theme_minimal()+
  labs(x = "Weight (g)", y = "Frequency")


crab%>%
  ggplot(aes(`length (cm)`))+
  geom_histogram(bins = 40, col = "wheat", fill = "burlywood4")+
  theme_minimal()+
  labs(x = "Length (cm)", y = "Frequency")
