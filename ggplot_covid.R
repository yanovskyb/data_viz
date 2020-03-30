library(tidyverse)
library(shadowtext)

theme_set(theme_bw())

state_trends = read.csv(
  "https://covidtracking.com/api/states/daily.csv", stringsAsFactors = F)

state_trends = state_trends %>% 
  filter(positive >= 10 | state %in% c("CA","WA")) %>%
  group_by(state) %>%
  dplyr::arrange(date) %>%
  mutate(new_date = seq(1:n()))

state_trends %>%
  ggplot(aes(x=new_date, y=positive, group = state)) +
  geom_line(data = state_trends %>% filter(!state %in% c('CA','NY','WA')), size = 1, color = 'gray50', alpha = .3) +
  geom_line(data = state_trends %>% filter(state %in% c('CA','NY','WA')), aes(color = state), size = 2) +
  scale_color_manual(values = c("#5B7BD6","#ed5168","#da9684")) +
  geom_point(data = state_trends %>% filter(state %in% c('CA','NY','WA') & new_date == max(new_date)), aes(color = state), size = 4) +
  scale_color_manual(values = c("#5B7BD6","#ed5168","#da9684")) +
  geom_shadowtext(
    data = state_trends %>% filter(state %in% c('CA','NY','WA') & new_date == max(new_date)),
    aes(label = state, y = positive), bg.color = c("#5B7BD6","#ed5168","#da9684"), color = "white", hjust=-.3, size = 5, fontface = 'bold',
    position = position_jitter(height=.15,width=1.1,seed=1)) +
  geom_shadowtext(
    data = state_trends %>% filter(state %in% c('NJ','MI','OR','NV') & new_date == max(new_date)),
    aes(label = state, y = positive), size = 4, fontface = 'bold', color = 'white', bg.color = 'black') +
  scale_colour_manual(values = c("#5B7BD6","#ed5168","#da9684")) +
  scale_y_log10(label = scales::comma, breaks = c(10, 100, 500, 1000, 2000, 5000, 10000, 20000, 40000)) +
  labs(x = "Days since first reporting 10+ cases", y = "Number of cases (log scale)",
       title = "COVID-19+ Cases, Trends by State") +
  theme(legend.position = 'none',
        axis.title = element_text(color = 'gray40', size = 14),
        plot.title = element_text(color = 'gray40', size = 19, face = 'bold'),
        axis.text = element_text(color = 'gray40', size = 15),
        panel.background = element_rect(fill = 'gray95'),
        plot.background = element_rect(fill = 'gray95'),
        panel.border = element_blank(),
        axis.line = element_line())
