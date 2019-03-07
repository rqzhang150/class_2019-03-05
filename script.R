library(gapminder)
library(tidyverse)
library(gganimate)
library(png)

gapminder %>% 
  mutate(continent = fct_collapse(continent, Asia = c("Asia", "Oceania"))) %>% 
  filter(continent != "Europe") %>% 
  group_by(continent, year) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
  scale_color_manual(values = country_colors) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  scale_x_log10(breaks = c(1000, 10000)) +
  facet_grid(~continent) +
  labs(x = "GDP Per Capita",
       y = "Life Expectancy") +
  transition_states(year,
                    transition_length = 1,
                    state_length = 0) +
  ease_aes() +
  ggtitle('The World Gets Better Every Year: {closest_state}')


gapminder %>% 
  mutate(contient = recode_factor(continent, "Oceania" = "Asia")) %>% 
  filter(continent %in% c('Americas', 'Africa', 'Asia')) 

gapminder %>% 
  filter(year == 1952) %>% 
  mutate(continent = fct_collapse(continent, Asia = c("Asia", "Oceania"))) %>% 
  filter(continent != "Europe") %>% 
  group_by(continent, year) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
    geom_point(show.legend = FALSE) +
    scale_x_log10() +
    facet_grid(~continent)
