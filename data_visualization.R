rm(list = ls())
# setwd("~/R/coupcats") # Emma's laptop. 

# source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 

# ------------------- Animated bar graph ------------------- #

# 1. Manage data. 
# Read in libraries. 
library(quantmod)
library(dplyr)
library(lubridate)
library(gganimate)
library(gifski_renderer)

# Bring in data. 
source("reading_data.R")

# 2. Calculate coup risk. 
# Running logistic regression. 
coup_logit <- feglm(coup_attempt ~ 
                      pres_elec_lag + polyarchy + polyarchy2 + milreg + 
                      lgdppcl + ch_gdppcl + 
                      cw + mobilization + 
                      solqual + 
                      cold + e_asia_pacific + LA_carrib + MENA + N_america + S_asia + Sub_africa + 
                      pce + pce2 + pce3, 
                    data = base_data2, family = 'binomial', cluster = ~ccode)

# Creating a column based on coup probability. 
probabilities <- predict(coup_logit, newdata = base_data2, type = "response")
base_data2$coup_risk <- probabilities
rm(probabilities)

# Rank countries. 
rank_data <- base_data2 %>%
  drop_na() %>% 
  group_by(year, month) %>%
  arrange(desc(coup_risk)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10)

# 3. Animate graph. 
# Create an animated race graph: (1) top fifteen at-risk countries, (2) by year
race_graph <- ggplot(rank_data, aes(rank, coup_risk, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reverse() +
  labs(title = 'Coup Risk Over Time', y = 'Coup Risk Score', x = 'Rank') +
  transition_states(interaction(year, month), transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

gganimate::animate(race_graph, fps = 3, duration = 15, width = 800, height = 600, renderer = gifski_renderer())

