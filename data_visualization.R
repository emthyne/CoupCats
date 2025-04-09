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
#2.a.domestic political
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.a.base_data.csv.gz"
base_data.2a <- fread(url)
rm(url)
#2.b.domestic economic
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.b.base_data.csv.gz"
base_data.2b <- fread(url)
rm(url)
#2.c.political instability
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.c.base_data.csv.gz"
base_data.2c <- fread(url)
rm(url)
#2.d.military variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.d.base_data.csv.gz"
base_data.2d <- fread(url)
rm(url)
#2.e.international variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.e.base_data.csv.gz"
base_data.2e <- fread(url)
rm(url)

base_data <- base_data.2a
rm(base_data.2a)
base_data.2b <- base_data.2b %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2b, by=c("ccode", "year", "month"))
rm(base_data.2b)
base_data.2c <- base_data.2c %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2c, by=c("ccode", "year", "month"))
rm(base_data.2c)    
base_data.2d <- base_data.2d %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2d, by=c("ccode", "year", "month"))
rm(base_data.2d)    
base_data.2e <- base_data.2e %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2e, by=c("ccode", "year", "month"))
rm(base_data.2e)

# Filtering out rows with NAs. 
columns <- c("pres_elec_lag", "polyarchy", "polyarchy2", 
             "lgdppcl", "ch_gdppcl", 
             "cw", 
             "cold", "e_asia_pacific", "LA_carrib", "MENA", "N_america", "S_asia", "Sub_africa", 
             "pce", "pce2", "pce3")

base_data2 <- base_data[complete.cases(base_data[, ..columns]), ] 
rm(columns)

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

# ------------------- Tree map ------------------- #

# 1. Manage data. 

# Read in libraries. 
library(treemap)

# 2. Sum up coups by region. 
regions <- base_data2 %>%
  mutate(region = case_when(Sub_africa == 1 ~ "Sub-Saharan Africa",
                            MENA == 1 ~ "Middle East/North Africa",
                            euro_cent_asia == 1 ~ "Europe/Central Asia", 
                            S_asia == 1 ~ "South Asia",
                            e_asia_pacific == 1 ~ "East Asian Pacific",
                            N_america == 1 ~ "North America",
                            LA_carrib == 1 ~ "Latin America/Caribbean",
                            TRUE ~ "Other")) %>%
  select(country, ccode, year, month, coup_attempt, coup_successful, coup_failed, region) %>% 
  group_by(region) %>%
  dplyr::summarize(coups = sum(coup_attempt, na.rm = TRUE)) 

# 3. Create treemap. 
library(extrafont)
font_import()
loadfonts(device="win") # On windows. 

png("coups_by_region.png", width = 1600, height = 1200, res = 300)  # Larger image size and higher resolution
treemap(regions,
        index = "region", vSize = "coups", type = "index",
        title = "Coup Attempts by Region", fontfamily.title = "Calibri", fontsize.title = 18, 
        fontcolor.labels = "ghostwhite", fontsize.label = 10, fontfamily.labels = "Calibri",
        border.col = c("black", "black"), border.lwds = c(2, 2)) 
dev.off()

