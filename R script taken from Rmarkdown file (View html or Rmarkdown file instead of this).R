library(tidyverse)
library(readxl)

surv_players <- read_excel("survivor_contestants.xlsx",
                           col_types = c("text", "text", "numeric",
                                         "text", "text", "text", "text"))

DT::datatable(surv_players, 
              options = list(searching = F,
                             pageLength = 20,
                             initComplete = htmlwidgets::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000',
                               'color': '#fff'});",
                               "}")
              )
)

colnames(surv_players) <- c("serial", "name", "age", "sex", "city", "state", "mode")


DT::datatable(surv_players, 
              options = list(searching = F,
                             pageLength = 20,
                             initComplete = htmlwidgets::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000',
                               'color': '#fff'});",
                               "}")
              )
)


summary(surv_players$age)

ggplot(surv_players, aes(x = as.numeric(age))) + geom_histogram(fill = "grey", col = "black") +
  theme_bw() +
  labs(title = "Total Age Distribution",
       x = "Age",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

surv_players %>%
  arrange(desc(age))
```
```{r message=FALSE, warning=FALSE}
surv_players %>%
  filter(name == "Rudy Boesch")

unique(surv_players$serial)

surv_players_clean <- surv_players %>%
  # serial
  separate(serial, c("serial", "to remove"), sep = "\\[") %>%
  select(., -"to remove") %>%
  # name
  separate(name, c("name", "to remove"), sep = "\\[") %>%
  select(., -"to remove") %>%
  # age
  separate(age, c("age", "to remove"), sep = "\\[") %>%
  select(., -"to remove") %>%
  # sex
  separate(sex, c("sex", "to remove"), sep = "\\[") %>%
  select(., -"to remove") %>%
  # city
  separate(city, c("city", "to remove"), sep = "\\[") %>%
  select(., -"to remove") %>%
  # state_s
  separate(state, c("state", "to remove"), sep = "\\[") %>%
  select(., -"to remove") %>%
  # mode
  separate(mode, c("mode", "to remove"), sep = "\\[") %>%
  select(., -"to remove") %>%
  mutate(serial = str_replace(serial, "2\\*", "2"),
         serial = str_replace(serial, "5\\*", "5")) %>%
  # player '*' did not participate in the end
  filter(serial != "*") %>%
  mutate(age = as.double(age))
unique(surv_players_clean$serial)

index <- which(surv_players_clean$serial == as.character(1))
index

length(index)

updated_index <- c(index, length(surv_players_clean$serial))

length(updated_index)

updated_index_diff <- c(updated_index[1], diff(updated_index))
updated_index_diff

length(updated_index_diff)

season <- c()

for (i in 1:length(updated_index_diff)) {
  season <- c(season, rep(i, (updated_index_diff[i])))
}

season

surv_players_clean$season <- as.factor(season)

glimpse(surv_players_clean)

updated_index_2 <- c(1, updated_index + 1)
updated_index_2 <- updated_index_2[-length(updated_index_2)]
updated_index_2

location <- list()

for (i in 1:length(updated_index_2)) {
  location[[i]] <- surv_players_clean[updated_index_2[i], ]
}

location <- do.call("rbind", location)

location <- location %>%
  select(name, season)

location

library(hash)
hash <- hash(as.vector(location$season), location$name)
hash

location_col <- c()

for (i in 1:length(location$season)) {
  location_col <- c(location_col, rep(hash[[toString(i)]], updated_index_diff[i])) 
}

location_col

surv_players_clean$location <- location_col


surv_players_clean <- surv_players_clean[-updated_index_2, ] 

DT::datatable(surv_players_clean, 
              options = list(searching = F,
                             pageLength = 20,
                             initComplete = htmlwidgets::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000',
                                 'color': '#fff'});",
                               "}")))

surv_players_clean %>%
  filter(name == "Rudy Boesch")

ggplot(na.omit(surv_players_clean), 
       aes(x = season,
           y = age)) + 
  geom_boxplot(fill = "#FFCC99") +
  theme_bw() +
  labs(title = "Boxplot of Survivor players from Season 1 to Season 17",
       x = "Seasons",
       y = "Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(20, 80), breaks = c(seq(from = 0, to = 80, by = 10)))

print(paste("Variance:", by(surv_players_clean$age, surv_players_clean$season, var)))



car::leveneTest(as.numeric(age) ~ season, data = surv_players_clean)

print(paste("Mean:", by(surv_players_clean$age, surv_players_clean$season, mean)))

facet_labels <- c()

for (i in 1:length(updated_index)) {
  facet_labels <- c(facet_labels, paste("Season", toString(i)))
}

names(facet_labels) <- 1:19

ggplot(data = na.omit(surv_players_clean), aes(x = age)) + geom_histogram(binwidth = 5, fill = "#FFCC99", col = "black") + facet_wrap(~ season, labeller = labeller(season = facet_labels)) +
  theme_bw() +
  labs(title = "Distribution of Age for each Season",
       x = "Age",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

aov(age ~ season, data = surv_players_clean)
summary(aov(age ~ season, data = surv_players_clean))

ggplot(surv_players_clean, aes(x = season, fill = sex)) + geom_bar(position = "fill", color = "black") +
  theme_bw() +
  labs(title = "Proportion of Gender Across Seasons",
       x = "Seasons",
       y = "Proportion",
       fill = "Sex") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
  scale_fill_manual(values = wesanderson::wes_palette("Moonrise2", n = 3))

glimpse(surv_players_clean %>% filter(sex == "F*"))

head(surv_players_clean %>%
       count(state) %>%
       arrange(desc(n)), n = 10)

states <- map_data("state")
head(states, n = 5)

state_long <- tolower(unlist(lapply(surv_players_clean$state, openintro::abbr2state)))
surv_players_clean$region <- state_long
glimpse(surv_players_clean)

p <- surv_players_clean %>%
  count(region) %>%
  arrange(desc(n)) %>%
  rename("Count" = n)

p1 <- states %>%
  left_join(p, by = "region")

p2 <- p1[order(p1$order),] 

head(p2, n = 5)

lon_lat_centerd <- read_excel("centered_long_lat_states_usa.xlsx")
colnames(lon_lat_centerd) <- c("region", "lat", "long")
lon_lat_centerd$region <- tolower(lon_lat_centerd$region)

p3 <- p %>%
  left_join(lon_lat_centerd, by = "region")

p3 <- p3 %>%
  filter(!is.na(region))

p3$region <- unlist(lapply(p3$region, openintro::state2abbr))

head(p3)

# Manually adjusting labels

p3.1 <- p3 %>%
  mutate(long = case_when(region == "WA" ~ long + 0.6,
                          region == "OR" ~ long + 0.3,
                          region == "NE" ~ long - 0.3,
                          region == "KS" ~ long - 0.3,
                          region == "LA" ~ long - 0.5,
                          region == "MI" ~ long - 0.1,
                          region == "MN" ~ long - 0.1,
                          region == "FL" ~ long + 0.1,
                          region == "TX" ~ long - 0.5,
                          region == "NE" ~ long - 0.1,
                          region == "KS" ~ long - 0.4,
                          region == "MA" ~ long + 2,
                          region == "NJ" ~ long + 0.5,
                          long == long ~ long),
         lat = case_when(region == "NV" ~ lat + 1,
                         region == "UT" ~ lat - 1,
                         region == "NY" ~ lat + 1,
                         region == "CT" ~ lat - 1.5,
                         region == "NJ" ~ lat - 1,
                         region == "RI" ~ lat - 1,
                         lat == lat ~ lat))

ggplot(p2, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Count)) +
  geom_path() +
  scale_fill_gradientn(colours = rev(heat.colors(10)),na.value = "grey90") +
  coord_map() +
  theme_bw() +
  labs(title = "Heat Map of Homestates",
       x = "Longitude",
       y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(data = p3.1, aes(x = long, y = lat + 0.4, group = NA, label = region), 
            size = 3.5) +
  geom_text(data = p3.1, aes(x = long, y = lat - 0.3, group = NA, label = Count),
            size = 2.8)

prop_mode <- surv_players_clean %>%
  # mode_binary variable
  separate(mode, c("mode", "to remove"), sep = "Applicant") %>%
  select(., -"to remove") %>%
  mutate(mode_binary = ifelse(mode == "", 1, 0)) %>%
  select(., -"mode")
glimpse(prop_mode)

ggplot(prop_mode, aes(x = season, fill = as.factor(mode_binary))) + geom_bar(position = "fill", color = "black") +
  theme_bw() +
  labs(title = "Proportion of 'Applicant' vs 'Recruited' Across Seasons",
       x = "Seasons",
       y = "Proportion",
       fill = "Mode") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%")) +
  
  scale_fill_manual(labels = c("Recruited", "Applicant"), values = wesanderson::wes_palette("Moonrise2", n = 2))

tmp <- surv_players_clean %>%
  # mode_binary variable
  separate(mode, c("mode", "to remove"), sep = "Applicant")

tmp <- tmp %>%
  filter(!(mode %in% c("", "n/a", "")))

tmp_2 <- tmp$mode[!duplicated(tmp$mode)]

tmp_2

model_df <- surv_players_clean %>%
  # mode_binary variable
  separate(mode, c("mode", "to remove"), sep = "Applicant") %>%
  select(., -"to remove") %>%
  mutate(mode_binary = ifelse(mode == "", 1, 0)) %>%
  select(., -"mode") %>%
  mutate(mode_binary = as.factor(mode_binary)) %>%
  # sex_binary variable
  mutate(sex_binary = ifelse(sex == "M", 1, 0)) %>%
  mutate(sex_binary = as.factor(sex_binary)) %>%
  # serial remove NA and Special Character (i.e *)
  na.omit() %>%
  # age as numeric
  mutate(age = as.double(age)) %>%
  # serial as numeric
  mutate(serial = as.double(serial)) %>%
  mutate(region = case_when(state %in% c("WA", "OR", "CA", "NV", "ID", "AZ", "MT", "WY", "UT", "CO", "NM") ~ "West",
                            state %in% c("TX", "OK", "AR", "LA", "MS", "AL", "TN", "KY", "WV", "DC", "MD", "DE", "VA", "NC", "SC", "GA", "FL") ~ "South",
                            state %in% c("ME", "NY", "NH", "VT", "PA", "NJ", "CT", "MA", "RI") ~ "East",
                            state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH") ~ "Midwest"))
glimpse(model_df)

model_df$region <- factor(model_df$region, levels = c("East", "South", "West", "Midwest"))

fit <- lm(serial ~ age + mode_binary + sex_binary + region, data = model_df)

anova(fit)

summary(fit)

par(mfrow=c(2,2))
plot(fit)

right2bjury <- tibble(
  season = 1:19,
  num_players = c(9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 12, 12, 10, 10, 10, 9, 12)
)
right2bjury

logreg_df <- model_df %>%
  mutate(right2bjury = case_when(season == 1 & serial <= 9 ~ 1,
                                 season == 2 & serial <= 9 ~ 1,
                                 season == 3 & serial <= 9 ~ 1,
                                 season == 4 & serial <= 9 ~ 1,
                                 season == 5 & serial <= 9 ~ 1,
                                 season == 6 & serial <= 9 ~ 1,
                                 season == 7 & serial <= 9 ~ 1,
                                 season == 8 & serial <= 9 ~ 1,
                                 season == 9 & serial <= 9 ~ 1,
                                 season == 10 & serial <= 9 ~ 1,
                                 season == 11 & serial <= 9 ~ 1,
                                 season == 12 & serial <= 9 ~ 1,
                                 season == 13 & serial <= 12 ~ 1,
                                 season == 14 & serial <= 12 ~ 1,
                                 season == 15 & serial <= 10 ~ 1,
                                 season == 16 & serial <= 10 ~ 1,
                                 season == 17 & serial <= 10 ~ 1,
                                 season == 18 & serial <= 9 ~ 1,
                                 season == 19 & serial <= 12 ~ 1),
         right2bjury = ifelse(is.na(right2bjury), 0, 1),
         right2bjury = as.factor(right2bjury))
glimpse(logreg_df)

log_fit <- glm(right2bjury ~ age + sex_binary + mode_binary + region, data = logreg_df, family = "binomial")
summary(log_fit)