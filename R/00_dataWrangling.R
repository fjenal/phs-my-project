# Download data from GitHub and use tidyverse
#install.packages("here")
library(tidyverse)
library(here)
library(labelled)
library(ggplot2)
library(unibeCols)
library(RColorBrewer)


# make code only accessible when run manually and not by source()
#if(FALSE){

  # Download data from GitHub and use base R ----
  
  data1 <- read.csv("./data/raw/insurance_with_date.csv")
  #data1_a <- read.csv(here("data", "raw", "insurance_with_date.csv"))
  str(data1)
  
  
  data2 <- read_csv(here("data", "raw", "insurance_with_date.csv"))
  str(data2)
  
  
  # transform sex and region into factor
  unique(data2$sex)
  unique(data2$region)
  
  data2$sex <- as.factor(data2$sex)
  data2$region <- as.factor(data2$region)
  
  data2 <- mutate(data2, children_2 = factor(children > 2))
  
  # data2_ref <- data2 %>%  
  # mutate(
  #   across(c(sex, region), factor),
  #   # sex = factor(sex),
  #   # region = factor(region),
  #   gt2_children = children > 2,
  #   smokes = (smoker == "yes"),
  #   date_6m = date + months(6)
  #   # date_6m = date + 30.4 * 6
  # )
  
  
  # Data visualization and plots ----
  # install.packages("unibeCols", repos = c("https://dcr-unibe-ch.r-universe.dev", "https://cloud.r-project.org"))
  
  ebola <- read_csv("./data/raw/ebola.csv")
  str(ebola)
  unique(ebola$Country)
  
  ebola_arr <- ebola %>% 
    arrange(Date) %>% 
    select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) %>% 
    filter(date <= ymd("2015-03-31")) %>% 
    filter(country == "Guinea" | country == "Liberia" | country == "Sierra Leone")
  
  head(ebola_arr)
  tail(ebola_arr)
  
  # Point plot
  plot_ebola_point_v1 <- ggplot(data = ebola_arr, 
                                mapping = aes(x = date, y = cum_conf_cases)) + 
    geom_point(aes(color = country, fill = country), 
               alpha = 0.7, shape = 21, size = 1.5) +
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
    scale_x_date(breaks = ymd(c("2014-08-01", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
                 labels = c("Aug", "Oct", "Dec", "Feb", "Apr"),
                 limits = ymd(c("2014-08-01", "2015-04-01"))) +
    ggtitle(label="Confirmed ebola cases before 2025-03-31 in 3 Countries") +
    xlab(label = "Time") +
    ylab(label = "# of confirmed cases") +
    theme_minimal()
  ggsave("./output/figures/plot_ebola_point_v1.pdf", plot = plot_ebola_point_v1, width = 8, height = 6)
  
  # line plot
  plot_ebola_line_v1 <- ggplot(data = ebola_arr, 
                               mapping = aes(x = date, y = cum_conf_cases, group = country)) + 
    geom_line(mapping = aes(color = country), 
              alpha = 0.7, linewidth = 1) +
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
    ggtitle(label="Confirmed ebola cases before 2025-03-31 in 3 Countries") +
    xlab(label = "Time") +
    ylab(label = "# of confirmed cases") +
    theme_minimal()
  ggsave("./output/figures/plot_ebola_line_v1.pdf", plot = plot_ebola_line_v1, width = 8, height = 6)
#}
  
# bar plot
plot_ebola_col_v1 <- ggplot(data = ebola_arr, 
                            mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_col(aes(fill = country, colour = country), position = "stack", 
           linetype = "solid", linewidth = 0.5, width = 0.7) +
  scale_fill_manual(name = "country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
  scale_colour_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
  ggtitle(label="Confirmed ebola cases before 2025-03-31 in 3 Countries") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases") +
  theme_minimal() +
  theme(legend.position = "bottom")
#ggsave("./output/figures/plot_ebola_col_v1.pdf", plot = plot_ebola_col_v1, width = 8, height = 6) # only run once
plot_ebola_col_v1

#this is a change for uploading to GitHub8 -- with comment 2.0


# --- --- --- --- --- --- ---

# Inspect data for final assessment ----

covid_region <- read_csv("./data/raw/COVID19Cases_geoRegion.csv")
covid_region_age <- read_csv("./data/raw/COVID19Cases_geoRegion_AKL10_w.csv")
str(covid_region_age)
unique(covid_region_age$geoRegion)
unique(covid_region_age$altersklasse_covid19)
unique(covid_region_age$type_variant)


## Swiss covid entries from 2020-2023 ----
covid_ch <- covid_region_age %>% 
  select(ageClass = altersklasse_covid19, geoRegion, entries, year = datum_dboardformated, pop) %>% 
  filter(geoRegion == "CH") %>% 
  filter(ageClass != "Unbekannt") %>% 
  mutate(week = as.numeric(substr(year, 6, 7))) %>% 
  filter(week != 53) %>% 
  mutate(year = as.factor(substr(year, 1, 4))) %>% 
  mutate(popPrct = (as.numeric((100/pop)*entries))) %>% 
  mutate(across(c(geoRegion, ageClass), factor)) %>% 
  arrange(year, week, ageClass)

levels(covid_ch$geoRegion)
levels(covid_ch$ageClass)
levels(covid_ch$year)
unique(covid_ch$week)
str(covid_ch)

## Table for Swiss covid entries from 2020-2023 ----
library(gtsummary)
covid_ch |> 
  select(year, entries) |> 
  tbl_summary(by = year) |> 
  add_overall()

## When were the most covid cases ----

covid_weekly <- covid_ch %>%
  group_by(year, week) %>%
  summarise(entries = sum(entries), .groups = "drop")

ggplot(covid_weekly, aes(x = week, y = entries)) +
  geom_col(mapping = aes(fill = year, colour = year), 
           alpha = 0.6, linetype = "solid", linewidth = 0.5, width = 0.7, show.legend = FALSE) +
  scale_fill_manual(name = "year",
                    breaks = c("2020", "2021", "2022", "2023"),
                    labels = c("2020", "2021", "2022", "2023"),
                    values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2], unibePastelS()[2])) +
  scale_colour_manual(name = "year",
                      breaks = c("2020", "2021", "2022", "2023"),
                      labels = c("2020", "2021", "2022", "2023"),
                      values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2], unibePastelS()[2])) +
  scale_x_continuous(
    breaks = seq(0, 52, by = 4),        # major ticks: 1, 9, 17, ...
    minor_breaks = seq(0, 52, by = 2),  # minor grid: every 4 weeks
    limits = c(0, 52)) +
  scale_y_continuous(breaks = seq(from = 0, to = 260000, by = 50000),
                     limits = c(0, 260000)) +
  ggtitle(label="COVID entries in Switzerland (2020-2023)") +
  xlab(label = "Week number") +
  ylab(label = "# COVID entries") +
  labs(colour = "Year", fill = "Year") +
  theme_bw() +
  #theme(legend.position = "bottom") +
  facet_wrap(.~year)

### Compare covid cases in 2022 across different age groups ----

covid_2022 <- covid_ch %>% 
  filter(year == "2022")
str(covid_2022)

  # generate good color palette
  my_palette <- brewer.pal(11, "PiYG")[c(1, 2, 3, 4, 5, 7, 8, 9, 10)]

ggplot(covid_2022, aes(x = week, y = entries)) + 
  geom_line(mapping = aes(colour = ageClass), linetype = "solid", linewidth = 1.0) + 
  #scale_color_viridis_d(direction = -1) +
  #scale_color_brewer(palette = "YlGnBu") +
  geom_point(mapping = aes(fill = ageClass, color = ageClass), shape = 21, size = 1.9) +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  scale_x_continuous(
    breaks = seq(0, 52, by = 4),        # major ticks: 1, 9, 17, ...
    minor_breaks = seq(0, 52, by = 2),  # minor grid: every 4 weeks
    limits = c(1, 52)) +
  scale_y_continuous(breaks = seq(from = 0, to = 50000, by = 5000),
                     limits = c(0, 50000)) +
  ggtitle(label="Covid entries across age groups in Switzerland (2022)") +
  xlab(label = "Week number") +
  ylab(label = "# covid entries") +
  labs(color = "Age group", fill = "Age group") +
  theme_bw() +
  theme(legend.position = "right")



# --- --- --- --- --- --- ---

# Data wrangling for final assessment ----
# Select data: geoRegion == "GE", "ZH", "LU" & AGE
covid_arr <- covid_region_age %>% 
  select(ageClass = altersklasse_covid19, geoRegion, entries, year = datum_dboardformated, pop) %>% 
  filter(geoRegion == "ZH" | geoRegion == "LU" | geoRegion == "GE") %>% 
  filter(ageClass != "Unbekannt") %>% 
  mutate(week = as.numeric(substr(year, 6, 7))) %>% 
  mutate(year = as.factor(substr(year, 1, 4))) %>% 
  mutate(popPrct = (as.numeric((100/pop)*entries))) %>% 
  mutate(across(c(geoRegion, ageClass), factor)) %>% 
  arrange(geoRegion, ageClass, year)
  
levels(covid_arr$geoRegion)
levels(covid_arr$ageClass)
str(covid_arr)

covid_2021 <- covid_arr %>% 
  filter(year == "2021" & geoRegion == "ZH")


## bar plot (facet) -- number of entries ----
covid_bar_plot <- ggplot(data = covid_arr, 
          mapping = aes(x = ageClass, y = entries)) + 
  geom_col(mapping = aes(fill = geoRegion, colour = geoRegion), 
           linetype = "solid", linewidth = 0.5, width = 0.7) +
  scale_fill_manual(name = "geoRegion",
                    breaks = c("GE", "LU", "ZH"),
                    labels = c("Geneva", "Lucerne", "Zurich"),
                    values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
  scale_colour_manual(name = "geoRegion",
                      breaks = c("GE", "LU", "ZH"),
                      labels = c("Geneva", "Lucerne", "Zurich"),
                      values = c(unibeGreenS()[2], unibeIceS()[2], unibeMustardS()[2])) +
  ggtitle(label="Covid entries in 3 different cantons in Switzerland") +
  xlab(label = "age classes") +
  ylab(label = "# covid entries") +
  theme_minimal() +
  theme(legend.position = "bottom")
  #facet_wrap(.~year)
covid_bar_plot


library(quantreg)
ggplot(covid_arr, aes(x = year, y = popPrct, color = geoRegion)) +
  geom_point()
#  geom_quantile()

