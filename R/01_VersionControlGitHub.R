# Download data from GitHub and use tidyverse 
#install.packages("here")
library(tidyverse)
library(here)
library(labelled)

# make code only accessible when run manually and not by source()

if(FALSE){
  # Download data from GitHub and use base R
  
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
  
  
  # Data visualization and plots
  library(ggplot2)
  install.packages("unibeCols", repos = c("https://dcr-unibe-ch.r-universe.dev", "https://cloud.r-project.org"))
  library(unibeCols)
  
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
  ggsave("plot_ebola_point_v1.pdf", plot = plot_ebola_point_v1, width = 8, height = 6)
  
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
  
  ggsave("plot_ebola_line_v1.pdf", plot = plot_ebola_line_v1, width = 8, height = 6)
}

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
#ggsave("plot_ebola_col_v1.pdf", plot = plot_ebola_col_v1, width = 8, height = 6) # only run once
plot_ebola_col_v1


# connect to GitHub
library(usethis)
library(gitcreds)

usethis::create_github_token()
gitcreds::gitcreds_set()
