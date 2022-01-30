pacman::p_load(tidyverse, ggtext, lubridate, viridis, patchwork, ggnewscale, cowplot)

owid_url <- "https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"
covid <- read_csv(owid_url)

# eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", 
#                   "Cyprus", "Czechia", "Denmark", 
#                   "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
#                   "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
#                   "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
#                   "Spain", "Sweden")
# 
# eu_countries <- c(#"Germany", "United States", "Italy", 
#   "United Kingdom")


first_rows <- covid %>% 
  # filter(location %in% eu_countries) %>% 
  distinct(location) %>%
  mutate(date = as_date("2020-01-01"), 
         new_cases = 0,
         new_cases_smoothed = 0,
         new_cases_per_million = 0, 
         new_cases_smoothed_per_million = 0, 
         new_deaths = 0,
         new_deaths_smoothed = 0,
         new_deaths_per_million = 0, 
         new_deaths_smoothed_per_million = 0)
# pull(location)


covid_cases <- covid %>% 
  # filter(location %in% eu_countries) %>% 
  select(date, 
         new_cases,
         new_cases_smoothed,
         new_deaths,
         new_deaths_smoothed,
         new_cases_per_million, 
         new_cases_smoothed_per_million, 
         new_deaths_per_million,
         new_deaths_smoothed_per_million,
         location) %>%
  # Add the dates before the 1st confirmed case
  bind_rows(first_rows) %>% 
  arrange(date) %>% 
  group_by(location) %>% 
  complete(date = seq(min(.$date), max(.$date), by = 1),
           fill = list(new_cases = 0,
                       new_cases_smoothed = 0,
                       new_cases_per_million = 0, 
                       new_cases_smoothed_per_million = 0, 
                       new_deaths = 0,
                       new_deaths_smoothed = 0,
                       new_deaths_per_million = 0, 
                       new_deaths_smoothed_per_million = 0)) %>% 
  mutate(day_of_year = yday(date),
         year = year(date)
  ) %>% 
  ungroup()  %>% 
  # 2020 is a leap year, we could drop Feb 29, 2020 for the sake of 365-day years
  filter(date != as_date("2020-02-29")) %>%
  group_by(year, location) %>%
  mutate(day_of_year = row_number()) %>%
  ungroup() 

saveRDS(covid_cases, file = "data/covid_cases.rds")


size_factor1 <- 15000

# Colors
outline_color <- "#D97C86"
fill_color <- "#F0C0C1"
base_grey <- "grey28"



month_length <- c(31, 28, 31, 30, 31, 30,
                  31, 31, 30, 31, 30, 31)

month_breaks <- cumsum(month_length) - 30

# covid_cases <- readRDS("data/covid_cases.rds")

p <- covid_cases %>% 
  filter(location %in% c("Italy", "United Kingdom", "United States", "Netherlands")
  ) %>% 
  mutate(location = ifelse(location == "United Kingdom", "UK", location)) %>% 
  ggplot() +
  # area
  geom_linerange(aes(x = day_of_year, 
                     ymin = as.POSIXct(date) - new_cases_smoothed_per_million / 2 * size_factor1,
                     ymax = as.POSIXct(date) + new_cases_smoothed_per_million / 2 * size_factor1,
                     group = year, color = new_cases_smoothed_per_million), width = 0,
                 # color = outline_color, 
                 size = 1.5, show.legend = T) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date)),
               col = base_grey, size = 0.3, alpha = 0.5) +
  scale_x_continuous(minor_breaks = month_breaks, 
                     breaks = month_breaks,
                     labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  coord_polar() +
  theme_void()  +
  labs(title = "Spirals of COVID-19") +
       # subtitle="7-day average cases per million people") +
       # caption="Data: Our World in Data | Inspired by: NYT | Initial Code: Ansgar Wolsing (@_ansgar) | Visualization: Fabio Votta (@favstats)") +
  theme(
    legend.position = "top", 
    plot.margin=unit(c(0,0,0.25,0),"cm"),
    plot.title = element_text(hjust = 0.5, size = 15, margin = margin(b = 8)),
    plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 1, size = 4),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5)
  ) +
  # facet_wrap(~location, ncol = 2) +
  viridis::scale_color_viridis(option = "rocket", direction = -1) + 
  guides(color = guide_colourbar(title = "7-day average cases per million", 
                                 title.vjust = 1,
                                 title.theme = element_text(size = 10.5),
                                 label.theme = element_text(size = 6.5),
                                 barwidth = 5, barheight = 0.5)) +
  facet_wrap(~location, nrow = 1)

size_factor2 <- 1500000

p2 <- covid_cases %>% 
  filter(location %in% c("Italy", "United Kingdom", "United States", "Netherlands")
  ) %>% 
  mutate(location = ifelse(location == "United Kingdom", "UK", location)) %>% 
  ggplot() +
  # area
  geom_linerange(aes(x = day_of_year, 
                     ymin = as.POSIXct(date) - new_deaths_smoothed_per_million / 2 * size_factor2,
                     ymax = as.POSIXct(date) + new_deaths_smoothed_per_million / 2 * size_factor2,
                     group = year, color = new_deaths_smoothed_per_million), width = 0,
                 # color = outline_color, 
                 size = 1.5, show.legend = T) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date)),
               col = base_grey, size = 0.3, alpha = 0.5) +
  scale_x_continuous(minor_breaks = month_breaks, 
                     breaks = month_breaks,
                     labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  coord_polar() +
  theme_void()  +
  labs(#title = "Spirals of COVID-19",
    # subtitle="7-day average deaths per million people",
    caption=paste0("Last Update: ", Sys.Date(), " | Data: Our World in Data | Inspired by: NYT | Initial Code: Ansgar Wolsing (@_ansgar) | Visualization: Fabio Votta (@favstats)")) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(t = 25, b = 15)),
    plot.caption = element_text(hjust = 1, size = 8),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5)
  ) +
  # facet_wrap(~location, ncol = 2) +
  viridis::scale_color_viridis(option = "mako", direction = -1) + 
  guides(color = guide_colourbar(title = "7-day average deaths per million", 
                                 title.vjust = 1,
                                 title.theme = element_text(size = 10.5),
                                 label.theme = element_text(size = 6.5),
                                 barwidth = 5, barheight = 0.5)) +
  facet_wrap(~location, nrow = 1)

# p

yes <- p / p2

ggsave(plot = yes, filename = "combined.png", width = 8, height = 5, dpi = 300, bg = "white")



### Combined Spirals


# Annotations for the years in a list (used in annotate())
year_annotations <- list(
  year = 2020:2022,
  x = rep(3, 3),
  y = as.POSIXct(paste(2020:2022, "01", "01", sep = "-"))
)



plot_combined_spirals <- function(cntries, size_factor1, size_factor2, color_upper = "black") {
  
  # Colors
  outline_color <- "#D97C86"
  fill_color <- "#F0C0C1"
  base_grey <- "grey28"
  
  month_length <- c(31, 28, 31, 30, 31, 30,
                    31, 31, 30, 31, 30, 31)
  
  month_breaks <- cumsum(month_length) - 30
  
  p <- covid_cases %>% 
    filter(location %in% cntries
    ) %>% 
    mutate(location = ifelse(location == "United Kingdom", "UK", location)) %>% 
    ggplot() +
    # basic line
    geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                     y = as.POSIXct(date), yend = as.POSIXct(date)),
                 col = base_grey, size = 0.3, alpha = 0.5) +
    geom_linerange(aes(x = day_of_year, 
                       ymin = as.POSIXct(date) - new_cases_smoothed_per_million,
                       ymax = as.POSIXct(date) + new_cases_smoothed_per_million / 2 * size_factor1,
                       group = year, color = new_cases_smoothed_per_million), width = 0,
                   # color = outline_color, 
                   size = 1, show.legend = T) +
    viridis::scale_color_viridis(option = "rocket", direction = -1) + 
    guides(color = guide_colourbar(title = "7-day average cases per million",
                                   title.vjust = 1,
                                   title.theme = element_text(size = 10.5),
                                   label.theme = element_text(size = 6.5),
                                   barwidth = 5, barheight = 0.5)) +
    # geoms below will use another color scale
    new_scale_color() +
    geom_linerange(aes(x = day_of_year, 
                       ymin = as.POSIXct(date) - new_deaths_smoothed_per_million / 2 * size_factor2,
                       ymax = as.POSIXct(date) + new_deaths_smoothed_per_million,
                       group = year, color = new_deaths_smoothed_per_million), width = 0,
                   # color = outline_color, 
                   size = 1, show.legend = T) +
    viridis::scale_color_viridis(option = "mako", direction = -1) + 
    guides(color = guide_colourbar(title = "7-day average deaths per million",
                                   title.vjust = 1,
                                   title.theme = element_text(size = 10.5),
                                   label.theme = element_text(size = 6.5),
                                   barwidth = 5, barheight = 0.5)) +
    
    scale_x_continuous(minor_breaks = month_breaks, 
                       breaks = month_breaks,
                       labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                       limits = c(1, 365),
                       expand = c(0, 0)
    ) +
    #' set the lower limit of the y-axis to a date before 2020 
    #' so that the spiral does not start in the center point
    scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                       expand = c(0, 0)) +
    coord_polar() +
    theme_void()  +
    labs(#title = "Spirals of COVID-19",
      # subtitle="7-day average deaths per million people",
      caption=paste0("Last Update: ", Sys.Date(), " | Data: Our World in Data | Inspired by: NYT | Initial Code: Ansgar Wolsing (@_ansgar) | Visualization: Fabio Votta (@favstats)")) +
    theme(
      legend.position = "none",
      # plot.title = element_text(hjust = 0.5, size = 11),
      # plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(t = 25, b = 15)),
      plot.caption = element_text(hjust = 1, size = 7),
      plot.background = element_rect(color = NA, fill = "white"),
      panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
      panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
      axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5)
    ) +
    # facet_wrap(~location, ncol = 2) +
    
    facet_wrap(~location, nrow = 1)  + 
    theme(plot.margin = unit(c(-2, 0, -2, 0), "cm")) +
    # # annotation: years
    annotate("text",  inherit.aes = FALSE,
             label = paste0(year_annotations$year[1], "\u2192"), 
             x = 3,
             y = year_annotations$y[1],
             family = "Arial",
             size = 2, vjust = -0.6, hjust = 0.15) +
    # # annotation: years
    annotate("text",  inherit.aes = FALSE,
             label = paste0(year_annotations$year[2], "\u2192"), 
             x = 3,
             y = year_annotations$y[2],
             family = "Arial",
             size = 2, vjust = -0.6, hjust = 0.15) +
    # # annotation: years
    annotate("text",  inherit.aes = FALSE,
             label = paste0(year_annotations$year[3], "\u2192"), 
             x = 3,
             y = year_annotations$y[3],
             family = "Arial", color = color_upper,
             size = 2, vjust = -0.6, hjust = 0.15)
  
  # p
  
  library(cowplot)
  
  p_cases <- covid_cases %>% 
    filter(location %in% cntries
    ) %>% 
    mutate(location = ifelse(location == "United Kingdom", "UK", location)) %>% 
    ggplot() +
    # basic line
    geom_linerange(aes(x = day_of_year, 
                       ymin = as.POSIXct(date) - new_cases_smoothed_per_million,
                       ymax = as.POSIXct(date) + new_cases_smoothed_per_million / 2 * size_factor1,
                       group = year, color = new_cases_smoothed_per_million), width = 0,
                   # color = outline_color, 
                   size = 1, show.legend = T) +
    viridis::scale_color_viridis(option = "rocket", direction = -1, n.breaks = 4) + 
    guides(color = guide_colourbar(title = "7-day average cases per million",
                                   title.vjust = 1,
                                   title.theme = element_text(size = 9.5),
                                   label.theme = element_text(size = 5.5),
                                   barwidth = 5, barheight = 0.5)) +
    scale_x_continuous(minor_breaks = month_breaks, 
                       breaks = month_breaks,
                       labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                       limits = c(1, 365),
                       expand = c(0, 0)
    ) +
    #' set the lower limit of the y-axis to a date before 2020 
    #' so that the spiral does not start in the center point
    scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                       expand = c(0, 0)) +
    coord_polar() +
    theme_void()  +
    facet_wrap(~location, nrow = 1)
  
  p_deaths <- covid_cases %>% 
    filter(location %in% c("Serbia", "Croatia")
    ) %>% 
    mutate(location = ifelse(location == "United Kingdom", "UK", location)) %>% 
    ggplot() +
    # geoms below will use another color scale
    new_scale_color() +
    geom_linerange(aes(x = day_of_year, 
                       ymin = as.POSIXct(date) - new_deaths_smoothed_per_million / 2 * size_factor2,
                       ymax = as.POSIXct(date) + new_deaths_smoothed_per_million,
                       group = year, color = new_deaths_smoothed_per_million), width = 0,
                   # color = outline_color, 
                   size = 1, show.legend = T) +
    viridis::scale_color_viridis(option = "mako", direction = -1) + 
    guides(color = guide_colourbar(title = "7-day average deaths per million",
                                   title.vjust = 1,
                                   title.theme = element_text(size = 9.5),
                                   label.theme = element_text(size = 5.5),
                                   barwidth = 5, barheight = 0.5)) +
    
    scale_x_continuous(minor_breaks = month_breaks, 
                       breaks = month_breaks,
                       labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                       limits = c(1, 365),
                       expand = c(0, 0)
    ) +
    #' set the lower limit of the y-axis to a date before 2020 
    #' so that the spiral does not start in the center point
    scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                       expand = c(0, 0)) +
    coord_polar() +
    theme_void()  +
    facet_wrap(~location, nrow = 1)
  
  legend_a <- get_legend(p_cases + theme(legend.position="top") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")))
  legend_b <- get_legend(p_deaths + theme(legend.position="top") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
  # add the legend underneath the row we made earlier. Give it 10% of the height
  # of one plot (via rel_heights).
  lss <- plot_grid(legend_a, legend_b)
  
  prow <- plot_grid(lss, p, ncol = 1, rel_heights = c(.1, 1))
  # prow
  # now add the title
  title <- ggdraw() + 
    draw_label(
      "More COVID-19 Spirals",
      fontface = 'bold', size = 20,
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 150)
    )
  
  pp <- plot_grid(
    title, prow,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.2, 1)
  )  
  
  return(pp)
}


size_factor1 <- 25000

size_factor2 <- 3500000

cntries <- c("Germany", "Netherlands")

pp <- plot_combined_spirals(cntries, size_factor1, size_factor2)

ggsave(plot = pp, filename = "deaths_and_cases_denl.png", width = 7, height = 5, dpi = 300, bg = "white")


cntries <- c("United Kingdom", "United States")

size_factor1 <- 25000

size_factor2 <- 2000000

pp2 <- plot_combined_spirals(cntries, size_factor1, size_factor2, "white")

ggsave(plot = pp2, filename = "deaths_and_cases_ukus.png", width = 7, height = 5, dpi = 300, bg = "white")

cntries <- c("Poland", "Hungary")

# size_factor1 <- 25000
# 
# size_factor2 <- 2000000

pp3 <- plot_combined_spirals(cntries, size_factor1, size_factor2, "black")

ggsave(plot = pp3, filename = "deaths_and_cases_hupl.png", width = 7, height = 5, dpi = 300, bg = "white")


