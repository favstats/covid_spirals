## Create NYT Spiral Graphs
## Most Code comes from here: https://bydata.github.io/nyt-corona-spiral-chart/


pacman::p_load(tidyverse, lubridate, gganimate, viridis, shadowtext, gifski)

owid_url <- "https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"
covid <- read_csv(owid_url)



first_rows <- covid %>%
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

covid_cases <- covid %>%
  select(date,
         new_cases,
         new_cases_smoothed,
         new_deaths,
         new_deaths_smoothed,
         new_cases_per_million,
         new_cases_smoothed_per_million,
         new_deaths_per_million,
         new_deaths_smoothed_per_million,
         location,
         population) %>%
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

#saveRDS(covid_cases, file = "data/covid_cases.rds")

#covid_cases <- readRDS("data/covid_cases.rds")

##### plot #####

size_factor <- 720000


# Colors
outline_color <- "#D97C86"
fill_color <- "#F0C0C1"
base_grey <- "grey28"


# Annotations for the years in a list (used in annotate())
year_annotations <- list(
  year = 2020:2022,
  x = rep(3, 3),
  y = as.POSIXct(paste(2020:2022, "01", "01", sep = "-"))
)

month_length <- c(31, 28, 31, 30, 31, 30,
                  31, 31, 30, 31, 30, 31)

month_breaks <- cumsum(month_length) - 30

# covid_cases %>% View

fun_labs <- function(x) {
  return(round(pop/1000000*x))
}

round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


turn_it <- function(x) {
  le_seq <- seq(min(x, na.rm = T),
                max(x, na.rm = T), length.out = 5
  ) 
  
  davec <- round_any(le_seq[1:4], 100)
  
  fin <- c(davec, round_any(le_seq[5], 10, floor))/pop*1000000
  
  return(fin)
}


create_anim <- function(cntry) {
  
  # cntry <- "United States"
  
  cov_data <-  covid_cases %>%
    mutate(size_group = ifelse(new_deaths_smoothed_per_million == 0, "0", "1")) %>% 
    mutate(date2 = date) %>% 
    mutate(dlabel = format(date2, "%b %y")) %>% 
    filter(location == cntry) 
  
  pop <<- cov_data$population[100]
  
  p <- cov_data %>% 
    ggplot() +
    ## I am plotting a ton of lines here because geom_linerange doesn't seem to work
    ## properly with gganimate
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date),
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.9,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.8,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.70,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.6,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.5,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.4,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.3,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.2,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) - new_deaths_smoothed_per_million * size_factor * 0.1,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.9,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.8,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.70,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.6,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.5,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.4,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.3,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.2,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    geom_line(aes(x = day_of_year, 
                  y = as.POSIXct(date) + new_deaths_smoothed_per_million * size_factor * 0.1,
                  group = year, color = new_deaths_smoothed_per_million, size = size_group), 
              width = 0,
              # color = outline_color, 
              # size=1.75, 
              show.legend = T) +
    # annotate("text", label = paste0(year_annotations$year, "\u2192"), x = year_annotations$x, 
    #          y = year_annotations$y, 
    #          family = "Arial",
    #          size = 1, vjust = 1.3, hjust = 0.15) +
    # basic line
    
    scale_x_continuous(minor_breaks = month_breaks, 
                       breaks = month_breaks,
                       labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."),
                       limits = c(1, 365),
                       expand = c(0, 0)
    ) +
    #' set the lower limit of the y-axis to a date before 2020 
    #' so that the spiral does not start in the center point
    scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                       expand = c(0, 0))  + 
    scale_size_manual(values=c(0.25, 0.75)) +
    coord_polar() +
    theme_void()  +
    labs(title =  cntry,
         subtitle="7-day Average COVID-19 deaths\n",
         caption="Data: Our World in Data | Inspired by: NYT and Ansgar Wolsing | Code & Animation: Fabio Votta (@favstats)") +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 7),
      plot.caption = element_text(hjust = 1, size = 3.5),
      plot.background = element_rect(color = NA, fill = "white"),
      panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
      panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
      axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5)
    ) +
    # facet_wrap(~location, ncol = 2) +
    viridis::scale_color_viridis(option = "mako", direction = -1, 
                                 labels = fun_labs, 
                                 breaks = turn_it(cov_data$new_deaths_smoothed)) + 
    guides(color = guide_colourbar(title = "7-day Average COVID deaths", 
                                   title.vjust = 1,
                                   title.theme = element_text(size = 6.5),
                                   label.theme = element_text(size = 3.5),
                                   barwidth = 5, barheight = 0.5), size = "none") +
    # # annotation: years
    annotate("text",  inherit.aes = FALSE,
             label = paste0(year_annotations$year[1], "\u2192"), 
             x = 3,
             y = year_annotations$y[1],
             family = "Arial", #color = "white",
             size = 1.22, 
             vjust = -0.6, 
             hjust = 0) +
    # # annotation: years
    annotate("text",  inherit.aes = FALSE,
             label = paste0(year_annotations$year[2], "\u2192"), 
             x = 3,
             y = year_annotations$y[2],
             family = "Arial", #color = "white",
             size = 1.2, 
             vjust = -3, 
             hjust = 0 #higher number, more to the left
    ) +
    # # annotation: years
    annotate("text",  inherit.aes = FALSE,
             label = paste0(year_annotations$year[3], "\u2192"), 
             x = 3,
             y = year_annotations$y[3],
             family = "Arial", color = "white",
             size = 1.2, 
             vjust = -2, 
             hjust = 0) +
    geom_shadowtext(aes(x = day_of_year, 
                  y = as.POSIXct(date),
                  color = new_deaths_smoothed_per_million,
                  label = paste0(format(round(new_deaths_smoothed)))),
                  bg.colour='white',
              size = 1.2, inherit.aes = T, hjust = -0.25, vjust = 0.5) +
    transition_reveal(date) +
    ease_aes('linear')  
  
  # p <- p %>%
  #   animate(nframes = 300, fps = 20, #duration = 25,
  #           width = 1000, height = 1000,
  #           res = 300, end_pause = 70)
  
  p <- p %>%
    animate(nframes = 400, fps = 20, #duration = 25,
            width = 1500, height = 1500,
            res = 450, end_pause = 70, renderer = gifski_renderer())
  
  # p <- p %>%
  #   animate(nframes = 100, fps = 20, #duration = 25,
  #           width = 1500, height = 1500,
  #           res = 450, end_pause = 5)
  
  return(p)
  
}


p <- create_anim("United States")

anim_save("us_deaths.gif", animation = p, renderer = gifski_renderer())

print("US")

p <- create_anim("Germany")

anim_save("de_deaths.gif", animation = p, renderer = gifski_renderer())


print("DE")

p <- create_anim("United Kingdom")

anim_save("uk_deaths.gif", animation = p, renderer = gifski_renderer())


print("UK")

p <- create_anim("Netherlands")

anim_save("nl_deaths.gif", animation = p, renderer = gifski_renderer())

print("NL")

p <- create_anim("European Union")

anim_save("eu_deaths.gif", animation = p, renderer = gifski_renderer())

print("EU")

# covid_cases %>% count(location) %>% View


  


# turn_it(cov_data$new_deaths_smoothed)
