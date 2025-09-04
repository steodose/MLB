library(baseballr)
library(tidyverse)
library(rvest)
library(ggchicklet) #for stylized bar charts
library(teamcolors)
library(extrafont) # for extra fonts
library(ggimage)
library(glue)
library(ggtext)
library(janitor)
library(scales)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom_floralwhite <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 


# Function for plot with logo generation
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
    
    # Requires magick R Package https://github.com/ropensci/magick
    
    # Useful error message for logo position
    if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
        stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
    }
    
    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)
    
    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
    
    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height
    
    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding
    
    if (logo_position == "top right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos = 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos = 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    }
    
    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
    
}



# load data (from ChatGPT: https://chatgpt.com/share/683d366c-d58c-8011-aa3e-86567d7567eb)
ncaa_hits <- read_csv('ncaa_baseball_hits.csv')

ncaa_hits <- ncaa_hits %>% 
    clean_names() %>% 
    mutate(hit_share = percent/100) %>% 
    select(-percent)
    

##### NCAA Website Scraping #####

# write a scraping function that takes year and stat_id as inputs
scrape_ncaa_stat <- function(year = 2025, stat_id = 323, hit_type = "HR") {
    # Build paginated URLs
    base_url <- paste0("https://www.ncaa.com/stats/baseball/d1/", year, "/team/", stat_id, "?page=")
    urls <- paste0(base_url, 0:5)
    
    # Scrape and bind all pages
    ncaa_stats <- map_dfr(urls, ~read_html(.x) %>%
                              html_node("table") %>%
                              html_table(fill = TRUE) %>%
                              mutate(page = .x)) %>%
        clean_names()
    
    # Handle column naming: add 'x' prefix only if hit_type starts with a number
    stat_col_name <- if (grepl("^[0-9]", hit_type)) {
        paste0("x", tolower(hit_type))
    } else {
        tolower(hit_type)
    }
    
    # Aggregate and return summary
    stat_summary <- ncaa_stats %>% 
        summarise(
            g = sum(g, na.rm = TRUE),
            stat_total = sum(.data[[stat_col_name]], na.rm = TRUE)
        ) %>%
        mutate(
            hits_per_game = stat_total / g,
            year = as.character(year),
            hit_type = hit_type
        ) %>%
        select(year, hit_type, hits_per_game)
    
    return(stat_summary)
}


hr_25 <- scrape_ncaa_stat(2024, 323, "HR") # HR per game for 2025 (the years are off by one on NCAA's website)
hr_24 <- scrape_ncaa_stat(2023, 323, "HR") # HR per game for 2024

doubles_25 <- scrape_ncaa_stat(2024, 324, "2B") # 2B per game for 2025
doubles_24 <- scrape_ncaa_stat(2023, 324, "2B") # 2B per game for 2024

triples_25 <- scrape_ncaa_stat(2024, 325, "3B") # 3B per game for 2025
triples_24 <- scrape_ncaa_stat(2023, 325, "3B") # 3B per game for 2024

total_hits_25 <- scrape_ncaa_stat(2024, 484, "H") # total hits per game for 2025
total_hits_24 <-scrape_ncaa_stat(2023, 484, "H") # total per game for 2024

all_hits_24_25 <- rbind(hr_24, hr_25, doubles_24, doubles_25, triples_24, triples_25, total_hits_24, total_hits_25)
all_hits_24_25 <- all_hits_24_25 %>%
    mutate(hit_type = recode(hit_type,
                             "HR" = "Home Run",
                             "2B" = "Double",
                             "3B" = "Triple")) %>% 
    mutate(year = recode(year,
                             "2023" = "2024",
                             "2024" = "2025"))


# calculate singles
all_hits_24_25_wide <- all_hits_24_25 %>%
    pivot_wider(names_from = hit_type, values_from = hits_per_game)

all_hits_24_25_pivot <- all_hits_24_25 %>%
    pivot_wider(names_from = hit_type, values_from = hits_per_game) %>%
    mutate(Single = H - (Double + Triple + `Home Run`)) %>%
    select(year, Single, Double, Triple, `Home Run`, H)

all_hits_24_25_long <- all_hits_24_25_pivot %>%
    pivot_longer(
        cols = c(Single, Double, Triple, `Home Run`),
        names_to = "hit_type",
        values_to = "hits_per_game"
    ) %>%
    mutate(hit_share = hits_per_game / H) %>%
    select(year, hit_type, hits_per_game, hit_share)

# combine with original hits df that has hits data back to 2009
ncaa_hits <- rbind(ncaa_hits, all_hits_24_25_long)
    


##### Data Visualization #####

## 1. Hits by Type Stacked Chicklet Chart

ncaa_hits %>% 
    mutate(hit_type = fct_relevel(hit_type, "Single", "Double", "Triple", "Home Run")) %>%
    ggplot(aes(year, hit_share)) + 
    geom_chicklet(aes(fill = hit_type)) +
    geom_text(data = . %>% 
                  filter(hit_share > .1), aes(label = percent(hit_share, accuracy = 1L)), 
              family = "Outfit", size = 3, color = "white", fontface = "bold", position = position_stack(vjust = 0.5)) +
    theme_custom_floralwhite() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x = element_text(angle = 0),
          plot.title = element_text(face = 'bold', size = 18, hjust = 0.5), 
          plot.subtitle = element_text(size = 10, hjust = 0.5), 
          plot.caption = element_text(size = 10)
    ) + 
    labs(x = "",
         y = "",
         title = "Home Runs have increased in NCAA Baseball", 
         subtitle = "Hits by type in NCAA D1 baseball since 2009. 2025 pending MCWS Finals.",
         caption = "Data: baseballR/NCAA.com | Graphic: @steodosescu") +
    guides(fill=guide_legend(
        keywidth= .5,
        keyheight= .2,
        default.unit="inch", 
        label.position = 'top', 
        nrow = 1) 
    ) +
    scale_fill_manual(values = c("#a9a9a9", "#009CDE", "black", "#C9082A")) 



ggsave("NCAA Hits Chicklet Chart.png")



## 2. Hits per Game chart

# calculate average hits per game by year
total_hits_per_year <- ncaa_hits %>%
    group_by(year) %>%
    summarise(total_hits = sum(hits_per_game)) %>%
    arrange(year) %>%
    mutate(yoy_change = (total_hits / lag(total_hits)) - 1)

ncaa_hits %>% 
    mutate(hit_type = fct_relevel(hit_type, "Single", "Double", "Triple", "Home Run")) %>%
    ggplot(aes(year, hits_per_game)) + 
    geom_chicklet(aes(fill = hit_type)) +
    geom_text(data = . %>% 
                  filter(hit_share > .1), 
              aes(label = round(hits_per_game, 2)), 
              family = "Outfit", size = 2.5, color = "white", fontface = "bold", position = position_stack(vjust = 0.5)) +
    geom_text(data = total_hits_per_year, 
                aes(x = year, y = total_hits, label = round(total_hits, 1)), 
                vjust = -0.5, fontface = "bold", family = "Outfit", size = 3) +
    theme_custom_floralwhite() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x = element_text(angle = 0),
          plot.title = element_text(face = 'bold', size = 18, hjust = 0.5), 
          plot.subtitle = element_text(size = 10, hjust = 0.5), 
          plot.caption = element_text(size = 10)
    ) + 
    labs(x = "",
         y = "",
         title = "Hits are down across College Baseball", 
         subtitle = "Hits per team-game by hit type in NCAA D1 baseball since 2009. 2025  data thru Regionals.",
         caption = "Data: baseballR/NCAA.com | Graphic: @steodosescu") +
    guides(fill=guide_legend(
        keywidth= .5,
        keyheight= .2,
        default.unit="inch", 
        label.position = 'top', 
        nrow = 1) 
    ) +
    scale_x_continuous(breaks = unique(ncaa_hits$year)) +
    scale_fill_manual(values = c("#a9a9a9", "#009CDE", "black", "#C9082A")) 


ggsave("NCAA Hits per Game.png")

