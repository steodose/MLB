#### 2025 Home Run Chase ####

library(baseballr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(gganimate)
# library(janitor)
# library(glue)
# library(ggimage)
# library(ggtext)
# library(showtext)
# library(extrafont) #for adding in new fonts


# Optional but makes R prefer not to display numbers in scientific notation
options(scipen = 9999)

# add fonts from Google fonts
#font_add_google("Chivo", "chivo")
#showtext_auto()

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size = 11, base_family = "Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
            plot.subtitle = element_text(color = "grey60"),
            strip.text = element_markdown(size = 5, family = "Outfit")
        )
}

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 

# Function for logo generation
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


# load teamcolors and logos
mlb_teamcolors <- read_csv('mlb_teamcolors.csv') %>% 
    mutate(team_abbr = case_when(team_abbr == "OAK" ~ "ATH", # A's are no longer called OAK
                                 TRUE ~ team_abbr,)
    )


# -------------------------------
# PLAYER HOME RUN INFO FUNCTION
# -------------------------------

# define players of interest and put them into a named vector
players <- c("Shohei Ohtani" = 19755,
             "Aaron Judge" = 15640,
             "Cal Raleigh" = 21534,
             "Kyle Schwarber" = 16478,
              "Eugenio Suárez" =12552)

# get game logs for one player to check if it works
#fg_batter_game_logs(playerid = 21534 , year = 2025)


# Safe wrapper with tryCatch
safe_fg_logs <- function(player_name, player_id) {
    message("Pulling logs for: ", player_name)
    tryCatch({
        logs <- fg_batter_game_logs(playerid = player_id, year = 2025)
        logs %>%
            mutate(Player = player_name,
                   GameNumber = row_number(),
                   HR = as.numeric(HR),
                   cumHR = cumsum(HR)) %>%
            select(Date, Team, Player, GameNumber, HR, cumHR)
    }, error = function(e) {
        message("Failed for ", player_name, ": ", e$message)
        return(NULL)
    })
}

# Run safely over all players
hr_data <- map2_dfr(names(players), players, safe_fg_logs)

hr_data2 <- hr_data %>% 
    left_join(mlb_teamcolors %>% 
                  select(team_abbr, division, primary, secondary, team_logo_espn), by = c("Team" = "team_abbr")) %>% 
    mutate(secondary = case_when(Team == "NYY" ~ "#003087", #change Yankees secondary color from white to blue
                                 TRUE ~ secondary,)
    )


# -------------------------------
# Home Run Visualization
# -------------------------------

label_data <- hr_data2 %>%
    group_by(Player) %>%
    filter(GameNumber == max(GameNumber)) %>%
    ungroup()


hr_data2 %>%
    ggplot(aes(x = GameNumber, y = cumHR, color = secondary, group = Player)) +
    geom_line(size = 1.2) +
    geom_label_repel(
        aes(label = paste0(sub(".* ", "", Player), " (", cumHR, ")"), color = secondary),
        data = function(d) d %>% group_by(Player) %>% slice_tail(n = 1),
        fill = "white",
        label.size = 0.20,
        size = 3,
        show.legend = FALSE,
        direction = "y",
        nudge_x = 5
    ) +
    scale_color_identity() +  # Use actual hex codes in `primary` column
    theme_custom() +
    labs(
        title = '2025 Home Run Title Chase',
        subtitle = "Cumulative HRs by game played. Labels show current leader positions.",
        x = "Games Played",
        y = "Total HRs",
        caption = "Data: Fangraphs via baseballR | Plot: @steodosescu"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none"
    )

# save image in working directory
ggsave("Total HRs.png", dpi = 300)


# -------------------------------
# Animation 
# -------------------------------

p <- hr_data2 %>%
    ggplot(aes(x = GameNumber, y = cumHR, group = Player, color = secondary)) +
    geom_line(size = 1.2) +
    geom_label_repel(
        aes(label = Player),
        data = function(d) d %>% group_by(Player) %>% slice_tail(n = 1),
        size = 4,
        fill = "white",
        label.size = 0.25,
        show.legend = FALSE,
        direction = "y",
        nudge_x = 5
    ) +
    scale_color_identity() +
    labs(
        title = '2025 Home Run Title Race',
        subtitle = 'Game {frame_along} of the season',
        x = 'Games Played',
        y = 'Cumulative HRs',
        caption = 'Data: FanGraphs via baseballR | Animation: @steodosescu'
    ) +
    theme_custom() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none"
    ) +
    transition_reveal(GameNumber) +
    ease_aes('linear')


animate(p, width = 800, height = 600, duration = 8, fps = 15, renderer = gifski_renderer("hr_race_2025.gif"))
