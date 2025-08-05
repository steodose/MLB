#### MLB Weather ####

library(baseballr)
library(tidyverse)
library(gt)
library(gtExtras)
library(janitor)
library(glue)
library(progress)
library(ggimage)
library(ggtext)
library(showtext)
library(extrafont) #for adding in new fonts


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
# GAME INFO FUNCTION
# -------------------------------

# read game data
games <- baseballr::mlb_schedule(season = "2025") %>%
    filter(game_type == 'R')

# Extract unique game_pk values
game_pks <- games$game_pk 
#game_pks <- game_pks[game_pks %in% c(778563, 778564)]


# initialize progress bar
pb <- progress_bar$new(
    total = length(game_pks),
    format = "  downloading [:bar] :percent eta: :eta",
    clear = FALSE, width = 60
)

# function to extract weather data
weather_data <- map_dfr(game_pks, function(pk) {
    result <- tryCatch({
        info <- mlb_game_info(pk)  # flat 1-row tibble
        
        tibble(
            game_pk = pk,
            stadium = info$venue_name[1],
            temperature = as.numeric(info$temperature[1]),
            condition = info$other_weather[1],
            wind = info$wind[1]
        )
    }, error = function(e) {
        tibble(
            game_pk = pk,
            stadium = NA_character_,
            temperature = NA_real_,
            condition = NA_character_,
            wind = NA_character_
        )
    })
    
    pb$tick()  # safely outside tryCatch, runs once per iteration
    return(result)
})


#load weather data directly instead of scraping every time
#weather_data <- read_csv('/Users/Stephan/Desktop/R Projects/MLB/weather_data.csv')

# -------------------------------
# DATA WRANGLING
# -------------------------------

# keeps only unique games (removes duplicates for postponed games)
games_unique <- games %>%
    arrange(game_pk, desc(game_date)) %>%
    distinct(game_pk, .keep_all = TRUE) %>%
    select(game_pk, game_date, team_name = teams_home_team_name)

# Join game date and team, retain stadium from weather_data
weather_data_final <- weather_data %>%
    left_join(games_unique, by = "game_pk") %>%
    drop_na() %>%
    filter(condition != "Dome", condition != "Roof Closed") %>%
    mutate(game_date = as.Date(game_date)) %>%
    left_join(
        mlb_teamcolors %>% select(name, primary, team_logo_espn),
        by = c("team_name" = "name")
    ) %>%
    filter(!is.na(temperature), !is.na(game_date), !is.na(team_name)) %>%
    group_by(team_name, stadium, game_date, primary, team_logo_espn) %>%
    summarise(avg_temp = mean(temperature), .groups = "drop")

# update naming conventions so they fit in plot strip text
weather_data_final <- weather_data_final %>% 
    mutate(stadium = case_when(stadium == "George M. Steinbrenner Field" ~ "Steinbrenner Field",
                               stadium == "Oriole Park at Camden Yards" ~ "Camden Yards",
                               stadium == "Great American Ball Park" ~ "Great American BP",
                               TRUE ~ stadium)
    )


# create column for inline HTML to plot logos in strip text
weather_data_final <- weather_data_final %>%
    mutate(
        team_label = glue("<img src='{team_logo_espn}' width='10'/> {stadium}")
    )

# set up duplicate team column for charting purposes 
weather_data_final$teamDuplicate <- weather_data_final$team_label


# compute team order for plot sorting
team_order <- weather_data_final %>%
    group_by(team_name, stadium, team_label, team_logo_espn) %>%
    summarise(mean_temp = mean(avg_temp, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_temp)) %>% 
    mutate(team_label = as.character(team_label))

weather_data_final <- weather_data_final %>%
    mutate(team_label = factor(team_label, levels = team_order$team_label))



# -------------------------------
# DATA VISUALIZATION
# -------------------------------


# 1. Faceted line plot
weather_data_final %>% 
    ggplot(aes(game_date, avg_temp)) +
    stat_smooth(data = mutate(weather_data_final, team_label = NULL), aes(group = teamDuplicate), 
                geom="line", alpha=0.3, linewidth=.25, span=0.2, colour = 'grey80') + 
    geom_smooth(aes(color = primary), linewidth = .35, span = .2, se = F) +
    facet_wrap(~team_label) +
    theme(strip.text = element_markdown()) +
    theme_custom() +
    scale_color_identity() + # use color variable as color scale
    scale_y_continuous(breaks = seq(0, 100, by = 20)) +
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold',
                                    size = 16,
                                    hjust = 0.5),
          plot.subtitle = element_text(
              size = 8,
              hjust = 0.5),
          #plot.margin = margin(10, 10, 15, 10), 
          axis.text.x = element_text(size = 6),  # Reduce x-axis tick label size
          axis.text.y = element_text(size = 6),
          panel.spacing = unit(0.5, 'lines')) +
    labs(x = "", 
         y = "Temperature (F)", 
         title = "Temperature Trends by MLB Stadium", 
         subtitle = "Sorted by average game time temperature throughout the 2025 season.  Data thru All-Star break.",
         caption = "Data: baseballR (Bill Petti) | Plot: @steodosescu")

# save plot
ggsave(
    "MLB Stadium Weather Rankings.png", dpi = 300, bg = "floralwhite", 
    width = 6, height = 6, units = "in"
)

# plot with logo
plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/MLB/MLB Stadium Weather Rankings.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/MLB/mlb-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "MLB Stadium Weather Rankings with Logo.png")


# 2. GT table

# load external data
attendance <- baseballr::mlb_attendance(league_list_id = 'mlb', season = 2025) # load attendance data
fg <- read_csv('/Users/Stephan/Desktop/R Projects/MLB/fangraphs_park_info.csv') # load Fangraphs data
savant <- read_csv('/Users/Stephan/Desktop/R Projects/MLB/savant_park_factors.csv')

# join in attendance data
team_order_gt <- team_order %>% 
    left_join(attendance %>% 
                  select(team_name, attendance_average_home),
    by = c("team_name" = "team_name")
) %>% 
    mutate(rank = row_number()) %>%
    select(rank, team_logo_espn, team_name, stadium, attendance_average_home, mean_temp
    )

# join in Fangraphs data
team_order_gt <- team_order_gt %>% 
    left_join(fg,
              by = c("stadium" = "Park")
              )

# join in Baseball Savant data
team_order_gt <- team_order_gt %>% 
    left_join(savant,
              by = c("team_name" = "Team")
    ) %>% 
    select(-Venue, -Year)
   
team_order_gt <- team_order_gt %>% 
    select(rank:mean_temp, `Park Factor`: HR, `Elevation (Meters)`:`Air Density (kg/m^3)`) %>% 
    mutate(`Elevation (Feet)` = `Elevation (Meters)`*3.28) %>% 
    select(-`Elevation (Meters)`, -R, -HR, -rank) %>% 
    arrange(-`Park Factor`) %>% 
    mutate(rank = row_number()) %>%
    select(rank, team_logo_espn:`Elevation (Feet)`)


# make GT table
team_order_gt %>% 
    gt() %>% 
    cols_label(rank = "",
               team_logo_espn = "",
               team_name = "Team",
               stadium = "Stadium",
               mean_temp = "Avg. Temp (F) 2025",
               attendance_average_home = "Avg. Attendance"
    ) %>% 
    tab_spanner(
        label = "Environmental Factors (Fangraphs)",
        columns = c(8:11)
    ) %>% 
    fmt_number(columns = mean_temp,
        decimals = 1
        ) %>% 
    fmt_number(
        columns = attendance_average_home,
        decimals = 0,
        use_seps = TRUE
    ) %>% 
    fmt_number(columns = `Elevation (Feet)`,
               decimals = 0
    ) %>% 
    gt_img_rows(columns = team_logo_espn, img_source = "web", height = 30) %>%
    data_color(
        columns = mean_temp, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction = 1
            ) %>% as.character(),
            domain = NULL, 
            na.color = "#005C55FF"
        )) %>%
    data_color(
        columns = `Park Factor`, 
        colors = scales::col_numeric(
            palette = c("blue", "white", "red"),
            domain = range(team_order_gt$`Park Factor`, na.rm = TRUE),
            na.color = "#D3D3D3"
        )
    ) %>% 
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**MLB Stadium Factors**"),
               subtitle = glue("League rankings (1-30) of stadiums from most to least hitter friendly so far this season, as well as other factors that affect offensive run-scoring. Not all data available for indoor parks, which are indicated with NA. Data thru All-Star break.")) %>%
    tab_source_note(
        source_note = md("DATA: baseballR (Bill Petti), Statcast, Fangraphs<br>TABLE: @steodosescu")) %>% 
    tab_options(
        column_labels.background.color = "white",
        table.border.top.width = px(3),
        table.border.top.color = "white",
        table.border.bottom.color = "white",
        table.border.bottom.width = px(3),
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "white",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 16,
        heading.title.font.size = 24,
        heading.align = "center"
    ) %>% 
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
    ) %>% 
    tab_footnote(
        footnote = "Quantifies how a baseball stadium affects offensive output, relative to the league average. Expressed as a percentage, with 100 being MLB average.",
        locations = cells_column_labels(columns = `Park Factor`)
    ) %>% 
    gtsave('MLB Temperature Table.png')


# plot with logo
plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/MLB/MLB Temperature Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/MLB/mlb-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "MLB Temperature Table.png with Logo.png")
