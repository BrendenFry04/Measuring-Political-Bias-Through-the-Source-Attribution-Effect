#Measuring Political Bias Through the Source Attribution Effect

#===============================================================================

#install required packages
install.packages(c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "gganimate",
  "gifski",
  "ggh4x"
))

#load required packages
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(ggh4x)

#===============================================================================

# Step 1: Define the agreement ratings and response times
model_data <- tibble::tribble(
  ~politician, ~measure, ~affiliation, ~correct, ~none, ~false,
  "Trump", "Agreement (1–6)", "Democrats", 4, 6, 4.5,
  "Trump", "Agreement (1–6)", "Republicans", 6, 5, 6,
  "Trump", "Agreement (1–6)", "Independents", 5.5, 6, 6,
  "Biden", "Agreement (1–6)", "Democrats", 6, 6, 5,
  "Biden", "Agreement (1–6)", "Republicans", 3, 5, 3,
  "Biden", "Agreement (1–6)", "Independents", 6, 6, 6,
  "Trump", "Response time (seconds)", "Democrats", 8.24, 15.56, 7.43,
  "Trump", "Response time (seconds)", "Republicans", 5.49, 17.32, 4.75,
  "Trump", "Response time (seconds)", "Independents", 10.15, 16.23, 15.67,
  "Biden", "Response time (seconds)", "Democrats", 8.43, 12.43, 10.04,
  "Biden", "Response time (seconds)", "Republicans", 3.30, 13.75, 4.38,
  "Biden", "Response time (seconds)", "Independents", 12.13, 11.98, 15.67
)

#===============================================================================

# Step 2: Arrange the variables and attribution conditions
plot_data <- model_data %>%
  pivot_longer(
    cols = c(correct, none, false),
    names_to = "attribution",
    values_to = "value"
  ) %>%
  mutate(
    politician = factor(politician, levels = c("Trump", "Biden")),
    measure = factor(measure, levels = c(
      "Agreement (1–6)", "Response time (seconds)"
    )),
    affiliation = factor(affiliation, levels = c(
      "Democrats", "Republicans", "Independents"
    )),
    condition_number = match(attribution, c("correct", "none", "false"))
  ) %>%
  arrange(politician, measure, affiliation, condition_number)

#===============================================================================

# Step 3: Create the four-panel line graph
p <- ggplot(
  plot_data,
  aes(x = condition_number, y = value,
      color = affiliation, group = affiliation)
) +
  geom_line(linewidth = 0.7) +
  geom_point(shape = 21, fill = "white", size = 3, stroke = 0.8) +
  facet_grid2(measure ~ politician, scales = "free_y",
              axes = "all", remove_labels = "none", switch = "y") +
  facetted_pos_scales(y = list(
    measure == "Agreement (1–6)" ~ scale_y_continuous(
      limits = c(1, 6), breaks = c(2, 4, 6),
      minor_breaks = c(1, 3, 5),
      expand = expansion(mult = 0.04)
    ),
    measure == "Response time (seconds)" ~ scale_y_continuous(
      limits = c(0, 20), breaks = seq(0, 20, 5),
      expand = expansion(mult = 0.04)
    )
  )) +
  scale_color_manual(values = c(
    Democrats = "blue",
    Republicans = "red",
    Independents = "forestgreen"
  )) +
  scale_x_continuous(
    breaks = 1:3,
    labels = c("Correctly attributed", "No attribution", "Falsely attributed"),
    limits = c(0.4, 3.6),
    minor_breaks = NULL
  ) +
  labs(
    title = "Measuring Political Bias Through the Source Attribution Effect",
    x = "Source attribution",
    y = NULL,
    color = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 21),
    legend.position = "bottom",
    legend.key.width = grid::unit(1.3, "cm"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey94"),
    panel.border = element_rect(color = "grey55", fill = NA),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 19, face = "bold"),
    strip.text.y.left = element_text(angle = 90, size = 16),
    axis.text = element_text(color = "black", size = 13),
    axis.title.x = element_text(size = 16),
    panel.spacing = grid::unit(1.1, "lines"),
    plot.margin = margin(12, 15, 12, 15)
  )

#===============================================================================

# Step 4: Animate the lines across attribution conditions
anim <- p + transition_reveal(condition_number)

animated_gif <- animate(
  anim,
  nframes = 230,
  fps = 15,
  width = 900,
  height = 600,
  start_pause = 15,
  end_pause = 150,
  renderer = gifski_renderer()
)

#===============================================================================

# Step 5: Save the GIF and print its location
output_file <- file.path(path.expand("~/Desktop"), "measuring_political_bias.gif")
if (!dir.exists(dirname(output_file))) {
  output_file <- file.path(getwd(), "measuring_political_bias.gif")
}
anim_save(output_file, animation = animated_gif)
message("GIF saved to: ", normalizePath(output_file, mustWork = FALSE))

#===============================================================================

# Step 6: Display the animated GIF
animated_gif

#===============================================================================
