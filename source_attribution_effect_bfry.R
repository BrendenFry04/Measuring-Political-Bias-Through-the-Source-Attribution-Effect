# Required libraries
# install.packages(c("haven", "dplyr", "tidyr",
#                    "ggplot2", "gganimate", "gifski"))

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(gifski)

# Step 1: Read the actual Study 3 dataset
data_file <- tempfile(fileext = ".sav")

download.file(
  "https://osf.io/download/4tgud/",
  destfile = data_file,
  mode = "wb"
)

original_data <- read_sav(data_file)

# Step 2: Prepare the variables
study_data <- original_data %>%
  transmute(
    affiliation = factor(
      as.numeric(OVERALL_POL_A2),
      levels = c(1, 2),
      labels = c("Liberal", "Conservative")
    ),
    support = factor(
      as.numeric(OVERALL_POL_EXT),
      levels = c(1, 2),
      labels = c("Weaker party support", "Stronger party support")
    ),
    attribution = factor(
      as.numeric(ORIGIN),
      levels = c(1, 2, 3),
      labels = c(
        "Correctly labeled",
        "Unlabeled",
        "Incorrectly labeled"
      )
    ),
    Democratic = as.numeric(QUOTES.D),
    Republican = as.numeric(QUOTES.R)
  ) %>%
  pivot_longer(
    cols = c(Democratic, Republican),
    names_to = "quote_origin",
    values_to = "agreement"
  ) %>%
  mutate(
    # Rescale the original 1–7 ratings to your 1–6 scale
    agreement = 1 + (agreement - 1) * 5 / 6,
    
    quote_origin = factor(
      quote_origin,
      levels = c("Democratic", "Republican"),
      labels = c(
        "Democratic quotes — Biden proxy",
        "Republican quotes — Trump proxy"
      )
    ),
    
    condition_number = as.numeric(attribution)
  ) %>%
  filter(
    !is.na(affiliation),
    !is.na(support),
    !is.na(attribution),
    !is.na(agreement)
  )

# Step 3: Calculate means and 95% confidence intervals
plot_data <- study_data %>%
  group_by(
    affiliation,
    support,
    quote_origin,
    condition_number
  ) %>%
  summarise(
    n = n(),
    mean_agreement = mean(agreement),
    se = sd(agreement) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    margin = qt(0.975, df = n - 1) * se,
    lower = mean_agreement - margin,
    upper = mean_agreement + margin
  )

# Step 4: Recreate Study 3's four-panel line-graph format
p <- ggplot(
  plot_data,
  aes(
    x = condition_number,
    y = mean_agreement,
    color = affiliation,
    group = affiliation
  )
) +
  geom_line(linewidth = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.06,
    color = "black"
  ) +
  geom_point(
    shape = 21,
    fill = "white",
    size = 3,
    stroke = 0.8
  ) +
  facet_grid(quote_origin ~ support) +
  scale_color_manual(values = c(
    Liberal = "blue",
    Conservative = "red"
  )) +
  scale_x_continuous(
    breaks = 1:3,
    labels = c(
      "Correctly labeled",
      "Unlabeled",
      "Incorrectly labeled"
    ),
    limits = c(0.8, 3.2)
  ) +
  scale_y_continuous(breaks = 1:6) +
  coord_cartesian(ylim = c(1, 6)) +
  labs(
    title = "Source Attribution and Agreement",
    subtitle = "Adaptation of Study 3",
    x = "Source Attribution",
    y = "Mean Agreement (1–6; 95% CI)",
    color = NULL,
    caption = paste(
      "Hanel et al. (2018) data; agreement rescaled from 1–7.",
      "Biden/Trump are proxy labels, not actual Biden/Trump-only data.",
      sep = "\n"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Step 5: Animate the lines across attribution conditions
# The horizontal progression represents categories, not response time.
anim <- p +
  transition_reveal(condition_number)

animated_gif <- animate(
  anim,
  nframes = 120,
  fps = 15,
  width = 1200,
  height = 800,
  start_pause = 15,
  end_pause = 40,
  renderer = gifski_renderer()
)

# Step 6: Save the GIF
anim_save(
  "source_attribution_study3_style.gif",
  animation = animated_gif
)

animated_gif

