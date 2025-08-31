# Load packages
require(tidyverse)

# Load data

site_data <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/site_data_clean.csv') %>% 
  filter(Era == 'HM')

species_climate_preferences <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/species_climate_preferences.csv')


# Plot

ggplot(species_climate_preferences) +
  geom_point(aes(x = MAT,
                 y = MAP))





site_data <- site_data %>%
  relocate(
    c(cti_temp_extinction:precip_rate),
    .before = Callospermophiluslateralis
  )


facet_levels <- c(
  "Total",
  "Extinction",
  "Colonization",
  "Extinction cold-adapted",
  "Extinction warm-adapted",
  "Colonization cold-adapted",
  "Colonization warm-adapted"
)

plot_data <- site_data |>
  # total = extinction + colonization
  mutate(cti_temp_total = cti_temp_extinction + cti_temp_colonization) |>
  select(
    site_name, temp_rate,
    cti_temp_total,
    cti_temp_extinction,
    cti_temp_colonization,
    cti_temp_extinction_warm,
    cti_temp_extinction_cool,
    cti_temp_colonization_warm,
    cti_temp_colonization_cool
  ) |>
  pivot_longer(
    cols = starts_with("cti_temp"),
    names_to = "component",
    values_to = "cti_temp"
  ) |>
  mutate(
    component = recode(
      component,
      cti_temp_total = "Total",
      cti_temp_extinction = "Extinction",
      cti_temp_colonization = "Colonization",
      cti_temp_extinction_warm = "Extinction warm-adapted",
      cti_temp_extinction_cool = "Extinction cold-adapted",
      cti_temp_colonization_warm = "Colonization warm-adapted",
      cti_temp_colonization_cool = "Colonization cold-adapted"
    ),
    component = factor(component, levels = facet_levels)
  )


#####
#
#####

load('~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_attributes.Rdata')
font_family = "Helvetica"

site_data_lassen <- site_data %>% 
  filter(Region == 'LA')

# specify the seven measures in the desired left→right order
meas_levels <- c(
  "cti_rate",
  "cti_temp_extinction",
  "cti_temp_colonization",        # assuming this is what you meant by cti_rate_colonization
  "cti_temp_extinction_cool",
  "cti_temp_extinction_warm",
  "cti_temp_colonization_cool",
  "cti_temp_colonization_warm"
)

# gather into long format
plot_data <- site_data_lassen |>
  select(all_of(meas_levels)) |>
  pivot_longer(
    everything(),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(measure, levels = meas_levels),
    # pretty labels for x-axis
    label = dplyr::recode(
      measure,
      cti_rate = "CTI rate",
      cti_temp_extinction = "Extinction",
      cti_temp_colonization = "Colonization",
      cti_temp_extinction_cool = "Extinction (cold)",
      cti_temp_extinction_warm = "Extinction (warm)",
      cti_temp_colonization_cool = "Colonization (cold)",
      cti_temp_colonization_warm = "Colonization (warm)"
    )
  )

# build the 7-boxplot figure with dashed separators between 1–2 and 3–4
plot_LA <- ggplot(plot_data, aes(x = label, y = value, fill = measure)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_boxplot(width = 0.7, alpha = 0.6, outlier.alpha = 0.35,
               outlier.shape = NA) +
  # dashed vertical separators: positions are between categories (1.5, 3.5)
  geom_vline(xintercept = c(1.5, 3.5), linetype = "dashed", color = "grey40") +
  labs(
    x = NULL,
    y = "Value"
  ) +
  geom_jitter(aes(y = value,
                  fill = measure),
              color = 'white',
              pch = 21,
              size = 1.5,
              position = position_jitterdodge(jitter.width = 0.5)) +
  scale_color_manual(
    values = c(
      cti_rate = "#D4AA00",
      cti_temp_extinction = "#BB5566",
      cti_temp_colonization = "#3B8BC2",
      cti_temp_extinction_cool = "#6C8EBF",
      cti_temp_extinction_warm = "#E07A5F",
      cti_temp_colonization_cool = "#81B29A",
      cti_temp_colonization_warm = "#F2CC8F"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      cti_rate = "#D4AA00",
      cti_temp_extinction = "#BB5566",
      cti_temp_colonization = "#3B8BC2",
      cti_temp_extinction_cool = "#6C8EBF",
      cti_temp_extinction_warm = "#E07A5F",
      cti_temp_colonization_cool = "#81B29A",
      cti_temp_colonization_warm = "#F2CC8F"
    ),
    guide = "none"
  ) +
  labs(y = expression(paste("Thermophilization rate (°C ", yr^-1, ")"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_text(color = text_color,
                                size = text_size,
                                family = font_family),
    axis.text = element_text(color = 'black',
                               family = font_family,
                               size = text_size),
    axis.text.x = element_text(angle = 40, hjust = 0.5, vjust = 1,
                               color = 'black',
                               size = text_size,
                               family = font_family),
    plot.title = element_text(face = "bold")
  )






site_data_lassen <- site_data %>% 
  filter(Region == 'YO')

# specify the seven measures in the desired left→right order
meas_levels <- c(
  "cti_rate",
  "cti_temp_extinction",
  "cti_temp_colonization",        # assuming this is what you meant by cti_rate_colonization
  "cti_temp_extinction_cool",
  "cti_temp_extinction_warm",
  "cti_temp_colonization_cool",
  "cti_temp_colonization_warm"
)

# gather into long format
plot_data <- site_data_lassen |>
  select(all_of(meas_levels)) |>
  pivot_longer(
    everything(),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(measure, levels = meas_levels),
    # pretty labels for x-axis
    label = dplyr::recode(
      measure,
      cti_rate = "CTI rate",
      cti_temp_extinction = "Extinction",
      cti_temp_colonization = "Colonization",
      cti_temp_extinction_cool = "Extinction (cold)",
      cti_temp_extinction_warm = "Extinction (warm)",
      cti_temp_colonization_cool = "Colonization (cold)",
      cti_temp_colonization_warm = "Colonization (warm)"
    )
  )

# build the 7-boxplot figure with dashed separators between 1–2 and 3–4
plot_YO <- ggplot(plot_data, aes(x = label, y = value, fill = measure)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_boxplot(width = 0.7, alpha = 0.6, outlier.alpha = 0.35,
               outlier.shape = NA) +
  # dashed vertical separators: positions are between categories (1.5, 3.5)
  geom_vline(xintercept = c(1.5, 3.5), linetype = "dashed", color = "grey40") +
  labs(
    x = NULL,
    y = "Value"
  ) +
  geom_jitter(aes(y = value,
                  fill = measure),
              color = 'white',
              pch = 21,
              size = 1.5,
              position = position_jitterdodge(jitter.width = 0.5)) +
  scale_color_manual(
    values = c(
      cti_rate = "#D4AA00",
      cti_temp_extinction = "#BB5566",
      cti_temp_colonization = "#3B8BC2",
      cti_temp_extinction_cool = "#6C8EBF",
      cti_temp_extinction_warm = "#E07A5F",
      cti_temp_colonization_cool = "#81B29A",
      cti_temp_colonization_warm = "#F2CC8F"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      cti_rate = "#D4AA00",
      cti_temp_extinction = "#BB5566",
      cti_temp_colonization = "#3B8BC2",
      cti_temp_extinction_cool = "#6C8EBF",
      cti_temp_extinction_warm = "#E07A5F",
      cti_temp_colonization_cool = "#81B29A",
      cti_temp_colonization_warm = "#F2CC8F"
    ),
    guide = "none"
  ) +
  labs(y = expression(paste("Thermophilization rate (°C ", yr^-1, ")"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_text(color = text_color,
                                size = text_size,
                                family = font_family),
    axis.text = element_text(color = 'black',
                             family = font_family,
                             size = text_size),
    axis.text.x = element_text(angle = 40, hjust = 0.5, vjust = 1,
                               color = 'black',
                               size = text_size,
                               family = font_family),
    plot.title = element_text(face = "bold")
  )






site_data_lassen <- site_data %>% 
  filter(Region == 'SS')

# specify the seven measures in the desired left→right order
meas_levels <- c(
  "cti_rate",
  "cti_temp_extinction",
  "cti_temp_colonization",        # assuming this is what you meant by cti_rate_colonization
  "cti_temp_extinction_cool",
  "cti_temp_extinction_warm",
  "cti_temp_colonization_cool",
  "cti_temp_colonization_warm"
)

# gather into long format
plot_data <- site_data_lassen |>
  select(all_of(meas_levels)) |>
  pivot_longer(
    everything(),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(measure, levels = meas_levels),
    # pretty labels for x-axis
    label = dplyr::recode(
      measure,
      cti_rate = "CTI rate",
      cti_temp_extinction = "Extinction",
      cti_temp_colonization = "Colonization",
      cti_temp_extinction_cool = "Extinction (cold)",
      cti_temp_extinction_warm = "Extinction (warm)",
      cti_temp_colonization_cool = "Colonization (cold)",
      cti_temp_colonization_warm = "Colonization (warm)"
    )
  )

# build the 7-boxplot figure with dashed separators between 1–2 and 3–4
plot_SS <- ggplot(plot_data, aes(x = label, y = value, fill = measure)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_boxplot(width = 0.7, alpha = 0.6, outlier.alpha = 0.35,
               outlier.shape = NA) +
  # dashed vertical separators: positions are between categories (1.5, 3.5)
  geom_vline(xintercept = c(1.5, 3.5), linetype = "dashed", color = "grey40") +
  labs(
    x = NULL,
    y = "Value"
  ) +
  geom_jitter(aes(y = value,
                  fill = measure),
              color = 'white',
              pch = 21,
              size = 1.5,
              position = position_jitterdodge(jitter.width = 0.5)) +
  scale_color_manual(
    values = c(
      cti_rate = "#D4AA00",
      cti_temp_extinction = "#BB5566",
      cti_temp_colonization = "#3B8BC2",
      cti_temp_extinction_cool = "#6C8EBF",
      cti_temp_extinction_warm = "#E07A5F",
      cti_temp_colonization_cool = "#81B29A",
      cti_temp_colonization_warm = "#F2CC8F"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      cti_rate = "#D4AA00",
      cti_temp_extinction = "#BB5566",
      cti_temp_colonization = "#3B8BC2",
      cti_temp_extinction_cool = "#6C8EBF",
      cti_temp_extinction_warm = "#E07A5F",
      cti_temp_colonization_cool = "#81B29A",
      cti_temp_colonization_warm = "#F2CC8F"
    ),
    guide = "none"
  ) +
  labs(y = expression(paste("Thermophilization rate (°C ", yr^-1, ")"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_text(color = text_color,
                                size = text_size,
                                family = font_family),
    axis.text = element_text(color = 'black',
                             family = font_family,
                             size = text_size),
    axis.text.x = element_text(angle = 40, hjust = 0.5, vjust = 1,
                               color = 'black',
                               size = text_size,
                               family = font_family),
    plot.title = element_text(face = "bold")
  )






# Precip
site_data_lassen <- site_data %>% 
  filter(Region == 'LA')

# specify the seven measures in the desired left→right order
meas_levels <- c(
  "cpi_rate",
  "cpi_precip_extinction",
  "cpi_precip_colonization",        # assuming this is what you meant by cpi_rate_colonization
  "cpi_precip_extinction_wet",
  "cpi_precip_extinction_dry",
  "cpi_precip_colonization_wet",
  "cpi_precip_colonization_dry"
)

# gather into long format
plot_data <- site_data_lassen |>
  select(all_of(meas_levels)) |>
  pivot_longer(
    everything(),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(measure, levels = meas_levels),
    # pretty labels for x-axis
    label = dplyr::recode(
      measure,
      cpi_rate = "CPI rate",
      cpi_precip_extinction = "Extinction",
      cpi_precip_colonization = "Colonization",
      cpi_precip_extinction_wet = "Extinction (wet)",
      cpi_precip_extinction_dry = "Extinction (dry)",
      cpi_precip_colonization_wet = "Colonization (wet)",
      cpi_precip_colonization_dry = "Colonization (dry)"
    )
  )

# build the 7-boxplot figure with dashed separators between 1–2 and 3–4
plot_LA_precip <- ggplot(plot_data, aes(x = label, y = value, fill = measure)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_boxplot(width = 0.7, alpha = 0.6, outlier.alpha = 0.35,
               outlier.shape = NA) +
  # dashed vertical separators: positions are between categories (1.5, 3.5)
  geom_vline(xintercept = c(1.5, 3.5), linetype = "dashed", color = "grey40") +
  labs(
    x = NULL,
    y = "Value"
  ) +
  geom_jitter(aes(y = value,
                  fill = measure),
              color = 'white',
              pch = 21,
              size = 1.5,
              position = position_jitterdodge(jitter.width = 0.5)) +
  scale_color_manual(
    values = c(
      cpi_rate = "#D4AA00",
      cpi_precip_extinction = "#BB5566",
      cpi_precip_colonization = "#3B8BC2",
      cpi_precip_extinction_wet = "#6C8EBF",
      cpi_precip_extinction_dry = "#E07A5F",
      cpi_precip_colonization_wet = "#81B29A",
      cpi_precip_colonization_dry = "#F2CC8F"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      cpi_rate = "#D4AA00",
      cpi_precip_extinction = "#BB5566",
      cpi_precip_colonization = "#3B8BC2",
      cpi_precip_extinction_wet = "#6C8EBF",
      cpi_precip_extinction_dry = "#E07A5F",
      cpi_precip_colonization_wet = "#81B29A",
      cpi_precip_colonization_dry = "#F2CC8F"
    ),
    guide = "none"
  ) +
  labs(y = expression(paste("Mesophilization rate (mm ", yr^-1, ")"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_text(color = text_color,
                                size = text_size,
                                family = font_family),
    axis.text = element_text(color = 'black',
                             family = font_family,
                             size = text_size),
    axis.text.x = element_text(angle = 40, hjust = 0.5, vjust = 1,
                               color = 'black',
                               size = text_size,
                               family = font_family),
    plot.title = element_text(face = "bold")
  )




site_data_lassen <- site_data %>% 
  filter(Region == 'YO')

# specify the seven measures in the desired left→right order
meas_levels <- c(
  "cpi_rate",
  "cpi_precip_extinction",
  "cpi_precip_colonization",        # assuming this is what you meant by cpi_rate_colonization
  "cpi_precip_extinction_wet",
  "cpi_precip_extinction_dry",
  "cpi_precip_colonization_wet",
  "cpi_precip_colonization_dry"
)

# gather into long format
plot_data <- site_data_lassen |>
  select(all_of(meas_levels)) |>
  pivot_longer(
    everything(),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(measure, levels = meas_levels),
    # pretty labels for x-axis
    label = dplyr::recode(
      measure,
      cpi_rate = "CPI rate",
      cpi_precip_extinction = "Extinction",
      cpi_precip_colonization = "Colonization",
      cpi_precip_extinction_wet = "Extinction (wet)",
      cpi_precip_extinction_dry = "Extinction (dry)",
      cpi_precip_colonization_wet = "Colonization (wet)",
      cpi_precip_colonization_dry = "Colonization (dry)"
    )
  )

# build the 7-boxplot figure with dashed separators between 1–2 and 3–4
plot_YO_precip <- ggplot(plot_data, aes(x = label, y = value, fill = measure)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_boxplot(width = 0.7, alpha = 0.6, outlier.alpha = 0.35,
               outlier.shape = NA) +
  # dashed vertical separators: positions are between categories (1.5, 3.5)
  geom_vline(xintercept = c(1.5, 3.5), linetype = "dashed", color = "grey40") +
  labs(
    x = NULL,
    y = "Value"
  ) +
  geom_jitter(aes(y = value,
                  fill = measure),
              color = 'white',
              pch = 21,
              size = 1.5,
              position = position_jitterdodge(jitter.width = 0.5)) +
  scale_color_manual(
    values = c(
      cpi_rate = "#D4AA00",
      cpi_precip_extinction = "#BB5566",
      cpi_precip_colonization = "#3B8BC2",
      cpi_precip_extinction_wet = "#6C8EBF",
      cpi_precip_extinction_dry = "#E07A5F",
      cpi_precip_colonization_wet = "#81B29A",
      cpi_precip_colonization_dry = "#F2CC8F"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      cpi_rate = "#D4AA00",
      cpi_precip_extinction = "#BB5566",
      cpi_precip_colonization = "#3B8BC2",
      cpi_precip_extinction_wet = "#6C8EBF",
      cpi_precip_extinction_dry = "#E07A5F",
      cpi_precip_colonization_wet = "#81B29A",
      cpi_precip_colonization_dry = "#F2CC8F"
    ),
    guide = "none"
  ) +
  labs(y = expression(paste("Mesophilization rate (mm ", yr^-1, ")"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_text(color = text_color,
                                size = text_size,
                                family = font_family),
    axis.text = element_text(color = 'black',
                             family = font_family,
                             size = text_size),
    axis.text.x = element_text(angle = 40, hjust = 0.5, vjust = 1,
                               color = 'black',
                               size = text_size,
                               family = font_family),
    plot.title = element_text(face = "bold")
  )



site_data_lassen <- site_data %>% 
  filter(Region == 'SS')

# specify the seven measures in the desired left→right order
meas_levels <- c(
  "cpi_rate",
  "cpi_precip_extinction",
  "cpi_precip_colonization",        # assuming this is what you meant by cpi_rate_colonization
  "cpi_precip_extinction_wet",
  "cpi_precip_extinction_dry",
  "cpi_precip_colonization_wet",
  "cpi_precip_colonization_dry"
)

# gather into long format
plot_data <- site_data_lassen |>
  select(all_of(meas_levels)) |>
  pivot_longer(
    everything(),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(measure, levels = meas_levels),
    # pretty labels for x-axis
    label = dplyr::recode(
      measure,
      cpi_rate = "CPI rate",
      cpi_precip_extinction = "Extinction",
      cpi_precip_colonization = "Colonization",
      cpi_precip_extinction_wet = "Extinction (wet)",
      cpi_precip_extinction_dry = "Extinction (dry)",
      cpi_precip_colonization_wet = "Colonization (wet)",
      cpi_precip_colonization_dry = "Colonization (dry)"
    )
  )

# build the 7-boxplot figure with dashed separators between 1–2 and 3–4
plot_SS_precip <- ggplot(plot_data, aes(x = label, y = value, fill = measure)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_boxplot(width = 0.7, alpha = 0.6, outlier.alpha = 0.35,
               outlier.shape = NA) +
  # dashed vertical separators: positions are between categories (1.5, 3.5)
  geom_vline(xintercept = c(1.5, 3.5), linetype = "dashed", color = "grey40") +
  labs(
    x = NULL,
    y = "Value"
  ) +
  geom_jitter(aes(y = value,
                  fill = measure),
              color = 'white',
              pch = 21,
              size = 1.5,
              position = position_jitterdodge(jitter.width = 0.5)) +
  scale_color_manual(
    values = c(
      cpi_rate = "#D4AA00",
      cpi_precip_extinction = "#BB5566",
      cpi_precip_colonization = "#3B8BC2",
      cpi_precip_extinction_wet = "#6C8EBF",
      cpi_precip_extinction_dry = "#E07A5F",
      cpi_precip_colonization_wet = "#81B29A",
      cpi_precip_colonization_dry = "#F2CC8F"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      cpi_rate = "#D4AA00",
      cpi_precip_extinction = "#BB5566",
      cpi_precip_colonization = "#3B8BC2",
      cpi_precip_extinction_wet = "#6C8EBF",
      cpi_precip_extinction_dry = "#E07A5F",
      cpi_precip_colonization_wet = "#81B29A",
      cpi_precip_colonization_dry = "#F2CC8F"
    ),
    guide = "none"
  ) +
  labs(y = expression(paste("Mesophilization rate (mm ", yr^-1, ")"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_text(color = text_color,
                                size = text_size,
                                family = font_family),
    axis.text = element_text(color = 'black',
                             family = font_family,
                             size = text_size),
    axis.text.x = element_text(angle = 40, hjust = 0.5, vjust = 1,
                               color = 'black',
                               size = text_size,
                               family = font_family),
    plot.title = element_text(face = "bold")
  )















extinction_colonization_plot <- ggpubr::ggarrange(plot_LA,
                                                  plot_LA_precip,
                                                  plot_YO,
                                                  plot_YO_precip,
                                                  plot_SS,
                                                  plot_SS_precip,
                                                  ncol = 2,
                                                  nrow = 3,
                                                  labels = 'auto')

ggsave(extinction_colonization_plot,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/extinction_colonization_plot.jpg',
       height = 11,
       width = 8.5,
       units = 'in')



# Analysis
View(site_data)
ext_col_analysis_data <- site_data %>% 
  select(site_name,
         Region,
         cti_rate,
         cti_temp_colonization,
         cti_temp_extinction,
         cti_temp_colonization_cool,
         cti_temp_colonization_warm,
         cti_temp_extinction_cool,
         cti_temp_extinction_warm,
         cpi_rate,
         cpi_precip_colonization,
         cpi_precip_extinction,
         cpi_precip_colonization_wet,
         cpi_precip_colonization_dry,
         cpi_precip_extinction_wet,
         cpi_precip_extinction_dry) %>% 
  reshape2::melt(id.vars = c('site_name',
                             'Region'))

# wilcox_results <- ext_col_analysis_data |>
#   group_by(Region, variable) |>
#   summarise(
#     n    = length(value),
#     mean = mean(value),
#     vals = list(value),
#     test = map(vals, ~ wilcox.test(.x, mu = 0)),
#     .groups = "drop"
#   ) |>
#   mutate(
#     statistic = map_dbl(test, ~ unname(.x$statistic)),
#     p_value   = map_dbl(test, ~ .x$p.value),
#     method    = map_chr(test, ~ .x$method)
#   ) |>
#   select(Region, variable, n, mean, statistic, p_value, method)

wilcox_results <- ext_col_analysis_data |>
  group_by(Region, variable) |>
  summarise(
    n        = length(value),
    mean     = mean(value),
    statistic     = wilcox.test(value, mu = 0)$statistic,
    p_value  = wilcox.test(value, mu = 0)$p.value,
    .groups  = "drop"
  )


# Save Wilcox Results
save(wilcox_results,
     file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/colonization_extinction_wilcox_results/colonization_extinction_wilcox_results.RData')

######
# Specify the min, max, and average number of species per site.
#####
View(site_data)

species_summary <- site_data |>
  rowwise() |>
  mutate(species_richness = sum(c_across(Callospermophiluslateralis:Zapusprinceps),
                                na.rm = TRUE)) |>
  ungroup() |>
  summarise(
    min_species  = min(species_richness),
    max_species  = max(species_richness),
    mean_species = mean(species_richness)
  )

species_summary


#####
# Caveats that are the result of efforts extracting climate by year.
#####

# Load occurrence_data_merged_with_climate_thinned
occurrence_data <- read_csv(file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_with_climate_thinned.csv')
colnames(occurrence_data)

summary_results <- occurrence_data |>
  filter(!is.na(year)) |>
  arrange(year) |>
  group_by(species) |>
  group_modify(\(.x, .y) {
    n <- nrow(.x)
    n_slice <- max(1, floor(0.2 * n))
    
    first_slice <- .x |> slice_head(n = n_slice)
    last_slice  <- .x |> slice_tail(n = n_slice)
    
    tibble(
      n_first      = nrow(first_slice),
      temp_first   = mean(first_slice$med_ann_temp, na.rm = TRUE),
      precip_first = mean(first_slice$med_ann_precip, na.rm = TRUE),
      year_first   = min(first_slice$year, na.rm = TRUE),
      
      n_last      = nrow(last_slice),
      temp_last   = mean(last_slice$med_ann_temp, na.rm = TRUE),
      precip_last = mean(last_slice$med_ann_precip, na.rm = TRUE),
      year_last   = min(last_slice$year, na.rm = TRUE)
    )
  }) |>
  ungroup()

ggplot(data = summary_results) +
  geom_point(aes(x = temp_first,
                 y = temp_last)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")


summary_results <- summary_results %>% 
  mutate(temp_time_dif = temp_last - temp_first)

t.test(summary_results$temp_time_dif,
       mu = 0)
hist(summary_results$temp_time_dif)

ggplot(data = summary_results) +
  geom_boxplot(aes())

mean_years <- occurrence_data |>
  filter(!is.na(year)) |>
  group_by(species) |>
  summarise(mean_year = mean(year, na.rm = TRUE), .groups = "drop")

ggplot(mean_years) +
  geom_point(aes(x = species, y = mean_year)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

