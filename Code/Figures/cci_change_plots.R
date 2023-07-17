#Author: Ethan Abercrombie

####
#Load Data
####

site_data <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/site_data_clean.csv')

community_data <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/site_data/community_data.csv') %>% 
  filter(Era == "H" |
           Era == "HM") %>% 
  mutate(site_name = if_else(str_detect(site_name,
                                        '_HM'),
                             str_remove(site_name,
                                        '_HM'),
                             site_name)) %>% 
  group_by(site_name) %>% 
  summarise(cti_change = diff(CTI),
            cpi_change = diff(CPI),
            Region = Region,
            Elev = Elev,
            CTI = CTI,
            CPI = CPI,
            site_name = site_name,
            Era = Era) %>% 
  mutate(temp_direction = as.factor(case_when(cti_change > 0 ~ 'increasing',
                                              cti_change < 0 ~ 'decreasing',
                                              cti_change == 0 ~ 'no_change')),
         precip_direction = as.factor(case_when(cpi_change > 0 ~ 'increasing',
                                                cpi_change < 0 ~ 'decreasing',
                                                cpi_change == 0 ~'no_change'))) %>% 
  distinct()

#General Graph Attributes

font_family = 'Helvetica'
text_size = 12
text_color = 'black'

####
#Yosemite
####

#Community Temperature Index

yosemite_cti_point <- ggplot(data = filter(site_data,
                                           Region == "YO")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = cti_change,
                   color = case_when(cti_change > 0 ~ 'increasing',
                                     cti_change < 0 ~ 'decreasing',
                                     cti_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev,
                 y = cti_change,
                 color = case_when(cti_change > 0 ~ 'increasing',
                                   cti_change < 0 ~ 'decreasing',
                                   cti_change == 0 ~ 'no_change'),
                 shape = case_when(cti_change > 0 | cti_change < 0 ~ 'change',
                                   cti_change == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab(expression(Delta ~ "CTI (°C)")) +
  theme(legend.position = 'none',
        text = element_text(color = text_color,
                            family = font_family),
        panel.border = element_rect(color = 'black',
                                    fill = NA,
                                    size = 2),
        plot.margin = margin(t = 5,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 5),
        axis.line = element_line(color = 'black',
                                 linewidth = 1))

yosemite_cti_bar <- ggplot(data = filter(site_data,
                                         Region == 'YO')) +
  geom_bar(aes(x = case_when(cti_change > 0 ~ 'increasing',
                             cti_change < 0 ~ 'decreasing',
                             cti_change == 0 ~ 'no change'),
               fill = case_when(cti_change > 0 ~ 'increasing',
                                cti_change < 0 ~ 'decreasing',
                                cti_change == 0 ~ 'no change'),
               color = case_when(cti_change > 0 ~ 'increasing',
                                 cti_change < 0 ~ 'decreasing',
                                 cti_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#eb8055ff','#664596ff','NA')) +
  scale_color_manual(values = c('transparent','transparent','black')) +
  ylab("Number of Sites") +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = 'black',
                                 family = font_family),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 15),
        legend.position = 'none')

#Merge panels into a single figure.
(yosemite_cti_merged <- yosemite_cti_point + yosemite_cti_bar +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 1,
      widths = c(6,1.5)
    ) +
    patchwork::plot_annotation(
      theme = theme(plot.background = element_rect(fill = 'white'),
                    plot.margin = margin(t = 0,  # Top margin
                                         r = 0,  # Right margin
                                         b = 0,  # Bottom margin
                                         l = 0))
    )
)

# Save Figure.

ggsave(yosemite_cti_merged,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/fig_changecti_yosemite.jpg',
       units = 'in',
       dpi = 300,
       width = 7.5,
       height = 4.5)

#Community Precipitation Index

yosemite_cpi_point <- ggplot(data = filter(site_data,
                                           Region == "YO")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = cpi_change,
                   color = case_when(cpi_change > 0 ~ 'increasing',
                                     cpi_change < 0 ~ 'decreasing',
                                     cpi_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev,
                 y = cpi_change,
                 color = case_when(cpi_change > 0 ~ 'increasing',
                                   cpi_change < 0 ~ 'decreasing',
                                   cpi_change == 0 ~ 'no_change'),
                 shape = case_when(cpi_change > 0 | cpi_change < 0 ~ 'change',
                                   cpi_change == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab(expression(Delta ~ "CPI (mm)")) +
  theme(legend.position = 'none',
        text = element_text(color = text_color,
                            family = font_family),
        panel.border = element_rect(color = 'black',
                                    fill = NA,
                                    size = 2),
        plot.margin = margin(t = 5,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 5),
        axis.line = element_line(color = 'black',
                                 linewidth = 1))

yosemite_cpi_bar <- ggplot(data = filter(site_data,
                                         Region == 'YO')) +
  geom_bar(aes(x = case_when(cpi_change > 0 ~ 'increasing',
                             cpi_change < 0 ~ 'decreasing',
                             cpi_change == 0 ~ 'no change'),
               fill = case_when(cpi_change > 0 ~ 'increasing',
                                cpi_change < 0 ~ 'decreasing',
                                cpi_change == 0 ~ 'no change'),
               color = case_when(cpi_change > 0 ~ 'increasing',
                                 cpi_change < 0 ~ 'decreasing',
                                 cpi_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#eb8055ff','#664596ff','NA')) +
  scale_color_manual(values = c('transparent','transparent','black')) +
  ylab("Number of Sites") +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = 'black',
                                 family = font_family),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 15),
        legend.position = 'none')

#Merge panels into a single figure.
(yosemite_cpi_merged <- yosemite_cpi_point + yosemite_cpi_bar +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 1,
      widths = c(6,1.5)
    ) +
    patchwork::plot_annotation(
      theme = theme(plot.background = element_rect(fill = 'white'),
                    plot.margin = margin(t = 0,  # Top margin
                                         r = 0,  # Right margin
                                         b = 0,  # Bottom margin
                                         l = 0))
    )
)

# Save Figure.

ggsave(yosemite_cpi_merged,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/fig_changecpi_yosemite.jpg',
       units = 'in',
       dpi = 300,
       width = 7.5,
       height = 4.5)

####
#Sequoia & Kings Canyon
####

#Community Temperature Index

sequoia_cti_point <- ggplot(data = filter(site_data,
                                           Region == "SS")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = cti_change,
                   color = case_when(cti_change > 0 ~ 'increasing',
                                     cti_change < 0 ~ 'decreasing',
                                     cti_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev,
                 y = cti_change,
                 color = case_when(cti_change > 0 ~ 'increasing',
                                   cti_change < 0 ~ 'decreasing',
                                   cti_change == 0 ~ 'no_change'),
                 shape = case_when(cti_change > 0 | cti_change < 0 ~ 'change',
                                   cti_change == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab(expression(Delta ~ "CTI (°C)")) +
  theme(legend.position = 'none',
        text = element_text(color = text_color,
                            family = font_family),
        panel.border = element_rect(color = 'black',
                                    fill = NA,
                                    size = 2),
        plot.margin = margin(t = 5,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 5),
        axis.line = element_line(color = 'black',
                                 linewidth = 1))

sequoia_cti_bar <- ggplot(data = filter(site_data,
                                         Region == 'SS')) +
  geom_bar(aes(x = case_when(cti_change > 0 ~ 'increasing',
                             cti_change < 0 ~ 'decreasing',
                             cti_change == 0 ~ 'no change'),
               fill = case_when(cti_change > 0 ~ 'increasing',
                                cti_change < 0 ~ 'decreasing',
                                cti_change == 0 ~ 'no change'),
               color = case_when(cti_change > 0 ~ 'increasing',
                                 cti_change < 0 ~ 'decreasing',
                                 cti_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#eb8055ff','#664596ff','NA')) +
  scale_color_manual(values = c('transparent','transparent','black')) +
  ylab("Number of Sites") +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = 'black',
                                 family = font_family),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 15),
        legend.position = 'none')

#Merge panels into a single figure.
(sequoia_cti_merged <- sequoia_cti_point + sequoia_cti_bar +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 1,
      widths = c(6,1.5)
    ) +
    patchwork::plot_annotation(
      theme = theme(plot.background = element_rect(fill = 'white'),
                    plot.margin = margin(t = 0,  # Top margin
                                         r = 0,  # Right margin
                                         b = 0,  # Bottom margin
                                         l = 0))
    )
)

# Save Figure.

ggsave(sequoia_cti_merged,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/fig_changecti_seqoia.jpg',
       units = 'in',
       dpi = 300,
       width = 7.5,
       height = 4.5)

#Community Precipitation Index

sequoia_cpi_point <- ggplot(data = filter(site_data,
                                           Region == "SS")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = cpi_change,
                   color = case_when(cpi_change > 0 ~ 'increasing',
                                     cpi_change < 0 ~ 'decreasing',
                                     cpi_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev,
                 y = cpi_change,
                 color = case_when(cpi_change > 0 ~ 'increasing',
                                   cpi_change < 0 ~ 'decreasing',
                                   cpi_change == 0 ~ 'no_change'),
                 shape = case_when(cpi_change > 0 | cpi_change < 0 ~ 'change',
                                   cpi_change == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab(expression(Delta ~ "CPI (mm)")) +
  theme(legend.position = 'none',
        text = element_text(color = text_color,
                            family = font_family),
        panel.border = element_rect(color = 'black',
                                    fill = NA,
                                    size = 2),
        plot.margin = margin(t = 5,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 5),
        axis.line = element_line(color = 'black',
                                 linewidth = 1))

sequoia_cpi_bar <- ggplot(data = filter(site_data,
                                         Region == 'SS')) +
  geom_bar(aes(x = case_when(cpi_change > 0 ~ 'increasing',
                             cpi_change < 0 ~ 'decreasing',
                             cpi_change == 0 ~ 'no change'),
               fill = case_when(cpi_change > 0 ~ 'increasing',
                                cpi_change < 0 ~ 'decreasing',
                                cpi_change == 0 ~ 'no change'),
               color = case_when(cpi_change > 0 ~ 'increasing',
                                 cpi_change < 0 ~ 'decreasing',
                                 cpi_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#eb8055ff','#664596ff','NA')) +
  scale_color_manual(values = c('transparent','transparent','black')) +
  ylab("Number of Sites") +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = 'black',
                                 family = font_family),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 15),
        legend.position = 'none')

#Merge panels into a single figure.
(sequoia_cpi_merged <- sequoia_cpi_point + sequoia_cpi_bar +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 1,
      widths = c(6,1.5)
    ) +
    patchwork::plot_annotation(
      theme = theme(plot.background = element_rect(fill = 'white'),
                    plot.margin = margin(t = 0,  # Top margin
                                         r = 0,  # Right margin
                                         b = 0,  # Bottom margin
                                         l = 0))
    )
)

# Save Figure.

ggsave(sequoia_cpi_merged,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/fig_changecpi_seqoia.jpg',
       units = 'in',
       dpi = 300,
       width = 7.5,
       height = 4.5)

####
#Lassen
####

#Community Temperature Index

lassen_cti_point <- ggplot(data = filter(site_data,
                                          Region == "LA")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = cti_change,
                   color = case_when(cti_change > 0 ~ 'increasing',
                                     cti_change < 0 ~ 'decreasing',
                                     cti_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cti_change,
                 color = case_when(cti_change > 0 ~ 'increasing',
                                   cti_change < 0 ~ 'decreasing',
                                   cti_change == 0 ~ 'no_change'),
                 shape = case_when(cti_change > 0 | cti_change < 0 ~ 'change',
                                   cti_change == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab(expression(Delta ~ "CTI (°C)")) +
  theme(legend.position = 'none',
        text = element_text(color = text_color,
                            family = font_family),
        panel.border = element_rect(color = 'black',
                                    fill = NA,
                                    size = 2),
        plot.margin = margin(t = 5,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 5),
        axis.line = element_line(color = 'black',
                                 linewidth = 1))

lassen_cti_bar <- ggplot(data = filter(site_data,
                                        Region == 'LA')) +
  geom_bar(aes(x = case_when(cti_change > 0 ~ 'increasing',
                             cti_change < 0 ~ 'decreasing',
                             cti_change == 0 ~ 'no change'),
               fill = case_when(cti_change > 0 ~ 'increasing',
                                cti_change < 0 ~ 'decreasing',
                                cti_change == 0 ~ 'no change'),
               color = case_when(cti_change > 0 ~ 'increasing',
                                 cti_change < 0 ~ 'decreasing',
                                 cti_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#eb8055ff','#664596ff','NA')) +
  scale_color_manual(values = c('transparent','transparent','black')) +
  ylab("Number of Sites") +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = 'black',
                                 family = font_family),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 15),
        legend.position = 'none')

#Merge panels into a single figure.
(lassen_cti_merged <- lassen_cti_point + lassen_cti_bar +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 1,
      widths = c(6,1.5)
    ) +
    patchwork::plot_annotation(
      theme = theme(plot.background = element_rect(fill = 'white'),
                    plot.margin = margin(t = 0,  # Top margin
                                         r = 0,  # Right margin
                                         b = 0,  # Bottom margin
                                         l = 0))
    )
)

# Save Figure.

ggsave(lassen_cti_merged,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/fig_changecti_lassen.jpg',
       units = 'in',
       dpi = 300,
       width = 7.5,
       height = 4.5)

#Community Precipitation Index

lassen_cpi_point <- ggplot(data = filter(site_data,
                                          Region == "LA")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = cpi_change,
                   color = case_when(cpi_change > 0 ~ 'increasing',
                                     cpi_change < 0 ~ 'decreasing',
                                     cpi_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cpi_change,
                 color = case_when(cpi_change > 0 ~ 'increasing',
                                   cpi_change < 0 ~ 'decreasing',
                                   cpi_change == 0 ~ 'no_change'),
                 shape = case_when(cpi_change > 0 | cpi_change < 0 ~ 'change',
                                   cpi_change == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab(expression(Delta ~ "CPI (mm)")) +
  theme(legend.position = 'none',
        text = element_text(color = text_color,
                            family = font_family),
        panel.border = element_rect(color = 'black',
                                    fill = NA,
                                    size = 2),
        plot.margin = margin(t = 5,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 5),
        axis.line = element_line(color = 'black',
                                 linewidth = 1))

lassen_cpi_bar <- ggplot(data = filter(site_data,
                                        Region == 'LA')) +
  geom_bar(aes(x = case_when(cpi_change > 0 ~ 'increasing',
                             cpi_change < 0 ~ 'decreasing',
                             cpi_change == 0 ~ 'no change'),
               fill = case_when(cpi_change > 0 ~ 'increasing',
                                cpi_change < 0 ~ 'decreasing',
                                cpi_change == 0 ~ 'no change'),
               color = case_when(cpi_change > 0 ~ 'increasing',
                                 cpi_change < 0 ~ 'decreasing',
                                 cpi_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#eb8055ff','#664596ff','NA')) +
  scale_color_manual(values = c('transparent','transparent','black')) +
  ylab("Number of Sites") +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = 'black',
                                 family = font_family),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 15),
        legend.position = 'none')

#Merge panels into a single figure.
(lassen_cpi_merged <- lassen_cpi_point + lassen_cpi_bar +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 1,
      widths = c(6,1.5)
    ) +
    patchwork::plot_annotation(
      theme = theme(plot.background = element_rect(fill = 'white'),
                    plot.margin = margin(t = 0,  # Top margin
                                         r = 0,  # Right margin
                                         b = 0,  # Bottom margin
                                         l = 0))
    )
)

# Save Figure.

ggsave(lassen_cpi_merged,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/fig_changecpi_lassen.jpg',
       units = 'in',
       dpi = 300,
       width = 7.5,
       height = 4.5)