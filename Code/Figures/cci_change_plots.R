#Author: Ethan Abercrombie

#Packages
require(tidyverse)

####
#Load Data
####

site_data <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/site_data_clean.csv')

#General Graph Attributes

#load figure attributes.
load('~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_attributes.Rdata')

#Define additional attributes
left_margin = 5
right_margin = 10
pval_size = 2

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
                   yend = cti_rate,
                   color = case_when(cti_rate > 0 ~ 'increasing',
                                     cti_rate < 0 ~ 'decreasing',
                                     cti_rate == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cti_rate,
                 color = case_when(cti_rate > 0 ~ 'increasing',
                                   cti_rate < 0 ~ 'decreasing',
                                   cti_rate == 0 ~ 'no_change'),
                 shape = case_when(cti_rate > 0 | cti_rate < 0 ~ 'change',
                                   cti_rate == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#664596ff','#eb8055ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab("TRsite (°C yr–1)") +
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
  geom_bar(aes(x = case_when(cti_rate > 0 ~ 'increasing',
                             cti_rate < 0 ~ 'decreasing',
                             cti_rate == 0 ~ 'no change'),
               fill = case_when(cti_rate > 0 ~ 'increasing',
                                cti_rate < 0 ~ 'decreasing',
                                cti_rate == 0 ~ 'no change'),
               color = case_when(cti_rate > 0 ~ 'increasing',
                                 cti_rate < 0 ~ 'decreasing',
                                 cti_rate == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#664596ff','#eb8055ff','NA')) +
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
                             r = right_margin,  # Right margin
                             b = 0,  # Bottom margin
                             l = left_margin),
        legend.position = 'none')

# Region-wide thermophilization rates
lassen_cti_box <- ggplot(data = filter(site_data,
                     Region == 'LA',
                     Era == 'HM'),
       aes(x = 'Region')) +
  geom_boxplot(aes(y = cti_rate)) +
  geom_jitter(aes(y = cti_rate,
                  color = case_when(cti_rate > 0 ~ 'increasing',
                                    cti_rate < 0 ~ 'decreasing',
                                    cti_rate == 0 ~ 'no change')),
             alpha = 0.4) +
  scale_color_manual(values = c('#664596ff','#eb8055ff','NA')) +
  geom_hline(yintercept = 0,
             color = 'black',
             linetype = 2) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(color = 'black'),
        panel.background = element_blank(),
        legend.position = 'none') +
  annotate('text', x = 1, y = max(filter(site_data,
                                         Region == "LA")$cti_rate + 0.002),
           label = paste("P <",round(lassen_cti_ttest$p.value,
                                     digits = 3)),
           family = font_family,
           size = pval_size)

#Merge panels into a single figure.
(lassen_cti_merged <- lassen_cti_box + lassen_cti_bar +
    patchwork::plot_layout(
      ncol = 2,
      nrow = 1,
      widths = c()
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
                   yend = cpi_rate,
                   color = case_when(cpi_rate > 0 ~ 'increasing',
                                     cpi_rate < 0 ~ 'decreasing',
                                     cpi_rate == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cpi_rate,
                 color = case_when(cpi_rate > 0 ~ 'increasing',
                                   cpi_rate < 0 ~ 'decreasing',
                                   cpi_rate == 0 ~ 'no_change'),
                 shape = case_when(cpi_rate > 0 | cpi_rate < 0 ~ 'change',
                                   cpi_rate == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab("MRsite (mm yr–1)") +
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
  geom_bar(aes(x = case_when(cpi_rate > 0 ~ 'increasing',
                             cpi_rate < 0 ~ 'decreasing',
                             cpi_rate == 0 ~ 'no change'),
               fill = case_when(cpi_rate > 0 ~ 'increasing',
                                cpi_rate < 0 ~ 'decreasing',
                                cpi_rate == 0 ~ 'no change'),
               color = case_when(cpi_rate > 0 ~ 'increasing',
                                 cpi_rate < 0 ~ 'decreasing',
                                 cpi_rate == 0 ~ 'no change'))) +
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
                             l = left_margin),
        legend.position = 'none')

# Region-wide mesophilization rates
lassen_cpi_box <- ggplot(data = filter(site_data,
                                       Region == 'LA',
                                       Era == 'HM'),
                         aes(x = 'Region')) +
  geom_boxplot(aes(y = cpi_rate)) +
  geom_jitter(aes(y = cpi_rate,
                  color = case_when(cpi_rate > 0 ~ 'increasing',
                                    cpi_rate < 0 ~ 'decreasing',
                                    cpi_rate == 0 ~ 'no change')),
              alpha = 0.4) +
  scale_color_manual(values = c('#eb8055ff','#664596ff','NA')) +
  geom_hline(yintercept = 0,
             color = 'black',
             linetype = 2) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(color = 'black'),
        panel.background = element_blank(),
        legend.position = 'none') +
  annotate('text', x = 1, y = max(filter(site_data,
                                         Region == "LA")$cpi_rate + 0.5),
           label = paste("P =",round(lassen_cpi_ttest$p.value,
                                     digits = 3)),
           family = font_family,
           size = pval_size)

#Merge panels into a single figure.
(lassen_cpi_merged <- lassen_cpi_point + lassen_cpi_box +
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
                   yend = cti_rate,
                   color = case_when(cti_rate > 0 ~ 'increasing',
                                     cti_rate < 0 ~ 'decreasing',
                                     cti_rate == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cti_rate,
                 color = case_when(cti_rate > 0 ~ 'increasing',
                                   cti_rate < 0 ~ 'decreasing',
                                   cti_rate == 0 ~ 'no_change'),
                 shape = case_when(cti_rate > 0 | cti_rate < 0 ~ 'change',
                                   cti_rate == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#664596ff','#eb8055ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab("TRsite (°C yr–1)") +
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
  geom_bar(aes(x = case_when(cti_rate > 0 ~ 'increasing',
                             cti_rate < 0 ~ 'decreasing',
                             cti_rate == 0 ~ 'no change'),
               fill = case_when(cti_rate > 0 ~ 'increasing',
                                cti_rate < 0 ~ 'decreasing',
                                cti_rate == 0 ~ 'no change'),
               color = case_when(cti_rate > 0 ~ 'increasing',
                                 cti_rate < 0 ~ 'decreasing',
                                 cti_rate == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#664596ff','#eb8055ff','NA')) +
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
                             r = right_margin,  # Right margin
                             b = 0,  # Bottom margin
                             l = left_margin),
        legend.position = 'none')

# Region-wide thermophilization rates
yosemite_cti_box <- ggplot(data = filter(site_data,
                                       Region == 'YO',
                                       Era == 'HM'),
                         aes(x = 'Region')) +
  geom_boxplot(aes(y = cti_rate)) +
  geom_jitter(aes(y = cti_rate,
                  color = case_when(cti_rate > 0 ~ 'increasing',
                                    cti_rate < 0 ~ 'decreasing',
                                    cti_rate == 0 ~ 'no change')),
              alpha = 0.4) +
  scale_color_manual(values = c('#664596ff','#eb8055ff','NA')) +
  geom_hline(yintercept = 0,
             color = 'black',
             linetype = 2) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(color = 'black'),
        panel.background = element_blank(),
        legend.position = 'none') +
  annotate('text', x = 1, y = max(filter(site_data,
                                         Region == "YO")$cti_rate + 0.002),
           label = paste("P =",round(yosemite_cti_ttest$p.value,
                                     digits = 3)),
           family = font_family,
           size = pval_size)

#Merge panels into a single figure.
(yosemite_cti_merged <- yosemite_cti_point + yosemite_cti_box +
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
                   yend = cpi_rate,
                   color = case_when(cpi_rate > 0 ~ 'increasing',
                                     cpi_rate < 0 ~ 'decreasing',
                                     cpi_rate == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cpi_rate,
                 color = case_when(cpi_rate > 0 ~ 'increasing',
                                   cpi_rate < 0 ~ 'decreasing',
                                   cpi_rate == 0 ~ 'no_change'),
                 shape = case_when(cpi_rate > 0 | cpi_rate < 0 ~ 'change',
                                   cpi_rate == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab(ylab("MRsite (mm yr–1)")) +
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
  geom_bar(aes(x = case_when(cpi_rate > 0 ~ 'increasing',
                             cpi_rate < 0 ~ 'decreasing',
                             cpi_rate == 0 ~ 'no change'),
               fill = case_when(cpi_rate > 0 ~ 'increasing',
                                cpi_rate < 0 ~ 'decreasing',
                                cpi_rate == 0 ~ 'no change'),
               color = case_when(cpi_rate > 0 ~ 'increasing',
                                 cpi_rate < 0 ~ 'decreasing',
                                 cpi_rate == 0 ~ 'no change'))) +
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
                             l = left_margin),
        legend.position = 'none')


# Region-wide mesophilization rates
yosemite_cpi_box <- ggplot(data = filter(site_data,
                                       Region == 'YO',
                                       Era == 'HM'),
                         aes(x = 'Region')) +
  geom_boxplot(aes(y = cpi_rate)) +
  geom_jitter(aes(y = cpi_rate,
                  color = case_when(cpi_rate > 0 ~ 'increasing',
                                    cpi_rate < 0 ~ 'decreasing',
                                    cpi_rate == 0 ~ 'no change')),
              alpha = 0.4) +
  scale_color_manual(values = c('#eb8055ff','#664596ff','NA')) +
  geom_hline(yintercept = 0,
             color = 'black',
             linetype = 2) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(color = 'black'),
        panel.background = element_blank(),
        legend.position = 'none') +
  annotate('text', x = 1, y = max(filter(site_data,
                                         Region == "YO")$cpi_rate + 0.2),
           label = paste("P =",round(yosemite_cpi_ttest$p.value,
                                     digits = 3)),
           family = font_family,
           size = pval_size)

#Merge panels into a single figure.
(yosemite_cpi_merged <- yosemite_cpi_point + yosemite_cpi_box +
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
                   yend = cti_rate,
                   color = case_when(cti_rate > 0 ~ 'increasing',
                                     cti_rate < 0 ~ 'decreasing',
                                     cti_rate == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cti_rate,
                 color = case_when(cti_rate > 0 ~ 'increasing',
                                   cti_rate < 0 ~ 'decreasing',
                                   cti_rate == 0 ~ 'no_change'),
                 shape = case_when(cti_rate > 0 | cti_rate < 0 ~ 'change',
                                   cti_rate == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#664596ff','#eb8055ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab("TRsite (°C yr–1)") +
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
  geom_bar(aes(x = case_when(cti_rate > 0 ~ 'increasing',
                             cti_rate < 0 ~ 'decreasing',
                             cti_rate == 0 ~ 'no change'),
               fill = case_when(cti_rate > 0 ~ 'increasing',
                                cti_rate < 0 ~ 'decreasing',
                                cti_rate == 0 ~ 'no change'),
               color = case_when(cti_rate > 0 ~ 'increasing',
                                 cti_rate < 0 ~ 'decreasing',
                                 cti_rate == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#664596ff','#eb8055ff','NA')) +
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
                             r = right_margin,  # Right margin
                             b = 0,  # Bottom margin
                             l = left_margin),
        legend.position = 'none')

# Region-wide thermophilization rates
sequoia_cti_box <- ggplot(data = filter(site_data,
                                       Region == 'SS',
                                       Era == 'HM'),
                         aes(x = 'Region')) +
  geom_boxplot(aes(y = cti_rate)) +
  geom_jitter(aes(y = cti_rate,
                  color = case_when(cti_rate > 0 ~ 'increasing',
                                    cti_rate < 0 ~ 'decreasing',
                                    cti_rate == 0 ~ 'no change')),
              alpha = 0.4) +
  scale_color_manual(values = c('#664596ff','#eb8055ff','NA')) +
  geom_hline(yintercept = 0,
             color = 'black',
             linetype = 2) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(color = 'black'),
        panel.background = element_blank(),
        legend.position = 'none') +
  annotate('text', x = 1, y = max(filter(site_data,
                                         Region == "SS")$cti_rate + 0.002),
           label = "P < 0.001",
           family = font_family,
           size = pval_size)

#Merge panels into a single figure.
(sequoia_cti_merged <- sequoia_cti_point + sequoia_cti_box +
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
                   yend = cpi_rate,
                   color = case_when(cpi_rate > 0 ~ 'increasing',
                                     cpi_rate < 0 ~ 'decreasing',
                                     cpi_rate == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = cpi_rate,
                 color = case_when(cpi_rate > 0 ~ 'increasing',
                                   cpi_rate < 0 ~ 'decreasing',
                                   cpi_rate == 0 ~ 'no_change'),
                 shape = case_when(cpi_rate > 0 | cpi_rate < 0 ~ 'change',
                                   cpi_rate == 0 ~ 'no_change')),
             size = 3) + #Add scatterplot points representing temp_change values.
  scale_color_manual(values = c('#eb8055ff','#664596ff','black')) +
  scale_shape_manual(values = c(19,1)) +
  xlab("Elevation (m)") +
  ylab("MRsite (mm yr–1)") +
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
  geom_bar(aes(x = case_when(cpi_rate > 0 ~ 'increasing',
                             cpi_rate < 0 ~ 'decreasing',
                             cpi_rate == 0 ~ 'no change'),
               fill = case_when(cpi_rate > 0 ~ 'increasing',
                                cpi_rate < 0 ~ 'decreasing',
                                cpi_rate == 0 ~ 'no change'),
               color = case_when(cpi_rate > 0 ~ 'increasing',
                                 cpi_rate < 0 ~ 'decreasing',
                                 cpi_rate == 0 ~ 'no change'))) +
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
                             l = left_margin),
        legend.position = 'none')

# Region-wide mesophilization rates
sequoia_cpi_box <- ggplot(data = filter(site_data,
                                       Region == 'SS',
                                       Era == 'HM'),
                         aes(x = 'Region')) +
  geom_boxplot(aes(y = cpi_rate)) +
  geom_jitter(aes(y = cpi_rate,
                  color = case_when(cpi_rate > 0 ~ 'increasing',
                                    cpi_rate < 0 ~ 'decreasing',
                                    cpi_rate == 0 ~ 'no change')),
              alpha = 0.4) +
  scale_color_manual(values = c('#eb8055ff','#664596ff','NA')) +
  geom_hline(yintercept = 0,
             color = 'black',
             linetype = 2) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_line(color = 'black'),
        panel.background = element_blank(),
        legend.position = 'none') +
  annotate('text', x = 1, y = max(filter(site_data,
                                         Region == "SS")$cpi_rate + 0.15),
           label = paste("P =",round(sequoia_cpi_ttest$p.value,
                                     digits = 3)),
           family = font_family,
           size = pval_size)

#Merge panels into a single figure.
(sequoia_cpi_merged <- sequoia_cpi_point + sequoia_cpi_box +
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

#Make a multipanelled figure.

fig_changecci_multipanel <- ggpubr::ggarrange(lassen_cti_merged,
                  lassen_cpi_merged,
                  yosemite_cti_merged,
                  yosemite_cpi_merged,
                  sequoia_cti_merged,
                  sequoia_cpi_merged,
                  ncol = 2,
                  nrow = 3,
                  labels = c("A", "D", "B", "E", "C", "F"))

ggsave(fig_changecci_multipanel,
       filename = '~/Desktop/Grinnell-Mammal-Community-Shifts/Manuscript/figure_exports/fig_changecci_multipanel.jpg',
       units = 'in',
       dpi = 300,
       width = 7.5,
       height = 6)
