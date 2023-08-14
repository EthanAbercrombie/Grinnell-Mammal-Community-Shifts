require(tidyverse)
require(comclim)

# Community Climate Framework
# See Blonder et al. 2015 Ecology for details.

#Load Data
##########
occurrence_records <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_with_climate.csv') %>%
  mutate_at("species_name", str_replace_all, "_", "") %>% 
  mutate(species_name = str_replace(species_name,
                                    pattern = 'Peromyscusmaniculatussonoriensis',
                                    replacement = "Peromyscusmaniculatus"))
  #Save species list
  species_list <- unique(occurrence_records$species_name)

site_data <- read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/site_data_clean.csv') %>% 
  mutate(scaled_precip = as.numeric(scale(precip)),
         scaled_temp = as.numeric(scale((temp))))

####
#Generate a dataframe to save community climate data.
####
site_communityclimate_data <- site_data %>% 
  transmute(site_name,
            Era,
            scaled_precip,
            scaled_temp,
            inferred_site_precip = NA,
            inferred_site_temp = NA,
            observed_precip = NA,
            observed_temp = NA,
            volume_magnitude = NA,
            mismatch_magnitude = NA,
            precip_mismatch = NA,
            temp_mismatch = NA)

####
#Perform this analysis on all sites.
####

for (i in 1:nrow(site_data)) {
  
  loop_community <- site_data[i,]
  
  species_list_regionalpool <<- loop_community %>% 
    select(
      any_of(species_list)
    ) %>% 
    select_if( ~ !any(is.na(.))) %>% 
    colnames()
  
  species_list_tinf <<- loop_community %>% 
    select(
      any_of(species_list)
    ) %>% 
    select_if( ~ !any(is.na(.))) %>% 
    select_if(colSums(.) == 1) %>% 
    colnames()
  
  climate_niches_tinf <- occurrence_records %>%
    transmute(taxon = species_name,
              precip = as.numeric(scale(med_ann_precip)),
              temp = as.numeric(scale(med_ann_temp)))
  
  observed_climate_tobs <<- setNames(c(site_data[i,]$precip,
                                      site_data[i,]$temp),
                                    c('precip','temp'))
  
  community_data <- inputcommunitydata(
    localcommunity = species_list_tinf,
    regionalpool = species_list_regionalpool,
    climateniches = climate_niches_tinf,
    observedclimate = observed_climate_tobs
  )
  
  community_climate <- communityclimate(community_data,
                                        numreplicates = 1,
                                        numsamplesperspecies = 1)
  
  #Update site_communityclimate_data
  site_communityclimate_data[i,'inferred_site_precip'] = community_climate@obsStats$inferredClimate[1]
  site_communityclimate_data[i,'inferred_site_temp'] = community_climate@obsStats$inferredClimate[2]
  site_communityclimate_data[i,'observed_precip'] = community_climate@obsStats$observedClimate[1]
  site_communityclimate_data[i,'observed_temp'] = community_climate@obsStats$observedClimate[2]
  site_communityclimate_data[i,'volume_magnitude'] = community_climate@obsStats$volumeMagnitude
  site_communityclimate_data[i,'mismatch_magnitude'] = community_climate@obsStats$mismatchMagnitude
  site_communityclimate_data[i,'precip_mismatch'] = community_climate@obsStats$mismatchDirections[1]
  site_communityclimate_data[i,'temp_mismatch'] = community_climate@obsStats$mismatchDirections[2]
}

#Save site_community_climate_data
write_csv(site_communityclimate_data,
          file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/site_communityclimate_data.csv')

fig_site_communityclimate_data <- site_communityclimate_data %>% 
  left_join(x = site_data,
            y = site_communityclimate_data,
            by = c('site_name',
                   'Era',
                   'scaled_precip',
                   'scaled_temp')) %>% 
  group_by(site_name) %>% 
  mutate(pinf_change = diff(inferred_site_precip),
         tinf_change = diff(inferred_site_temp))

#General Graph Attributes

font_family = 'Helvetica'
text_size = 12
text_color = 'black'

####
#Lassen
####

#Community Temperature Index

lassen_cti_point <- ggplot(data = filter(fig_site_communityclimate_data,
                                         Region == "LA")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = tinf_change,
                   color = case_when(tinf_change > 0 ~ 'increasing',
                                     tinf_change < 0 ~ 'decreasing',
                                     tinf_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = tinf_change,
                 color = case_when(tinf_change > 0 ~ 'increasing',
                                   tinf_change < 0 ~ 'decreasing',
                                   tinf_change == 0 ~ 'no_change'),
                 shape = case_when(tinf_change > 0 | tinf_change < 0 ~ 'change',
                                   tinf_change == 0 ~ 'no_change')),
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

lassen_cti_bar <- ggplot(data = filter(fig_site_communityclimate_data,
                                       Region == 'LA')) +
  geom_bar(aes(x = case_when(tinf_change > 0 ~ 'increasing',
                             tinf_change < 0 ~ 'decreasing',
                             tinf_change == 0 ~ 'no change'),
               fill = case_when(tinf_change > 0 ~ 'increasing',
                                tinf_change < 0 ~ 'decreasing',
                                tinf_change == 0 ~ 'no change'),
               color = case_when(tinf_change > 0 ~ 'increasing',
                                 tinf_change < 0 ~ 'decreasing',
                                 tinf_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#664596ff','#eb8055ff','black')) +
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

lassen_cpi_point <- ggplot(data = filter(fig_site_communityclimate_data,
                                         Region == "LA")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = pinf_change,
                   color = case_when(pinf_change > 0 ~ 'increasing',
                                     pinf_change < 0 ~ 'decreasing',
                                     pinf_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = pinf_change,
                 color = case_when(pinf_change > 0 ~ 'increasing',
                                   pinf_change < 0 ~ 'decreasing',
                                   pinf_change == 0 ~ 'no_change'),
                 shape = case_when(pinf_change > 0 | pinf_change < 0 ~ 'change',
                                   pinf_change == 0 ~ 'no_change')),
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

lassen_cpi_bar <- ggplot(data = filter(fig_site_communityclimate_data,
                                       Region == 'LA')) +
  geom_bar(aes(x = case_when(pinf_change > 0 ~ 'increasing',
                             pinf_change < 0 ~ 'decreasing',
                             pinf_change == 0 ~ 'no change'),
               fill = case_when(pinf_change > 0 ~ 'increasing',
                                pinf_change < 0 ~ 'decreasing',
                                pinf_change == 0 ~ 'no change'),
               color = case_when(pinf_change > 0 ~ 'increasing',
                                 pinf_change < 0 ~ 'decreasing',
                                 pinf_change == 0 ~ 'no change'))) +
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

####
#Yosemite
####

#Community Temperature Index

yosemite_cti_point <- ggplot(data = filter(fig_site_communityclimate_data,
                                           Region == "YO")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = tinf_change,
                   color = case_when(tinf_change > 0 ~ 'increasing',
                                     tinf_change < 0 ~ 'decreasing',
                                     tinf_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = tinf_change,
                 color = case_when(tinf_change > 0 ~ 'increasing',
                                   tinf_change < 0 ~ 'decreasing',
                                   tinf_change == 0 ~ 'no_change'),
                 shape = case_when(tinf_change > 0 | tinf_change < 0 ~ 'change',
                                   tinf_change == 0 ~ 'no_change')),
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

yosemite_cti_bar <- ggplot(data = filter(fig_site_communityclimate_data,
                                         Region == 'YO')) +
  geom_bar(aes(x = case_when(tinf_change > 0 ~ 'increasing',
                             tinf_change < 0 ~ 'decreasing',
                             tinf_change == 0 ~ 'no change'),
               fill = case_when(tinf_change > 0 ~ 'increasing',
                                tinf_change < 0 ~ 'decreasing',
                                tinf_change == 0 ~ 'no change'),
               color = case_when(tinf_change > 0 ~ 'increasing',
                                 tinf_change < 0 ~ 'decreasing',
                                 tinf_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#664596ff','#eb8055ff','black')) +
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

yosemite_cpi_point <- ggplot(data = filter(fig_site_communityclimate_data,
                                           Region == "YO")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = pinf_change,
                   color = case_when(pinf_change > 0 ~ 'increasing',
                                     pinf_change < 0 ~ 'decreasing',
                                     pinf_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = pinf_change,
                 color = case_when(pinf_change > 0 ~ 'increasing',
                                   pinf_change < 0 ~ 'decreasing',
                                   pinf_change == 0 ~ 'no_change'),
                 shape = case_when(pinf_change > 0 | pinf_change < 0 ~ 'change',
                                   pinf_change == 0 ~ 'no_change')),
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

yosemite_cpi_bar <- ggplot(data = filter(fig_site_communityclimate_data,
                                         Region == 'YO')) +
  geom_bar(aes(x = case_when(pinf_change > 0 ~ 'increasing',
                             pinf_change < 0 ~ 'decreasing',
                             pinf_change == 0 ~ 'no change'),
               fill = case_when(pinf_change > 0 ~ 'increasing',
                                pinf_change < 0 ~ 'decreasing',
                                pinf_change == 0 ~ 'no change'),
               color = case_when(pinf_change > 0 ~ 'increasing',
                                 pinf_change < 0 ~ 'decreasing',
                                 pinf_change == 0 ~ 'no change'))) +
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

sequoia_cti_point <- ggplot(data = filter(fig_site_communityclimate_data,
                                          Region == "SS")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = tinf_change,
                   color = case_when(tinf_change > 0 ~ 'increasing',
                                     tinf_change < 0 ~ 'decreasing',
                                     tinf_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = tinf_change,
                 color = case_when(tinf_change > 0 ~ 'increasing',
                                   tinf_change < 0 ~ 'decreasing',
                                   tinf_change == 0 ~ 'no_change'),
                 shape = case_when(tinf_change > 0 | tinf_change < 0 ~ 'change',
                                   tinf_change == 0 ~ 'no_change')),
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

sequoia_cti_bar <- ggplot(data = filter(fig_site_communityclimate_data,
                                        Region == 'SS')) +
  geom_bar(aes(x = case_when(tinf_change > 0 ~ 'increasing',
                             tinf_change < 0 ~ 'decreasing',
                             tinf_change == 0 ~ 'no change'),
               fill = case_when(tinf_change > 0 ~ 'increasing',
                                tinf_change < 0 ~ 'decreasing',
                                tinf_change == 0 ~ 'no change'),
               color = case_when(tinf_change > 0 ~ 'increasing',
                                 tinf_change < 0 ~ 'decreasing',
                                 tinf_change == 0 ~ 'no change'))) +
  scale_fill_manual(values = c('#664596ff','#eb8055ff','black')) +
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

sequoia_cpi_point <- ggplot(data = filter(fig_site_communityclimate_data,
                                          Region == "SS")) +
  geom_hline(yintercept = 0,
             linetype = 2)  + #Add a horizontal line at y = 0
  geom_segment(aes(x = Elev_prism, 
                   xend = Elev_prism,
                   y = 0,
                   yend = pinf_change,
                   color = case_when(pinf_change > 0 ~ 'increasing',
                                     pinf_change < 0 ~ 'decreasing',
                                     pinf_change == 0 ~ 'no_change'))) + #Add segments from y = 0 to temp_change value
  geom_point(aes(x = Elev_prism,
                 y = pinf_change,
                 color = case_when(pinf_change > 0 ~ 'increasing',
                                   pinf_change < 0 ~ 'decreasing',
                                   pinf_change == 0 ~ 'no_change'),
                 shape = case_when(pinf_change > 0 | pinf_change < 0 ~ 'change',
                                   pinf_change == 0 ~ 'no_change')),
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

sequoia_cpi_bar <- ggplot(data = filter(fig_site_communityclimate_data,
                                        Region == 'SS')) +
  geom_bar(aes(x = case_when(pinf_change > 0 ~ 'increasing',
                             pinf_change < 0 ~ 'decreasing',
                             pinf_change == 0 ~ 'no change'),
               fill = case_when(pinf_change > 0 ~ 'increasing',
                                pinf_change < 0 ~ 'decreasing',
                                pinf_change == 0 ~ 'no change'),
               color = case_when(pinf_change > 0 ~ 'increasing',
                                 pinf_change < 0 ~ 'decreasing',
                                 pinf_change == 0 ~ 'no change'))) +
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





fig_site_communityclimate_data %>% 
  ggplot() +
  geom_boxplot(aes(y = precip_mismatch,
                   fill = Era)) +
  facet_grid(rows = 'Region')

model <- aov(data = fig_site_communityclimate_data,
            formula = precip_mismatch ~ Era + Region) 

anova(model)

TukeyHSD(model)
