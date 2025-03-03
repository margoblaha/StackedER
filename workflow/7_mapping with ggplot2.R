# spatial prediction 
library(terra)
library(sf)
library(tidyverse)
library(stars)
library(viridis)
library(geodata)
library(gridExtra)

outdir <- "C:/Dati/Output/Figures"

# ---- load data ----
file <- "C:/Dati/Output/spatialPredictions_2022_2024-03-18.RDS"
my_spPred <- readRDS(file)
my_spPred$week <- paste0("w_", sub(".*_", "", my_spPred$temporalID))
my_spPred$year <- substr(my_spPred$temporalID, 1,4)
head(my_spPred)

my_spPred <- my_spPred %>%
  select(lon, lat, week, spatial_pred) %>% 
  pivot_wider(names_from = "week", values_from = "spatial_pred") 

# check if you have 1131 observations!!! 

# extract year from filename
year <- sub(".*_([0-9]{4})_.*", "\\1", file)
year

spat.pred<-terra::rast(my_spPred, type="xyz")
spat.pred[spat.pred<0]<-0

# ---- geodata for plotting ----
countries <- st_as_sf(geodata::gadm(country=c("ITA"), level=2, path = "/home/.")) %>% 
  filter(NAME_1 ==  "Emilia-Romagna")
neigh_regions <- st_as_sf(geodata::gadm(country=c("ITA"), level=1, path = "/home/.")) %>% 
  filter(NAME_1 ==  "Veneto" | NAME_1 ==  "Toscana" | NAME_1 ==  "Lombardia"| NAME_1 ==  "Liguria" | NAME_1 ==  "Marche" | NAME_1 ==  "Piemonte")

stars.pred <- spat.pred[[c(8, 10, 15, 20, 25, 30, 35, 40, 42, 45)]]
names(stars.pred) <- LETTERS[seq(from = 1, to = nlyr(stars.pred))]
stars.pred <- st_as_stars(stars.pred) 

# remember to change the dates in the week labels for a given year

plot <- ggplot() +
  geom_stars(data = stars.pred, inherit.aes = FALSE)+
  geom_sf(data = neigh_regions, fill=NA, 
          color="#000000", linewidth=0.2, 
          alpha=0.6)+
  geom_sf(data = countries, fill=NA, 
          color="#000000", linewidth=0.2, 
          alpha=0.6)+
  ggspatial::annotation_scale(data=data.frame(band = c("J"),
                                              location = c("br")), 
                              aes(location = location),  
                              width_hint = 0.25 # proportion of the plot
  ) +
  scale_fill_viridis_b(option="rocket", direction = -1,
                       limits = c(0, 500), breaks=c(0, 10, 25, 50, 100, 200, 400, 500),
                       oob = scales::squish, na.value = "transparent")+
  labs(x="Longitude",
       y="Latitude",
       fill="Median number of eggs",
       #title = "Predictions for 2021"
       )+
  scale_y_continuous(limits = c(43.7, 45.15), breaks = seq(44.0, 45.0, by = 0.5)) +
  scale_x_continuous(limits = c(9, 13), breaks = seq(10, 13, by = 1)) +
  #ylim(43.7, 45.15)+
  #xlim(9, 13)+
  
  # dates for 2021
  # facet_wrap(~band, ncol = 5,
  #            labeller = labeller(band = c("A" = "Week 8 (22 - 28 February)",
  #                                         "B" = "Week 10 (8 - 14 March)",
  #                                         "C" = "Week 15 (12 - 18 April)",
  #                                         "D" = "Week 20 (17 - 23 May)",
  #                                         "E" = "Week 25 (21 - 27 June)",
  #                                         "F" = "Week 30 (26 July - 1 August)",
  #                                         "G" = "Week 35 (30 Aug. - 5 Sep.)",
  #                                         "H" = "Week 40 (4 - 10 October)",
  #                                         "I" = "Week 42 (18 - 24 October)",
  #                                         "J" = "Week 45 (8 - 14 November)"))
  # )+
  # 
  # dates for 2022
  facet_wrap(~band, ncol = 5,
             labeller = labeller(band = c("A" = "Week 8 (21 - 27 February)",
                                          "B" = "Week 10 (7 - 13 March)",
                                          "C" = "Week 15 (11 - 17 April)",
                                          "D" = "Week 20 (16 - 22 May)",
                                          "E" = "Week 25 (20 - 26 June)",
                                          "F" = "Week 30 (25 - 31 July)",
                                          "G" = "Week 35 (29 Aug. - 4 Sep.)",
                                          "H" = "Week 40 (3 - 9 October)",
                                          "I" = "Week 42 (17 - 23 October)",
                                          "J" = "Week 45 (7 - 13 November)"))
  )+
  
  # dates for 2023
  # facet_wrap(~band, ncol = 5, 
  #            labeller = labeller(band = c("A" = "Week 8 (20 - 26 February)",
  #                                         "B" = "Week 10 (6 - 12 March)",
  #                                         "C" = "Week 15 (10 - 16 April)", 
  #                                         "D" = "Week 20 (15 - 21 May)",
  #                                         "E" = "Week 25 (19 - 25 June)", 
  #                                         "F" = "Week 30 (24 - 30 July)", 
  #                                         "G" = "Week 35 (28 Aug. - 3 Sep.)",
  #                                         "H" = "Week 40 (2 - 8 October)",
  #                                         "I" = "Week 42 (16 - 22 October)",
  #                                         "J" = "Week 45 (6 - 12 November)"))
  # )+                             
                                 
  theme_classic()+
  theme(legend.position = "bottom",  
        text = element_text(size=12),
        legend.text = element_text(size = 10),
        legend.key.width = unit(1,'cm'),
        legend.key.height = unit(0.35,'cm')
  )

ggsave(filename = paste0("C:/Dati/Output/Figures/predictions_", year, "_", Sys.Date(),".png"),
       plot, "png",
       width = 3200,
       height = 1150,
       units = "px",
       dpi = 300)
