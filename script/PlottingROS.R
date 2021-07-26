pacman::p_load(tidyverse, wesanderson)

load('./FB/ros/GreenROS.Rdata')

unique(GreenROS$PropLive)

GreenROS %>%
  filter(TotalLoad %in% c(min(TotalLoad), max(TotalLoad))) %>%
  mutate(`Fine fuel\nratio` = round(PropFine, 1),
         PropLive = as.factor(PropLive*100), 
         `Total load` = recode(as.character(TotalLoad), 
                             '0.1' = "0.1 kg/ha", 
                             '1' = "1.0 kg/ha")) %>%
  ggplot( ) + theme_bw(16) + 
  geom_line(aes(x = LiveMoisture, 
                y = ros, 
                color = PropLive, 
                group = PropLive), 
            size = 1.5) +
  labs(x = "Live fuel moisture content (%)", 
       y = 'Rate of spread (m/min)') + 
  scale_color_manual("Live fuel (%)",
                     values = wes_palette("Zissou1", 5, type = "continuous")) +
  facet_grid(`Fine fuel\nratio`~`Total load`, 
             labeller = label_both) +
  coord_cartesian(ylim = c(0,97)) + 
  scale_x_continuous(breaks = seq(30, 120, length.out = 4), 
                     labels = seq(30, 120, length.out = 4)) +
  theme(strip.text.y = element_text(angle = 0), 
        panel.grid.minor.x = element_blank() ) 