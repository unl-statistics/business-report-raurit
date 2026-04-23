library(tidyverse)
library(maps)
library(ggplot2)

bridges = read.csv("NTAD_National_Bridge_Inventory_5655614151403109778.csv")

unique(bridges$STATE_CODE_001)
ne_bridges = bridges %>% 
  filter(STATE_CODE_001 == 31, DECK_COND_058 != "N",
         SUBSTRUCTURE_COND_060 != "N", SUPERSTRUCTURE_COND_059 != "N",
         SERVICE_UND_042B %in% c("5","6","7","8","9")) %>%
  select(RECORD_TYPE_005A,YEAR_BUILT_027,TRAFFIC_LANES_ON_028A,
         TRAFFIC_LANES_UND_028B,ADT_029,YEAR_ADT_030,SERVICE_UND_042B,
         MAX_SPAN_LEN_MT_048,STRUCTURE_LEN_MT_049,ROADWAY_WIDTH_MT_051,
         DECK_WIDTH_MT_052,VERT_CLR_OVER_MT_053,DECK_COND_058,
         SUPERSTRUCTURE_COND_059,SUBSTRUCTURE_COND_060,LATDD,LONGDD)

dim(ne_bridges)
head(ne_bridges)

min(bridges$LONGDD)

nebraska = map_data("state") %>%
  filter(region == "nebraska")

ggplot() +
  geom_polygon(data = nebraska,aes(x = long, y = lat, group = group),
               fill = "white", color = "black", linewidth = 1) +
  geom_point(data = ne_bridges,aes(x = LONGDD, y = LATDD),
    color = "darkblue", size = 1) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(x = "Longitude",y = "Latitude",title = "Bridges over Waterways in Nebraska")

scottkey = bridges %>%
  filter(STATE_CODE_001 == 24, ROUTE_PREFIX_005B == 1, 
         MAX_SPAN_LEN_MT_048 > 350, YEAR_BUILT_027 == 1976) %>%
  select(RECORD_TYPE_005A,YEAR_BUILT_027,
         TRAFFIC_LANES_ON_028A,TRAFFIC_LANES_UND_028B,ADT_029,YEAR_ADT_030,
         OPEN_CLOSED_POSTED_041,SERVICE_UND_042B,MAX_SPAN_LEN_MT_048,
         STRUCTURE_LEN_MT_049,ROADWAY_WIDTH_MT_051,DECK_WIDTH_MT_052, 
         VERT_CLR_OVER_MT_053,DECK_COND_058,SUPERSTRUCTURE_COND_059,
         SUBSTRUCTURE_COND_060)

scottkey
