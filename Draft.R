# Import libraries
library(tidyverse)
library(stringr)
library(magick)

# Load all data
lego <- list(colors = read.csv("data/colors.csv.gz"),
             elements = read.csv("data/elements.csv.gz"),
             inventories = read.csv("data/inventories.csv.gz"),
             inventory_minifigs = read.csv("data/inventory_minifigs.csv.gz"),
             inventory_parts = read.csv("data/inventory_parts.csv.gz"),
             inventory_sets = read.csv("data/inventory_sets.csv.gz"),
             minifigs = read.csv("data/minifigs.csv.gz"),
             part_categories = read.csv("data/part_categories.csv.gz"),
             part_relationships = read.csv("data/part_relationships.csv.gz"),
             parts = read.csv("data/parts.csv.gz"),
             sets = read.csv("data/sets.csv.gz"),
             themes = read.csv("data/themes.csv.gz"))

lego$inventories %>%
  dplyr::filter(set_num == "41408-1") %>%
  inner_join(lego$inventory_minifigs, by = c("id" = "inventory_id")) %>%
  inner_join(lego$minifigs, by = c("fig_num" = "fig_num"))


#---------------What are the sets that have the most parts / the most different parts---------------
#The most spare parts
lego$sets %>%
  inner_join(lego$inventories, by = c("set_num" = "set_num")) %>%
  inner_join(lego$inventory_parts, by = c("id" = "inventory_id")) %>%
  select(set_num, name, is_spare, quantity) %>%
  filter(is_spare=='t') %>%
  group_by(set_num, name) %>%
  summarise(spare_parts=sum(quantity)) %>%
  arrange(desc(spare_parts))
  

#---------------Get mean piece number depending of the theme---------------
#Mean nb of parts
lego$sets %>%
  summarise(mean(num_parts))

lego$sets %>%
  inner_join(lego$themes, by = c("theme_id" = "id")) %>%
  group_by(name.y) %>%
  summarise(mean_num_part=mean(num_parts)) %>%
  arrange(desc(mean_num_part))

