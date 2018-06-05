library(tidyverse)
library(lubridate)
trees <- read_csv("Urban_Forestry_Street_Trees.csv")

glimpse(trees)

#organizes all trees by ward
tree_ward <- trees %>%
  group_by(`WARD`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#
type(trees$DATE_PLANT)
trees$year_planted <- (format(trees$DATE_PLANT, "%Y"))

year_planted <- trees %>%
  group_by( `year_planted`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

subset_trees <- trees %>%
  subset(trees$year_planted > 2005)

trees_2017 <- trees %>%
  subset(trees$year_planted == 2017)

tree_ward_2017 <- trees_2017 %>%
  group_by(`WARD`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

trees_2016 <- trees %>%
  subset(trees$year_planted == 2016)

tree_ward_2016 <- trees_2016  %>%
  group_by(`WARD`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

tree_names <- trees %>%
  group_by( `CMMN_NM`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

tree_genus <- trees %>%
  group_by(GENUS_NAME) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))
  
write_csv(tree_genus, "tree_genus.csv")
write_csv(year_planted, "year_planted.csv")
write_csv(trees_2017, "2017_trees.csv")
write_csv(trees_2016, "2016_trees.csv")
write_csv(tree_ward, "trees_by_ward.csv")
write_csv(tree_ward_2016, "2016_trees_ward.csv")
write_csv(tree_ward_2017, "2017_trees_ward.csv")
