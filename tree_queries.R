library(tidyverse)
library(lubridate)
library(tidyr)
trees <- read_csv("Urban_Forestry_Street_Trees.csv")
#downloaded Aug 7, 2018 from http://opendata.dc.gov/datasets/urban-forestry-street-trees?geometry=-77.481%2C38.812%2C-76.577%2C38.999

#tree mortality -- warranty claims? 



glimpse(trees)



#let's look at the Tboxx field, it's been idded by staff as important 
tree_tbox <- trees %>%
  group_by(TBOX_STAT) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))
#let's look at the Tboxx field, it's been idded by staff as important 
tree_condition <- trees %>%
  #filter( TBOX_STAT == "Open" ) %>%
  group_by(CONDITION, WARD) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#Turns the tbox query into a more-easily digested grid 
tree_condition_together <- tree_condition %>%
  spread(CONDITION, count)


write_csv(tree_condition, "open_all_ward.csv")
write_csv(tree_condition_together, "all_conditions_by_ward.csv")
#this query examines the trees database and pulls out the tbox status and groups it by ward.
tree_health <- trees %>%
  filter(TBOX_STAT == "Plant" | TBOX_STAT == "Open" | TBOX_STAT == "Proposed") %>%
  group_by(TBOX_STAT, WARD) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#this groups the dataframe in a more digestable way using the tidyr spread command, 
tree_health_together <- tree_health %>%
  spread(TBOX_STAT, count)

#now we can look at the percentage of open tboxes by ward
tree_health_together = mutate(tree_health_together, 
                pct_empty = ((Open /(Plant))* 100),
                pct_proposed =(( (Open-Proposed) / Plant)* 100)
                )

write_csv(tree_health_together, "tree_health_together.csv")


#organizes all trees by ward
tree_ward <- trees %>%
  filter(TBOX_STAT == "Plant" ) %>%
  group_by(`WARD`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

# Takes the year from date planted and creates a new column, year planted. 
type(trees$DATE_PLANT)
trees$year_planted <- (format(trees$DATE_PLANT, "%Y"))

#now we can look at our trees data and filter 'planted' t boxes before counting the year trees were planted 
year_planted <- trees %>%
  filter(TBOX_STAT == "Plant" ) %>%
  group_by( `year_planted`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#now we can look at where trees have been planted since 2001...that's not neccessary all the time. 
subset_trees <- trees %>%
  subset(year_planted > 2008)

write_csv(subset_trees, "trees_since_2008.csv")

#now we can look at where the trees are proposed in the future
Proposed_trees <- trees %>%
  filter(TBOX_STAT == "Proposed")


write_csv(Proposed_trees, "Proposed_trees.csv")

#and we can see where the trees have been planted by ward
o2008_subset_trees <- trees  %>%
  filter(TBOX_STAT == "Plant" & year_planted > 2008)  %>%
  group_by(`WARD`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#let's get a percentage of trees planted after 2008
o2008_subset_trees = mutate(o2008_subset_trees, 
                              pct_planted = ((count /sum(count))* 100))

#let's make that a csv



write_csv(o2008_subset_trees, "o2008_subset_trees.csv")

#identifies trees planted in 2017
tree_2017_ward_1 <- trees %>%
  #filter(TBOX_STAT == "Plant" ) %>%
  filter(year_planted == 2017 ) %>%
  group_by(`WARD`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#identifies trees planted in 2016
tree_ward_2016 <- trees_2016  %>%
  filter(TBOX_STAT == "Plant" ) %>%
  filter(year_planted == 2016 ) %>%
  group_by(`WARD`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#looks at the number of each type of tree in the city
tree_names <- trees %>%
  filter(TBOX_STAT == "Plant" ) %>%
  group_by( `CMMN_NM`) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

#looks at the number of each genus of tree in the city
tree_PRE2001_genus <- trees %>%
  #filter(TBOX_STAT == "Plant" ) %>%
  filter(year_planted < 2000) %>%
 # group_by(GENUS_NAME) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))


#looks at the number of each genus of tree in the city
tree_POST2001_genus <- trees  %>%
  filter(TBOX_STAT == "Plant" )  %>%
  filter(year_planted > 2000) %>%
  group_by(GENUS_NAME) %>%
  summarise(count=n()) %>%
  arrange(desc(`count`))

tree_POST2001_genus = mutate(tree_POST2001_genus, 
                            pct_of_genus = ((count /sum(count))* 100))

tree_POST2001_genus <- mutate(tree_POST2001_genus, rounded_pct_genus = round(tree_POST2001_genus$pct_of_genus, digits = 2))

#write into a csv 

write_csv(tree_POST2001_genus, "tree_POST2001_genus.csv")

#now we can look at where trees have been planted since 2001...that's not neccessary all the time. 
subset_trees <- trees %>%
  subset(trees$year_planted > 2001)



write_csv(tree_genus, "tree_genus.csv")
write_csv(year_planted, "year_planted.csv")
write_csv(trees_2017, "2017_trees.csv")
write_csv(trees_2016, "2016_trees.csv")
write_csv(tree_ward, "trees_by_ward.csv")
write_csv(tree_ward_2016, "2016_trees_ward.csv")
write_csv(tree_ward_2017, "2017_trees_ward.csv")
