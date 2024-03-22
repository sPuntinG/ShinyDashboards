
#------------- R version 4.3.2 (2023-10-31 ucrt) -- "Eye Holes" --------------------


# Gini 1988 - 2024
# Script to check, cleand and tidy up data, output to be used for the Shiny App


library(tidyverse)
library(here)
library(readxl)

# Import Country groups ------------------------------------
country_groups <- read_excel(path = "./in/Global_income_inequality_database_ver_9Feb2024/Global_income_inequality_database_ver_9Feb2024.xls",
                             sheet = "READ ME")
# Skip 65 lines

## Table with definition -----------------------------
country_groups_definition <- country_groups %>%
  slice(43:57) %>%   
  select(1:2)  %>% 
  rename(
    Group = 1,        
    Definition = 2   
  )

## Table with Countries listed per group -------------------------------

country_groups_list <- country_groups %>%
  slice(65, 67:211) %>%   
  select(2:16)  %>% 
  set_names(.[1, ]) %>% 
  slice(-1) %>% 
  rename(Country = W145) %>% # This is a stupid name
  mutate(W145 = as.character(rep(1, 145)) ) #%>% view() # This is more meaningful 
  

# Pivot longer 
country_groups_list <- country_groups_list %>% 
  pivot_longer(
    cols = -Country,
    names_to = "Group",
    values_to = "Values"
  ) %>% 
  drop_na() %>% 
  select(-Values) #%>% view()



# Import Gini by country groups --------------------------
# sheet = "Database"

gini <- read_excel(path = "./in/Global_income_inequality_database_ver_9Feb2024/Global_income_inequality_database_ver_9Feb2024.xls",
                    sheet = "Database")

# Remove blank space from col names
gini <- gini %>% 
  rename_with(~ str_replace_all(., " ", "_"), everything()) #%>% view()


# Make long 
gini <- gini %>% 
  # select(-Variable_name) %>% # redundant info
  pivot_longer(
    cols = -c(Type:Variable_name),  # Group
    names_to = "Year",
    values_to = "Gini"
  ) # %>% view()



# Exploratory plot: Gini over time by group --------------------------

summary(gini)
unique(gini$Group)

gini %>% 
  filter(Type == "NET") %>% 
  filter(Mean_income == "GDPPC") %>% 
  filter(Method == "LD") %>% 
  ggplot(data = ., aes(x = Year, y = Gini, 
                       color = Group, group = Group)) +
  geom_point() +
  geom_line() 


# Exploratory plot: map of countries per group --------------------------


## Load world map ------------------------
# install.packages('rnaturalearth')
world_map <- rnaturalearth::ne_countries(returnclass = "sf")

## Check that Country names match with ne_countries ---------------------

GINI <- country_groups_list$Country %>% unique() %>% sort()
wm_subunit <- world_map$subunit %>% unique()
wm_name_long <- world_map$name_long %>% unique()
wm_geounit <- world_map$geounit %>% unique()

# List of vectors
v_list <- list(wm_subunit, wm_name_long, wm_geounit)

# Create a function to check if each country is found in a vector
check_country <- function(country, vector) {
  if (country %in% vector) {
    return("1")
  } else {
    return(NA)
  }
}

# Create a tibble
result_tibble <- tibble(
  country = GINI,
  wm_subunit = map_chr(GINI, ~ check_country(.x, wm_subunit)),
  wm_name_long = map_chr(GINI, ~ check_country(.x, wm_name_long)),
  wm_geounit = map_chr(GINI, ~ check_country(.x, wm_geounit))
) %>% 
  mutate_at(vars(starts_with("wm")), as.numeric)

result_tibble # Explore visually


# Check which Counrties of the Gini data set have no matches in world map
result_tibble %>% 
  mutate(simple = wm_subunit + wm_name_long + wm_geounit) %>% 
  filter(is.na(simple)) %>% view()


# Correct (recode!) Country names in my data--------------------------

# Correct names: 

# Bosnia and Herzegovina
# Czechia
# South Korea
# Laos
# North Macedonia
# Republic of Serbia
# Tanzania

country_groups_list$Country <- recode(country_groups_list$Country, "Bosnia/Herzegovina" = "Bosnia and Herzegovina")
country_groups_list$Country <- recode(country_groups_list$Country, "Czech Republic" = "Czechia")
country_groups_list$Country <- recode(country_groups_list$Country, "Korea" = "South Korea")
country_groups_list$Country <- recode(country_groups_list$Country, "Lao" = "Laos")
country_groups_list$Country <- recode(country_groups_list$Country, "Serbia" = "Republic of Serbia")
country_groups_list$Country <- recode(country_groups_list$Country, "Tanzania, United Republic of" = "Tanzania")



# Not found: (ignore for now) probably too small
# Barbados                   
# Cape Verde
# Hong Kong, China
# Malta (too small?)
# Mauritius
# Micronesia
# Singapore

         


# Merge your data with world map data
merged_data <- left_join(world_map, country_groups_list, by = c("subunit" = "Country")) # name # sovereignt # admin #geounit  #subunit***

# Keep only necessary cols
CGG <-  merged_data %>% 
  select(subunit, Group, geometry) %>% 
  rename(Country = subunit)

# Plot worldmap where only countries that are in a group are colored
ggplot(CGG) +
  geom_sf(aes(fill = !is.na(Group))) +
  coord_sf(crs = "+proj=moll") +
  scale_fill_manual(values = c("grey", "red")) + #, guide = "none") +
  # labs(title = "Selected Countries") +
  theme_void()


CGG %>% 
  filter(Group != "W145") %>% 
  ggplot(.) +
  geom_sf(aes(fill = Group),
          alpha = 0.5,
          color = "black") +
  coord_sf(crs = "+proj=moll") +
  # geom_hline(yintercept = c(-23.5, 0, 23.5)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_d() +
  # scale_fill_manual(values = c("grey", "red")) + #, guide = "none") +
  # labs(title = "Selected Countries") +
  theme_void()



# Export table to be imported in Shiny script ---------------------

write_csv(gini, "./out/gini.csv")

saveRDS(CGG, "./out/CGG.rds")


