library(tidyverse)
library(tidygeocoder)
library(rvest)
library(xml2)

zillow_url <- "https://www.zillow.com/portland-or/rentals/"

numberoftiles <- 40 # Number of listings per page

zillow_pg <- read_html(zillow_url) # Read in page

# count the total results
zillow_cnt <- zillow_pg %>%
  html_nodes(".result-count") %>%
  html_text() %>%
  str_remove(" results") %>%
  as.numeric()
zillow_cnt

# Set the page count
zillow_pg_cnt <- ceiling(zillow_cnt / numberoftiles)

links <- sprintf("https://www.zillow.com/portland-or/rentals/%d_p/", 1:20)
results <- map(links, ~ {
  # http://selectorgadget.com/
  # <body
  # class="photo-cards
  houses <- read_html(.x) %>%
    html_nodes(".photo-cards li article")
  z_id <- houses %>%
    html_attr("id")
  
  zillow_link <- houses %>%
    html_node(".list-card-link") %>%
    html_attr("href")
  
  image <- houses %>%
    html_node(".list-card-top a img") %>%
    html_attr("src")
  
  address <- houses %>%
    html_node(".list-card-addr") %>%
    html_text()
  
  typeofsale <- houses %>%
    html_node(".list-card-statusText") %>%
    html_text()
  
  price <- houses %>%
    html_node(".list-card-price") %>%
    html_text() %>%
    readr::parse_number()
  
  params <- houses %>%
    html_node(".list-card-info") %>%
    html_text()
  # number of bedrooms
  beds <- params %>%
    str_extract("\\d+(?=\\s*bds)") %>%
    as.numeric()
  # number of bathrooms
  baths <- params %>%
    str_extract("\\d+(?=\\s*ba)") %>%
    as.numeric()
  # total square footage
  house_a <- params %>%
    str_extract("[0-9,]+(?=\\s*sqft)") %>%
    str_replace(",", "") %>%
    as.numeric()
  
  tibble(address = address, price = price, beds= beds, baths=baths, house_area = house_a, type = typeofsale, zillow_link = zillow_link, image = image)
  
}
) %>%
  bind_rows(.id = 'page_no')

# Clean Data

final_df <- results %>%
  filter(beds > 2) %>%
  mutate(zillow_link = map(zillow_link, ~ htmltools::a(href = .x, "Link")),
         zillow_link = map(zillow_link, ~ gt::html(as.character(.x)))) %>%
  mutate(type = str_remove_all(type, "- "))

final_df_full <- results %>%
  mutate(zillow_link = map(zillow_link, ~ htmltools::a(href = .x, "Link")),
         zillow_link = map(zillow_link, ~ gt::html(as.character(.x)))) %>%
  mutate(type = str_remove_all(type, "- "))

write_rds(final_df, "Data/housing_partial.rds")
write_rds(final_df_full, "Data/housing_full.rds")
# write_rds(final_df, here::here("PortlandHousing/Data/housing_partial.rds"))
# write_rds(final_df_full, here::here("PortlandHousing/Data/housing_full.rds"))

# Map data

map_df <- final_df %>% 
  mutate(color = case_when(type == "House for rent" ~ "red",
                           type == "Apartment for rent" ~ "blue",
                           type == "Townhouse for rent" ~ "green")) %>%
  mutate(address = str_remove_all(address, " UNIT ."),
         address = str_remove_all(address, "The 1607 Apartments |")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude)

map_df_full <- final_df_full %>% 
  mutate(color = case_when(type == "House for rent" ~ "red",
                           type == "Apartment for rent" ~ "blue",
                           type == "Townhouse for rent" ~ "green")) %>%
  mutate(address = str_remove_all(address, " UNIT ."),
         address = str_remove_all(address, "The 1607 Apartments |")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude)

write_rds(map_df, "Data/housing_partial_map.rds")
write_rds(map_df_full, "Data/housing_full_map.rds")
# write_rds(map_df, here::here("PortlandHousing/Data/housing_partial_map.rds"))
# write_rds(map_df_full, here::here("PortlandHousing/Data/housing_full_map.rds"))



