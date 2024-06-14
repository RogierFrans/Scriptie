# load the packages
library(dplyr)
library(ggplot)
library(tidyr)
library(readxl)
library(writexl)
library(readr)

setwd("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/data")
# import data csv
manual_checked <- read_xlsx("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Output/site_info_manual_checked_pass.xlsx")
Morel <- read_csv("Cookie Paywalls - Cookie Paywalls.csv") %>% 
  merge(manual_checked, by.x = "URLs", by.y = "site", all.x = TRUE) %>%
  select(URLs, `Category 1` = `Category 1.x`, Country = Country.x, `Price/month` =`Price/month.x` , Freechoice, Contentpass)
Rasaii <- read_delim("detected_285.txt", delim = "\t", col_names = "URLs")
# if have checked the data of rasaii manually to get the price of the websites this can be found in the output folder site_info_manual_checked_pass.xlsx
#add the data to the dataset including if there is a pass
Rasaii <- merge(Rasaii, manual_checked, by.x = "URLs", by.y = "site", all.x = TRUE)
Rasaii <- Rasaii %>% select(URLs, 'Category 1', `Price/month`, Freechoice, Contentpass)
Rasaii$Country <- NA
Rogier <- read_xlsx("Rogier.xlsx")
Rogier$Freechoice <- NA
Rogier$Contentpass <- NA

# Function to extract the base domain
extract_base_domain <- function(site) {
  parts <- strsplit(site, '\\.')[[1]]
  if (length(parts) > 2) {
    return(paste(parts[length(parts)-1], parts[length(parts)], sep = '.'))
  }
  return(site)
}

# add data that is scraped from the contentpass and/or freechoice website
Freechoice_website <- read_xlsx("Freechoice.xlsx") %>%
  mutate(base_domain = extract_base_domain(sites))
Contentpass_website <- read_xlsx("Contentpass.xlsx") %>%
  mutate(base_domain = extract_base_domain(Website))

# Rasaii <- Rasaii %>% 
#   mutate("Category 1" = NA, Country = NA, "Price/month" = NA)

combined_data <- rbind(Rasaii, Morel, Rogier) %>% 
  distinct(URLs, .keep_all = TRUE) %>%
  mutate(base_domain = extract_base_domain(URLs))

# change the name of category 1 to category
colnames(combined_data)[colnames(combined_data) == "Category 1"] <- "Category"
# change the name of Price/month to Price
colnames(combined_data)[colnames(combined_data) == "Price/month"] <- "Price"
#price is now a character, change it to numeric, to do this we need to remove the € sign
combined_data$Price <- gsub("€", "", combined_data$Price)
combined_data$Price <- as.numeric(combined_data$Price)

# Check the NAs for freechoice and contentpass in the dataset, if the URLs match with the freechoice and contentpass dataset then add a one, otherwise add a zero.
# add the leftovers from the freechoice and contentpass dataset as new rows to the combined_data dataset.
# the price of Freechoice is 2.99 and the price of Contentpass is 3.99
combined_data %>% filter(Freechoice == 1 | Contentpass == 1) %>% nrow()
# first subset the data where the freechoice and contentpass are NA, then add 1 to the Freechoice column if the URL is in the Freechoice_website dataset, same for the Contentpass column
combined_data <- combined_data %>% 
  mutate(Freechoice = ifelse(is.na(Freechoice) & base_domain %in% Freechoice_website$base_domain, 1, 0),
         Contentpass = ifelse(is.na(Contentpass) & URLs %in% Contentpass_website$base_domain, 1, 0))

combined_data %>% filter(Freechoice == 1 | Contentpass == 1) %>% nrow()
# add the leftovers from the freechoice and contentpass dataset as new rows to the combined_data dataset.
# first clean the Freechoice_website dataset so it matches the combined_data dataset
Freechoice_website <- Freechoice_website %>%
  mutate(URLs = sites, Price = 2.99, Category = NA, Country = NA, Freechoice = 1, Contentpass = 0) %>%
  select(-sites,-'web-scraper-order', -'web-scraper-start-url')

# same with the Contentpass_website dataset
Contentpass_website <- Contentpass_website %>%
  mutate(URLs = Website, Price = 3.99, Category = NA, Country = NA, Freechoice = 0, Contentpass = 1) %>%
  select(-Website, -'web-scraper-order', -'web-scraper-start-url')


combined_data <- rbind(Freechoice_website, Contentpass_website, combined_data) %>%
  distinct(URLs, .keep_all = TRUE)

# how many sites are in the combined dataset that have a pass
combined_data %>% filter(Freechoice == 1 | Contentpass == 1) %>% nrow()
############################
## Merge whotracksme data ##
############################

places <- c("de","eu", "fr", "global", "us")
amount <- c()
site_info_d <- data.frame()
for (i in places){
  sites_whotracksme <- read_csv(paste("whotracksme/", i, "/sites.csv", sep = ""))
  sites_whotracksme %>% filter(site %in% combined_data$base_domain) -> sites_whotrackme_both
  # how many unique sites are in the sites_tracked_both dataset
  site_info_d <- rbind(site_info_d, sites_whotrackme_both)
  amount <- c(amount, sites_whotrackme_both %>% distinct(site) %>% nrow())
}
# remove duplicates from the tracking_info only if the whole row is the same
site_info <- site_info_d %>% 
  distinct() %>%
  arrange(desc(popularity)) %>%
  distinct(site, .keep_all = TRUE) %>%
  mutate(base_domain = extract_base_domain(site))
# add the info of both to tracking info
site_info <- merge(site_info, combined_data, by.x = "base_domain", by.y = "base_domain")

site_info %>% filter(Freechoice == 0 & Contentpass == 0) %>% nrow()

# write end dataset
write_xlsx(site_info, "C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Output/dataset_using_5-6-2024.xlsx")
