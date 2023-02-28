# Load necessary packages
library(rvest)
library(stringr)
library(tidyverse)

# Scrape data from CIA website on international boundary disputes
disputes <- "https://www.cia.gov/the-world-factbook/field/disputes-international"
transcript_function <- function(link) {
  # Read webpage content and extract relevant text
  page_read <- read_html(link)
  page_content <- page_read %>% 
    html_element("#index-content-section") %>% 
    html_text()
  smashed_text <- paste(page_content, collapse = " ")
  
  # Store data in a dataframe with two columns: link and text
  final <- data.frame(link = link, 
                      text = smashed_text)
}

# Scrape data for all international boundary disputes
transcript_data <- transcript_function(disputes)

# Extract text with disputes from the dataframe
Wacky <- transcript_data$text

# Clean the text by removing unnecessary elements
result <- gsub("([a-z])([A-Z])", "\\1 \\2", Wacky) %>% 
  gsub("257 Results Clear Filters Filter All ABCDEFGHIJKLMNOPQRSTUVWXYZALL", "", .) %>% 
  gsub("[[:upper:]]+-[[:upper:]]+:[[:space:]]*none[[:space:]]*identified/[[:upper:]]+-[[:upper:]]+:[[:space:]]*None[[:space:]]", "", .)

# Extract country names involved in disputes
name_name <- str_extract_all(result, "[A-Z]+[a-z]*-[A-Z]+[a-z]*")
countries <- str_split(name_name[[1]], "-")
countries_new <- unlist(countries)

# Create a table of the most recurring country names
countries_table <-table(countries_new) %>% 
  as.data.frame(.)
most_disputes <- countries_table %>% 
  arrange(desc(Freq)) %>% 
  head(10)


# Scrape data from CIA website on tipping cultures for a random sample of 20 countries
file <- GET("https://www.cia.gov/the-world-factbook/page-data/countries/page-data.json")
all.links <- jsonlite::fromJSON(content(file, as = "text"))$result$data$countries$edges$node$uri
non.full.link <- paste0("https://www.cia.gov/the-world-factbook", all.links, "travel-facts") 

# Select 20 random countries to retrieve data for
set.seed(123)
Twenty.links <- sample(non.full.link, 20)

# Define a function to extract the tipping culture for a given country
transcript_function <- function(link) {
  output = tryCatch({
    # Read webpage content and extract relevant text
    page_read <- read_html(link)
    page_content <- page_read %>% 
      html_nodes("span:nth-child(19) p") %>% 
      html_text()
    smashed_text <- paste(page_content, collapse = " ")
    
    # Store data in a dataframe with two columns: link and text
    final <- data.frame(link = link, 
                        text = smashed_text)
    return(final)
  }, error = function(e){
    # If there is no tipping culture section, return NA values
    data.frame(link = NA, text = NA)
  })
  return(output)
}

# Extract tipping culture data for each country and store in a dataframe
travel_tips <- purrr::map_df(.x = Twenty.links, ~{
  countries_tips <- transcript_function(.x)
  return(countries_tips)
})
