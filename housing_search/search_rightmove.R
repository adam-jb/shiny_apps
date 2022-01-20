
# scrript to find rightmove URLs which correspond to postcodes, and 
# wrangle results into datafrane the app uses

library(dplyr)

#library(RSelenium)
#library(rvest)
#library(xml2)
#library(stringr)


get_postcode_from_code <- function(code_used) {
  query_url <- paste0('https://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E',
                      code_used,
                  '&radius=0.5&sortType=6&propertyTypes=&mustHave=&dontShow=&furnishTypes=&keywords=')
                  
  # 
  webshot::webshot(query_url, 'www/site.png')  # https://www.listendata.com/2018/05/take-screenshot-of-webpage-using-r.html
  
  s <- magick::image_read('www/site.png') %>%
    magick::image_crop( "120x60") %>%
    tesseract::ocr() 
  
  matches <- gregexpr("[A-Z]", s)[[1]]
  final_letter_of_interest <- matches[length(matches)] 
  postcode <- substr(s, 1, final_letter_of_interest)  # remove text after final capital letter
  postcode
}





df <- fread('data/all_data_lsoa.csv', data.table = F) %>%
  filter(commute_hours <= 2.5,
         price <= 500000)

postcode_lsoa_lookup <-
  fread("raw_data/PCD_OA_LSOA_MSOA_LAD_AUG21_UK_LU.csv",
        data.table = F) %>% 
  select(pcds, lsoa11cd) %>%
  filter(lsoa11cd %in% df$lsoa11cd)  # only looking for postcodes of interest


lsoa_of_choice <- 'E01000007'

acceptable_postcodes <- postcode_lsoa_lookup %>% filter(lsoa11cd == lsoa_of_choice) %>% pull(pcds)

# pos_of_first_acceptable_postcode <- which(postcode_lsoa_lookup$pcds == acceptable_postcodes[1]) / nrow(postcode_lsoa_lookup)




## Getting sample of rightmove code to postcode relationships 
i <- 1
iter_row_store <- 1
value_store <- rep(0, 10 ^ 5)
postcode_store <- rep(0, 10 ^ 5)

while (T) {
  candidate_value <- 10 ^ 6 + 3000 * i + round(runif(1, 0, 2000))
  returned_postcode <-
    tryCatch({
      get_postcode_from_code(candidate_value)
    },
    error = "")
  
  print(returned_postcode)
  
  value_store[iter_row_store] <- candidate_value
  postcode_store[iter_row_store] <- returned_postcode
  
  iter_row_store <- iter_row_store + 1
  i <- i + 1
  if (i %% 100 == 2) {
    fwrite(data.frame(cand = value_store, postcode = postcode_store),
           'out.csv')
  }
  
  
  if (i > 1300) {
    i <- 1
  }
  
}

# estimating size of jump between rightmove codes as a proportion in jump in postcodes











ht <- read_html(example_url) 

cht <- as.character(ht) 
str_locate(cht, 'searchFilters')

body_nodes <- ht %>% 
  html_node('body') %>% 
  html_children()

body_nodes %>% html_children()  # nodes inside the nodes

searchFilters

 ht %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@id, 'searchFilters')]") %>% 
  rvest::html_text()





## running selenium in Docker (https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused)
#system('docker pull selenium/standalone-firefox') # only need to do this once
system('docker run -d -p 127.0.0.1::80 selenium/standalone-firefox')

remDr <- remoteDriver(remoteServerAddr = "localhost", port = 80, browserName = "firefox'")
remDr$open()
remDr$navigate("http://www.google.com/ncr")
remDr$getTitle()