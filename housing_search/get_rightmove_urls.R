

# Getting URLs of postcode searches from rightmove: using Selenium to make the queries
# or using a search algorithm

library(dplyr)
library(rvest)
library(RSelenium)


# r selenium:
# http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html


## postcode could link to row number in some table (postcode lookup if lucky)
# # SW18 4TS, 4TU, uh, un
# https://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E1245620&radius=0.5&sortType=6&propertyTypes=&mustHave=&dontShow=&furnishTypes=&keywords=
# https://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E1245621&radius=0.5&sortType=6&propertyTypes=&mustHave=&dontShow=&furnishTypes=&keywords=
# https://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E1245622&radius=0.5&sortType=6&propertyTypes=&mustHave=&dontShow=&furnishTypes=&keywords=
# https://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E1245623&radius=0.5&sortType=6&propertyTypes=&mustHave=&dontShow=&furnishTypes=&keywords=
# 
# 
# # ip4 5ll    
# rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E1111405&radius=0.5&sortType=6&propertyTypes=&includeSSTC=false&mustHave=&dontShow=&furnishTypes=&keywords=
# 
# 
# rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E0000001&radius=0.5&sortType=6&propertyTypes=&includeSSTC=false&mustHave=&dontShow=&furnishTypes=&keywords=
# 
# 
# 
# rightmove_url <- 'https://www.rightmove.co.uk/property-for-sale/map.html?areaSizeUnit=sqft&channel=BUY&
# currencyCode=GBP&locationIdentifier=USERDEFINEDAREA%5E%7B%22polylines%22%3A%22_hfyHnfSld%40%7CcNylFc
# PjgEyrM%22%7D&mustHave=&propertyTypes=&radius=0.0&sortType=2&viewType=MAP'


which(postcode_lsoa_lookup$pcds == 'SW18 4TS')
which(postcode_lsoa_lookup$pcds == 'IP4 5LL')


i = 1
lsoa_of_choice <- df$lsoa11cd[i]
acceptable_postcodes <- postcode_lsoa_lookup %>% filter(lsoa11cd == lsoa_of_choice) %>%
pull(pcds)

which(postcode_lsoa_lookup$pcds == acceptable_postcodes[1])

## scrape to find postcode with seeking algo


response <- read_html('https://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E0000001&radius=0.5&sortType=6&propertyTypes=&includeSSTC=false&mustHave=&dontShow=&furnishTypes=&keywords=')
# 
# https://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=POSTCODE%5E1245620&radius=0.5&sortType=6&propertyTypes=&mustHave=&dontShow=&furnishTypes=&keywords=


            