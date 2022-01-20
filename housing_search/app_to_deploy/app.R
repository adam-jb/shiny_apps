#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# to get size of all files in repo (ignoring .gitignore files):
# git count-objects -vH

# might need to install latest fastmap to have shiny and altair work
#remotes::install_version("fastmap", "1.1.0", upgrade=FALSE)


library(shiny)
library(data.table)
library(dplyr)
library(sf)
library(altair)
library(bs4Dash)


#setwd("~/Desktop/shiny_apps/housing_search/housing_search")
nuts1_sample_points <- fread('data/nuts1_sample_points.csv', data.table = F)

df <- fread('data/all_data_lsoa.csv', data.table = F) %>%
    filter(commute_hours <= 2.5,
           price <= 500000)

simple_postcode_lsoa_lookup <- fread('data/lsoa_simple_postcode_lookup.csv', data.table = F) %>%
    select(-lsoa11nm)
    
df <- df %>% merge(simple_postcode_lsoa_lookup, by = "lsoa11cd") 




columns_user_may_care_about <- c(
    "price",
    "crime_deprivation" ,
    "health_deprivation",
    "education_deprivation",
    "outdoors_deprivation",
    "geographic_barriers_deprivation" ,
    "flood_areas_count",
    "commute_hours"  ,
    "noise_decibels_mean" ,
    "NUMBER_HABITABLE_ROOMS",
    "CURRENT_ENERGY_EFFICIENCY",
    "PROPERTY_TYPE",
    "MAINS_GAS_FLAG"
)

names(columns_user_may_care_about) <- c(
    'Price',
    'Crime',
    'Health services',
    'Education',
    'Outdoor living environment',
    'Distance to amenities',
    'Flood risk',
    'Drive to central London (hours)',
    'Noise levels',
    'Bigger houses',
    'Energy efficiency',
    'House not flat',
    'Connected to gas grid'
)

## making all columns numeric
df <- df %>% mutate(PROPERTY_TYPE = case_when(PROPERTY_TYPE %in% c("Bungalow", "House") ~ 1, T~0 ),
                    MAINS_GAS_FLAG = case_when(MAINS_GAS_FLAG=="Y"~1,T~0))

## normalising columns
normalise <- function(x) {(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

normalised_matrix <- df%>%select(columns_user_may_care_about) %>% 
    mutate_all(normalise) %>%
    mutate(
        `Drive to central London (hours)` = `Drive to central London (hours)` * 0.3  # reducing weight of this as small input weight goes a long way
    ) %>%
    mutate(  # making all missing values have meaning
        `Noise levels` = case_when(is.na(`Noise levels`)~0,T~`Noise levels`), # missing data means quieter roads 
        `Bigger houses` = case_when(is.na(`Bigger houses`)~0,T~`Bigger houses`), # missing means small house
        `House not flat` = case_when(is.na(`House not flat`)~0,T~`House not flat`) # missing means is flat
    ) %>%
    mutate( # inverting scores so higher is always better
        `Flood risk` = 1 - `Flood risk`,
        `Drive to central London (hours)` = 1 - `Drive to central London (hours)`,
        `Noise levels` = 1 - `Noise levels`,
        Price = 1 - Price
    ) %>%
    data.matrix()  # matrix for faster computation



## values will come from slider module
# vec_keys_and_multiplers <- runif(length(columns_user_may_care_about), 0, 3)
# names(vec_keys_and_multiplers) <- names(columns_user_may_care_about)
vector_keys <- names(columns_user_may_care_about) 
# 
# 
# output <- data.frame(# adding lat/long for plot
#     score = (normalised_matrix %*% vec_keys_and_multiplers),
#     testurl = 'https://www.w3schools.com',
#     click_me = paste('LSOA', df$lsoa11cd, 'on Rightmove'),
#     Long = df$Long,
#     Lat = df$Lat,
#     simple_postcode = df$simple_postcode,
#     price = df$price) %>%
#     arrange(desc(score)) %>%  # take top 10
#     slice(1:10) %>%
#     mutate(price = signif(price, 3),
#            score = signif(score, 4))
# 
# make_altair_graphic(output)




### start of func 
make_altair_graphic <- function(chosen_lsoas_df, column_width = 120) {

    selector <- alt$selection_single(on='mouseover', nearest=T)
    
    ranked_text = alt$Chart(chosen_lsoas_df, width = column_width)$
        mark_text(align='center')$
        encode(
        y=alt$Y('row_number:O', title = ''),
        href='testurl:N',
        color=alt$condition(selector, alt$value('orange'), alt$value('Black'))
        )$transform_window(
        row_number='row_number()'
        )
    
    ranked_text_clickable_url = alt$Chart(chosen_lsoas_df, width = 1)$
        mark_text(align='center')$
        encode(
            y=alt$Y('row_number:O', title = ''),
            href='testurl:N',
            color=alt$condition(selector, alt$value('orange'), alt$value('Black'))
        )$transform_window(
            row_number='row_number()'
        )
    
    simple_postcode = ranked_text$encode(text='simple_postcode:N',  href='testurl:N')$properties(title='Area')
    price = ranked_text$encode(text='price:N', href='testurl:N')$properties(title='Price')
    url_column = ranked_text_clickable_url$encode(text='click_me:N', href='testurl:N')$properties(title='Rightmove link')
    #text = alt$hconcat(simple_postcode, price, url_column) # Combine data tables
    text = simple_postcode |  price | url_column # Combine data tables
    
    text_table = text$add_selection(selector)
    text_table
    
    
    background_nuts1_points = alt$Chart(nuts1_sample_points)$mark_circle()$encode(
        longitude = 'X:Q',
        latitude = 'Y:Q',
        size = alt$value(3),
        color = alt$value('Black')
    )
    
    map_points = alt$Chart(chosen_lsoas_df)$mark_circle()$encode(
        longitude='Long:Q',
        latitude='Lat:Q',
        href='testurl:N',
        size=alt$value(40),
        color=alt$value('red'),
        tooltip=c('Long', 'Lat', 'price', 'testurl')
    )$transform_filter(
        selector
    )
    
    full_graphic <- text_table | background_nuts1_points + map_points
    full_graphic2 <- full_graphic$configure_view(stroke='white')$  # drop table bprders
        configure(background='#F7F7F7')$ # set background
        configure_axisY(  # drop y labs
            disable = T
        )
    #full_graphic2$save('/Users/apple/Downloads/example2.html')
    
    full_graphic2
    # to make colours from RGB inputs: https://convertingcolors.com/rgbpercent-color-98_98_98.html?search=RGBPercent(98%,%2098%,%2098%)
}




make_altair_graphic_in_shiny_from_inputs <- function(vec_keys_and_multiplers) {

    names(vec_keys_and_multiplers) <- names(columns_user_may_care_about)
    vector_keys <- names(vec_keys_and_multiplers) 
    
    
    output <- data.frame(# adding lat/long for plot
        score = (normalised_matrix %*% vec_keys_and_multiplers),
        testurl = 'https://www.w3schools.com',
        click_me = paste('LSOA', df$lsoa11cd, 'on Rightmove'),
        Long = df$Long,
        Lat = df$Lat,
        simple_postcode = df$simple_postcode,
        price = df$price) %>%
        arrange(desc(score)) %>%  # take top 10
        slice(1:10) %>%
        mutate(price = signif(price, 3),
               score = signif(score, 4))
    
    make_altair_graphic(output)
}

# 
# df$score <- runif(nrow(df), min = 1, max = 5)
# df$testurl = 'https://www.w3schools.com'
# df$click_me = 'Find houses on on Rightmove'
# cols_used <- c('Long', 'Lat', 'price', 'score', 'testurl', 'click_me')
# chosen_lsoas_df <- df[1:10, cols_used] %>%
#     mutate(price = signif(price, 3),
#            score = signif(score, 4))
# 
# make_altair_graphic(chosen_lsoas_df)



# get vector of columns user cares about











# topojson_nuts1 <- geojsonsf::sf_geojson(nuts1_shape) %>% geojsonio::geo2topo()
# states = alt$topo_feature(topojson_nuts1, feature="nuts118cd")
# 
# background = alt$Chart(states)$mark_geoshape(
#     fill='white',
#     stroke='black'
# )$properties(
#     width=500,
#     height=300
# )



## webgl good for many graphics: https://observablehq.com/@bmschmidt/dot-density-election-maps-with-webgl
## and https://peterbeshai.com/blog/2017-05-26-beautifully-animate-points-with-webgl-and-regl/
### if can learn it in JS





make_input_slider_simple <- function(input, ns) {sliderInput(ns(input), label = input, value =0 ,min=0,max=50,step=1)}
make_input_numeric_simple <- function(input, ns) {numericInput(ns(input), label = input, value =0 ,min=0,max=1000)}


counterInput <- function(id, label = "Counter") {
    ns <- NS(id)
    tagList(
        lapply(vector_keys, make_input_numeric_simple, ns)
    )
}

counterButton <- function(id, label = "Counter") {
    ns <- NS(id)
    tags$style(HTML("btn {
                        background-color: #32cd32;
                     }
                    .btn:hover {
                        color: #fff;
                        background-color: #2ecc71;
                      }
                    "))
    tagList(
        actionButton(ns('calculate'), 
                     "Calculate!") #,   
                   #  style="background-color: #32cd32")
    )
}

counterChart <- function(id, label = "Counter") {
    ns <- NS(id)
    tagList(
        vegawidget::vegawidgetOutput(ns('altair_graphic'))
    )
}

counterServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            
            observeEvent(input$calculate, {
                
                # evaluating text as code
                txt <- paste0('"', vector_keys, '" = ', 'input$`', vector_keys, '`,', collapse = '')
                txt <- substr(txt, 1, nchar(txt) - 1)
                txt_to_exec <- paste0('list(', txt, ')')
                
                output$altair_graphic <- vegawidget::renderVegawidget(
                    eval(parse(text = txt_to_exec)) %>% 
                        unlist() %>%
                        make_altair_graphic_in_shiny_from_inputs() %>% 
                        altair::vegawidget()
                )
                
            })
            
        }
    )
}

# ui <- fluidPage(
#     counterButton("counter1")
# )

# menu <- bs4Dash::controlbarMenu(
#     id = "controlbarMenu",
#     bs4Dash::controlbarItem(
#         "Options",
#         numericInput("num", "Observations:", 0, min = 1, max = 1000, step = 0),
#         numericInput("num", "Observations:", 0, min = 1, max = 1000, step = 0)
#     )
# )

ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(#title = "House ", 
                                      # bs4Dash::dashboardBrand(title = "House finder",
                                      #               color = "primary"),
                                      # sidebarIcon =NULL),
                            disable = T),
    
    #sidebar = bs4Dash::dashboardSidebar(disable = T),
    sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        collapsed = T,
        sidebarMenu(
            menuItem("House finder", tabName = "main_app", icon = icon("dashboard")),
            menuItem("About House finder", tabName = "about_app", icon = icon("search")),
            menuItem("About the developer", tabName = "about_me", icon = icon("grin-beam"))
    )), 
   # body = bs4Dash::dashboardBody(counterButton("counter1")),
   body = bs4Dash::dashboardBody(
       tabItems(
           tabItem(tabName = "main_app",
                h3('House finder'),
                fluidRow(  # put it in fluidRow() to make these two side by side
                    sidebarPanel(counterInput("counter1"), width = 3),
                    mainPanel(
                        counterButton('counter1'),
                        br(),
                        br(),
                        counterChart("counter1"), 
                        width = 8, 
                        collapsible = F)
                )
           ),
           tabItem(tabName = "about_app",
                   h3('use it and learn!')
           ),
           tabItem(tabName = "about_me",
                   userBox(
                       width = 12,
                       title = userDescription(
                           title = "Adam Bricknell",
                           subtitle = "Data Scientist",
                           type = 2,
                           image = "https://miro.medium.com/fit/c/262/262/0*jTKQkXlthbRyBT37.",
                       ),
                       status = "warning",
                       HTML('Adam began...<br>Contact him on <a href="https://www.linkedin.com/in/adam-bricknell-b25462144/">LinkedIn</a>')
                       #footer = "The footer here!"
                       # user box from https://rinterface.com/shiny/shinydashboardPlus/
                   )
           )
        ))
    #controlbar = bs4Dash::dashboardControlbar(menu),
   # title = "DashboardPage"
)

server <- function(input, output, session) {
    counterServer("counter1")
}

# Run the application 
shinyApp(ui = ui, server = server)


# example_input  to use fpr testing



#### For ref only
# 
# lsoa_shp <- st_read('data/lsoa_shapefile/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp')
# 
# lsoa_shp <- lsoa_shp %>% merge(df, by = 'lsoa11cd') %>%  
#     rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)
# 
# st_write(lsoa_shp, 'data/lsoa_shapefile_simpler/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp')
# 
# qtm(lsoa_shp[1:1000,], fill = "commute_hours", borders = NULL)
# 
# pal <- colorBin("YlOrRd", domain = lsoa_shp$commute_hours[1:100])
# labels <- lsoa_shp$price[1:100] %>% lapply(htmltools::HTML)
# leaflet() %>%
#     addTiles() %>%
#     addPolygons(
#         data = lsoa_shp[1:100,],
#         weight = 2,
#         opacity = 0,
#         color = ~pal(commute_hours),
#         dashArray = "3",
#         fillOpacity = 1,
#         highlightOptions = highlightOptions(
#             weight = 5,
#             color = "#666",
#             dashArray = "",
#             opacity = 1,
#             bringToFront = T),
#         label = labels,
#         labelOptions = labelOptions(
#             style = list("font-weight" = "normal", padding = "3px 8px"),
#             textsize = "15px",
#             direction = "auto"))
# 
# #### makes map
# 
# # alternative
# library(plotly)
# fig <- plot_ly(x = df$Long, y = df$Lat, color=df$commute_hours)
# fig <- fig %>% toWebGL()
# fig
# 
# 
# 
