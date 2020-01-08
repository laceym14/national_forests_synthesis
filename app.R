#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(leaflet)
library(plyr)
library(shiny)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(RColorBrewer)

simp_nf <- readOGR("data/national_forests_simplified.shp")
#nf <- readOGR("data/national_forests.shp")
nf_table <- read.csv("data/national_forest_table.csv")
species_data <- read.csv("data/ESAIntrastateSpecies.csv")
states <- readOGR("data/states_proj.shp")
mergenf <- merge(simp_nf, nf_table, by="NFSLANDU_2")


ui <- fluidPage(theme = "leaflet_adj.css",
  br(),
  column(9,leafletOutput("map", height="800px", width = "100%")),
  column(3,textOutput("testHTML")),
  br(),
  sidebarLayout(
    sidebarPanel( #come up with some sort of if nothing selected, then...
      br(),
      p(strong("Click on each National Forest"), "on the map to learn more about Defenders' work there!"),
      h1(textOutput("nf")),
      br(),
      #h4(strong("Species of Interest:")),
      h4(textOutput("spint")),   # Testing out blank side panel if nothing selected
      p(textOutput("sp")),
      htmlOutput("image", inline = FALSE),
      #uiOutput("image"),
      #(imageOutput("image", width = "100%", height = "200px", inline = FALSE)),
      #imageOutput("<img style=max-height:200px;max-width:100%; src=img' />", inline = FALSE),
      #br(),
      #em("Image credit:", textOutput("imgcred")),
      #br(),
      h4(strong("What We Are Defending:")),
      p(textOutput("text")),
      #em(textOutput("sciname")),
      br(),
      h4(strong("What You Can Do:")),
      p(textOutput("todo")),
      br(),
      #p(textOutput("text")),
      width = 3),
    mainPanel()
  ))


server <- function(input, output) {


  # create a reactive value that will store the click location
  observeEvent(input$map_shape_click,{
    click_loc <- input$map_shape_click
    lat <- click_loc$lat
    lng <- click_loc$lng
    coords <- data.frame(x = lng, y = lat)
    coords <- SpatialPoints(coords)
    proj4string(coords) <- proj4string(mergenf)
    polygon_select <- as.data.frame(over(coords, mergenf))
    forest_select <- polygon_select$NFSLANDU_2
    text_select <- polygon_select$DESC
    todo_select <- polygon_select$TODO
    img_select <- polygon_select$IMG
    imgID_select <- polygon_select$IMG_ID
    imgcred_select <- polygon_select$CRED
    species_select <- polygon_select$SP_OF_INT
    output$nf <- renderText({paste(forest_select)})
    output$img <- renderImage({paste(src = img_select)})
    #output$image <- renderText(paste0("<img style=max-height:200px;max-width:100%; src=", img_select, "' />"))
    #src = img_select
    output$image <-renderText({c("<img style=max-height:200px;max-width:300px; src=", img_select,"'/>")})
    output$imgID <- renderImage(normalizePath(file.path('./images/', paste('image', imgID_select, '.jpg', sep=''))), deleteFile = FALSE)
    output$imgcred <- renderText({paste(imgcred_select)})
    output$text <- renderText({paste(text_select)})
    output$todo <- renderText({paste(todo_select)})
    output$sp <- renderText({paste(species_select)})
    output$spint <- renderPrint({
      if (length(click_loc) > 0) {
        "Species of Interest:"
      } else {
        ""
      }
    })

  })


  # popup layout
  pop_up_layout <- paste(sep = "<br/>",
                         paste0("<h3><b>" ,mergenf$NFSLANDU_2, "</b></h3>"),
                         paste0("<b>Number of ESA Listed Species in National Forest: </b>", mergenf$NUM_SP),
                         paste0("<img style=max-height:200px;max-width:300px; src=", mergenf$IMG, "' />"),
                         paste0("<i>Image Credit: </i>", mergenf$CRED),
                         paste0("<a href=" ,mergenf$LINK, ">Click for more information</a>"))

  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet(states)  %>% addTiles() %>% setView(lng = -87.251475, lat=39.835402,zoom=4) %>%
      addProviderTiles(providers$Esri.WorldTerrain) %>%
      addPolygons(data=states,weight=1, smoothFactor = 0.5, col="grey", fillColor = "rgba(255,255,255,0)",
                  popup = pop_up_layout,
                  fillOpacity = 0.75) %>%
      addPolygons(data=mergenf,weight=1, smoothFactor = 0.5, col="grey", fillColor = "#ccebc5",
                  popup = pop_up_layout,
                  fillOpacity = 0.75)

  })

}


# Run the application
shinyApp(ui = ui, server = server)

