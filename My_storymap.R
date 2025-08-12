library(shiny)
library(mapgl)
library(mapboxapi)
library(sf)

# Property coordinates
property_coords <- c(-157.86160999718547, 21.316433304586724)

# Create sf object for the property
property_sf <- st_as_sf(data.frame(
  id = "apt1",
  name = "New Class A Apartment",
  lon = property_coords[1],
  lat = property_coords[2]
), coords = c("lon", "lat"), crs = 4326)

# Generate isochrone polygon using Mapbox API
isochrone <- mb_isochrone(property_coords, profile = "driving", time = 20)

# UI
ui <- fluidPage(
  tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", rel = "stylesheet"),
  story_map(
    map_id = "map",
    font_family = "Poppins",
    sections = list(
      "intro" = story_section(
        title = "Waena Apartment Complex",
        content = list(
          p("Honolulu, Hawaii"),
          img(src = "waena.png", width = "300px")
        ),
        position = "center"
      ),
      "marker" = story_section(
        title = "PROPERTY LOCATION",
        content = list(
          p("The property is located in the Chinatown region of Honolulu in Hawaii, this gated apartent complex is built around a large community park")
        )
      ),
      "isochrone" = story_section(
        title = "HONOLULU AT YOUR FINGERTIPS",
        content = list(
          p("The property is within a 20-minute drive of downtown Honolulu, Including universties of Hawaii, and Chaminade, as well as Waikiki beach")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$map <- renderMapboxgl({
    mapboxgl(
      scrollZoom = FALSE,
      center = c(-157.86160999718547, 21.316433304586724),
      zoom = 12
    )
  })
  
  on_section("map", "intro", {
    mapboxgl_proxy("map") |>
      clear_layer("property_layer") |>
      clear_layer("isochrone") |>
      fly_to(
        center = c(-157.86160999718547, 21.316433304586724),
        zoom = 12,
        pitch = 0,
        bearing = 0
      )
  })
  
  on_section("map", "marker", {
    proxy <- mapboxgl_proxy("map")
    
    proxy |>
      clear_layer("isochrone") |>
      add_source(id = "property", data = property_sf) |>
      add_circle_layer(
        id = "property_layer",
        source = "property",
        circle_color = "#CC5500",
        circle_radius = 10,
        circle_opacity = 0.8,
        popup = "name"
      ) |>
      fly_to(
        center = property_coords,
        zoom = 16,
        pitch = 45,
        bearing = -90
      )
    
  })
  
  on_section("map", "isochrone", {
    mapboxgl_proxy("map") |>
      add_fill_layer(
        id = "isochrone",
        source = isochrone,
        fill_color = "#CC5500",
        fill_opacity = 0.5
      ) |>
      fit_bounds(
        isochrone,
        animate = TRUE,
        duration = 8000,
        pitch = 75
      )
  })
}

# Run the app
shinyApp(ui, server)