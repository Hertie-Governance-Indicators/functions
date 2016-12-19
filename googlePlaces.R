library(googleway)

# your API key goes here
key <- "AIzaSyDVY7tmSx14poROrl_ka4H0LpBr9KgDJXs"

# get list of places
df_places <- googleway::google_places(search_string = "cafe", 
                           location = c(-37.81827, 144.9671),   ## melbourne, AU
                           key = key)

df_places$results$name

# get distance between places
df_distance <- googleway::google_distance(origins = list(c("Melbourne Airport, Australia"),
                                                         c("MCG, Melbourne, Australia"),
                                                         c(-37.81659, 144.9841)),
                                          destinations = c("Portsea, Melbourne, Australia"),
                                          key = "")