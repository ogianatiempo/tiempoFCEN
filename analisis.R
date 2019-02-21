library(sf)
library(mapsapi)
library(tidyverse)

# Google Maps api key
source('apiKey.R')

radiosCensales <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")

centrosRadiosCensales <- radiosCensales %>%
  mutate(geometry = st_point_on_surface(geometry))

# Chequeo los centroides
ggplot(radiosCensales) +
  geom_sf() +
  geom_sf(data = centrosRadiosCensales, color = "orange")

# Coordenadas del Pabellon II hardcodeadas
exactas <- c(-58.4444588,-34.5419158)

# Un ejemplo de consulta a la api
doc = mp_directions(
  origin = centrosRadiosCensales[103,9],
  destination = exactas,
  mode = 'transit', # Esto es medios de transporte publicos
  region = 'ar',
  key = apiKey
)

# Ruta
r = mp_get_routes(doc)

ggplot(r) +
  geom_sf()

# Segmentos de la ruta
s = mp_get_segments(doc)

ggplot(s, aes(color = instructions)) +
  geom_sf()


r$duration_s


# Ahora lo aplico a todos los centros
# Trate de hacerlo con dplyr (rowwise() %>% mutate()) y con purr pero no pude.
tiempoViaje <- c()

for (i in 1:length(centrosRadiosCensales$geometry)) {
  doc = mp_directions(
    origin = centrosRadiosCensales[i,9],
    destination = exactas,
    mode = 'transit',
    region = 'ar',
    key = apiKey
  )
  r = mp_get_routes(doc)
  tiempoViaje <- append(tiempoViaje, r$duration_s)
}

# Agrego tiempo de viaje a los radios censales
radiosCensalesCT <- bind_cols(radiosCensales, tiempoViaje_s = tiempoViaje) %>%
  mutate(tiempoViaje_m = tiempoViaje_s / 60)

#write_csv(radiosCensalesCT, 'radiosCensalesCT.csv')

# Grafico por radio censal
ggplot(radiosCensalesCT, aes(fill = tiempoViaje_m)) +
  geom_sf() +
  scale_fill_gradient(low = 'green', high = 'red') +
  labs(title = "Tiempo de viaje a Ciudad Universitaria",
       subtitle = "Desde el centro de cada radio censal.",
       fill = "Tiempo en minutos")

# Promedio por barrio
radiosCensalesCT %>%
  group_by(BARRIO) %>%
  summarize(tiempoViaje_m = mean(tiempoViaje_m, na.rm = T)) %>%
  ggplot(aes(fill = tiempoViaje_m)) +
  geom_sf() +
  scale_fill_gradient(low = 'green', high = 'red') +
  labs(title = "Tiempo de viaje a Ciudad Universitaria por barrio",
       subtitle = "Promedio desde los centros de los radios censales de cada barrio.",
       fill = "Tiempo en minutos")
  