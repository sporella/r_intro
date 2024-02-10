library(tidyverse)
library(janitor)
library(readxl)

desastres_csv <- read_csv("data/number-of-deaths-from-natural-disasters.csv")
pokemon_csv <- read_csv("data/pokemon.csv")

esta_es_una_variable <- "Hola"


# pipe de magritte %>% (atajo de teclado shift + cmd (control) + m)
# pipe nativo |>

desastres_xlsx <- readxl::read_xlsx("data/number-of-deaths-from-natural-disasters.xlsx")
 
glimpse(desastres_xlsx)

desastres_limpio <- desastres_xlsx %>% 
  janitor::clean_names() %>% 
  filter(entity == "Wildfire") %>% 
  select(-entity) %>% 
  mutate(tipo_desastre = "Incendio Forestal", 
         year = as.character(year)) %>% 
  arrange(desc(total_deaths))

tabla_totales <- desastres_xlsx %>% 
  janitor::clean_names() %>% 
  filter(entity != "All natural disasters") %>% 
  group_by(entity) %>% 
  summarise(numero_total_muertes = sum(total_deaths, na.rm = T)) %>% 
  arrange(desc(numero_total_muertes))

# options(scipen = 999)

tabla_totales %>% 
  ggplot(aes(y = entity, x = numero_total_muertes, fill = entity))+
  geom_col(show.legend = F)+
  labs(y = "", x = "Total de Muertes")
