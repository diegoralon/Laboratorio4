library(tidyverse)
library(readr)
library(highcharter)
library(readxl)

df <- read_csv("tabla_completa.csv")
View(df)

## ¿Debemos invertir en la contratación de más personal?

df <- mutate_if(df, is.character, as.factor)


# Cuantos pilotos tenemos 
df %>% 
  summarise(Npilotos = n_distinct(PILOTO))
# Contamos con 9 pilotos

# Cuantos viajes hacen los pilotos al anio
viajesa <- df %>% 
  select(X1,PILOTO) %>% 
  group_by(PILOTO) %>% 
  summarise(viajesxp = n())

write.csv(viajesa,"viajesalanio.csv")

# Cuantos viajes hacen los pilotos en promedio al mes
df %>% 
  select(X1,MES) %>% 
  group_by(MES) %>% 
  summarise(viajesxmp = n()) %>% 
  hchart("column",hcaes(x = MES, y = viajesxmp)) %>% 
  hc_title(text = "<b>viajes de pilotos al mes<b>")

## ¿Debemos invertir en la compra de más vehículos de distribución? ¿Cuántos y de que tipo?

# Cuantos tipos de unidades tenemos 
df %>% 
  summarise(Tipos = n_distinct(UNIDAD))

# Cuantos viajes se realizan por unidad
Unidadesmes <- df %>% 
  select(X1,UNIDAD,MES) %>% 
  group_by(UNIDAD,MES) %>% 
  summarise(viajesxu = n())

write.csv(Unidadesmes,"unidadesmesx1.csv")

df %>% 
  select(UNIDAD,CANTIDAD) %>% 
  group_by(UNIDAD) %>% 
  summarise(minimoq = min(CANTIDAD))

df %>% 
  select(UNIDAD,CANTIDAD) %>% 
  group_by(UNIDAD) %>% 
  summarise(maxq = max(CANTIDAD))


df %>% 
  select(X1,UNIDAD,MES) %>% 
  group_by(UNIDAD,MES) %>% 
  summarise(viajesxu = n()) %>% 
  filter(UNIDAD == 'Camion Grande') %>% 
  hchart("column",hcaes(x = MES, y = viajesxu)) %>% 
  hc_title(text = "<b>viajes camion grande<b>")
  
df %>% 
  select(X1,UNIDAD,MES) %>% 
  group_by(UNIDAD,MES) %>% 
  summarise(viajesxu = n()) %>% 
  filter(UNIDAD == 'Camion Pequeño') %>% 
  hchart("column",hcaes(x = MES, y = viajesxu)) %>% 
  hc_title(text = "<b>viajes camion bajo<b>")


df %>% 
  select(X1,UNIDAD,MES) %>% 
  group_by(UNIDAD,MES) %>% 
  summarise(viajesxu = n()) %>% 
  filter(UNIDAD == 'Panel') %>% 
  hchart("column",hcaes(x = MES, y = viajesxu)) %>% 
  hc_title(text = "<b>viajes panel<b>")


  

  


  
  

