
######################################################################################################
#######################DESCARGAR Y CURAR DATOS DE GBIF################################################
######################################################################################################

#Para leer la lista de un CSV:

setwd("C:/Users/DAVID COLLADO/Desktop/Doctorado/AA R")

datos<- read.csv("Listado de los NNT de la Península Ibérica.csv")
species_list <- datos[, 1, drop = FALSE]
View(species_list)


# =========================================================
# 1. Resolver nombres contra el backbone de GBIF
#    (incluye sinónimos automáticamente)
# =========================================================

library(rgbif)

taxa <- name_backbone_checklist(species_list)

# Ver resultados
print(taxa[, c(
  "verbatim_name",
  "scientificName",
  "usageKey",
  "matchType"
)])

# =========================================================
# 2. Obtener las claves aceptadas
# =========================================================

library(dplyr)

taxon_keys <- taxa %>%
  pull(usageKey) %>%
  unique()                  #Elimina duplicados de finalKey que llevaría a descargar dos veces lo mismo

print(taxon_keys)

# =========================================================
# 3. Crear descarga GBIF
#    - España
#    - excluye islas (esto hay que hacerlo en QGIS)
#    - excluye fósiles
#    - excluye living specimens
# =========================================================
#pred() y pred_in() son funciones de rgbif que sirven para construir predicados de filtrado en las descargas de GBIF
#Un “predicado” simplemente significa:  una condición que deben cumplir los registros.

# fill in your gbif.org credentials 
user <- "davidchz2002" # your gbif.org username 
pwd <- "David3742" # your gbif.org password
email <- "david.colladoh@uah.es" # your email 

download_key <- occ_download(  #funcion de rGBIF para solicitar una descarga de datos de ocurrencias
  
  # Taxones
  pred_in("taxonKey", taxon_keys), #A diferencia de pred, pred_in se usa cuando quieres varios valores posibles
  
  # España
  pred("country", "ES"),
  
  # Solo registros con coordenadas
  pred("hasCoordinate", TRUE),
  
  # Excluir coordenadas sospechosas
  pred("hasGeospatialIssue", FALSE),
  
  # Excluir fósiles
  pred_not(pred("basisOfRecord", "FOSSIL_SPECIMEN")),
  
  # Excluir jardines botánicos, zoos, colecciones vivas
  pred_not(pred("basisOfRecord", "LIVING_SPECIMEN")),
  # Excluir iNaturalist
  pred_not(pred(
    "datasetKey",
    "50c9509d-22c7-4a22-a47d-8c48425ef4a7"
  )),
  
  # Excluir Pl@ntNet observations
  pred_not(pred(
    "datasetKey",
    "7a3679ef-5582-4aaa-81f0-8c2545cafc81"
  )),
  
  # Excluir Pl@ntNet auto-ID
  pred_not(pred(
    "datasetKey",
    "14d5676a-2c54-4f94-9023-1e8dcd822aa0"
  )),
  
  format = "SIMPLE_CSV",
  
  user=user,pwd=pwd,email=email)


# =========================================================
# 6. Importar datos descargados
# =========================================================
library(readxl)
mis_datos <- read_excel("C:/Users/DAVID COLLADO/Desktop/Doctorado/Registros de presencia/Registros GBIF/GBIF.xlsx")
View(GBIF)

gbif_data <- mis_datos

# =========================================================
# 7. Limpiar coordenadas
#    - centroides
#    - mar
#    - capitales
#    - instituciones
#    - coordenadas 0,0
# =========================================================

library(dplyr)
library(CoordinateCleaner)

# =========================================================
# Convertir coordenadas a numéricas
# =========================================================

gbif_data <- gbif_data %>%
  
  mutate(
    
    decimalLongitude = as.numeric(
      gsub(",", ".", decimalLongitude)
    ),
    
    decimalLatitude = as.numeric(
      gsub(",", ".", decimalLatitude)
    )
    
  )

# =========================================================
# Eliminar coordenadas imposibles
# =========================================================

gbif_data <- gbif_data %>%
  
  filter(
    
    !is.na(decimalLongitude),
    !is.na(decimalLatitude),
    
    decimalLatitude >= -90,
    decimalLatitude <= 90,
    
    decimalLongitude >= -180,
    decimalLongitude <= 180
    
  )

# =========================================================
# Ahora sí: limpiar coordenadas
# =========================================================

clean_flags <- clean_coordinates(
  
  x = gbif_data,
  
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  
  species = "species",
  
  tests = c(
    "centroids",
    "seas",
    "zeros",
    "equal"
  )
)

# Mantener solo registros válidos

gbif_clean <- clean_flags %>%
  filter(.summary == TRUE)

# =========================================================
# 8. Conservar solo registros válidos
# =========================================================

gbif_clean <- clean_flags %>%
  filter(.summary == TRUE)

# =========================================================
# 9. Exportar CSV limpio
# =========================================================

write.csv(
  gbif_clean,
  "GBIF_registros_limpios.csv",
  row.names = FALSE
)
