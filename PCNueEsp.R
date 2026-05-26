
##########ARMONIZACIÓN CAMPO PCNueEsp del IFN 4################################

setwd("C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/PCNueEsp")

# =========================================================
# ARMONIZACION IFN4 - PCNueEsp
# =========================================================

library(tidyverse)
library(readxl)
library(janitor)

# =========================================================
# 1. RUTA
# =========================================================

path <- "C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/PCNueEsp"

# =========================================================
# 2. ARCHIVOS EXCEL
# =========================================================

files <- list.files(
  path = path,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# =========================================================
# 3. FUNCION DE LECTURA
# =========================================================

leer_provincia <- function(file){
  
  message("Leyendo: ", basename(file))
  
  df <- read_excel(file) |>
    
    clean_names() |>
    
    # asegurar mismos tipos
    mutate(across(everything(), as.character)) |>
    
    # seleccionar solo columnas necesarias
    select(
      provincia,
      estadillo,
      cla,
      subclase,
      especie
    )
  
  return(df)
}

# =========================================================
# 4. LEER Y UNIR TODO
# =========================================================

PCNueEsp <- map_dfr(
  files,
  leer_provincia
)

# =========================================================
# 5. COMPROBACIONES
# =========================================================

glimpse(PCNueEsp)

# numero de provincias
length(unique(PCNueEsp$provincia))

# registros por provincia
PCNueEsp |>
  count(provincia)

# comprobar NA
colSums(is.na(PCNueEsp))

# =========================================================
# 6. EXPORTAR
# =========================================================

write_csv(
  PCNueEsp,
  "C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/Prueba IFN/C_val.csv"
)



#################################################################################
################Filtrado de NNT del campo PCNueEsp del IFN 4#####################
#################################################################################

setwd("C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4")

# Lista de códigos que quieres conservar
codigos_interes <- c(
  "11","17","18","27","28","33","34","48",
  "61","62","64","79","92","207","217","235",
  "236","258","264","275","279","292","307","317",
  "335","336","356","364","376","392","435","436",
  "457","464"
)

# Leer el CSV
# Cambia "datos.csv" por el nombre de tu archivo
NNT <- read.csv(
  "C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/PCNueEspCompleto.csv",
  sep = ";",
  stringsAsFactors = FALSE
)

# Cambia "codigo" por el nombre real de la columna con los códigos
NNT <- subset(NNT, COD_ESPECIE %in% codigos_interes)

# Guardar el nuevo CSV
write.csv(NNT, "NNT_IFN4.csv", row.names = FALSE)

#################################################################################
###########################CREACIÓN DE UN CAMPO ID###############################
#################################################################################

Limpio <- read.csv(
  "C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/NNT_IFN4.csv",
  stringsAsFactors = FALSE
)

# Crear el campo COD_ID
Limpio$COD_ID <- paste0(
  Limpio$COD_INVENTARIO,
  Limpio$COD_PROVINCIA,
  Limpio$ESTADILLO,
  Limpio$CLASE
)

# Ver los primeros resultados
head(Limpio$COD_ID)

# Exportar el dataframe a CSV
write.csv(
  Limpio,
  "NNT_COD_ID.csv",
  row.names = FALSE
)
