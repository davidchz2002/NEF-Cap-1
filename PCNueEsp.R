
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

# Diccionario de provincias
provincias <- c(
  "01" = "Álava",
  "02" = "Albacete",
  "03" = "Alicante",
  "04" = "Almería",
  "05" = "Ávila",
  "06" = "Badajoz",
  "07" = "Baleares",
  "08" = "Barcelona",
  "09" = "Burgos",
  "10" = "Cáceres",
  "11" = "Cádiz",
  "12" = "Castellón",
  "13" = "Ciudad Real",
  "14" = "Córdoba",
  "15" = "A Coruña",
  "16" = "Cuenca",
  "17" = "Girona",
  "18" = "Granada",
  "19" = "Guadalajara",
  "20" = "Guipúzcoa",
  "21" = "Huelva",
  "22" = "Huesca",
  "23" = "Jaén",
  "24" = "León",
  "25" = "Lleida",
  "26" = "La Rioja",
  "27" = "Lugo",
  "28" = "Madrid",
  "29" = "Málaga",
  "30" = "Murcia",
  "31" = "Navarra",
  "32" = "Ourense",
  "33" = "Asturias",
  "34" = "Palencia",
  "35" = "Las Palmas",
  "36" = "Pontevedra",
  "37" = "Salamanca",
  "38" = "Santa Cruz de Tenerife",
  "39" = "Cantabria",
  "40" = "Segovia",
  "41" = "Sevilla",
  "42" = "Soria",
  "43" = "Tarragona",
  "44" = "Teruel",
  "45" = "Toledo",
  "46" = "Valencia",
  "47" = "Valladolid",
  "48" = "Vizcaya",
  "49" = "Zamora",
  "50" = "Zaragoza",
  "51" = "Ceuta",
  "52" = "Melilla"
)

NNT_IFN4 <- read.csv("C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/NNT_IFN4.csv")

# Crear nuevo campo con el nombre de la provincia
NNT_IFN4$PROVINCIA <- provincias[as.character(NNT_IFN4$COD_PROVINCIA)]

Limpio <- NNT_IFN4


# Crear el campo COD_ID
Limpio$COD_ID <- paste0(
  sprintf("%02d", Limpio$COD_PROVINCIA),  # siempre 2 dígitos
  sprintf("%04d", Limpio$ESTADILLO),      # opcional: rellena también estadillo
  Limpio$CLASE,
  Limpio$SUBCLASE
)

# Ver los primeros resultados
head(Limpio$COD_ID)

# Exportar el dataframe a CSV
write.csv(
  Limpio,
  "NNT_COD_ID.csv",
  row.names = FALSE
)

#Unión NNT - COORDENADAS:

library(readxl)

# Leer los CSV
data1 <- read.csv("C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/NNT_COD_ID.csv")
data2 <- read_xlsx("C:/Users/DAVID COLLADO/Desktop/Doctorado/IFN 4/COORD IFN4 MEJORADAS_ED50-ETRS89.xlsx")

# Unir mediante el campo PARCELA
datos_unidos <- merge(
  data1,
  data2,
  by = "PARCELA",
  all = TRUE
)


# Exportar resultado
write.csv(
  datos_unidos,
  "NNT_COORD.csv",
  row.names = FALSE
)

