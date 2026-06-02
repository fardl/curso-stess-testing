rm(list=ls())
source("sbapi.R")
sb_set_key(Sys.getenv("SB_API_KEY"))

# Caso (a) — por tipo de entidad
df_bm <- sb_indicadores(desde = "2024-06", hasta = "2024-06", tipoEntidad = "BM")
str(df_bm)

# Caso (b) — entidad específica
df_br <- sb_indicadores(desde = "2024-06", hasta = "2024-06", entidad = "BANRESERVAS")
str(df_br)

# Construir BD completa
sb_construir_bd("simbad.sqlite",
                desde = "2023-01", hasta = "2024-12",
                tipos = c("BM","AAyP","BAyC"))
sb_resumen("simbad.sqlite")