rm(list=ls())
source("sbapi.R")
source("sb_etl.R")
sb_set_key(Sys.getenv("SB_API_KEY"))

# Prueba con 6 meses, así si algo falla no perdiste 30 minutos
sb_etl("simbad_test.sqlite", modo = "full",
       desde = "2018-01", hasta = "2026-04")

sb_cobertura("simbad_test.sqlite")
sb_log("simbad_test.sqlite")

sb_log("simbad_test.sqlite") |> 
  subset(estado %in% c("error", "vacio"))
