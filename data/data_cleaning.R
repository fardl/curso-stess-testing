rm(list=ls())
library(tidyverse)
library(readxl)
library(tidyr)
library(dplyr)
setwd(r'(C:\Users\t490\Documents\MacroPol\stress-testing\data)')


df <- read_excel("rentabilidad_sipen.xlsx", sheet="Por FONDO")
df_ <- df %>%
  slice(-c(1,2,277:282)) %>%
  select(-c(13:16))

colnames(df_) <- as.character(unlist(df_[1, ]))
df_ <- df_[-1, ] %>%
  mutate(
    date = seq.Date("2003-07-01", "2026-03-01",by="month")
  )

df_long <- df_ %>%
  pivot_longer(
    cols = -date,
    names_to="fondo",
    values_to = "rentabilidad"
  ) 

write.csv(df_long, "rentabilidad_by_fund.csv")
