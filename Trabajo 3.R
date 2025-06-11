# Trabajo III
# Integrantes: cristobal astorga, matilde hermosilla y daniela pavez 

# 0.0 Ajustes iniciales 
rm(list=ls())
options(scipen=999) 

# 1.0 Cargar librerias 
pacman::p_load(dplyr, 
               sjmisc, 
               sjPlot,
               sjlabelled, 
               kableExtra, 
               GGally, 
               corrplot,
               car,
               sumarytools,
               psych, 
               haven,
               texreg,
               ggplot2, 
               fastDummies,
               ggefect)

# 1.1 Cargar base de datos
load(url("https://raw.githubusercontent.com/tobal-ux/Trabajo-n-2/main/Input/ola_2022.RData"))

# 2.0 Exploracion inicial de la BBDD
dim(ola_2022)
names(ola_2022)
