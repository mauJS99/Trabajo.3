### Preparación trabajo "pobreza multidimensional rural 8va region"

# 0.Ajustes iniciales:
rm(list=ls())
options(scipen=999)

setwd("D:/UNIVERSIDARKSSS/SEXTO AÑO/PROF.R/Trabajo.3")

# 1.Paquetes:
install.packages("pacman")
pacman::p_load(haven,
               sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2,  # Para la mayoría de los gráficos
               mice)   #Para imputar NA's


# 2.Cargamos base de datos:
CASEN_2022<- read_dta("input/Casen_2022.dta") #para bases de datos de STATA

####### BASE RData optimizada, resultado de la filtración, recodificación y selección.
load(file = "Input/PROC_DATA.RData")

# 2.1.Filtramos para trabajar con la 8va región:
CASEN_8_RURAL <- CASEN_2022 %>%
  filter(region==8, #Region del Biobio.
         area==2) #Area rural

# 2.2.Seleccionamos variables:
DATA <- CASEN_8_RURAL %>%
  select(e6a, #Educacion
         oficio1_08, #Trabajo u oficio 
         y1, #Ingresos
         s13, #Salud
         r6, #Red de apoyo
         v1, v3, v5, v7) #Vivienda y estado de vivienda  

summary(DATA)
names(DATA)
sjlabelled::get_label(DATA)

# 2.3.Guardamos para optimizar
save(DATA,file = "input/DATA.RData")

# 3.Exploramos la nueva Base y sus variables:
frq(DATA$e6a)
frq(DATA$oficio1_08)
frq(DATA$y1)
frq(DATA$s13)
frq(DATA$r6)
frq(DATA$v1)
frq(DATA$v3)
frq(DATA$v5)
frq(DATA$v7)

# 3.1.Casos perdidos:

DATA <- DATA %>% set_na(., na = c(-88, -99, -66))
DATA <- na.omit(DATA)

# 3.2.Recodificación de variables:
### reordenamos las categorías de respuesta
DATA$v3 <- as_factor(DATA$v3) 
DATA$v3 <- recode(DATA$v3, "1" = "3", "2" = "2", "3" = "1")

DATA$v5 <- as_factor(DATA$v5)  
DATA$v5 <- recode(DATA$v5, "1" = "3", "2" = "2", "3" = "1")

DATA$v7 <- as_factor(DATA$v7) 
DATA$v7 <- recode(DATA$v7, "1" = "3", "2" = "2", "3" = "1")

DATA$v3 <- as.numeric(as.character(DATA$v3))
DATA$v5 <- as.numeric(as.character(DATA$v5))
DATA$v7 <- as.numeric(as.character(DATA$v7))

# 3.2.Agrupamos y renombramos variables:

#Renombramos variables
frq(DATA$v1)
DATA <- DATA %>% rename("Tipo_vivienda" = v1)
frq(DATA$y1)
DATA <- DATA %>% rename("Ingresos" = y1)
frq(DATA$e6a)
DATA <- DATA %>% rename("Nivel_educacional" = e6a)
frq(DATA$s13)
DATA <- DATA %>% rename("Sistema_salud" = s13)
frq(DATA$r6)
DATA <- DATA %>% rename("Red_apoyo" = r6)
frq(DATA$oficio1_08)
DATA <- DATA %>% rename("Ocupacion_u_oficio" = oficio1_08)
#Agrupamos variables
DATA$Estado_vivienda <- (DATA$v3 + DATA$v5 + DATA$v7)

view_df(DATA)

# 3.3.Seleccionamos variables para el trabajo:
PROC_DATA <- DATA %>%
  select(Ingresos, Red_apoyo, Tipo_vivienda, Estado_Vivienda, Nivel_educacional, Ocupacion_u_oficio, Sistema_salud)
### Pasamos a numericas
PROC_DATA$Ingresos <- as.numeric(as.character(PROC_DATA$Ingresos))
PROC_DATA$Tipo_vivienda <- as.numeric(as.character(PROC_DATA$Tipo_vivienda))
PROC_DATA$Nivel_educacional <- as.numeric(as.character(PROC_DATA$Nivel_educacional))
PROC_DATA$Sistema_salud <- as.numeric(as.character(PROC_DATA$Sistema_Salud))
PROC_DATA$Red_apoyo <- as.numeric(as.character(PROC_DATA$Red_Apoyo))
PROC_DATA$Estado_Vivienda <- as.numeric(as.character(PROC_DATA$Estado_Vivienda))
PROC_DATA$Ocupacion_u_oficio <- as.numeric(as.character(PROC_DATA$Trabajo))

# 3.4.Guardamos para optimizar:
save(PROC_DATA,file = "input/PROC_DATA.RData")

# 4.Tabla descriptiva.
summarytools::dfSummary(PROC_DATA, plain.ascii = FALSE)
view(dfSummary(PROC_DATA, headings=FALSE))

## 4.1.Gráficos de las Variables:
graf_Ingresos <- ggplot(PROC_DATA, aes(x = Ingresos)) +
  geom_bar(fill = "coral") + 
  labs (title = "Ingresos totales")
graf_Apoyo <- ggplot(PROC_DATA, aes(x = Red_apoyo))+
  geom_bar(fill = "coral") +
  labs (title = "Red de apoyo")
graf_Viv <- ggplot(PROC_DATA, aes(x = Tipo_vivienda))+
  geom_bar(fill = "coral") +
  labs(title = "Tipo de Vivienda")
graf_Viv_estado <- ggplot(PROC_DATA, aes(x = Estado_Vivienda))+
  geom_bar(fill = "coral") +
  labs(title = "Estado de Vivienda")
graf_Educ <- ggplot(PROC_DATA, aes(x = Nivel_educacional))+
  geom_bar(fill = "coral") +
  labs(title = "Nivel educativo")
graf_Salud <- ggplot(PROC_DATA, aes(x = Sistema_salud))+
  geom_bar(fill = "coral") +
  labs(title = "Sistema de salud")
graf_Trabajo <- ggplot(PROC_DATA, aes(x = Ocupacion_u_oficio))+
  geom_bar(fill = "coral") +
  labs(title = "Oficio u ocupación")

# 4.2.Guardamos:
ggsave(graf_Ingresos, file="output/graf_Ing.png")
ggsave(graf_Apoyo, file="output/graf_Apoyo.png")
ggsave(graf_Viv, file="output/graf_Viv.png")
ggsave(graf_Educ, file="output/graf_Educ.png")
ggsave(graf_Salud, file="output/graf_Salud.png")
ggsave(graf_Trabajo, file="output/graf_Trabajo.png")
ggsave(graf_Viv_estado, file="output/graf_Viv_estado.png")

# 5.0 Asociacion de variables:
dim(PROC_DATA)

sjmisc::descr(PROC_DATA,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 5.1.Correlación
PROC_DATA <- mutate_all(PROC_DATA, as.numeric)

COR_PROC_DATA <- cor(PROC_DATA,
         use = "complete.obs")
COR_PROC_DATA

sjPlot::tab_corr(PROC_DATA, 
                 triangle = "lower")

corrplot.mixed(COR_PROC_DATA)
# 5.2.Confiabilidad del conjunto

###Alpha de Cronbach:
psych::alpha(PROC_DATA, check.keys = TRUE)

###Alpha de Cronbach recodificado:

psych::alpha(dplyr::select(PROC_DATA, Ingresos, Estado_Vivienda, Nivel_educacional, Ocupacion_u_oficio),check.keys = TRUE)

# 5.3.Construccion de escala:
PROC_DATA <- PROC_DATA %>% 
  rowwise() %>% 
  mutate(Pobreza_multidi_rural = sum(Ingresos, Estado_Vivienda, Nivel_educacional, Ocupacion_u_oficio))
summary(PROC_DATA$Pobreza_multidi_rural)


ggplot(PROC_DATA, aes(x = Pobreza_multidi_rural)) +
  geom_histogram(binwidth = 150000, colour = "black", fill = "yellow") +
  theme_bw() +
  xlab("Pobreza multidimensional rural") +
  ylab("Frecuencia")
