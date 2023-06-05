### TAREA 5 - GRUPO3

# Importar las librerías necesarias
library(readxl)
library(haven)
library(dplyr)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

                            # GRAFICO 1 #

# Ruta y nombre del archivo de Excel
data_1 <- read_excel("C:/Users/ALICIA/Documents/GitHub/1ECO35_2031_1_APUNTES/data_tarea5/6.1.1_-_Illicit_coca_bush_cultivation.xlsx")

# Filtrar las columnas correspondientes a países y fechas
data_1 <- data_1[c(2, 4, 5, 6,7), ]

# Transponer el dataframe data_1
data_transposed <- as.data.frame(t(data_1))


# Cambiar los nombres de los encabezados
nw_names <- c("year","Bolivia", "Colombia", "Peru")
colnames(data_transposed) <- replace(colnames(data_transposed), TRUE, nw_names)

#Eliminar la primera fila de un data frame
data_nw <- data_transposed[-1, ]

#Eliminar la ultima columna
data_nw <- data_nw[, -ncol(data_nw)]

# Definir los nuevos nombres de los valores
nw_names_2 <- c("2009","2010", "2011", "2012", "2013","2014","2015", "2016", "2017", "2018", "2019", "2020")

# Obtener la columna específica
clm <- data_nw$year

# Convertir la columna en un factor con los nuevos nombres
data_nw$year <- factor(clm, levels = unique(clm), labels = nw_names_2)

# Convertir la columna en un factor con los nuevos nombres
data$columna1 <- factor(data$columna1, levels = unique(data$columna1), labels = nw_names_2)

# Convertir las columnas a formato numérico
data_nw <- data_nw %>%
  mutate(Bolivia = as.numeric(Bolivia),
         Colombia = as.numeric(Colombia),
         Peru = as.numeric(Peru))
str(data_nw)

# GRAFICANDO

ggplot(data_nw) +
  aes(x = year) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed", group = 1) +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid", group = 1) +
  geom_line(aes(y = Peru, color = "Perú"), size = 0.6, linetype = "dashed", group = 1) +
  theme_minimal() +
  labs(x = "Años", y = "Coca production", title = "Figure 1: Coca production in the Andean region") +
  scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                     labels = c("Bolivia", "Colombia", "Perú"))+
  labs(color = "Leyenda")

                        # GRAFICO 2 #
# Ruta y nombre del archivo de Excel
data_2 <- read_excel("C:/Users/ALICIA/Documents/GitHub/1ECO35_2031_1_APUNTES/data_tarea5/6.1.2_-_Eradication_of_coca_bush.xlsx")


# Eliminar filas desde la fila 5 hasta el final del data frame
data_nw2 <- data_2 %>% slice(1:4)

# Eliminar las columnas con índices 2, 3 y 16
data_nw2 <- data_nw2[, -c(2, 3, 16)]

# Transponer el dataframe data_1
data_nw2 <- as.data.frame(t(data_nw2))

# Cambiar los nombres de los encabezados
nw_names_3 <- c("year","Bolivia", "Colombia", "Peru")
colnames(data_nw2) <- replace(colnames(data_nw2), TRUE, nw_names_3)

#Eliminar la primera fila de un data frame
data_nw2 <- data_nw2[-1, ]


# Definir los nuevos nombres de los valores
nw_names_4 <- c("2009","2010", "2011", "2012", "2013","2014","2015", "2016", "2017", "2018", "2019", "2020")

# Obtener la columna específica
clm <- data_nw2$year

# Convertir la columna en un factor con los nuevos nombres
data_nw2$year <- factor(clm, levels = unique(clm), labels = nw_names_4)


# Convertir las columnas a formato numérico
data_nw2 <- data_nw2 %>%
  mutate(Bolivia = as.numeric(Bolivia),
         Colombia = as.numeric(Colombia),
         Peru = as.numeric(Peru))
str(data_nw2)


# GRAFICANDO

ggplot(data_nw2) +
  aes(x = year) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed", group = 1) +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid", group = 1) +
  geom_line(aes(y = Peru, color = "Perú"), size = 0.6, linetype = "dashed", group = 1) +
  theme_minimal() +
  labs(x = "Year", y = "Reported Eradication (in hectare)", title = "Reported eradication of coca bush, 2009-2020") +
  scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                     labels = c("Bolivia", "Colombia", "Perú"))+
  labs(color = "Leyenda")

                                      # GRAFICO 3 #
# Hacemos el merge
DATA <- merge(data_nw,data_nw2,by="year")

# Eliminar las columnas con índices 
DATA_l <- DATA[, -c(2,3,5,6)]


# Cambiar los nombres de los encabezados
nw_names_5 <- c("year","Production", "Erradication")
colnames(DATA_l) <- replace(colnames(DATA_l), TRUE, nw_names_5)


# Convertir las columnas a formato numérico
DATA_l <- DATA_l %>%
  mutate(Production = as.numeric(Production),
         Erradication = as.numeric(Erradication))
str(DATA_l)


ggplot(DATA_l) +
  aes(x = year) +
  geom_line(aes(y = Production, color = "Producción"), size = 0.6, linetype = "dashed", group = 1) +
  geom_line(aes(y = Erradication, color = "Erradicación"), size = 0.6, linetype = "solid", group = 1) +
  theme_minimal() +
  labs(x = "Year", y = "Hectareas", title = "Figure 3: Producción y erradicación de hoja de coca en el Perú (2009 - 2020)") +
  scale_color_manual(values = c(Producción = "purple", Erradicación = "turquoise"),
                     labels = c("Producción", "Erradicación")) +
  labs(color = "Leyenda")
