# Introducción a R
# Jornadas de Informática en Salud - 2020

# Instalar paquetes para el taller (única vez) ----------
install.packages(c("tidyverse",
                   "dslabs"))

# Librerías
library(tidyverse)
library(dslabs)

# Crear objetos con operador asignar `<-` --------
a <- 3
a

a <- 4 # se sobre-escribe
a

b <- 5

# Funciones básicas: operaciones matemáticas ------
a + b
a * b
b^2
sqrt(a)

# Tipos de objetos --------------------------------
a <- 3.5
typeof(a)

b <- "string de texto"
typeof(b)

c <- 2L  # L significa integer
typeof(c)

d <- TRUE
typeof(d)

# Vectores: función 'combinar' `c()` ---------------
# Vectores atómicos: todos los objetos son del mismo tipo
c(1,2,4,7)
1:5
c(TRUE, FALSE, TRUE, TRUE, FALSE)
c("primer elemento", "segundo", "tercero")

  # Asignar
  va <- 1:4
  va
  
  vb <- c(5,5,8,9)
  vb
  
  # Secuencias
  ?seq()  # manual de la función
  seq(from = 2, to = 10, by = 2)
  
# Acceder a elementos de un vector con [] --------
  vect <- c(3,3,4,5,9)
  vect
  
  # Por posición (primer lugar es 1)
  vect[1]
  vect[5]
  vect[1:3]
  
  # usando vectores lógicos
   #ejemplo: generar un vector lógico con una condición
   vect > 4
   vect[vect > 4]
   
   vect[vect > 4 & vect < 9]
   
# Data.frames ------------------------------------
   # Crear un data.frame
   base <- data.frame(columna1 = c(1, 2, 3, 4),  #<<
                   columna2 = c("A", "B", "C", "D"),
                   columna3 = 2019:2022)
   base
   
# Exploración de datos ---------------------------
 # Base de datos 'gapminder' del paquete dslabs
 # Requiere instalación de dslabs con:
 # install.packages("dslabs")
 mundo <- dslabs::gapminder

dim(mundo) # dimensiones: filas y columnas
names(mundo) # nombres de variables
str(mundo)
glimpse(mundo)
head(mundo)
View(mundo)

# Carga de datos externos ---------------------
# Función general (seudocódigo)
# carga de datos: recursos para Excel
getwd()  # ver directorio de trabajo
consultas <- read_csv("datos/consultas.csv")
consuultas <- read_csv("https://raw.githubusercontent.com/f-binder/datos_intro/main/consultas.csv")


  # Exploración de base consultas:
  dim(consultas)
  names(consultas)
  glimpse(consultas)
  head(consultas)
  View(consultas)
  
# Tidyverse: paquetes para análisis de datos. ==================
 # Funciones arrange, select y filter---------------
  
arrange(consultas, edad)
arrange(consultas, desc(edad))

select(.data = consultas, id, edad, embarazos)

filter(consultas, edad >= 70)
filter(consultas, edad >= 70 & glucemia > 140)

 # Pipes -----------------------
consultas %>% arrange(edad)
consultas %>% filter(edad > 40) %>%
  arrange(edad)

consultas %>%
  select(id, edad, embarazos, glucemia) %>%
  filter(edad > 40) %>%
  arrange(edad)

consultas %>%
  select(id, edad, embarazos, glucemia) %>%
  filter(edad > 40) %>%
  arrange(edad)

 # Crear variables: función mutate -------------
  # mutate crea una nueva columna o sobre-escribe una columna existente
mutate(consultas, horas_espera = mins_espera/60)

  # para ver el resutlado, puede usarse un pipe y select()
  mutate(consultas, horas_espera = mins_espera/60) %>%
    select(horas_espera, mins_espera)
  
  # Crear variable dicotómica para antecedente de embarazo
  consultas %>%
  mutate(embarazo_dicot = ifelse(embarazos > 0, 
                               "atc_embarazos", 
                               "nuligesta")) %>% 
  select(embarazos, embarazo_dicot, everything())

  
    # Verificar resultados de código anterior
    consultas %>%
      mutate(partos_dicot = ifelse(embarazos > 0,
                               "atc_embarazos",
                               "nuligesta")) %>%
      distinct(embarazos, partos_dicot) %>%
      arrange(embarazos)
  
  # Crear variable para seguimiento de glucemia con condiciones complejas
    consultas %>%
      mutate(seguimiento =
           case_when(glucemia > 126 ~ "consulta endocr.",
                     glucemia >= 110 ~ "estrecho",
                     glucemia >= 100 | edad >= 65 ~ "intermedio",
                     TRUE ~ "bajo"))

    # TODO: agregar a filminas summarise

 # Summarise ----------------------
  # resumen general
 consultas %>% summarise(promedio_espera = mean(mins_espera))

 consultas %>% summarise(n = n(),
                  promedio_espera = mean(mins_espera),
                  maxima_espera = max(mins_espera))

  # resumen por grupos
consultas %>%
  group_by(dia_semana) %>%
  summarise(n = n(),
            promedio_espera = mean(mins_espera),
            maxima_espera = max(mins_espera))

  # Combinaciones con otras funciones de dplyr
consultas %>%
  mutate(gestas = ifelse(embarazos > 0,
                         "atc_gestas",
                         "nuligesta")) %>%
  group_by(gestas) %>%
  summarise(n = n(),
            gluc_media = mean(glucemia),
            gluc_min = min(glucemia))

# Gráficos con ggplot2 ------------------------------
mundo <- dslabs::gapminder  # si no se creó el objeto 'mundo' previamente

 # gráfico de barras simple
 ggplot(data = mundo, aes(x = continent)) +
  geom_bar()

 # colores de relleno en las barras
 ggplot(data = mundo, aes(x = continent)) +
  geom_bar(aes(fill = continent))
 
 # Relleno por país
 ggplot(data = mundo, aes(x = continent)) +
  geom_bar(aes(fill = country)) +
   theme(legend.position = "none")
 
 ## Usar Pipes con Ggplot ---------------------------
 # filtrar observaciones antes de construir el gráfico:
 dim(mundo)  # el dataset original tiene 10,000+ filas
  
   # subset: Oceanía
 mundo %>%
  filter(continent == "Oceania") %>%
  dim()
 
 mundo %>%
  filter(continent == "Oceania" & life_expectancy < 65) %>%
  ggplot(aes(x = country)) +
  geom_bar()

  # Gráfico de puntos: esperanza de vida en 2000,
    # países con "A"
   mundo %>%
     filter(str_detect(country, "^A") & year == 2000) %>% # "^A" es una 'expresión regular', busca la letra A al comienzo de un string
     ggplot(aes(x = country, y = life_expectancy)) +
     geom_point()
   
   
    # Agregar mapping para puntos: tamaño y color
    mundo %>%
      filter(str_detect(country, "^A") &
               year == 2000) %>%
        ggplot(aes(x = country,
                   y = life_expectancy)) +
        geom_point(aes(size = population, color = continent)) #<<
         
  # Versión de diapositiva
  mundo %>%
  filter(str_detect(country, "^A") &  # filtra países que comienzan con "A"
           year == 2000) %>%
  ggplot(aes(x = country,
             y = life_expectancy)) +
  geom_point(aes(size = population, color = continent), alpha = .9) + 
  geom_hline(yintercept = 65, lty = "dashed", color = "brown") +
  annotate("text", y = 65, x = 1, vjust = -0.5, hjust = -0.05, color = "brown",
           label = "65 años", size = 5) +
  labs(title = "Esperanza de vida en año 2000",
       y = "Esperanza de vida (años)", x = "",
       caption = "El tamaño de los puntos refleja la población en año 2000") +
  scale_size_continuous(range = c(4, 12), guide = FALSE) +  #rango de tamaño de puntos
  scale_color_brewer(type = "qual", palette = 6) +  # paleta de colores
    #   coord_flip() + # des-comentar para invertir ejes
  guides(colour = guide_legend(override.aes = list(size=7))) +
  theme_bw() +  #<<
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), #<<
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15), #<<
        plot.title = element_text(size = 16, color = "grey20"),
        legend.text = element_text(size = 14),
        legend.title = element_blank())
  
# Tests de hipótesis ========================================

  # Pregunta: es diferente el tiempo de espera en los extremos de la semana
  # (lunes o viernes) comparado con los días de mitad de semana?
  
  # Exploración de datos
  dim(consultas)
  
   # Variables de interés (primeras 10 filas)
  consultas %>%
    select(mins_espera, dia_semana, hora_presente, hora_atencion) %>%
    head(10)
  
   # días de la semana
      consultas %>%
        distinct(dia_semana) #<<

    # contar vacíos
        consultas %>%
          summarise(n_vacios_dias = sum(is.na(dia_semana)),
                    n_vacios_espera = sum(is.na(mins_espera)))

        sum(is.na(consultas$mins_espera))

    ## dias de la semana
        consultas %>%
          count(dia_semana)

    # Minutos de espera
    summary(consultas$mins_espera)

    consultas %>%
      ggplot(aes(x = mins_espera)) +
      geom_histogram()

      # agrupados por día de la semana
    consultas %>%
      ggplot(aes(x = mins_espera)) +
      geom_histogram() +
      facet_wrap(~dia_semana)
      
 # Crear variables para testeo de hipótesis: Grupos de días -------------
    # Prueba de código para crear días
      consultas %>%
        mutate(extr_semana = 
                 ifelse(dia_semana %in% c("Lun", "Vie"),
                        "extremo",
                        "centro"))
    
      consultas %>%
        mutate(extr_semana = 
                 ifelse(dia_semana %in% c("Lun", "Vie"),
                        "extremo",
                        "centro")) %>%
        distinct(dia_semana, extr_semana)


      # asignar base con columna nueva a objeto original
      dim(consultas)
      consultas <- consultas %>%
              mutate(extr_semana = 
                 ifelse(dia_semana %in% c("Lun", "Vie"),
                        "extremo",
                        "centro"))
      dim(consultas)  # ahora tiene 1 columna más
      
# Descripción de variable de interés por grupos -------
# Resumen: tiempo de espera por grupos
# Summarise()
consultas %>%
  group_by(extr_semana) %>% #<<
  summarise(promedio_espera = mean(mins_espera),
            maxima_espera = max(mins_espera))

# Exploración gráfica - histograma
consultas %>%
  ggplot(aes(x = mins_espera)) +
  geom_histogram(aes(fill = extr_semana)) +
  facet_wrap(~extr_semana, ncol = 1) +
  theme_bw()

# Exploración gráfica - boxplots
consultas %>%
  ggplot(aes(x = extr_semana, #<<
             y = mins_espera)) + #<<
  geom_boxplot(aes(fill = extr_semana)) +
  labs(title = "Tiempo de espera según grupos de días") +
  theme_bw()

# Exploración gráfica - gráficos de densidad (no incluido en el taller)
consultas %>%
  ggplot(aes(x = mins_espera)) + #<<
  geom_density(aes(fill = extr_semana),
               alpha = 0.5) +
  labs(title = "Tiempo de espera a la atención") +
  theme_bw()


# Test de hipótesis: T-Test (test de Student)
t.test(consultas$mins_espera ~ consultas$extr_semana)

# Test de hipótesis: Wilcoxon-Mann-Whitney
wilcox.test(consultas$mins_espera ~ consultas$extr_semana)

  # extraer el p-valor
t.test(consultas$mins_espera ~ consultas$extr_semana)["p.value"]
