Sebastian Herrera
Para esta ayudantia utilizaremos un dataset que contiene la calidad de diversos vinos que se evaluaron

## Importar Librerias
```{r, message=FALSE, warning=FALSE}
library(tidyverse)
library(cluster)
library(factoextra)
library(mclust)
```

Para un primer intento tomaremos todas las variables del dataset, ver que cluster obtenemos y como se comportan los indicadores de cada modelo

## Cargar Datos

```{r}
setwd("C:/Users/sebah/Desktop/U/mineria de datos/ayudantia 6")

data <- read.csv("Spotify_Songs.csv")

summary(data)
```


# Pre Procesamiento de los Datos
## Limpieza Datos:
Primero verificaremos la existencia de valores NA o valores faltantes faltantes
```{r limpieza na}
# Para las observaciones incompletas, le asignamos el valor NA para eliminarlos en el siguiente paso
data[data == ""] <- NA

# Verificamos donde hay valores NAs
data %>% 
  summarise_all(funs(sum(is.na(.))))

# Eliminamos todas las observaciones que presenten NA
data_pre <- data %>% 
  filter(!(is.na(track_name)|is.na(track_artist)|is.na(track_album_name)|is.na(duration_ms)))

# Corroboramos que no queden datos NA
data_pre %>% 
  summarise_all(funs(sum(is.na(.))))

```
#Filtro y duplicados
filtrar y remover datos duplicados
```{r limpieza duplicados}
data_pre <- data_pre[!duplicated(data_pre$track_id),]
```

verificamos la existencia de errores
```{r limpieza errores}
# Al explorar la base de datos podemos darnos cuenta de que hay varias observaciones que tiene mal ingresado los datos
# la columna track_popularity la transformaremos de factor a numerico, por lo que todas las observaciones que no sean numeros se ingresara NA por defecto

data_pre$track_popularity <- as.numeric(as.character(data_pre$track_popularity))

# Como generamos nuevos valores NA dentro de nuestra BBDD, debemos volver a ejecutar el paso uno de la limpieza de datos

data_pre <- data_pre %>% 
  filter(!(is.na(track_popularity)))

# Eliminamos el <U que aparece en algunas observaciones en track_name y track_artist

data_pre <- data_pre[!grepl("<U",data_pre$track_name),]
data_pre <- data_pre[!grepl("<U",data_pre$track_artist),]

# Ahora veremos si existen canciones duplicadas

data_pre %>% count(duplicated(data_pre$track_name))

# Como existen canciones duplicadas realizamos la consulta para obtener los valores distintos, pero este hecho obvia que hayan canciones con el mismo nombre pero de distinto artistas  
data_pre %>% distinct(track_name, .keep_all = TRUE, )


# Por lo anterior creamos una variable que almacene si existe duplicidad en la cacion y/o en el artista
data_pre$duplicate <- duplicated(data_pre[,c("track_name", "track_artist")])

# Generamos un sub data frame que almacenara solo los valores que haya obtenido el valor TRUE a la consulta anterior y los ordenamos por track popularity
data_dupli <- data_pre %>% 
  filter(data_pre$duplicate == TRUE) %>% 
  arrange("track_name", "track_popularity", desc(track_popularity))

# Seleciono las filas que sean distintas, borro todas las canciones duplicadas y me quedo con la mayor track popularity
data_dupli <- data_dupli %>% 
  distinct(track_name, track_artist, .keep_all = TRUE)

# Elimino de mi data pre procesada los datos que dieron positivo a la duplicidad, para que al momento de re insertar los datos sobrevivieron a la limpieza de duplicidad no se genere la duplicidad que se estaba evitando
data_pre <- data_pre[!(data_pre$duplicate == TRUE),]

# Junto la data pre procesada con los datos que sobrevivieron a la limpieza de duplicidad
data_pre <- rbind(data_pre, data_dupli)

# Elimino la columna que me indicaba duplicidad ya que no sera util mas adelante
data_pre$duplicate <- NULL

```

Una vez limpiados los datos, el siguiente paso en el pre procesamiento será escalar los datos pero antes debemos revisar los datos por si hay que transformar alguna variable

## Revisar Estructura Datos
```{r transformar tipo datos}
# Transformamos cada variables al tipo de variable que sale en el archivo .txt con la descripcion de cada una

data_pre$track_id <- as.character(data_pre$track_id)
data_pre$track_name <- as.character(data_pre$track_name)
data_pre$track_artist <- as.character(data_pre$track_artist)
data_pre$track_album_id <- as.character(data_pre$track_album_id)
data_pre$track_album_name <-  as.character(data_pre$track_album_name)
data_pre$playlist_name <- as.character(data_pre$playlist_name)
data_pre$playlist_id <- as.character(data_pre$playlist_id)
data_pre$playlist_genre <- as.character(data_pre$playlist_genre)
data_pre$playlist_subgenre <- as.character(data_pre$playlist_subgenre)

data_pre$danceability <- as.double(as.character(data_pre$danceability))
data_pre$energy <- as.double(as.character(data_pre$energy))
data_pre$key <- as.double(as.character(data_pre$key))
data_pre$loudness <- as.double(as.character(data_pre$loudness))
data_pre$mode <- as.double(as.character(data_pre$mode))
data_pre$speechiness <- as.double(as.character(data_pre$speechiness)) 
data_pre$acousticness <- as.double(as.character(data_pre$acousticness))
data_pre$instrumentalness <- as.double(as.character(data_pre$instrumentalness))
data_pre$liveness <- as.double(as.character(data_pre$liveness))
data_pre$valence <- as.double(as.character(data_pre$valence))
data_pre$tempo <- as.double(as.character(data_pre$tempo))
data_pre$duration_ms <- as.double(as.character(data_pre$duration_ms))

# transformacion de milisegundos a minutos
data_pre <- data_pre %>% mutate(duration_min = data_pre$duration_ms/60000)

# Character
data_char <- c("track_id", "track_name", "track_artist", "track_album_id", "track_album_name", "playlist_name", "playlist_id", "playlist_genre", "playlist_subgenre")

# Double
data_dou <- c("track_popularity","danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")

# Volvemos a borrar los datos que puedan haber quedado como NA con el cambio de tipo de variable
data_pre <- data_pre %>% 
  filter(!(is.na(key)|is.na(danceability)))

summary(data_pre)

str(data_pre)

```