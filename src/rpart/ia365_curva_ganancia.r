# Grafico de la ganancia que visualiza el overfitting
# La idea es probar con distintos hiperparametros del arbol de decision
# y ver como se acercan o separan las curvas de ganancia
# MUY importante :  notar que Training = 50%  y  Testing = 50%

# Notar que la curva en training es siempre convexa
# mientras que la de testing puede tener concavidades

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("ggplot2")


# cambiar aqui los parametros
PARAM <- list()
PARAM$minsplit <- 650
PARAM$minbucket <- 130
PARAM$maxdepth <- 3
PARAM$cp <- -1

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
       if (!is.na(seed)) set.seed(seed)

       bloque <- unlist(mapply(
              function(x, y) {
                     rep(y, x)
              },
              division, seq(from = start, length.out = length(division))
       ))

       data[, (campo) := sample(rep(
              bloque,
              ceiling(.N / length(bloque))
       ))[1:.N],
       by = agrupa
       ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

#cargo MI amada primera semilla, que esta en MI bucket
tabla_semillas <- fread("./datasets//mis_semillas.txt")
ksemilla_azar <- tabla_semillas[1, semilla]  # 1 es mi primera semilla

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# a partir de ahora solo trabajo con 202107, el mes que tiene clase
dataset <- dataset[foto_mes == 202107] # defino donde voy a entrenar


# La division training/testing es 50%, 50%
#  que sea 50/50 se indica con el c(1,1)
particionar(dataset,
       division = c(1, 1),
       agrupa = "clase_ternaria", seed = ksemilla_azar
)

# Entreno el modelo
# los datos donde voy a entrenar
# aqui es donde se deben probar distintos hiperparametros
modelo <- rpart(
       formula = "clase_ternaria ~ . -fold",
       data = dataset[fold == 1, ],
       xval = 0,
       cp = PARAM$cp,
       minsplit = PARAM$minsplit,
       minbucket = PARAM$minbucket,
       maxdepth = PARAM$maxdepth
)


# aplico el modelo a TODOS los datos, inclusive los de training
prediccion <- predict(modelo, dataset, type = "prob")

# Pego la probabilidad de  BAJA+2
dataset[, prob_baja2 := prediccion[, "BAJA+2"]]


# Dibujo la curva de ganancia acumulada
setorder(dataset, fold, -prob_baja2)

# agrego una columna que es la de las ganancias
# la multiplico por 2 para que ya este normalizada
#  es 2 porque cada fold es el 50%
dataset[, gan := 2 *ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]
dataset[, ganancia_acumulada := cumsum(gan), by = fold]
dataset[, pos := sequence(.N), by = fold]


# Esta hermosa curva muestra como en el mentiroso training
#   la ganancia es siempre mejor que en el real testing
# segundo grafico solo los primeros 20k enviso
gra <- ggplot(
           data = dataset[pos <= 20000],
           aes(x = pos, y = ganancia_acumulada, color = ifelse(fold == 1, "train", "test"))
             ) + 
             geom_line() +
             scale_y_continuous(labels = scales::comma_format()) +
             labs(title = "Árbol menos profundo",
                  subtitle = "maxdepth = 3, minsplit = 650, minbucket = 130, cp = -1",
                  x = "Posición",
                  y = "Ganancia Acumulada",
                  color = "Grupo") +
             scale_color_manual(values = c("train" = "#2F00FF", "test" = "#FF008C"))

             

print(gra)

dir.create("./plots/", showWarnings = FALSE)
ggsave("./plots/grafico_curva_ganancia_arbol_menos_profundo.png", gra, width = 10, height = 10, dpi = 300, units = "in")

cat("Train gan max: ", dataset[fold == 1, max(ganancia_acumulada)], "\n")
cat("Test  gan max: ", dataset[fold == 2, max(ganancia_acumulada)], "\n")

# Probe 3 arboles, profundo, no-profundo y neutro

 # Arbol produndo --> grafico_curva_ganancia_arbol_profundo.png
 # maxdepth = 20, minsplit = 50, minbucket = 10, cp = -1

 # Arbol no-produndo --> grafico_curva_ganancia_arbol_menos_profundo.png
 # maxdepth = 3, minsplit = 650, minbucket = 130, cp = -1

 # Arbol neutro --> grafico_curva_ganancia_arbol_neutro.png
 # maxdepth = 6, minsplit = 375, minbucket = 75, cp = -1
