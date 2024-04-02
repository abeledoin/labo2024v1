# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dataset[clase_ternaria == "BAJA+2", clase_binaria1 := "pos"]
dataset[clase_ternaria != "BAJA+2", clase_binaria1 := "neg"]

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_binaria1 ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 0, # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 3
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)