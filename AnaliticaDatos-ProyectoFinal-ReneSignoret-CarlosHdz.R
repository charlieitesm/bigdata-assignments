################################
#
# Instituto Tecnologico de Estudios Superiores de Monterrey
# MCC
#
# Analitica de Datos
#
# Proyecto Final
#
# Rene Signoret - A
# Carlos E. Hernandez - A01181616
#
# Agosto 2020
################################


getwd()

# Modificar esto a la carpeta donde se encuentran los datasets
dataset.folder = ""

setwd(project.home)


# 1. Cargar Datasets

load("AsistenciasTotales.R")
load("perfilAlumnos.R")
load("ResultadosExamenes.R")
load("ResultadoTrabajos.R")
load("UsoBiblioteca.R")
load("UsoPlataforma.R")
load("ApartadoDeLibros.R")
load("Becas.R")
load("HistorialPagos.R")
load("CambioCarrera.R")



# 2. Truncar la informacion para usar solo 2 semestres



# 3. Feature engineering - Codificar cada uno de los features



# 4. Feature scaling



# 5. Dataframes para alumnos


# Salvamos los resultados
save(datos.alumnos.integrados, file="datos.alumnos.integrados.R")

# 6. Train/test split 90%/10%



# 7. Generacion de labels usando K-Means



# 8. Entrenamiento de red neuronal



# 9. Predicciones para el set de test



# 10. Algoritmo genetico para accion remedial


