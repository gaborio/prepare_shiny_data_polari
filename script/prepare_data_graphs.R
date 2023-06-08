# MVP preguntas grandes de investigación polarización
# Created by: Gabriel N. Camargo-Toledo
# Created on: Jun/08/2023
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Jun/08/2023
# Contact: gn.camargo215@uniandes.edu.co; gabriel.n.c.t182@gmail.com
# Asus VivoBook Pop!_OS 22.04 8gb Ram R4.1.2


# Dependencies and functions -------
library(sensataDataAnalysis)
## recode 100 funcion ---------
recode_100 <- function(df, col, values) {
  # Create a new column name by adding "_r" to the original column name
  new_col <- paste0(col, "_r")
  # recode the values to 100 and assign the result to the new column
  df[[new_col]] <- recode(df[[col]], .default = 0, !!!setNames(rep(100, length(values)), values))
  # Return the modified data frame
  return(df)
}

### summarySE with label function ------
# Define a function that takes a column name and a label as arguments
summary_label <- function(col, label) {
  df <- summarySE(pol_data, col,
                  weightsVar = "ponde",
                  groupVars = c("Pais", "Grupo2"),
                  na.rm = TRUE
  )
  # Filter out the rows with NA in Grupo2
  df <- df[!is.na(df$Grupo2), ]
  # Add a new column with the label
  df$Pregunta <- label
  # Return the data frame
  return(df)
}

# Data --------------------------------------------------------------------
pol_data <- readRDS("data/input/polari_analisis.rds") |> selectCols() |> rename_with(~str_remove(., "q_POL_"))

# create data by countries ------------------------------------------------
arg_data <- pol_data %>% filter(Pais == "Argentina")
bra_data <- pol_data %>% filter(Pais == "Brasil")
col_data <- pol_data %>% filter(Pais == "Colombia")
mex_data <- pol_data %>% filter(Pais == "México")
pol_data <- pol_data %>% filter(Pais != "Otro")

# Todas las de afecto empresarios -----------------------------------------
## Creando variables para porcentaje aprobación -----
# valores de columnas a cambiar 
cols_values <- list(
  EMO_10 = c("5", "6. Positiva"),
  PAF_10 = c("5", "6. Honestos"),
  PAF_13 = c("5", "6. Solidarios"),
  PAF_01 = c("4", "5. De acuerdo"),
  PAF_02 = c("4", "5. De acuerdo"),
  PAF_07 = c("Smiling", "Very happy")
)

# loop 
for (col in names(cols_values)) {
  pol_data <- recode_100(pol_data, col, cols_values[[col]])
}

# creating graph data -----------------------------------------------------
#  list of column names and labels to use
cols_labels <- list(
  EMO_10_r = "Opinión positiva",
  PAF_10_r = "Los considera honestos",
  PAF_13_r = "Los considera solidarios",
  PAF_01_r = "Confia que pagan salarios justos",
  PAF_02_r = "Confia que cumplen normas de tránsito",
  PAF_07_r = "Estaría feliz de que sus hijos sean amigos"
)

# Loop over the list and apply the summary_label function to each element
graph_data <- NULL # Initialize an empty data frame
for (col in names(cols_labels)) {
  graph_data <- rbind(graph_data, summary_label(col, cols_labels[[col]]))
}

# Factorize the Grupo2 column and set the levels
graph_data$Grupo2 <- factor(graph_data$Grupo2, levels = c(
  "Sector privado",
  "Empresarios informales",
  "Estudiantes",
  "Líderes sociales",
  "Otros ciudadanos",
  "Total"
))


# save afecto empresarios -------------------------------------------------
saveRDS(graph_data, file = "data/output/afectoEmpresarios.rds")


# Todas las de afecto sindicalistas ----------------------------------------------------
## Creando variables para porcentaje aprobación -----
# valores de columnas a cambiar 
cols_values <- list(
  EMO_11 = c("5", "6. Positiva"),
  PAF_11 = c("5", "6. Honestos"),
  PAF_14 = c("5", "6. Solidarios"),
  PAF_03 = c("4", "5. De acuerdo"),
  PAF_04 = c("4", "5. De acuerdo"),
  PAF_08 = c("Smiling", "Very happy")
)

# loop 
for (col in names(cols_values)) {
  pol_data <- recode_100(pol_data, col, cols_values[[col]])
}

#  list of column names and labels to use
cols_labels <- list(
  EMO_11_r = "Opinión positiva",
  PAF_11_r = "Los considera honestos",
  PAF_14_r = "Los considera solidarios",
  PAF_03_r = "Confia que pagan salarios justos",
  PAF_04_r = "Confia que cumplen normas de tránsito",
  PAF_08_r = "Estaría feliz de que sus hijos sean amigos"
)

# Loop over the list and apply the summary_label function to each element
graph_data <- NULL # Initialize an empty data frame
for (col in names(cols_labels)) {
  graph_data <- rbind(graph_data, summary_label(col, cols_labels[[col]]))
}

# Factorize the Grupo2 column and set the levels
graph_data$Grupo2 <- factor(graph_data$Grupo2, levels = c(
  "Sector privado",
  "Empresarios informales",
  "Estudiantes",
  "Líderes sociales",
  "Otros ciudadanos",
  "Total"
))

# save afecto sindicalistas -------------------------------------------------
saveRDS(graph_data, file = "data/output/afectoSindicalistas.rds")


# todas las de afecto estudiantes ----------------------------------------------------
## Creando variables para porcentaje aprobación -----
# valores de columnas a cambiar 
cols_values <- list(
  EMO_09 = c("5", "6. Positiva"),
  PAF_12 = c("5", "6. Honestos"),
  PAF_15 = c("5", "6. Solidarios"),
  PAF_05 = c("4", "5. De acuerdo"),
  PAF_06 = c("4", "5. De acuerdo"),
  PAF_09 = c("Smiling", "Very happy")
)

# loop 
for (col in names(cols_values)) {
  pol_data <- recode_100(pol_data, col, cols_values[[col]])
}

#  list of column names and labels to use
cols_labels <- list(
  EMO_09_r = "Opinión positiva",
  PAF_12_r = "Los considera honestos",
  PAF_15_r = "Los considera solidarios",
  PAF_05_r = "Confia que pagan salarios justos",
  PAF_06_r = "Confia que cumplen normas de tránsito",
  PAF_09_r = "Estaría feliz de que sus hijos sean amigos"
)

# Loop over the list and apply the summary_label function to each element
graph_data <- NULL # Initialize an empty data frame
for (col in names(cols_labels)) {
  graph_data <- rbind(graph_data, summary_label(col, cols_labels[[col]]))
}

# Factorize the Grupo2 column and set the levels
graph_data$Grupo2 <- factor(graph_data$Grupo2, levels = c(
  "Sector privado",
  "Empresarios informales",
  "Estudiantes",
  "Líderes sociales",
  "Otros ciudadanos",
  "Total"
))

# save afecto sindicalistas -------------------------------------------------
saveRDS(graph_data, file = "data/output/afectoEstudiantes.rds")
