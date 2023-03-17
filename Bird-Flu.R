# Instalar ggplot2 y reshape2 si aún no están instalados
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}

# Cargar las bibliotecas ggplot2 y reshape2
library(ggplot2)
library(reshape2)

# Parámetros del modelo
n_poblacion <- 1000        # Tamaño total de la población
n_expuestos_iniciales <- 10 # Número inicial de individuos expuestos
n_infectados_iniciales <- 5 # Número inicial de individuos infectados
tasa_infeccion <- 1.2       # Tasa de infección
tasa_incubacion <- 0.5      # Tasa de incubación (transición de expuesto a infectado)
tasa_recuperacion <- 0.25   # Tasa de recuperación (transición de infectado a recuperado)
n_dias <- 100               # Número de días para simular

# Inicializar vectores de estado
susceptibles <- rep(0, n_dias) # Vector para almacenar el número de susceptibles en cada día
expuestos <- rep(0, n_dias)    # Vector para almacenar el número de expuestos en cada día
infectados <- rep(0, n_dias)   # Vector para almacenar el número de infectados en cada día
recuperados <- rep(0, n_dias)  # Vector para almacenar el número de recuperados en cada día

# Condiciones iniciales
susceptibles[1] <- n_poblacion - n_expuestos_iniciales - n_infectados_iniciales # Número inicial de individuos susceptibles
expuestos[1] <- n_expuestos_iniciales # Número inicial de individuos expuestos
infectados[1] <- n_infectados_iniciales # Número inicial de individuos infectados
recuperados[1] <- 0 # Número inicial de individuos recuperados

# Crear un vector para los días
dias <- 1:n_dias

# Simulación del modelo
for (dia in 2:n_dias) {
  # Calcular el número de nuevos expuestos, infectados y recuperados en el día actual
  nuevos_expuestos <- rbinom(1, size = susceptibles[dia - 1], prob = tasa_infeccion * infectados[dia - 1] / n_poblacion)
  nuevos_infectados <- rbinom(1, size = expuestos[dia - 1], prob = tasa_incubacion)
  nuevos_recuperados <- rbinom(1, size = infectados[dia - 1], prob = tasa_recuperacion)
  
  # Actualizar los vectores de estado
  susceptibles[dia] <- susceptibles[dia - 1] - nuevos_expuestos
  expuestos[dia] <- expuestos[dia - 1] + nuevos_expuestos - nuevos_infectados
  infectados[dia] <- infectados[dia - 1] + nuevos_infectados - nuevos_recuperados
  recuperados[dia] <- recuperados[dia - 1] + nuevos_recuperados
}

# Crear un data.frame con los resultados de la simulación
resultados <- data.frame(dias, susceptibles, expuestos, infectados, recuperados)

# Reestructurar el data.frame para su visualización en ggplot2
resultados_melt <- melt(resultados, id.vars = "dias") # Convertir el formato de datos para ggplot2

# Crear el gráfico con ggplot2
grafico <- ggplot(resultados_melt, aes(x = dias, y = value, color = variable)) + # Crear un objeto de gráfico con ggplot2
  geom_line(linewidth = 1) + # Dibujar líneas con un grosor de 1
  labs(title = "Simulación de propagación de la influenza aviar (Modelo SEIR)", # Título del gráfico
       x = "Días", # Etiqueta del eje x
       y = "Número de individuos", # Etiqueta del eje y
       color = "Estado") + # Leyenda
  theme_minimal() # Tema visual minimalista para el gráfico

# Visualizar el gráfico en RStudio
print(grafico)

# Guardar el gráfico en un archivo PNG
ggsave("simulacion_influenza_aviar.png", grafico, width = 10, height = 6, dpi = 300) # Guardar el gráfico en un archivo PNG con dimensiones y resolución especificadas

