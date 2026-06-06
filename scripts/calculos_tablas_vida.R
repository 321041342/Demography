# ==========================================================
# Proyecto: Tablas de vida de Nuevo León
# Archivo: scripts/calculos_tablas_vida.R
# Autor: 
#- "Alcibar Perez Atzabel"
#- "Cisneros Calavazo Bitia Eleonai"


rm(list = ls())


paquetes <- c("data.table", "dplyr", "ggplot2", "knitr", "kableExtra")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if(length(instalar) > 0) install.packages(instalar)

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)


if (!dir.exists("data"))   dir.create("data",   recursive = TRUE)
if (!dir.exists("output")) dir.create("output", recursive = TRUE)
if (!dir.exists("images")) dir.create("images", recursive = TRUE)


datos <- fread("data/base_nuevo_leon.csv")


cols_calc <- c("mx", "qx", "lx", "dx", "Lx", "Tx", "ex", "APV_85")
datos[, (intersect(cols_calc, names(datos))) := NULL]


datos <- datos %>%
  arrange(anio, sexo, edad_inicio)

# Fórmulas de tabla de vida 
# mx = D_x / P_x
# qx = n*mx / (1 + (n-ax)*mx)
# l0 = 100000
# dx = lx*qx
# Lx = n*lx - (n-ax)*dx
# Tx = suma acumulada de Lx desde la edad x hasta la última edad
# ex = Tx / lx

calcular_tabla_vida <- function(df) {
  df <- df %>% arrange(edad_inicio)

  df <- df %>%
    mutate(
      ax = ifelse(is.na(ax), n/2, ax),
      mx = defunciones / poblacion,
      qx = ifelse(edad_inicio >= 85, 1, (n * mx) / (1 + (n - ax) * mx))
    )

  df$lx <- NA_real_
  df$dx <- NA_real_
  df$Lx <- NA_real_
  df$Tx <- NA_real_
  df$ex <- NA_real_

  df$lx[1] <- 100000

  for(i in seq_len(nrow(df))) {
    df$dx[i] <- df$lx[i] * df$qx[i]

    if(df$edad_inicio[i] >= 85) {
      df$Lx[i] <- df$lx[i] / df$mx[i]
    } else {
      df$Lx[i] <- df$n[i] * df$lx[i] - (df$n[i] - df$ax[i]) * df$dx[i]
    }

    if(i < nrow(df)) {
      df$lx[i + 1] <- df$lx[i] - df$dx[i]
    }
  }

  df$Tx <- rev(cumsum(rev(df$Lx)))
  df$ex <- df$Tx / df$lx
  df$APV_85 <- ifelse(df$edad_inicio == 0, 85 - df$ex, NA)

  return(df)
}

tabla_vida <- datos %>%
  group_by(anio, sexo) %>%
  group_modify(~ calcular_tabla_vida(.x)) %>%
  ungroup()


fwrite(tabla_vida, "output/tabla_vida_nuevo_leon_calculada.csv")

esperanza_vida <- tabla_vida %>%
  filter(edad_inicio == 0) %>%
  select(anio, sexo, esperanza_vida_al_nacer = ex, APV_85)

fwrite(esperanza_vida, "output/esperanza_vida_nuevo_leon_calculada.csv")

# ── Gráficas ────────────────────────────────────────────────────────────────── 
g1 <- ggplot(esperanza_vida, aes(x = factor(anio), y = esperanza_vida_al_nacer, group = sexo)) +
  geom_line(aes(linetype = sexo)) +
  geom_point(aes(shape = sexo), size = 2.5) +
  labs(
    title = "Esperanza de vida al nacer por sexo en Nuevo León",
    x = "Año",
    y = "Esperanza de vida al nacer"
  ) +
  theme_minimal()

ggsave("images/esperanza_vida_nuevo_leon.png", g1, width = 8, height = 5, dpi = 300)

g2 <- ggplot(tabla_vida, aes(x = edad_inicio, y = qx, group = interaction(anio, sexo))) +
  geom_line(aes(linetype = sexo)) +
  facet_wrap(~ anio) +
  scale_y_log10() +
  labs(
    title = "Probabilidad de muerte qx por edad, sexo y año",
    x = "Edad inicial del grupo",
    y = "qx"
  ) +
  theme_minimal()

ggsave("images/qx_nuevo_leon.png", g2, width = 9, height = 5, dpi = 300)

g3 <- ggplot(tabla_vida, aes(x = edad_inicio, y = lx, group = interaction(anio, sexo))) +
  geom_line(aes(linetype = sexo)) +
  facet_wrap(~ anio) +
  labs(
    title = "Sobrevivientes lx por edad, sexo y año",
    x = "Edad inicial del grupo",
    y = "lx"
  ) +
  theme_minimal()

ggsave("images/lx_nuevo_leon.png", g3, width = 9, height = 5, dpi = 300)

g4 <- tabla_vida %>%
  group_by(anio, sexo) %>%
  summarise(defunciones = sum(defunciones), .groups = "drop") %>%
  ggplot(aes(x = factor(anio), y = defunciones, group = sexo)) +
  geom_line(aes(linetype = sexo)) +
  geom_point(aes(shape = sexo), size = 2.5) +
  labs(
    title = "Defunciones estimadas en la base de trabajo",
    x = "Año",
    y = "Defunciones"
  ) +
  theme_minimal()

ggsave("images/defunciones_estimadas.png", g4, width = 8, height = 5, dpi = 300)


cat("\nProceso terminado. Archivos generados:\n")
cat("- output/tabla_vida_nuevo_leon_calculada.csv\n")
cat("- output/esperanza_vida_nuevo_leon_calculada.csv\n")
cat("- images/esperanza_vida_nuevo_leon.png\n")
cat("- images/qx_nuevo_leon.png\n")
cat("- images/lx_nuevo_leon.png\n")
cat("- images/defunciones_estimadas.png\n")
