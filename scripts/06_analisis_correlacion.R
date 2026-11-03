
# ============================================
# ANALISIS DE CORRELACION - ENESEM 2019
# ============================================

# 1. CARGAR LA BASE PROCESADA

base <- readRDS(
  "datos/procesados/base_estudio.rds"
)

# 2. SELECCIONAR OBSERVACIONES VALIDAS

base_corr <- subset(
  base,
  is.finite(horas_por_trabajador) &
    is.finite(productividad_por_trabajador) &
    !is.na(sector)
)

cat(
  "Empresas analizadas:",
  nrow(base_corr), "\n"
)


# 3. CORRELACION GENERAL

correlacion_general <- data.frame(
  Analisis = "General",
  Empresas = nrow(base_corr),
  
  Pearson = cor(
    base_corr$horas_por_trabajador,
    base_corr$productividad_por_trabajador,
    method = "pearson"
  ),
  
  Spearman = cor(
    base_corr$horas_por_trabajador,
    base_corr$productividad_por_trabajador,
    method = "spearman"
  )
)

cat("\nCORRELACION GENERAL:\n")

print(
  correlacion_general,
  row.names = FALSE
)


# 4. CORRELACION POR SECTOR ECONOMICO

resultados <- lapply(
  split(base_corr, base_corr$sector),
  function(d) {
    
    data.frame(
      Sector = d$sector[1],
      Empresas = nrow(d),
      
      Pearson = cor(
        d$horas_por_trabajador,
        d$productividad_por_trabajador,
        method = "pearson"
      ),
      
      Spearman = cor(
        d$horas_por_trabajador,
        d$productividad_por_trabajador,
        method = "spearman"
      )
    )
  }
)

correlacion_sectorial <- do.call(
  rbind,
  resultados
)

rownames(correlacion_sectorial) <- NULL

cat("\nCORRELACION POR SECTOR:\n")

print(
  correlacion_sectorial,
  row.names = FALSE
)


# 5. REVISION DE VALORES EXTREMOS

# Percentiles 1 y 99 de ambas variables

limites_horas <- quantile(
  base_corr$horas_por_trabajador,
  probs = c(0.01, 0.99)
)

limites_productividad <- quantile(
  base_corr$productividad_por_trabajador,
  probs = c(0.01, 0.99)
)

# Crear una muestra de sensibilidad
# No modifica ni elimina datos de la base principal

base_central <- subset(
  base_corr,
  
  horas_por_trabajador >= limites_horas[1] &
    horas_por_trabajador <= limites_horas[2] &
    
    productividad_por_trabajador >=
    limites_productividad[1] &
    
    productividad_por_trabajador <=
    limites_productividad[2]
)

correlacion_sensibilidad <- data.frame(
  Analisis = "Percentiles 1-99",
  Empresas = nrow(base_central),
  
  Pearson = cor(
    base_central$horas_por_trabajador,
    base_central$productividad_por_trabajador,
    method = "pearson"
  ),
  
  Spearman = cor(
    base_central$horas_por_trabajador,
    base_central$productividad_por_trabajador,
    method = "spearman"
  )
)

cat("\nCORRELACION SIN VALORES EXTREMOS:\n")

print(
  correlacion_sensibilidad,
  row.names = FALSE
)


# 6. GUARDAR LOS RESULTADOS EN RDS

saveRDS(
  correlacion_general,
  "datos/procesados/correlacion_general_2019.rds"
)

saveRDS(
  correlacion_sectorial,
  "datos/procesados/correlacion_sectorial_2019.rds"
)

saveRDS(
  correlacion_sensibilidad,
  "datos/procesados/correlacion_sensibilidad_2019.rds"
)


# 7. GRAFICO DE DISPERSION

plot(
  base_central$horas_por_trabajador,
  base_central$productividad_por_trabajador,
  main = "Horas de trabajo y productividad - Ecuador 2019",
  xlab = "Horas anuales por trabajador",
  ylab = "Valor agregado por trabajador (USD)",
  pch = 16,
  cex = 0.4
)

# Linea de tendencia descriptiva
abline(
  lm(
    productividad_por_trabajador ~ horas_por_trabajador,
    data = base_central
  ),
  lwd = 2
)
