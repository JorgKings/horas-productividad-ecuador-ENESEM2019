
# ============================================
# BRECHAS DE PRODUCTIVIDAD - ENESEM 2019
# ============================================

# 1. Cargar los diagnosticos
sectorial <- readRDS(
  "datos/procesados/diagnostico_sectorial_2019.rds"
)

cruce <- readRDS(
  "datos/procesados/diagnostico_provincia_sector_2019.rds"
)

# 2. Conservar grupos con al menos 10 empresas
# Es un criterio exploratorio, no oficial
cruce <- subset(cruce, !Revisar_muestra)

# 3. Incorporar la referencia sectorial nacional
comparacion <- merge(
  cruce,
  sectorial[, c(
    "Sector",
    "Horas_por_trabajador",
    "Productividad_USD"
  )],
  by = "Sector",
  suffixes = c("_provincia", "_sector")
)

# 4. Calcular brecha de productividad (%)
comparacion$Brecha_productividad_pct <- round(
  100 * (
    comparacion$Productividad_USD_provincia /
      comparacion$Productividad_USD_sector - 1
  ),
  2
)

# 5. Calcular diferencia de horas anuales
comparacion$Diferencia_horas <- round(
  comparacion$Horas_por_trabajador_provincia -
    comparacion$Horas_por_trabajador_sector,
  1
)

# 6. Mostrar grupos con mayor brecha positiva
cat("\nMAYORES BRECHAS POSITIVAS:\n")

positivas <- subset(
  comparacion,
  Brecha_productividad_pct > 0
)

positivas <- positivas[
  order(-positivas$Brecha_productividad_pct),
]

print(
  head(
    positivas[, c(
      "Provincia",
      "Sector",
      "Empresas_muestra",
      "Brecha_productividad_pct",
      "Diferencia_horas"
    )],
    10
  ),
  row.names = FALSE
)

# 7. Mostrar grupos con menor productividad relativa
cat("\nMAYORES BRECHAS NEGATIVAS:\n")

negativas <- comparacion[
  order(comparacion$Brecha_productividad_pct),
]

print(
  head(
    negativas[, c(
      "Provincia",
      "Sector",
      "Empresas_muestra",
      "Brecha_productividad_pct",
      "Diferencia_horas"
    )],
    10
  ),
  row.names = FALSE
)

# 8. Guardar en formato RDS
saveRDS(
  comparacion,
  "datos/procesados/brechas_productividad_2019.rds"
)
