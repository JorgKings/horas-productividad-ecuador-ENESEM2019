
# ============================================
# RESUMEN DEL TRATAMIENTO - ENESEM 2019
# ============================================

# Cargar base procesada
base <- readRDS(
  "datos/procesados/base_estudio.rds"
)

# Identificar registros utilizables
validas <- with(
  base,
  is.finite(horas_por_trabajador) &
    is.finite(productividad_por_trabajador) &
    !is.na(sector)
)

# Construir tabla de tratamiento
tabla_tratamiento <- data.frame(
  Indicador = c(
    "Empresas originales",
    "Sin indicador de horas valido",
    "Sin indicador de productividad valido",
    "Productividad no positiva",
    "Horas anuales por trabajador menores a 1000",
    "Empresas validas para correlacion",
    "Empresas validas con valor agregado positivo"
  ),
  
  Observaciones = c(
    nrow(base),
    sum(!is.finite(base$horas_por_trabajador)),
    sum(!is.finite(base$productividad_por_trabajador)),
    sum(base$productividad_por_trabajador <= 0,
        na.rm = TRUE),
    sum(base$horas_por_trabajador < 1000,
        na.rm = TRUE),
    sum(validas),
    sum(validas & base$valor_agregado > 0)
  )
)

# Mostrar resumen
print(tabla_tratamiento, row.names = FALSE)

# Guardar en RDS
saveRDS(
  tabla_tratamiento,
  "datos/procesados/resumen_tratamiento_2019.rds"
)
