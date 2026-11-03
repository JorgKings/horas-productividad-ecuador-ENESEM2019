
# ============================================
# DIAGNOSTICO SECTORIAL - ENESEM 2019
# ============================================

# 1. Cargar la base procesada
base <- readRDS(
  "datos/procesados/base_estudio.rds"
)

# 2. Seleccionar registros validos
# Conservamos el valor agregado negativo o cero
base_valida <- subset(
  base,
  !is.na(sector) &
    is.finite(horas_anuales) &
    is.finite(valor_agregado) &
    is.finite(personal_ocupado) &
    is.finite(factor_expansion) &
    horas_anuales > 0 &
    personal_ocupado > 0 &
    factor_expansion > 0
)

# 3. Calcular indicadores por sector
resultado <- lapply(
  split(base_valida, base_valida$sector),
  function(d) {
    
    personal_total <- sum(
      d$personal_ocupado * d$factor_expansion
    )
    
    data.frame(
      Sector = d$sector[1],
      Empresas_muestra = nrow(d),
      
      Horas_por_trabajador = sum(
        d$horas_anuales * d$factor_expansion
      ) / personal_total,
      
      Productividad_USD = sum(
        d$valor_agregado * d$factor_expansion
      ) / personal_total
    )
  }
)

# 4. Unir resultados
tabla_sectorial <- do.call(rbind, resultado)
rownames(tabla_sectorial) <- NULL

# 5. Redondear
tabla_sectorial$Horas_por_trabajador <- round(
  tabla_sectorial$Horas_por_trabajador, 1
)

tabla_sectorial$Productividad_USD <- round(
  tabla_sectorial$Productividad_USD, 2
)

# 6. Mostrar tabla
print(tabla_sectorial)

# 7. Guardar resultados
saveRDS(
  tabla_sectorial,
  "datos/procesados/diagnostico_sectorial_2019.rds"
)
