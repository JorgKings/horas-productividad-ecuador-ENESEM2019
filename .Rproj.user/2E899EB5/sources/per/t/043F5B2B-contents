
# ============================================
# CORRELACION PONDERADA POR SECTOR
# ENESEM 2019
# ============================================

# 1. Cargar la base
base <- readRDS(
  "datos/procesados/base_estudio.rds"
)

# 2. Seleccionar observaciones validas
d <- subset(
  base,
  !is.na(sector) &
    is.finite(horas_por_trabajador) &
    is.finite(productividad_por_trabajador) &
    is.finite(factor_expansion) &
    factor_expansion > 0
)

# 3. Calcular correlaciones por sector
resultados <- lapply(
  split(d, d$sector),
  function(x) {
    
    correlacion_ponderada <- cov.wt(
      cbind(
        horas = x$horas_por_trabajador,
        productividad = x$productividad_por_trabajador
      ),
      wt = x$factor_expansion,
      cor = TRUE
    )
    
    data.frame(
      Sector = x$sector[1],
      Empresas = nrow(x),
      
      Pearson_ponderada =
        correlacion_ponderada$cor[1, 2],
      
      Spearman_no_ponderada = cor(
        x$horas_por_trabajador,
        x$productividad_por_trabajador,
        method = "spearman"
      )
    )
  }
)

# 4. Unir los resultados
tabla <- do.call(rbind, resultados)

rownames(tabla) <- NULL

# 5. Redondear
tabla$Pearson_ponderada <- round(
  tabla$Pearson_ponderada, 4
)

tabla$Spearman_no_ponderada <- round(
  tabla$Spearman_no_ponderada, 4
)

# 6. Mostrar resultados
print(tabla, row.names = FALSE)

# 7. Guardar en RDS
saveRDS(
  tabla,
  "datos/procesados/correlacion_sectorial_ponderada_2019.rds"
)
