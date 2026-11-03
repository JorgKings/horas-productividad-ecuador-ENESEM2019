
# ============================================
# VERIFICACION DE CORRELACION - ENESEM 2019
# ============================================

base <- readRDS(
  "datos/procesados/base_estudio.rds"
)

# Conservar observaciones validas
d <- subset(
  base,
  is.finite(horas_por_trabajador) &
    is.finite(productividad_por_trabajador) &
    is.finite(factor_expansion) &
    factor_expansion > 0
)

# Correlacion de Pearson ponderada
resultado <- cov.wt(
  cbind(
    horas = d$horas_por_trabajador,
    productividad = d$productividad_por_trabajador
  ),
  wt = d$factor_expansion,
  cor = TRUE
)

cat(
  "Correlacion Pearson ponderada:",
  resultado$cor[1, 2],
  "\n"
)

cat(
  "Empresas utilizadas:",
  nrow(d),
  "\n"
)

# Revisar mineria
mineria <- subset(d, sector == "Minería")

cat("\nMINERIA - PERCENTILES DE PRODUCTIVIDAD:\n")

print(
  quantile(
    mineria$productividad_por_trabajador,
    c(0, 0.50, 0.95, 0.99, 1)
  )
)

# Comparar correlacion minera con y sin extremos
limites <- quantile(
  mineria$productividad_por_trabajador,
  c(0.01, 0.99)
)

mineria_central <- subset(
  mineria,
  productividad_por_trabajador >= limites[1] &
    productividad_por_trabajador <= limites[2]
)

cat("\nPEARSON MINERIA COMPLETA:\n")

print(cor(
  mineria$horas_por_trabajador,
  mineria$productividad_por_trabajador
))

cat("\nPEARSON MINERIA SIN EXTREMOS:\n")

print(cor(
  mineria_central$horas_por_trabajador,
  mineria_central$productividad_por_trabajador
))
