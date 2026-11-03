
# ============================================
# PROYECTO: HORAS DE TRABAJO Y PRODUCTIVIDAD
# ECUADOR - ENESEM 2019
# ============================================

# 1. IMPORTAR LA BASE ORIGINAL

datos <- read.csv2(
  "datos/originales/2019_estructural_empresarial_bdd.csv",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  colClasses = c(
    id_empresa = "character",
    provincia = "character"
  )
)

# Verificar cantidad de empresas y variables
dim(datos)


# 2. SELECCIONAR LAS VARIABLES NECESARIAS

base_estudio <- data.frame(
  id_empresa = datos$id_empresa,
  anio = 2019,
  provincia = datos$provincia,
  region = datos$des_region,
  sector = datos$des_sector,
  actividad = datos$des_letra,
  tamano = datos$des_tamano,
  horas_anuales = datos$hanuales,
  personal_ocupado = datos$totalpeoc,
  valor_agregado = datos$valag,
  activos_fijos = datos$sal31dic,
  remuneraciones = datos$totremun,
  factor_expansion = datos$f_exp
)


# 3. CONSTRUIR LOS INDICADORES ECONOMICOS

# Horas anuales divididas por personal ocupado
base_estudio$horas_por_trabajador <- with(
  base_estudio,
  ifelse(
    personal_ocupado > 0 & horas_anuales > 0,
    horas_anuales / personal_ocupado,
    NA_real_
  )
)

# Valor agregado por trabajador (USD)
base_estudio$productividad_por_trabajador <- with(
  base_estudio,
  ifelse(
    personal_ocupado > 0,
    valor_agregado / personal_ocupado,
    NA_real_
  )
)


# 4. IDENTIFICAR OBSERVACIONES UTILIZABLES

# Para análisis descriptivos y de correlación
base_estudio$muestra_correlacion <- with(
  base_estudio,
  !is.na(horas_por_trabajador) &
    !is.na(productividad_por_trabajador)
)

# Para un posible modelo con logaritmos
base_estudio$muestra_log <- with(
  base_estudio,
  muestra_correlacion & valor_agregado > 0
)


# 5. GUARDAR LA BASE PROCESADA

saveRDS(
  base_estudio,
  "datos/procesados/base_estudio.rds"
)


# 6. COMPROBAR LOS RESULTADOS

cat("Empresas originales:", nrow(datos), "\n")

cat(
  "Empresas para correlacion:",
  sum(base_estudio$muestra_correlacion),
  "\n"
)

cat(
  "Empresas con valor agregado positivo:",
  sum(base_estudio$muestra_log),
  "\n"
)

cat(
  "Provincias:",
  length(unique(base_estudio$provincia)),
  "\n"
)

cat("\nEmpresas por sector:\n")
print(table(base_estudio$sector))

cat("\nTipos de variables principales:\n")
str(
  base_estudio[
    c(
      "horas_por_trabajador",
      "productividad_por_trabajador",
      "factor_expansion"
    )
  ]
)



# 7. TRATAMIENTO Y CONTROL DE CALIDAD

# Horas semanales equivalentes
base_estudio$horas_semanales <- 
  base_estudio$horas_por_trabajador / 52

# Identificar horas anuales inferiores a 1000
base_estudio$revision_horas <- 
  !is.na(base_estudio$horas_por_trabajador) &
  base_estudio$horas_por_trabajador < 1000

# Identificar productividad no positiva
base_estudio$revision_productividad <- 
  !is.na(base_estudio$productividad_por_trabajador) &
  base_estudio$productividad_por_trabajador <= 0

# Guardar la base con los indicadores de revisión
saveRDS(
  base_estudio,
  "datos/procesados/base_estudio.rds"
)

# Comprobar
cat("Empresas totales:", nrow(base_estudio), "\n")
cat("Horas bajas:", sum(base_estudio$revision_horas), "\n")
cat("Productividad no positiva:",
    sum(base_estudio$revision_productividad), "\n")
