
# ============================================
# DIAGNOSTICO PROVINCIA Y SECTOR - ENESEM 2019
# ============================================

# 1. CARGAR LA BASE PROCESADA

base <- readRDS(
  "datos/procesados/base_estudio.rds"
)

# 2. SELECCIONAR REGISTROS VALIDOS

base_valida <- subset(
  base,
  !is.na(provincia) &
    !is.na(sector) &
    is.finite(horas_anuales) &
    is.finite(valor_agregado) &
    is.finite(personal_ocupado) &
    is.finite(factor_expansion) &
    horas_anuales > 0 &
    personal_ocupado > 0 &
    factor_expansion > 0
)

# 3. PREPARAR VARIABLES PONDERADAS

base_valida$horas_pond <-
  base_valida$horas_anuales *
  base_valida$factor_expansion

base_valida$valor_pond <-
  base_valida$valor_agregado *
  base_valida$factor_expansion

base_valida$personal_pond <-
  base_valida$personal_ocupado *
  base_valida$factor_expansion

base_valida$n_empresas <- 1

# 4. AGRUPAR POR PROVINCIA Y SECTOR

tabla_cruzada <- aggregate(
  cbind(
    horas_pond,
    valor_pond,
    personal_pond,
    n_empresas
  ) ~ provincia + sector,
  data = base_valida,
  FUN = sum
)

# 5. CALCULAR INDICADORES ECONOMICOS

tabla_cruzada$Horas_por_trabajador <- round(
  tabla_cruzada$horas_pond /
    tabla_cruzada$personal_pond,
  1
)

tabla_cruzada$Productividad_USD <- round(
  tabla_cruzada$valor_pond /
    tabla_cruzada$personal_pond,
  2
)

# 6. AGREGAR NOMBRES DE PROVINCIAS

provincias <- c(
  "01" = "Azuay",
  "02" = "Bolivar",
  "03" = "Canar",
  "04" = "Carchi",
  "05" = "Cotopaxi",
  "06" = "Chimborazo",
  "07" = "El Oro",
  "08" = "Esmeraldas",
  "09" = "Guayas",
  "10" = "Imbabura",
  "11" = "Loja",
  "12" = "Los Rios",
  "13" = "Manabi",
  "14" = "Morona Santiago",
  "15" = "Napo",
  "16" = "Pastaza",
  "17" = "Pichincha",
  "18" = "Tungurahua",
  "19" = "Zamora Chinchipe",
  "20" = "Galapagos",
  "21" = "Sucumbios",
  "22" = "Orellana",
  "23" = "Santo Domingo",
  "24" = "Santa Elena"
)

tabla_cruzada$Nombre_provincia <- unname(
  provincias[as.character(tabla_cruzada$provincia)]
)

# 7. IDENTIFICAR GRUPOS CON POCAS EMPRESAS

tabla_cruzada$Revisar_muestra <-
  tabla_cruzada$n_empresas < 10

# No publicar indicadores de grupos con menos
# de 10 empresas en este diagnostico preliminar

tabla_cruzada$Horas_por_trabajador[
  tabla_cruzada$Revisar_muestra
] <- NA_real_

tabla_cruzada$Productividad_USD[
  tabla_cruzada$Revisar_muestra
] <- NA_real_

# 8. ORGANIZAR LA TABLA FINAL

tabla_final <- data.frame(
  Provincia = tabla_cruzada$Nombre_provincia,
  Sector = tabla_cruzada$sector,
  Empresas_muestra = tabla_cruzada$n_empresas,
  Horas_por_trabajador =
    tabla_cruzada$Horas_por_trabajador,
  Productividad_USD =
    tabla_cruzada$Productividad_USD,
  Revisar_muestra =
    tabla_cruzada$Revisar_muestra
)

tabla_final <- tabla_final[
  order(tabla_final$Provincia, tabla_final$Sector),
]

rownames(tabla_final) <- NULL

# 9. MOSTRAR RESULTADOS

cat(
  "Total de combinaciones provincia-sector:",
  nrow(tabla_final), "\n"
)

cat(
  "Combinaciones con menos de 10 empresas:",
  sum(tabla_final$Revisar_muestra), "\n"
)

print(
  tabla_final[!tabla_final$Revisar_muestra, ],
  row.names = FALSE
)

# 10. GUARDAR TABLA

saveRDS(
  tabla_final,
  "datos/procesados/diagnostico_provincia_sector_2019.rds"
)