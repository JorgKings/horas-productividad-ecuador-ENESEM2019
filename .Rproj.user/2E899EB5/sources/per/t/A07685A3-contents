
# ============================================
# DIAGNOSTICO TERRITORIAL - ENESEM 2019
# ============================================

# 1. Cargar la base procesada
base <- readRDS(
  "datos/procesados/base_estudio.rds"
)

# 2. Seleccionar registros validos
base_valida <- subset(
  base,
  !is.na(provincia) &
    is.finite(horas_anuales) &
    is.finite(valor_agregado) &
    is.finite(personal_ocupado) &
    is.finite(factor_expansion) &
    horas_anuales > 0 &
    personal_ocupado > 0 &
    factor_expansion > 0
)

# 3. Calcular indicadores por provincia
resultado <- lapply(
  split(base_valida, base_valida$provincia),
  function(d) {
    
    personal_total <- sum(
      d$personal_ocupado * d$factor_expansion
    )
    
    data.frame(
      Provincia = d$provincia[1],
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
tabla_territorial <- do.call(rbind, resultado)
rownames(tabla_territorial) <- NULL

# 5. Redondear resultados
tabla_territorial$Horas_por_trabajador <- round(
  tabla_territorial$Horas_por_trabajador, 1
)

tabla_territorial$Productividad_USD <- round(
  tabla_territorial$Productividad_USD, 2
)

# 6. Mostrar la tabla
print(tabla_territorial)

# 7. Guardar los resultados
write.csv2(
  tabla_territorial,
  "datos/procesados/diagnostico_territorial_2019.csv",
  row.names = FALSE
)


# ============================================
# 8. IDENTIFICAR PROVINCIAS
# ============================================

nombres_provincias <- c(
  "01" = "Azuay",
  "02" = "Bolívar",
  "03" = "Cañar",
  "04" = "Carchi",
  "05" = "Cotopaxi",
  "06" = "Chimborazo",
  "07" = "El Oro",
  "08" = "Esmeraldas",
  "09" = "Guayas",
  "10" = "Imbabura",
  "11" = "Loja",
  "12" = "Los Ríos",
  "13" = "Manabí",
  "14" = "Morona Santiago",
  "15" = "Napo",
  "16" = "Pastaza",
  "17" = "Pichincha",
  "18" = "Tungurahua",
  "19" = "Zamora Chinchipe",
  "20" = "Galápagos",
  "21" = "Sucumbíos",
  "22" = "Orellana",
  "23" = "Santo Domingo de los Tsáchilas",
  "24" = "Santa Elena"
)

# Agregar nombres a la tabla
tabla_territorial$Nombre_provincia <-
  unname(nombres_provincias[
    as.character(tabla_territorial$Provincia)
  ])

# Comprobar que todas las provincias tengan nombre
stopifnot(
  !anyNA(tabla_territorial$Nombre_provincia)
)

# Identificar provincias con menos de 10 registros
tabla_territorial$Revisar_muestra <-
  tabla_territorial$Empresas_muestra < 10

# Ordenar columnas
tabla_territorial <- tabla_territorial[, c(
  "Provincia",
  "Nombre_provincia",
  "Empresas_muestra",
  "Horas_por_trabajador",
  "Productividad_USD",
  "Revisar_muestra"
)]

# Mostrar la tabla completa
print(tabla_territorial, row.names = FALSE)

# Guardar la tabla actualizada
saveRDS(
  tabla_territorial,
  "datos/procesados/diagnostico_territorial_2019.rds"
)
