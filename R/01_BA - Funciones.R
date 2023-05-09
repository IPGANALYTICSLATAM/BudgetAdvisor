#----------------------------------------------------------#
# FUNCIONES A EJERCUTAR PARA PODER CORRER
# EL MODELO BUDGET ADVISOR
# ESTE SCRIPT DEBE EJECUTARSE PREVIO A "01_Budget Advisor"
# HECHO POR: DANIEL CAMACHO (JR ANALITICS)
#----------------------------------------------------------#


# 1. FUNCION PARA ESTABLECER PREREQUISITOS PARA EL MODELO

  Setup <- function(df_Off, df_On, df_tr, W_sov, W_buz, W_soi, yr){


    # Activar paquetes
    library(devtools)
    library(MASS)
    library(reshape)
    library(reshape2)
    library(dplyr)
    library(tidyr)
    library(writexl)
    library(data.table)
    library(imputeTS)
    library(ggplot2)


    # Combinar Medios Off y On para tener Inversion total
    df_Off = subset(df_Off, select = c(Year, Month, Brand, Investment_Off, GRPS))
    df_On  = subset(df_On,  select = c(Year, Month, Brand, Investment_On))


    # Convertir la columna "BRAND" a mayúsculas
    df_Off$Brand <- toupper(df_Off$Brand)
    df_On$Brand  <- toupper(df_On$Brand)


    # Se agrupa la base Off a mes (esta en semanas)
    df_Off <- na_replace(df_Off, 0)
    df_Off <- data.table(df_Off)
    df_Off <- df_Off[, .(Investment_Off = sum(Investment_Off), GRPS = sum(GRPS)), by = .(Year, Month, Brand)][order(Year, Month, Brand)]

    # Merge para sumar inversiones
    df_OnOff <- merge(x = df_Off , y = df_On, by = c("Year", "Month", "Brand"), all = TRUE)
    df_OnOff <- na_replace(df_OnOff, 0)
    df_OnOff$Investment <- df_OnOff$Investment_Off + df_OnOff$Investment_On


    # Ordenar columnas
    df_OnOff <- df_OnOff[,c("Year","Month","Brand","Investment","GRPS")]



# Ordenar base de trends y generar SOV

    # Borrar variable semana de trends
    df_tr <- df_tr[ , !(names(df_tr) %in% c("Week","X"))]


    # Pivotear y agregar a nivel de mes
    df_tr <- df_tr %>% pivot_longer(!c("Year", "Month"), names_to = "Brand", values_to = "Trend")
    df_tr[is.na(df_tr)] = 0
    df_tr <- data.table(df_tr)
    df_tr <- df_tr[, .(Trend = round(sum(Trend), 0)), by = .(Year, Month, Brand)]

    # Merge
    df_join  <- merge(x = df_OnOff , y = df_tr, by = c('Year', 'Month','Brand'), all=TRUE)

    # Varios missings en trends - reemplazar missings por 0
    colSums(is.na(df_join))
    df_join[is.na(df_join)] = 0

    # Crear SOV, SOI y trend
    df_join <- df_join[, .(Brand,
                           Investment,
                           SOI   = round(100*Investment /sum(Investment) , 1),
                           SOV   = round(100*GRPS       /sum(GRPS)       , 1),
                           Trend = round(100*Trend      /sum(Trend)      , 1)),
                       by=.(Year, Month)]


    # Generar Media Strategy Index
    df_join$Index <- (df_join$SOV * W_sov) + (df_join$Trend * W_buz) + (df_join$SOI * W_soi)
    df_jn <<- df_join
    # cor.test(df_join$Investment, df_join$Index)


    # Correlaciones entre INDEX e INVERSION de cada marca
    len    <- length(unique(df_join[["Brand"]]))
    cors   <- numeric(len)
    brands <- character(len)
    obs    <- numeric(len)
    n=1


    # Solo se generan correlaciones para marcas con informacion para 8 o mas meses
    for(i in unique(df_join[["Brand"]])){
      df_br <- df_join[Brand == i]
      if (nrow(df_br) > 7){
        cor <- round(cor.test(df_br$Investment, df_br$Index)$estimate, 2)
      }else next
      cors[n]   <- cor
      brands[n] <- i
      obs[n]    <- nrow(df_br)
      n=n+1
    }
    df_cor <-  data.table(brands, obs, cors)
    df_cor <-  df_cor[cors != 0]
    df_cor <<- df_cor[cors != 0]
    cat("\n>>> TABLA DE CORRELACIONES INDEX VS INVESTMENT\n")
    print(df_cor)


    # Seleccion de marcas en consola
    cat("\n >>> Con base en las correlaciones, escriba las marcas elegidas para el analisis en la consola (abajo), \
 >>> Escribalos en mayusculas \
 >>> Ejemplo: CHEETOS, CHIPS " )

    brs <<- readline(">>> MARCAS ELEGIDAS :")
  }

# 2. MODELACION Y SELECCION DE INDEX DESEADO

  Modelacion <- function(){

    # Crear uan lista con los nombres de las marcas seleccionadas
    vector <- character(0)
    for (i in unique(df_jn[["Brand"]])){
      test <- grepl(i, brs, fixed=TRUE)
      if (test == TRUE){
        vector <- append(vector, i)
      }
      else next
    }

    # Se conservan las marcas elegidas en la base de datos
    df_model <<- df_jn[Brand %in% vector]

    # Modelo
    model <<- lm(sqrt(Investment) ~ Index, df_model)

    # Tabla para elegir index para proyeccion
    tab <-df_model[Year == yr,
                     .(Investment = sum(Investment),
                       SOV        = round(mean(SOV),1),
                       SOI        = round(mean(SOI),1),
                       Trend      = round(mean(Trend),1),
                       Index      = round(mean(Index),1)),
                   by = .(Brand)][order(-Index)]

    tab <- tab[, Optimal := fcase(SOV > SOI, "True", Trend  > SOI, "True", default = "False" )]

    # Grafica de regresion
    p <- ggplot(df_model, aes(x=sqrt(Investment), y=Index))+
          geom_point()+ geom_smooth(method="lm", col="black")+
          ggtitle("Linear Regression Plot")
    print(p)

    cat("\n >>> RESULTADOS MODELO DE REGRESION: \n ")
    print(summary(model))


    cat("\n >>> TABLA DESCRIPTIVA, AÑO :", yr, "\n\n")
    print(tab)
    writexl::write_xlsx(tab, "Tab.xlsx")


    cat("\n >>> ANALIZANDO LA TABlA ANTERIOR, ELIGA UN INDEX ENTRE 0 Y 100 \n\n")
    index <<- readline(">>> INDEX ELEGIDO :")
  }

# 3. PREDICCIONES

  Prediccion<- function(){

    set.seed(1)                                           # Semilla para reproducir resultados
    index   <- as.numeric(index)                          # Index a numerico
    delta   <- 1 - mean(m_clave$Total)                    # Cuanto le falta al indicador para tener media 1
    m_clave$ind_mc <- m_clave$Total + delta               # Se suma delta para que indicador tenga media 1
    Index <- m_clave$ind_mc*index                         # Se crea vector con media = index que sigue meses clave


    Index   <- data.frame(Index)
    df_pred <- predict(model, newdata = Index, interval = "confidence")


    df_pred <- data.frame(df_pred)
    df_pred$fit <- df_pred$fit  * df_pred$fit
    df_pred$lwr <- df_pred$lwr  * df_pred$lwr
    df_pred$upr <- df_pred$upr  * df_pred$upr
    print(df_pred)

    print(paste(">>>TOTAL inversión recomendada: $", round(sum(df_pred$fit),0)))
    print(paste(">>>TOTAL inversión recomendada (min): $", round(sum(df_pred$lwr),0)))
    print(paste(">>>TOTAL inversión recomendada (max): $", round(sum(df_pred$upr),0)))

    # writexl::write_xlsx(predict, "PredictTable.xlsx")
    p <<-ggplot(df_pred, aes(x = 1:12, y = fit)) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymax = upr, ymin = lwr)) +
      ggtitle("Investment next 12 months \nData saved in excel file \"Predictions\"")
    print(p)

    df_save <- list(data_prediction=df_pred, data_model=df_model, data=df_jn )
    df_save <<- lapply(df_save, function(x) cbind(" "=rownames(x), x))
    xl <- writexl::write_xlsx(df_save, "Predictions.xlsx")


    cat("\n>>> ARRIBA SE MUESTRAN LOS VALORES PREDICHOS SEGUN SU INDEX ESTABLECIDO:")
    cat("\n>>> LA DATA FUE GUARDADA EN EL EXCEL Predictions.xlsx")
  }

# 4. BUDGET ADVISOR
  BudgetAdvisor <- function(){
    Modelacion()
    Prediccion()
  }

