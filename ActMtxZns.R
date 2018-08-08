#~input
  ##diccionary
  dicc_ESTRAUS <- read.csv("dicc_ESTRAUS.csv", header = T, sep = ";")
  ##travel database
  viajes_Base <- read.csv("C:/Users/Administrador/Documents/PO/Demanda/asignación/procesos/viajes_etapas/Redistribucion/B1km100per/B1km100perL6/6mtxfixÑuñ.csv", 
                  header = T)
V_ActZ <- function(field1, field2, value1, value2, fcrec){
  #libraries
  library(dplyr)
  ##set dictionary
    names(dicc_ESTRAUS) <- c("idNodo", "ZONA", "MACROZONA")
    #ODNodes
    dicc_ESTRAUS$idnodoorigen <- dicc_ESTRAUS$idNodo
    dicc_ESTRAUS$idnododestino <- dicc_ESTRAUS$idNodo
    #ODZones
    dicc_ESTRAUS$Z_ESTRAUSOr <- dicc_ESTRAUS$ZONA
    dicc_ESTRAUS$Z_ESTRAUSDe <- dicc_ESTRAUS$ZONA
    #ODMacroZones
    dicc_ESTRAUS$MACROAREAOr <- dicc_ESTRAUS$MACROZONA
    dicc_ESTRAUS$MACROAREADe <- dicc_ESTRAUS$MACROZONA
  #ODTravel with Zones & Macrozones incorporated
  viajes_Base <- left_join(viajes_Base, dicc_ESTRAUS[,c(4,6,8)])
  viajes_Base <- left_join(viajes_Base, dicc_ESTRAUS[,c(5,7,9)])
  #Modified DataBase
  #field <- enquo(field)
   attach(viajes_Base)
   viajes_Base_1 <- viajes_Base[which(field1 %in% value1 & field2 %in% value2), ]
   viajes_Base_1$Viajes <- viajes_Base_1$Viajes*fcrec
   viajes_Base_2 <- viajes_Base[which(!(field1 %in% value1 & field2 %in% value2)), ]
   detach(viajes_Base)
   viajes_Base <- bind_rows(viajes_Base_1, viajes_Base_2)
   rm(viajes_Base_1, viajes_Base_2)
  viajes_Base
}

#Buffer Zones filter
Origen <- c("SUR")
Destino <- c("SUR", "CENTRO", "OCCIDENTE", "ORIENTE")

   ##########################
###550662.4*1.0421 = 573845.3###
   ##########################

library(dplyr)
#Travels on Buffer Zones
V_aR <- group_by(.data =  V_ActZ(MACROAREAOr, MACROAREADe, Origen, Destino, 1.0), 
                 MACROAREAOr, MACROAREADe) %>% 
  summarise(Viajes = sum(Viajes))%>% filter(MACROAREAOr == Origen & MACROAREADe %in% Destino) %>% 
  summarise(viajes = sum(Viajes))

#growing factor to apply 
fcrec_apl <- sum(1,sum(viajes_Base$Viajes, na.rm = T)*0.0421/V_aR$viajes)
#result matrix
Res_mtx <- V_ActZ(MACROAREAOr, MACROAREADe, Origen, Destino, fcrec_apl)
sum(Res_mtx$Viajes, na.rm = T)

#write file
write.csv(x = Res_mtx, file = "resultado/MacroZNSur.csv", quote = F, row.names = F)
