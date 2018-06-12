#library
library(readxl)
#datatable
viajes_id <- read_excel("viajes_definitivos.xlsx", sheet = 2, col_names = T)
mtxFix = function(ni_dicc, nf_dicc, nd, pmDOr, pmDNu, pmOOr, pmONu){
  library(readxl)
  library(dplyr)
  #########DestinationProcessCE
  #SubsetMtx2Change
  dicc_id <- read_excel("viajes_definitivos.xlsx", sheet = 4, col_names = T)
  selmtx <- dicc_id[c(ni_dicc:nf_dicc), 1]
  names(selmtx) <- c("idnododestino")
  viajes_idDest <- inner_join(viajes_id, selmtx)
  #ReplaceDestinyNode
  viajes_idDest$idnododestino2 <- nd
  #SettingStopSharingParameters
  viajes_idDest$ViajeParOr <- viajes_idDest$Viajes*pmDOr
  viajes_idDest$ViajeParNu <- viajes_idDest$Viajes*pmDNu
  #SubsetMtxWithOriginalStops
  viajes_idDestOrSt <- viajes_idDest[,c(1,2,5)]
  names(viajes_idDestOrSt)[3] <- c("Viajes")
  #SubsetMtxWithNewStops
  viajes_idDestNuSt <- viajes_idDest[,c(1,4,6)]
  names(viajes_idDestNuSt)[c(2,3)] <- c("idnododestino", "Viajes")
  rm(viajes_idDest)
  #UnchangeableMtx
  viajes_idOut <- anti_join(viajes_id, selmtx)
  #FinalDestinationMatrix
  viajes_idDestMod <- bind_rows(viajes_idOut, viajes_idDestOrSt, viajes_idDestNuSt)
  rm(viajes_idOut, viajes_idDestOrSt, viajes_idDestNuSt)
  #########OriginProcessCE
  #SubsetMtx2Change
  names(selmtx) <- c("idnodoorigen")
  viajes_idOrig <- inner_join(viajes_idDestMod, selmtx)
  #ReplaceOriginNode
  viajes_idOrig$idnodoorigen2 <- nd
  #SettingStopSharingParameters
  viajes_idOrig$ViajeParOr <- viajes_idOrig$Viajes*pmOOr
  viajes_idOrig$ViajeParNu <- viajes_idOrig$Viajes*pmONu
  #SubsetMtxWithOriginalStops
  viajes_idOrigOrSt <- viajes_idOrig[,c(1,2,5)]
  names(viajes_idOrigOrSt)[3] <- c("Viajes")
  #SubsetMtxWithNewStops
  viajes_idOrigNuSt <- viajes_idOrig[,c(4,2,6)]
  names(viajes_idOrigNuSt)[c(1,3)] <- c("idnodoorigen", "Viajes")
  #UnchangeableMtx
  viajes_idOut <- anti_join(viajes_idDestMod, selmtx)
  rm(viajes_idDestMod)
  #FinalMatrix
  viajes_idMod <- bind_rows(viajes_idOut, viajes_idOrigOrSt, viajes_idOrigNuSt)
  rm(viajes_idOut, viajes_idOrigOrSt, viajes_idOrigNuSt)
  viajes_idMod$idOD <- paste(viajes_idMod$idnodoorigen, viajes_idMod$idnododestino)
  #RemoveIllogicalTrips
  viajes_idMod <- viajes_idMod %>% filter(idOD != paste(nd, nd))
  viajes_idMod <- viajes_idMod[,c(1:3)]
  #FinalResult
  viajes_idMod %>% group_by(idnodoorigen, idnododestino) %>% summarise(Viajes = sum(Viajes))
  viajes_idMod
}
mtxFix(1,23, 774, 0.0, 1.0, 1.0, 0.0)
100 - sum(mtxFix(1,23, 774, 0.0, 1.0, 1.0, 0.0)$Viajes, na.rm = T)/sum(viajes_id$Viajes, 
                                                                         na.rm = T)*100
FixMtxtb <- as.data.frame(mtxFix(1,23, 774, 0.0, 1.0, 1.0, 0.0))
sum(FixMtxtb$Viajes, na.rm = T)

write.csv(x = mtxFix(1,23, 774, 0.0, 1.0, 1.0, 0.0), file = "mtxfixAtraccCE774.csv", 
          row.names = F, quote = F)
