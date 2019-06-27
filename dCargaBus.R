#input -----------------------
base <- "[baseviajesDLN201804]"
ipt_dir <- "dicc_abr18"
file <- "BaseDCarga2.csv"
dCarga <- function(base, ipt_dir, file){
  #libraries
  library(RODBC)
  library(ff)
  library(ETLUtils)
  library(readr)
  library(dplyr)
  library(readxl)
  library(tidyr)
  library(tibble)
  #queries
  q_sube <- paste0("SELECT [periodo], [serv_1era] AS serv, [paraderosubida_1era] AS parada, SUM([Demanda]) AS Sube
              FROM [Asig].[dbo].",base,
              "WHERE [tipotransporte_1era] IN ('ZP','BUS')
              GROUP BY [periodo], [serv_1era], [paraderosubida_1era]
              UNION ALL
              SELECT [periodo], [serv_2da] AS serv, [paraderosubida_2da] AS parada, SUM([Demanda]) AS Sube
              FROM [Asig].[dbo].",base,
              "WHERE [tipotransporte_2da] IN ('ZP','BUS')
              GROUP BY [periodo], [serv_2da], [paraderosubida_2da]
              UNION ALL
              SELECT [periodo], [serv_3era] AS serv, [paraderosubida_3era] AS parada, SUM([Demanda]) AS Sube
              FROM [Asig].[dbo].",base,
              "WHERE [tipotransporte_3era] IN ('ZP','BUS')
              GROUP BY [periodo], [serv_3era], [paraderosubida_3era]
              UNION ALL
              SELECT [periodo], [serv_4ta] AS serv, [paraderosubida_4ta] AS parada, SUM([Demanda]) AS Sube
              FROM [Asig].[dbo].",base,
              "WHERE [tipotransporte_4ta] IN ('ZP','BUS')
              GROUP BY [periodo], [serv_4ta], [paraderosubida_4ta];")
q_baja <- paste0("SELECT [periodo], [serv_1era] AS serv, [paraderobajada_1era] AS parada, SUM([Demanda]) AS Baja
            FROM [Asig].[dbo].", base,
            "WHERE [tipotransporte_1era] IN ('ZP','BUS')
            GROUP BY [periodo], [serv_1era], [paraderobajada_1era]
            UNION ALL
            SELECT [periodo], [serv_2da] AS serv, [paraderobajada_2da] AS parada, SUM([Demanda]) AS Baja
            FROM [Asig].[dbo].",base,
            "WHERE [tipotransporte_2da] IN ('ZP','BUS')
            GROUP BY [periodo], [serv_2da], [paraderobajada_2da]
            UNION ALL
            SELECT [periodo], [serv_3era] AS serv, [paraderobajada_3era] AS parada, SUM([Demanda]) AS Baja
            FROM [Asig].[dbo].",base, "WHERE [tipotransporte_3era] IN ('ZP','BUS')
            GROUP BY [periodo], [serv_3era], [paraderobajada_3era]
            UNION ALL
            SELECT [periodo], [serv_4ta] AS serv, [paraderobajada_4ta] AS parada, SUM([Demanda]) AS Baja
            FROM [Asig].[dbo].", base,
            "WHERE [tipotransporte_4ta] IN ('ZP','BUS')
            GROUP BY [periodo], [serv_4ta], [paraderobajada_4ta];")
#ff2DF --------------
sube <- as.data.frame(read.odbc.ffdf(odbcConnect.args = list("Asig", uid = "sa", 
                                         pwd = "Estudios2017.."), VERBOSE = T, query = q_sube))
j <- sapply(sube, is.factor)
sube[j] <- lapply(sube[j], as.character) #factor2character
sube <- dplyr::rename(sube, COD_SINRUT = serv) %>% 
  group_by(periodo, COD_SINRUT, parada) %>% summarise(Sube = sum(Sube))

baja <- as.data.frame(read.odbc.ffdf(odbcConnect.args = list("Asig", uid = "sa", 
                                                             pwd = "Estudios2017.."), VERBOSE = T, query = q_baja))
k <- sapply(baja, is.factor)
baja[k] <- lapply(baja[k], as.character)
baja <- dplyr::rename(baja, COD_SINRUT = serv) %>% 
  group_by(periodo, COD_SINRUT, parada) %>% summarise(Baja = sum(Baja))

odbcCloseAll()
rm(j,k)
#dictionaries ----------
dicc_serv <- readr::read_delim(file = paste0("output viajes_etapas/DCargaBus/", ipt_dir, 
 "/Diccionario-Servicios.csv"), delim = ";", col_types = cols_only(UN = "c", COD_SINRUT = "c", 
                                                                              COD_USUSEN = "c"))
dicc_par <- readr::read_delim(file = paste0("output viajes_etapas/DCargaBus/", ipt_dir,
              "/ConsolidadoParadas.csv"), delim = ";", locale = readr::locale(encoding = "latin1"), 
              col_types = cols_only(`Servicio Usuario` = "c", `Sentido Servicio` = "c", Orden = "i", 
                `Código  paradero Usuario` = "c", `Código paradero TS` = "c")) %>% tidyr::drop_na() %>%
  tidyr::unite("COD_USUSEN", c("Servicio Usuario", "Sentido Servicio"), sep = "") %>% 
  left_join(dicc_serv) %>% tidyr::unite("CodSerPar", c("COD_SINRUT",
  "Código paradero TS"), sep = "|") %>% dplyr::rename(PAR_USU = "Código  paradero Usuario")
#process -----
sube <- dplyr::left_join(sube, dicc_serv) %>% 
  tidyr::unite("CodSerPar", c("COD_SINRUT", "parada"), sep = "|") %>% left_join(dicc_par) %>%
  tidyr::unite("CodSerParPer", c("CodSerPar", "periodo"), sep = "|", remove = F) %>%
  group_by(CodSerParPer, periodo, UN, COD_USUSEN, PAR_USU, Orden) %>% summarise(Sube = max(Sube))

baja <- dplyr::left_join(baja, dicc_serv) %>% 
  tidyr::unite("CodSerPar", c("COD_SINRUT", "parada"), sep = "|") %>% left_join(dicc_par) %>%
  tidyr::unite("CodSerParPer", c("CodSerPar", "periodo"), sep = "|", remove = F) %>%
  group_by(CodSerParPer, periodo, UN, COD_USUSEN, PAR_USU, Orden) %>% summarise(Baja = max(Baja))

dCargaDF <- dplyr::full_join(sube, baja) %>% tibble::as_data_frame(dCargaDF) %>% 
  select(-CodSerParPer) %>% 
   mutate_at(.vars = c("Baja", "Sube"), .funs = ~replace(., is.na(.), 0)) %>% 
   mutate(Carga = Sube - Baja) %>% arrange(periodo, UN, COD_USUSEN, Orden) %>%
  group_by(periodo, UN, COD_USUSEN, Orden, PAR_USU) %>%
  summarise(Sube = sum(Sube), Baja = sum(Baja), Carga = sum(Carga)) %>% na.omit() %>%
  tidyr::complete(Orden = seq(from = 1, to = (max(Orden)))) %>% distinct(Orden, .keep_all = T) %>% 
  #complete it's heavy maybe parallelize
  mutate_at(.vars = c("Carga"), .funs = ~replace(., is.na(.), 0)) %>%
  mutate_at(.vars = c("Baja", "Sube", "Carga"), .funs = ~replace(., is.na(.), 0)) %>%
  tibble::as_data_frame() %>% 
  mutate(rowname = row_number(), COD_USUSEN2 = lag(COD_USUSEN, 1, 
    order_by = rowname), Carga2 = lag(Carga, 1, order_by = rowname), 
    periodo2 = lag(periodo, 1, order_by = rowname)) %>% mutate_at(.vars = "Carga2", 
     .funs = ~replace(., is.na(.), 0)) %>% mutate_at(.vars = c("COD_USUSEN2", "periodo2", "PAR_USU"), 
     .funs = ~replace(., is.na(.), "-")) %>% group_by(COD_USUSEN, periodo) %>% 
  mutate(CargaAcum = cumsum(Carga)) %>% select(c("periodo",  "UN",  "COD_USUSEN", "Orden",  "PAR_USU", 
   "Sube", "Baja",  "CargaAcum"))

readr::write_delim(dCargaDF, path = paste0("output viajes_etapas/", file), delim = ";")

return(dCargaDF)

}

dCargaBus <- dCarga(base = base, ipt_dir = ipt_dir, file = file)
