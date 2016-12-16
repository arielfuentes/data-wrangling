od_trxx <- function(ser, t_dia){
  #"B26 00I"
  #t_dia -> c("'DOMINGO'", "'LABORAL'", "'SABADO'")  
  library(RPostgreSQL)
  library(dplyr)
  library(stringr)
  con <- dbConnect("PostgreSQL", host = "10.222.128.87", user= "postgres", password="estudios2015..", 
                   dbname = "BASE_U_CHILE")
  qsql_parx <- paste0("SELECT s_usu, s_ser, cod_ts, x, y, orden
                      FROM par_ts
                      WHERE s_ser = '", str_sub(ser, nchar(ser), nchar(ser)),"'AND s_usu = '", 
                      strsplit(ser, " ")[[1]][1], "'", ";")
  par_dbx <- dbGetQuery(con, qsql_parx)
  par_dbx$par_subida <- par_dbx$cod_ts
  par_dbx$par_bajada <- par_dbx$cod_ts
  par_dbx$orden.baj <- par_dbx$orden
  par_dbx <- par_dbx[,c("x", "y", "par_subida", "par_bajada", "orden", "orden.baj")]
  #par_dbx
  
  qsqlx <- paste0("Select count(*), par_subida, par_bajada, serv_un_zp2, 
                  tipo_dia from etapas201504_transparencia 
                  WHERE serv_un_zp2 LIKE '", ser,"%'", "AND tipo_dia = ",t_dia,
                  " GROUP BY par_subida, par_bajada, serv_un_zp2, tipo_dia;")
  trx_dbx <- dbGetQuery(con, qsqlx)
  #head(trx_dbx)
  
  trx_pos_ <- left_join(trx_dbx, par_dbx, by = "par_subida")
  
  trx_pos_ <- trx_pos_[, names(trx_pos_) %in% c("count", "par_subida", "par_bajada.x", "serv_un_zp2", 
                                                "tipo_dia", "x", "y", "orden")]
  
  names(trx_pos_) <- c("count", "par_subida", "par_bajada", "serv_un_zp2",  "tipo_dia", "x", "y", 
                       "orden.sub")
  trx_pos_ <- left_join(trx_pos_, par_dbx, by = "par_bajada")
  trx_pos_ <- trx_pos_[, names(trx_pos_) %in% c("count", "par_subida.x", "par_bajada", "serv_un_zp2", 
                                                "tipo_dia", "x.x", "y.x", "orden.sub", "x.y", "y.y", 
                                                "orden.baj")]
  
  names(trx_pos_) <- c("count",  "par_sub", "par_baj", "serv_un_zp2",  "tipo_dia", "x.sub",  "y.sub",  
                       "orden.sub", "x.baj", "y.baj", "orden.baj")
  trx_pos_ <- arrange(trx_pos_,orden.sub, orden.baj)
  trx_pos_$ID <- as.integer(row.names(trx_pos_))
  
  
  dbDisconnect(con)  
  trx_pos_
  #write.csv(trx_pos_, paste0(ser,t_dia, ".csv"), row.names = F)
}

od_trxx("B21 00I", "'SABADO'")

od_trx.per <- function(ser, per){
  #"B26 00I"
  #per <- seq(1:29)  
  library(RPostgreSQL)
  library(dplyr)
  library(stringr)
  con <- dbConnect("PostgreSQL", host = "10.222.128.87", user= "postgres", password="estudios2015..", 
                   dbname = "BASE_U_CHILE")
  qsql_parx <- paste0("SELECT s_ts, s_ser, cod_ts, x, y, orden
                      FROM par_ts
                      WHERE s_ser = '", str_sub(ser, nchar(ser), nchar(ser)),"'AND s_ts = '", 
                      strsplit(ser, " ")[[1]][1], "'", ";")
  par_dbx <- dbGetQuery(con, qsql_parx)
  par_dbx$par_subida <- par_dbx$cod_ts
  par_dbx$par_bajada <- par_dbx$cod_ts
  par_dbx$orden.baj <- par_dbx$orden
  par_dbx <- par_dbx[,c("x", "y", "par_subida", "par_bajada", "orden", "orden.baj")]
  #par_dbx
  
  qsqlx <- paste0("Select count(*), par_subida, par_bajada, serv_un_zp2, 
                  etapas201504_transparencia.tipo_dia, per_mh.periodo
                  from etapas201504_transparencia, per_mh 
                  WHERE serv_un_zp2 LIKE '", ser,"%'", " AND per_mh.periodo = '",per,"'", 
                  "AND per_mh.media_hora = etapas201504_transparencia.media_hora
                  AND per_mh.tipo_dia = etapas201504_transparencia.tipo_dia
                  GROUP BY par_subida, par_bajada, serv_un_zp2, etapas201504_transparencia.tipo_dia, 
                  periodo;")
  
  trx_dbx <- dbGetQuery(con, qsqlx)
  #head(trx_dbx)
  
  trx_pos_ <- left_join(trx_dbx, par_dbx, by = "par_subida")
  
  trx_pos_ <- trx_pos_[, names(trx_pos_) %in% c("count", "par_subida", "par_bajada.x", "serv_un_zp2", 
                                                "tipo_dia", "periodo","x", "y", "orden")]
  
  names(trx_pos_) <- c("count", "par_subida", "par_bajada", "serv_un_zp2",  "tipo_dia", "periodo",
                       "x", "y", "orden.sub")
  trx_pos_ <- left_join(trx_pos_, par_dbx, by = "par_bajada")
  trx_pos_ <- trx_pos_[, names(trx_pos_) %in% c("count", "par_subida.x", "par_bajada", "serv_un_zp2", 
                                                "tipo_dia", "periodo","x.x", "y.x", "orden.sub", 
                                                "x.y", "y.y", "orden.baj")]
  
  names(trx_pos_) <- c("count",  "par_sub", "par_baj", "serv_un_zp2",  "tipo_dia", "periodo",
                       "x.sub",  "y.sub", "orden.sub", "x.baj", "y.baj", "orden.baj")
  trx_pos_ <- arrange(trx_pos_,orden.sub, orden.baj)
  trx_pos_$ID <- as.integer(row.names(trx_pos_))
  
  
  dbDisconnect(con)  
  #trx_pos_
  write.csv(trx_pos_, paste0(ser,per, ".csv"), row.names = F)
}

od_trx.per("B51 00R", 4)
