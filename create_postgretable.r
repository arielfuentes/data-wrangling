library(readxl)
par <- read_excel("2016-07-02_consolidado_anexo4_(Circunvalación)_V2.xlsm", sheet = 1, skip = 1,
                  col_names = c("orden",  "s_ts", "s_usu", "s_ser", "var",  "un", "cod_ts", "cod_usu",          
                                "comuna", "eje", "desde",  "hacia", "x",  "y", "nombre", "zp", 
                                "excepciones"), 
                  col_types = c("numeric", "text", "text", "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", "numeric", "numeric", "text", "text", 
                                "text"))

library(RPostgreSQL)

#qsql_table <- "CREATE TABLE paradas(Orden integer,  S_TS character varying(100),                       
 #               SUsu character varying(100), S_Ser character varying(100),                  
#Var character varying(100),  UN integer,                                
#Cod_TS character varying(100), Cod_Usu character varying(100),          
#Comuna character varying(100), Eje character varying(100),                               
#Desde character varying(100),  Hacia character varying(100),                  
#x double precision,  y double precision,                                 
#Nombre character varying(100), ZP character varying(100),
#Excepciones character varying(100)) WITH (
 # OIDS=FALSE
#);"

con <- dbConnect("PostgreSQL", host = "10.222.128.87", user= "postgres", password="estudios2015..", 
                 dbname = "BASE_U_CHILE")

#table_db <- dbGetQuery(con, qsql_table)

dbWriteTable(con, "par_ts", data.frame(par), row.names = F)
