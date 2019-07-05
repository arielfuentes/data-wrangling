ev_mes_zona <- function(file){
  #libraries
  library(readxl)
  library(cellranger)
  library(dplyr)
  library(stringr)
  #input
  ##orden
  mes <- tibble::tribble(
                 ~Mes_2, ~Orden,
                "Enero",     1L,
              "Febrero",     2L,
                "Marzo",     3L,
                "Abril",     4L,
                 "Mayo",     5L,
                "Junio",     6L,
                "Julio",     7L,
               "Agosto",     8L,
           "Septiembre",     9L,
              "Octubre",    10L,
            "Noviembre",    11L,
            "Diciembre",    12L
           )

  data <- read_xlsx(path = paste0("input/", file), 
             col_types = c("text", "text", "text", "text", "text", "numeric", "text", "text", "text", 
                           "text", "text", "numeric", "numeric"), 
             range = cellranger::cell_cols("C:O")) %>%
    select("Mes_2", "Servicio", "Pagan", "No Pagan") %>% 
    mutate(Zona = case_when(stringr::str_sub(Servicio, end = 1) ==  "B" ~ "Norte", 
                            stringr::str_sub(Servicio, end = 1) ==  "C" ~ "Oriente", 
                           TRUE ~ "Norte 2" 
    )) %>% group_by(Mes_2, Zona) %>% summarise(Ev = 100*(sum(`No Pagan`) / (sum(`No Pagan`, Pagan)))) %>%
    left_join(mes) %>% arrange(desc(Orden)) %>% select(-Orden)
    rm(mes)
    return(data)
}

ev_mes_zona("Acumulado Mediciones 2019_2.xlsx")
