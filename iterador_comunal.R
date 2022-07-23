# LIBRERIAS ---------------------------------------------------------------


library(sf)
library(purrr)
library(dplyr)

make_dir <- function(path){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}
# DATOS -------------------------------------------------------------------

comunas <- st_read("") %>% 
  st_transform(32719) %>% 
  select(REGION,
         NOM_REGION,
         COMUNA,
         NOM_COMUNA) %>% 
  rename(COD_REG = REGION,
        NOM_REG = NOM_REGION,
        COD_COM = COMUNA,
        NOM_COM = NOM_COMUNA)
  

mh <- st_read() %>% 
  st_zm() %>% 
  st_cast("POINT") %>% 
  st_transform(32719) %>% 
  #filter(categoria == "Monumento Histórico") %>%
  #filter(comuna != "Multiterritorial") %>% 
  select(objectid,
         codigo,
         nombre,
         categoria,
         actualizac,
         tipo_decre,
         num_dec,
         pub_dec
  ) %>% 
  rename(ID = objectid,
         CODIGO = codigo, 
         NOMBRE = nombre,
         CATEGORIA = categoria,
         ACTUALIZACION = actualizac,
         DECRETO = tipo_decre,
         NUMERO_DEC = num_dec,
         FECHA = pub_dec) 

snaspe <- st_read("") %>% 
  st_zm() %>% 
  st_cast("POLYGON") %>% 
  rename(ID = gid,
         NOMBRE = Unidad,
         CATEGORIA= Categoria,
         NOM_COM = Comuna,
         NOM_PROV = Provincia,
         NOM_REG = Region, 
         AREA = Sup_ha)

area_ind <- st_read("") %>%
  st_zm() %>% 
  st_cast("POLYGON") %>% 
  select(COD_COMUNA,
         COD_PROVIN,
         COD_REGION,
         COMUNAS,
         ETNIA,
         HA,
         NOMBRE,
         OBJECTID,
         PROVINCIAS,
         REGION
  ) %>% 
  rename(COD_COM = COD_COMUNA,
         COD_PROV = COD_PROVIN,
         COD_REG= COD_REGION,
         ID = OBJECTID)


reg_ind <- st_read("") %>% 
  st_zm() %>% 
  st_cast("POINT") %>% 
  st_transform(4326) %>% 
  select(REGI_N,
         PROVINCIA,
         COMUNA,
         SECTOR,
         COMUNIDAD,
         REGISTRO,
         FECHA,
         FUENTE_INF
  ) %>% 
  rename(NOM_REG = REGI_N,
         NOM_PROV = PROVINCIA,
         NOM_COM= COMUNA,
         FUENTE = FUENTE_INF)


# ORDEN DE COLUMNAS -------------------------------------------------------

mh_join <- st_join(mh, comunas, join = st_intersects)
mh <- mh_join %>% filter(!is.na(NOM_REG))
# algunos na son de la antartica

col_mh <- c("ID", "NOMBRE", "CATEGORIA",
               "COD_COM", "NOM_COM" ,
            "COD_REG", "NOM_REG", "CODIGO", "ACTUALIZACION",
            "DECRETO", "NUMERO_DEC", "FECHA")
mh <- mh[, col_mh]


# ITERADOR POR COMUNA -----------------------------------------------------


for(com in unique(mh$NOM_COM)) {

  mh_com = mh[mh$NOM_COM==com,]
  reg <- unique(mh_com$NOM_REG)
  com <- unique(mh_com$NOM_COM)
  cod <- unique(mh_com$COD_REG)

  
  path_salida <-  paste0("resultados/", reg,"/",com, "/")
  make_dir(path_salida)
  
  st_write(mh_com, 
           paste0(path_salida, "R", cod,"_","MH","_", com, ".shp"), 
           append = F, delete_dsn = T)
}


# POR REGION --------------------------------------------------------------


for(reg in unique(mh$NOM_REG)) {
  
  mh_reg = mh[mh$NOM_REG==reg,]
  reg <- unique(mh_reg$NOM_REG)
  # com <- unique(mh_com$NOM_COM)
  cod <- unique(mh_reg$COD_REG)
  
  
  path_salida <-  paste0("resultados3/", reg,"/")
  make_dir(path_salida)
  
  st_write(mh_reg, 
           paste0(path_salida, "R", cod,"_","MH", ".shp"), 
           append = F, delete_dsn = T)
}




