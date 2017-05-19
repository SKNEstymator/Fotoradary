library(readxl)
library(purrr)
library(data.table)
library(snakecase)
library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)


## zmiany w nazwach plików
# 'Luszczow-Pierwszy'

file.rename('data-raw/ind/Luszczow-1-po_VC2_061015_078_1.xlsx',
            'data-raw/ind/Luszczow-Pierwszy-1-po_VC2_061015_078_1.xlsx')

file.rename('data-raw/ind/Luszczow-2-po_VC2_061015_002_1.xlsx',
            'data-raw/ind/Luszczow-Pierwszy-2-po_VC2_061015_002_1.xlsx')

file.rename('data-raw/ind/Luszczow-3-po_VC2_061015_007_1.xlsx',
            'data-raw/ind/Luszczow-Pierwszy-3-po_VC2_061015_007_1.xlsx')

## Lubliniec

file.rename('data-raw/ind/VC2_040915_053_1.xlsx',
            'data-raw/ind/Lubliniec-1-po_VC2_040915_053_1.xlsx')

file.rename('data-raw/ind/VC2_040915_236_1.xlsx',
            'data-raw/ind/Lubliniec-2-po_VC2_040915_236_1.xlsx')

file.rename('data-raw/ind/VC2_040915_238_1.xlsx',
            'data-raw/ind/Lubliniec-3-po_VC2_040915_238_1.xlsx')

files <- list.files('data-raw/ind', full.names = TRUE)


dane <- files %>%
  set_names() %>% 
  map_df(~ read_excel(path = .x, sheet = 'raw(T)', skip = 1, col_names = T), .id = "plik")  %>%
  dplyr::select(plik,Data:Pojazd)  %>%
  setNames(snakecase::to_snake_case(names(.))) %>%
  mutate(plik = gsub('data-raw/ind/','', plik)) %>%
  separate(col = plik, into = c('miasto_pomiar','reszta'), sep='VC2')  %>%
  mutate(miasto = stri_extract(miasto_pomiar, regex = 'Gorzyce\\-\\d\\-\\d'),
         miasto = ifelse(is.na(miasto), 
                         gsub('(\\-|\\_)\\d(\\-|\\_).+','',miasto_pomiar),
                         miasto),
         pomiar = stri_extract(miasto_pomiar, regex = '\\d'),
         pomiar = ifelse(stri_detect(miasto, fixed = 'Gorzyce'),
                         stri_extract_last(miasto, regex = '\\d'),
                         pomiar),
         przed_po = stri_extract(miasto_pomiar, regex = '(\\-|\\_)(przed|po)'),
         przed_po = stri_replace(przed_po, regex = '(\\-|\\_)', replacement =''),
         miasto = ifelse(stri_detect(miasto, fixed = 'Gorzyce'),
                         stri_replace_last(miasto, replacement = '', regex = '\\-\\d'),
                         miasto)) %>%
  select(miasto,pomiar,przed_po,data:pojazd) %>%
  mutate(type = ifelse(miasto %in% c('Gietrzwald', 'Gorzyce-1', 'Gorzyce-2', 'Losiow', 'Luszczow', 
                                     'Polesie', 'Sochaczew', 'Szymaki', 'Tarnowskie', 'Warmiaki', 
                                     'Wilcza', 'Zabludow'),
                       'opp','fotoradar')) %>%
  mutate(date = ymd_hms(data),
         date2 = ifelse(is.na(date), dmy_hms(data), NA),
         date_final = ifelse(is.na(date2), date, date2),
         date_final = as_datetime(date_final),
         date_ymd = ymd(format(date_final, '%Y-%m-%d'))) %>%
  select(miasto,pomiar,przed_po,prędkość,kierunek_ruchu,odstęp,`długość_(_radar_)`, `długość_(_cm_)`,
         pojazd, type, date_final) %>%
  rename(city = miasto,
         measurement = pomiar,
         when = przed_po, 
         speed = prędkość,
         direction = kierunek_ruchu,
         distance_between = odstęp,
         length_radar = `długość_(_radar_)`,
         length_vehicle = `długość_(_cm_)`,
         vehicle_type = pojazd,
         radar_type = type,
         measurement_date = date_final) %>%
  select(city:when, measurement_date, speed:measurement_date)


### add info about limits...


fwrite(x = dane, file = 'data/raw-data.csv') 
saveRDS(dane, file = 'data/raw-data.RDS', compress = TRUE)


### ograniczenia prędkości

files <- list.files('data-raw/ind', full.names = TRUE)

ograniczenia <- files %>%
  .[stri_detect(.,regex = '1(\\-|\\_)po')] %>%
  set_names() %>% 
  map_df(~ read_excel(path = .x, sheet = 'Tabela', skip = 15, col_names = F), .id = "plik") %>%
  filter(!is.na(X__2)) %>%
  filter(!is.na(X__1)) %>%
  mutate(plik = gsub('data-raw/ind/','', plik)) %>%
  separate(col = plik, into = c('miasto_pomiar','reszta'), sep='VC2')  %>%
  mutate(miasto = stri_extract(miasto_pomiar, regex = 'Gorzyce\\-\\d\\-\\d'),
         miasto = ifelse(is.na(miasto), 
                         gsub('(\\-|\\_)\\d(\\-|\\_).+','',miasto_pomiar),
                         miasto),
         pomiar = stri_extract(miasto_pomiar, regex = '\\d'),
         pomiar = ifelse(stri_detect(miasto, fixed = 'Gorzyce'),
                         stri_extract_last(miasto, regex = '\\d'),
                         pomiar),
         przed_po = stri_extract(miasto_pomiar, regex = '(\\-|\\_)(przed|po)'),
         przed_po = stri_replace(przed_po, regex = '(\\-|\\_)', replacement =''),
         miasto = ifelse(stri_detect(miasto, fixed = 'Gorzyce'),
                         stri_replace_last(miasto, replacement = '', regex = '\\-\\d'),
                         miasto)) %>%
  rename(time_5_23=X__2,
         time_23_5=X__3) %>%
  mutate(all_time_osob = ifelse(stri_detect(X__1,fixed='całą'),time_5_23,NA),
         all_time_ciezar = ifelse(stri_detect(X__1,fixed='całą'),time_23_5,NA),
         time_5_23 = ifelse(stri_detect(X__1,fixed='podziale'),time_5_23,NA),
         time_23_5 = ifelse(stri_detect(X__1,fixed='podziale'),time_23_5,NA)) %>%
  select(miasto, all_time_osob,all_time_ciezar,time_5_23,time_23_5) %>%
  mutate_each(funs(as.numeric(.)),-miasto)


dane <- dane %>%
  left_join(ograniczenia, by = c('city'='miasto')) %>%
  tbl_df()

## pozbawiam polskich znaków

dane <- dane %>%
  mutate(direction = stri_trans_general(direction, "latin-ascii"),
         vehicle_type = stri_trans_general(vehicle_type, "latin-ascii"),
         radar_type = ifelse(when == 'przed','none',radar_type))

dane %>%
  count(radar_type)


fwrite(x = dane, file = 'data/raw-data.csv') 
saveRDS(dane, file = 'data/raw-data.RDS', compress = TRUE)




