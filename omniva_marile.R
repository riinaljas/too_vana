library(tidyverse)
library(janitor)
library(gsheet)

hiiumaa <-
  gsheet2tbl(
    "https://docs.google.com/spreadsheets/d/1rkHTh-qav32K2YoSM78uS0SXGvAUwonkKoq8JUCeiMY/edit#gid=1512140273"
  ) %>%
  clean_names() %>%
  mutate(nimi = nimi %>%
           str_remove_all("KOKKU: ") %>%
  str_remove_all(" \\(.*eraisikule.*| jur\\.isikule.*| juriidilisele.*| eraisikule|eraisik| jur\\.is| \\(.*| jur.*") %>%
  str_remove_all("eraisikule") %>%
  trimws()
) %>% 
  group_by(nimi) %>% 
  summarise(tellimusi_kokku = sum(kokku, na.rm = T)) %>% 
  arrange(desc(tellimusi_kokku))
  

tartu_linn <-
  gsheet2tbl(
    "https://docs.google.com/spreadsheets/d/1rkHTh-qav32K2YoSM78uS0SXGvAUwonkKoq8JUCeiMY/edit#gid=1079194357"
  ) %>%
  clean_names() %>%
  filter(str_detect(nimi, "Tartu linna")) %>%
  mutate(
    nimi = nimi %>%
      str_remove_all("KOKKU: ") %>%
      str_remove_all(
        ".Tartu li.*|.Elva.* |.Roiu.*|.L.hte.*|.Uht.*|.Tartu K.*|.Tammis.*|.Vana.*|.Ahja"
      ) %>%
      trimws() %>%
      str_remove_all(
        " \\(.*eraisikule.*| jur\\.isikule.*| juriidilisele.*| eraisikule|eraisik| jur\\.is| \\(.*| jur.*"
      ) %>%
      str_remove_all("eraisikule")
  ) %>% 
  mutate(nimi = ifelse(
    str_detect(nimi, "Saarte"),
    tartu_linn[56,1],
    nimi) %>% 
  group_by(nimi) %>%
  summarise(tellimusi_kokku = sum(kokku, na.rm = T)) %>%
  arrange(nimi) %>% 
    mutate((nimi = ifelse(
      str_detect(nimi, "Saarte"),
      tartu_linn[56,1],
      nimi)

lasnamae <-  gsheet2tbl(
  "https://docs.google.com/spreadsheets/d/1rkHTh-qav32K2YoSM78uS0SXGvAUwonkKoq8JUCeiMY/edit#gid=1580492308"
) %>%
  clean_names() %>%
  rename(nimi = 1) %>%
  mutate(nimi = nimi %>%
           str_remove_all("KOKKU: ")) %>%
  mutate(
    asukoht = nimi %>%
      str_extract("LASNA.*$|MUSTA.*$|LOO.*$|SIKU.*$|PRIISLE.*$") %>%
      trimws(),
    nimi = nimi %>%
      str_remove_all(asukoht) %>%
      str_remove_all(" \\(.*eraisikule.*| jur\\.isikule.*| juriidilisele.*") %>%
      str_remove_all("eraisikule") %>% 
      trimws()) %>%
  mutate(nimi = ifelse(str_detect(nimi, "P.hjarannik"), 
            str_remove_all(nimi, " .*$"), 
            nimi)
         )%>% 
  mutate(nimi = ifelse(str_detect(nimi, "Saarte"), 
                       lasnamae[19,1] %>% as.character(), 
                       nimi)) %>% 
  group_by(nimi) %>% 
  summarise(tellimusi_kokku = sum(x1, na.rm = T)) %>% 
  arrange(desc(tellimusi_kokku))


hiiu <- 


  
kokkukohti<- tartu_linn %>% 
  mutate(koht="tartu") %>% 
  bind_rows(lasnamae %>% 
              mutate(koht="lasnamae")) %>% 
  bind_rows(hiiumaa %>% 
              mutate(koht="hiiumaa")) 

write_csv(kokkukohti, "kokkukohti.csv")
