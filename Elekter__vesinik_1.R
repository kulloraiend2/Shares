# 
# Elektrist vesiniku tegemine ja vesiniku müük
# Elekter ostetakse, tehakse vesinik. 
# Vesinik müüakse
# 
# 14.07.2022 Kullo Raiend
# 
# Elektri börsihinnad
# https://dashboard.elering.ee/et/nps/price?interval=minute&period=years&start=2021-12-31T22:00:00.000Z&end=2022-12-31T21:59:59.999Z&show=table
# 
library(tidyverse)
library(lubridate)
library(stringr)

# Loe tunnihinnad [€/MWh]
loe_tunnihinnad <- function(file) {
  
  tunnid <- read_csv2(file) %>% 
    # võta 2. (kuupäev) ja 6. veerg (Eesti)
    select(c(2, 6)) %>% 
    # Pane muutujatele nimed
    set_names(c("tund", "hind")) %>% 
    mutate(
    # teisenda tund
    tund = as.POSIXlt(tund, tz = "", format = "%d.%m.%Y %H:%M") # "01.01.2021 00:00"
    )
  
}

tunnid_2021 <- loe_tunnihinnad("Tunnihinnad_2021.csv")
str(tunnid_2021)

tunnid_2022 <- loe_tunnihinnad("Tunnihinnad_2022.csv")
str(tunnid_2022)

# tunnid <- tunnid_2021
# tunnid <- tunnid_2022

alates_kp <- as.Date("2021-08-01")
kuni_kp   <- as.Date("2022-08-01")

# Võta viimane aasta
tunnid <- bind_rows(
  tunnid_2021 %>% filter(as.Date(tund) >= alates_kp),
  tunnid_2022 %>% filter(as.Date(tund) <  kuni_kp)
)
str(tunnid)

puuduvad_hinnad <- tunnid %>% 
  filter(is.na(hind))

# Minimaalne hind
min_hind <- min(tunnid$hind)

# Maksimaalne hind
max_hind <- max(tunnid$hind)

# Keskmine tunnihind
keskm_hind <- mean(tunnid$hind)

# Mediaan tunnihind
mediaan_hind <- median(tunnid$hind)

# Standardhälve
sd_hind <- sd(tunnid$hind)

# Vesiniku tegemise seadme elektri tarbimise max võimsus [MW]
max_el_võimsus <- 0.6 # 600 KW

# Seadme vesiniku tootlikkus maksimaalsel elektri targimise võimsusel
# 260 kg per day 
vesiniku_tootlikkus <- 260/24 # kg/tunnis Seade maksab umbes miljon €

# Vesiniku mahuti maht, kg
mahuti <- 1000 # mahuti maksab umbes 200 €/kg

# Oletame, et vesinikku müüakse pidevalt kiirusega [kg/tunnis]
müügikiirus <- 5 # umbes pool maksimaalsest tootlikkusest

# Mahuti soovitav, mõistlik laotagavara
# Mahuti täituvus [%], mille korral toota vesinikku igaks juhuks tagavaraks, 
# kui elektri hind on alla keskmise ostulävendi
mahuti_tagavara <- 0.3

# Vesiniku müügihind [€/kg]
müügi_hind <- 50 #25 # 50 

# Tunnis toodetava vesiniku maksumus [€/tunnis]
vesiniku_maksumus_tunnis <- vesiniku_tootlikkus * müügi_hind

# Kui kõrge võiks olla elektri hind, et tasub vesinikku toota [€/MWh]
kasumlik_el_hind <- vesiniku_maksumus_tunnis / max_el_võimsus 

# Milise hinnaga tasub elektrit hakata ostma (hakata elektrist vesinikku tegema) [€/MWh]
ostu_lävend <- keskm_hind * (1 - 0.6) # 60% alla keskmise tunnihinna
ostu_lävend <- mediaan_hind * (1 - 0.6) # 60% alla tunnihinna mediaani

ostu_lävend <- 50
ostu_lävend <- keskm_hind - 1.2 * sd_hind

# 
ostu_lävend_keskmine <- keskm_hind + 1.5 * sd_hind 

ostu_lävend_madal <- keskm_hind - 0.3 * sd_hind 

# Mida igas tunnis on võimalik teha
tunni_tegevus <- function(i, tunnid) {
  
  tund <- tunnid$tund[[i]]
  hind <- tunnid$hind[[i]]
  
  # Kas toota vesinikku tund aega selle elektri hinna juures?
  # Oletame, et kogu tunni seade töötab täisvõimsusel
  to_produce <- function(hind, salvest_kogus, mahuti, vesiniku_tootlikkus, müügikiirus) {
    
    # vesiniku müügikohustus tunni lõpuks on vaja täita
    # Vesinikku ei jätku müügikohustuse tätimiseks tunni lõpuks, kui selles tunnis ei tooda
    if (salvest_kogus < müügikiirus) return(TRUE)
    
    # Kui mahuti on täis, siis ei saa toota
    if (salvest_kogus + vesiniku_tootlikkus > mahuti) return(FALSE)
    
    # Võiks teha ka keerulisema otsustuse, kuidas laotagavara juhtida sõltuvana elektri hinnast
    # Võiks vaadata laoseisu ja tuleviku elektri hinda, kas läheb odavamaks või kallimaks
    # Võib vaadata elektri hinna trendi mingis ajavahemikus ja ost lävendit muuta dünaamiliselt 
    # trendist ja laoseisust lähtuvalt
    
    # Kui elektri hind on väga madal
    if (hind < ostu_lävend_madal) {
      # siis tooda igal juhul
      return(TRUE)
    } else {
      # Kui elektri hind on alla keskmise ostuhinna lävendi ja 
      # mahuti mõistlik laotagavara ei ole kogunenud
      if (hind < ostu_lävend_keskmine && salvest_kogus < mahuti * mahuti_tagavara) {
        # Siis tooda
        TRUE
      } else {
        # muidu seisa
        FALSE
      }
    }
    
  } # end to_produce
  
  if (to_produce(hind, salvest_kogus, mahuti, vesiniku_tootlikkus, müügikiirus)) {
    
    # Tooda  vesinikku
    tegevus <- "tooda"
    # Tunni lõpuks on toodetud vesinikku [kg]
    toodetud_kogus <- vesiniku_tootlikkus
    # Elektri maksumus tunni jooksul 
    el_maksumus_tunnis <- max_el_võimsus * hind
    
  } else {
    
    # Seisa
    tegevus <- "seisa"
    toodetud_kogus <- 0
    el_maksumus_tunnis <- 0
    
  }
    
  # Tunni lõpuks on salvestatud vesinikku [kg]
  salvest_kogus <<- salvest_kogus + toodetud_kogus - müügikiirus
  
  # Tunni lõpuks tulu kokku [€]
  tulu_kokku <<- tulu_kokku - el_maksumus_tunnis + müügikiirus * müügi_hind
  
  return(tibble(
    tund = tund,
    hind = hind,
    tegevus = tegevus,
    salvest_kogus = salvest_kogus,
    tulu_kokku = tulu_kokku
  ))
    
}

# x <- tunni_tegevus(1, tunnid)  
# y <- tunni_tegevus(317, tunnid)  
# z <- tunni_tegevus(355, tunnid)  

# Salvestatud vesiniku kogus [kg]
salvest_kogus <- 0

# Tulu kokku = Vesiniku müümisel saadud kogusumma - elektri ostmisele kulunud kogusumma [€]
tulu_kokku <- 0

tegevus_tunnis <- map_dfr(seq_len(nrow(tunnid)), tunni_tegevus, tunnid)

tegevus <- table(tegevus_tunnis$tegevus)
tegevus

tunde <- sum(tegevus)
tunde

suhteline_tegevus <- round(tegevus/tunde * 100)
suhteline_tegevus

# proportions(tegevus, 2)
salvest_kogus
tulu_kokku


tegevus_tunnis %>% 
  
  ggplot(aes(tund, salvest_kogus)) +
  geom_point(mapping = aes(x = tund, y = hind, color = tegevus)) +
  geom_line() 
  

  
