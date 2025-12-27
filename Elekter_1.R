# 
# Vesiniku kaudu elektri ost ja müük
# Elekter ostetakse, tehakse vesinik. 
# Vesinikust tehakse elekter, elekter müüakse
# 
# 29.05.2022 Kullo Raiend
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

alates_kp <- as.Date("2021-06-01")
kuni_kp   <- as.Date("2022-06-01")

# tunnid_2022õta viimane aasta
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

# Võimsus elektri ostmisel [MW]
võimsus_ost <- 10

# Võimsus elektri müümisel [MW]
võimsus_müük <- 10

# Kasutegur
# 80% elektrist lähe raisku, kui elektrist teha vesinik ja vesinikust tagasi elekter
# Ostame 100 ühikut elektrit, müüa saame 20 ühikut elektrit
kasutegur <- 0.2 

# Milise hinnaga tasub elektrit hakata ostma (hakata elektrist vesinikku tegema) [€/MWh]
ostu_lävend <- keskm_hind * (1 - 0.6) # 60% alla keskmise tunnihinna
ostu_lävend <- 50
ostu_lävend <- keskm_hind - 1.2 * sd_hind

# Milise hinnaga tasub elektrit hakata müüma (hakata vesinikust elektrit tegema) [€/MWh]
müügi_lävend <- keskm_hind * (1 + 0.8) # 80% üle keskmise tunnihinna
müügi_lävend <- 400
müügi_lävend <- keskm_hind + 2.2 * sd_hind

# Mida igas tunnis on võimalik teha
tunni_tegevus <- function(i, tunnid) {
  
  tund <- tunnid$tund[[i]]
  hind <- tunnid$hind[[i]]
  
  if (hind < ostu_lävend) {
    
    # Osta, tee vesinikku
    tegevus <- "osta"
    # Tunni jooksul salvesta (vesiniku) energiat, tunni lõpuks on kogunenud energiat [MWh]
    salvest_kogus <<- salvest_kogus + võimsus_ost * kasutegur
    # ostule kulunud summa võrra tulu kokku vähenemine
    tulu_kokku <<- tulu_kokku - võimsus_ost * hind
    
  } else if (hind > müügi_lävend) {
    
    # Müü, tee elektrit
    tegevus <- "müü"
    
    # Müüdava energia kogus tunni jooksul [MWh]
    # Kui salvestatud energiat on vähem alles kui võimsus müüa lubaks, siis müü niipalju energiat kui on  
    kogus_tunnis <- min(salvest_kogus, võimsus_müük)
    # Tunni jooksul tee vesinikust elektrit, tunni lõpuks on kogunenud energiat alles jäänud [MWh]
    salvest_kogus <<- salvest_kogus - kogus_tunnis
    # müügist saadud summa võrra tulu kokku suurenemine
    tulu_kokku <<- tulu_kokku + kogus_tunnis * hind 
    
  } else {
    # seisa
    tegevus <- "seisa"
  }
  
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

# Salvestatud ja müümata elektri energi (kogus) [MWh]
salvest_kogus <- 0

# Tulu kokku = Müümisel saadud kogusumma - Ostmisele kulunud kogusumma [€]
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
  

  
