library(RMySQL)
library(tidyverse)
library(lubridate)
library(stringr)
# library(googlesheets)
options(java.parameters = "-Xmx16g")  ## memory set to 16 GB
getOption("java.parameters")
library(xlsx)
#library(lobstr)

# tidyverse_update() # update tidyverse packages

# install.packages("installr") # install R installer
# installr::updateR() # updating R. Running “updateR()” will detect if there is a new R version available, and if so it will download+install it (etc.).

source("SharesCommon.R")

# Connect to DB
con <- dbConnect(
  RMySQL::MySQL(),
  user = localUserName,
  password = localUserPassword,
  dbname = dbName,
  host = host
) #,
#encoding = "native.enc")

#x <- dbGetQuery(con, "SELECT * FROM shareholder WHERE name LIKE 'seitse%'")

# available dates of statements
dates <-
  as.Date(dbGetQuery(con, "SELECT DISTINCT date FROM statement
                            ORDER BY date DESC")$date)

maxDates <-  27 # + 10
#maxDates <- 11 + 24
# maxDates <- 400

# daily, weekly or monthly reports for current day, week or month
currentDay   <- TRUE
currentWeek  <- FALSE # TRUE
currentMonth <- FALSE # TRUE


# Daily
reportDates <- head(dates, n = maxDates)

dailyRep <- list(reportDates = reportDates, 
                fileName = paste("Statement Differencies daily", reportDates[1]))

# Weekly

# the date is a weekend?
isWeekend <- function(i, dates) {
  
  isWeekly <- if (wday(dates[i], week_start = 1) %in% c(5, 6, 7)) TRUE else FALSE
  
  if (i == 1) {
    # previous date in the different week?
    # See on vale esmaspäeval
    # if (isoweek(dates[1]) != isoweek(dates[2])) TRUE else isWeekly
    # Midagi täiendavat ei oska teha
    isWeekly
    
  } else {
    # next date in the different week
    if (isoweek(dates[i]) != isoweek(dates[i - 1])) TRUE else isWeekly
    
  } 
  
}

# last date is a weekend or previous date in the different week
if (isWeekend(1, dates) | currentWeek) { 

  # is the date weekend
  isWeekly <- map_lgl(seq_along(dates), isWeekend, dates) 
  
  if (currentWeek) isWeekly[1] <- TRUE
  
  # Keep last days of week and only first maxDates of them
  reportDates <- head(dates[isWeekly], n = maxDates)
  
  weeklyRep <- list(reportDates = reportDates, 
                    fileName = paste("Statement Differencies weekly",
                                   if (isWeekend(1, dates)) reportDates[1] else "current")
                    )
  
} else weeklyRep <- NULL

# Monthly
# tmpDate <- as.Date("2021-06-30")
# mday(tmpDate + 1)
# month(tmpDate + 1)
# days_in_month(tmpDate)

# the date is end of a month?
isMonthend <- function(i, dates) {
  
  if (mday(dates[i]) == days_in_month(dates[i])) return(TRUE)
  
  # If Friday and next Monday is in the next month
  if (wday(dates[i], week_start = 1) == 5 & 
      month(dates[i]) != month(dates[i] + 3)) return(TRUE)
  
  if (i == 1) {
    # The last date and the previous date in the same month?
    # Vale!
    # if (month(dates[1]) != month(dates[2])) TRUE else FALSE
    # Ei oska täiendavalt midagi teha.
    FALSE
    
    } else {
      # otherwise, is the next date in the same month?
      if (month(dates[i]) == month(dates[i - 1])) FALSE else TRUE
  }
  
}

if (isMonthend(1, dates) | currentMonth)  { # last date informs about the end of month

  isMonthly <- map_lgl(seq_along(dates), isMonthend, dates) 
  
  if (currentMonth) isMonthly[1] <- TRUE
  
  # Keep last days of month and only first maxDates of them
  reportDates <- head(dates[isMonthly], n = maxDates)
  
  monthlyRep <- list(reportDates = reportDates, 
                     fileName = paste("Statement Differencies monthly",
                                      if (isMonthend(1, dates)) reportDates[1] else "current")
                     )

} else monthlyRep <- NULL

# Report requests
repRequests <- list()

# # Days report is requested always
# repRequests <- list(dailyRep)

# add request of daily report
if (currentDay) repRequests <- append(repRequests, list(dailyRep))

# add request of weekly report  
# repRequests <- list(weeklyRep)
if (!is.null(weeklyRep)) repRequests <- append(repRequests, list(weeklyRep))

# add request of monthly report  
# repRequests <- list(monthlyRep)
if (!is.null(monthlyRep)) repRequests <- append(repRequests, list(monthlyRep))

str(repRequests)

# delRec <- dbGetQuery(con, paste0("DELETE FROM statement WHERE date='", dates[1], "'"))

# get statements
getStatements <-
  function(reportDates,
           shareSymbol = NULL,
           shareholderName = NULL) {
    query <-
      paste0(
        "SELECT sh.symbol, shh.name AS shareholder, st.securities, st.date
                  FROM statement AS st
                  LEFT JOIN share AS sh ON st.share_id = sh.id
                  LEFT JOIN shareholder AS shh ON st.shareholder_id = shh.id
                  WHERE date in (", str_c("'", reportDates, "'", collapse = ","), ")"
      )
    
    if (!is.null(shareSymbol)) {
      query <- paste0(query, " AND sh.symbol = '", shareSymbol, "'")
    }
    
    if (!is.null(shareholderName)) {
      query <-
        paste0(query,
               " AND UPPER(shh.name) IN ('",
               str_c(str_to_upper(shareholderName), collapse = "','"),
               "')")
    }
    
    statements <-
      dbGetQuery(con, query) #%>%   # 13.02.2020 kindlustame, et shareholdername on query-s native encodingus
    
      # Versioonis R 4.0.5
      # dbGetQuery(con, enc2native(query)) #%>%   # 13.02.2020 kindlustame, et shareholdername on query-s native encodingus
    
    # 13.02.2020 pole vaja alates R ver 3.6.2. Selle asemel oli vaja kodeerida UTF-8  to native (latin1) shareholder name otsimisel ja
    #salvestamisel  getShareholderIds funktsioonis, kui väljavõtet sisestatakse (LoadStatements)
    #    mutate(
    # täpitähed
    #shareholder = iconv(shareholder, from="UTF-8",to="UTF-8")
    #    )
    
    return(statements)
    
  }

# reverse date strings in format "yyyy.mm.dd"
reverseDate <- function(dateStr) {
  # dateStr <- c("2020.07.06", "2020.07.07" )
  
  # split dates
  splitted <- str_split(dateStr, pattern = "[.-]", simplify = FALSE)
  # reverse each date
  reversed <- map(splitted, rev)
  # combine each date
  map_chr(reversed, str_c, collapse = ".")
}

# create nice date column name
niceDateColumnName <- function(dateColumnName) {
  # dateColumnName <- c("X2020.07.03", "X2020.07.04.1")
  
  #nice <- paste0("`", reverseDate(str_remove(dateColumnName, "X")), "`")
  # nonsyntactic column names work. Reference them using backtics ``
  nice <- dateColumnName %>%
    str_remove("X") %>%
    str_sub(start = 1, end = 10) %>%
    reverseDate()
  
  # shorten year 20yy to yy
  str_replace(nice, ".20", ".")
}

# replace daily securities to daily differences from column fromCol
# last column is last daily statements
securities2differences <- function(statements, fromCol) {
  # fromCol <- 3
  
  nColumns <- ncol(statements)
  nStatements <- nColumns - fromCol + 1
  
  if (nStatements > 2) {
    # replace daily to differences
    prevDaily <- statements[[fromCol - 1]]
    for (j in fromCol:nColumns) {
      # j <- 3
      daily <- statements[[j]]
      statements[[j]] <- daily - prevDaily
      prevDaily <- daily
    }
    
    # remember last daily statements in the last column
    statements[nColumns + 1] <- daily
    colnames(statements)[nColumns + 1] <-
      paste0(colnames(statements)[nColumns], ".1")
    
  }
  
  return(statements)
  
}

# add sheet for a shareSymbol
addShareSheet <- function(shareSymbol) {
  # shareSymbol <-  "ARC1T"  #"MRK1T"
  
  print(shareSymbol)
  
  # get statements
  statements <-
    getStatements(reportDates =  reportDates,
                  shareSymbol = shareSymbol) %>%
    
    # summarise securities of shareholder by dates
    group_by(shareholder, date) %>%
    summarise(securities = sum(securities), .groups = "drop_last") %>%
    
    # securities in different dates in separate columns
    spread(date, securities) %>% data.frame()
  
  if (ncol(statements) < 3) {
    # no data for the both dates, skip the share
    return()
  }
  
  # set missing securities to 0
  statements[is.na(statements)] <- 0
  
  # replace daily securities to daily differences from column 3
  statements <- securities2differences(statements, fromCol = 3)
  
  # remove shareholders where all differencies are zero
  
  # # columns containing differencies
  selectedCols <-
    statements[, seq(3, ncol(statements) - 1), drop = FALSE]
  # single column dataframe (1 difference of dates available only) is converted to vector by default. drop=FALSE
  # keeps dataframe for using correctly apply function later on matrix
  # Or use list subsetting columns, Always returns dataframe (list), even for single column
  # selectedCols <- statements[seq(3, ncol(statements) - 1)]
  
  # zeroRows <- which(apply(selectedCols, 1, function(x) all(x == 0)) )
  # if (length(zeroRows) > 0) {         # if there is at least 1 unchanged shareholder
  #   statements <- statements[-zeroRows, ] # remove unchanged shareholders
  # }
  
  # are all differences in the row equal to zero
  zeroRowTrue <- apply(selectedCols, 1, function(x)
    all(x == 0))
  # remove rows where all differences are zero
  statements <- statements[!zeroRowTrue,]
  
  # reorder shareholders
  statements <- statements %>%
    
    mutate(difference = .[[ncol(statements)]] - .[[2]],) %>%
    
    # sort by abs(difference) and then by the first, last date of the period and then by shareholder
    arrange(desc(abs(difference)), desc(.[[2]]), desc(.[[3]]), shareholder)
  
  # compose table for printing
  #statements <- cbind(statements[1:2], statements[ncol(statements)-1], statements[ncol(statements)], statements[3:(ncol(statements)-2)])
  #statements <- select(statements, 1:2, ncol(statements)-1, ncol(statements), 3:(ncol(statements)-2))
  statements <-
    select(statements,
           1:2,
           last_col(offset = 1),
           last_col(),
           3:last_col(offset = 2))
  
  # set zeroes to blanks
  statements[statements == 0] <- NA # "" for Google spreadsheet
  
  # reorder by the last date column if the report is about last few days
  if (ncol(statements) < 41) {
    #statements <- statements[order(statements[, ncol(statements)]), ]
    #statements <- statements[order(statements[[ncol(statements)]]), ]
    statements <-
      arrange(statements,!!sym(colnames(statements)[[ncol(statements)]]))
  }
  
  # update date column names so that year is last
  colnames(statements)[c(2, 3)] <-
    niceDateColumnName(colnames(statements)[c(2, 3)])
  if (ncol(statements) > 4) {
    colnames(statements)[c(5:ncol(statements))] <-
      niceDateColumnName(colnames(statements)[c(5:ncol(statements))])
  }
  
  # # write sheet
  # write.xlsx(statements, file = "Statements_trial.xlsx", sheetName = shareSymbol,
  #   col.names = TRUE, row.names = FALSE,
  #   append = if (shareSymbol != shareSymbols[1]) TRUE else FALSE,
  #   showNA = FALSE,
  #   password = NULL
  # )
  
  
  # export into Excel sheet
  sheet <- createSheet(wb, sheetName = shareSymbol)
  addDataFrame(
    statements,
    sheet = sheet,
    row.names = FALSE,
    colnamesStyle = csb
  )
  setColumnWidth(sheet, 1, 40)
  createFreezePane(sheet, 2, 5) # freeze from 2. row and 5.column = freeze first row and 4 first columns
  setZoom(sheet, numerator = 65, denominator = 100)
  
  # # Save to the worksheet of the statement differences sheet # much slower than saving in Excel
  # if (any(gs_ws_ls(ss) == shareSymbol)) {
  #   # delete existing worksheet
  #   ss <- gs_ws_delete(ss, ws = shareSymbol)
  # }
  # ss <- gs_ws_new(ss, ws_title = shareSymbol, input = statements, trim = TRUE)
  
  #cat(mem_used())
  #rm(statements)
  
  #gcinfo(TRUE)
  #gc()
  
}



# Implement period request
perRequest <- function(per) {
  
  # per <- repRequests[[2]]
  
  reportDates <<- per$reportDates
  fileName    <<- per$fileName
  
  print(reportDates)
  print(fileName)
  
  # open wb for saving in Excel
  wb <<- createWorkbook()
  
  fob <<- Font(wb, isBold = TRUE)
  csb <<- CellStyle(wb, font = fob)
  
  
  # # Statement differencies spreadsheet
  # sheetURL <- "https://docs.google.com/spreadsheets/d/1Nn77BMt7JJFb_t33wY0qxv7bA4GFxTvhaqsJS6Sz33U/edit?usp=sharing"
  # # register sheet
  # ss <- gs_url(sheetURL)
  
  # add sheets for all shareSymbols
  walk(shareSymbols, addShareSheet)
  
  # Shareholders overview
  print("Start shareholders overview")
  
  shareholderName <- c(
    "Alforme OÜ",
    "Andres Kull",
    "ANDRES JÄRVING",
    "Anu Lill",
    "AS AVRAAL",
    "Jaak Roosaare",
    "Müügiguru OÜ",
    # Jaak Roosaare
    "GAMMA HOLDING INVESTMENT OÜ", # Arvo Nõges
    "SVEN EINAR STEFAN ANDERSSON",
    "CITIBANK ( NEW YORK) / GOVERNMENT OF NORWAY",
    "OÜ GEOPLAST",
    "Osaühing OKOB Invest",
    # Charlie Viikberg
    "CITIBANK (LONDON) / OP CUSTODY LTD CL AC-ESTONIA NP",
    "STATE STREET BANK - WEST CLIENT - TREATY",
    "ALAR SISTOK",
    "Siseinfo OÜ",
    # Oliver Peek
    "Seitse Samuraid OÜ",
    # Rene Ilves
    "AS ALTAMIRA",
    "AS AMALFI", "OÜ ALTAMIRA INVESTEERINGUD",
    # Heldur Meerits
    "ALPHA CAPMAN OÜ",
    "ANDRES NIINEPUU",
    # Andres Niinepuu
    "Endel Palla",
    "HANS PALLA",
    "ANDRUS RAND",
    "OÜ MIDAS INVEST",
    # Andrus Rand
    "Mindgap OÜ", # Raul Koosel
    "OÜ OBSERVA", # Sten Sumberg
    "OÜ BLAURET", # Lauri Lind
    "TAVOLARA OÜ",
    # Kristjan Mitt
    "24 HOLDING OÜ",
    "ELMO SARAPUU",
    # Marko Milius, Elmo Sarapuu
    "MARKO TEIMANN",
    "ICHIBAN OÜ", # Raido Toonekurg, Modera üks omanikke
    "MADIS MÜLLER",
    "RASMUS KATTAI", 
    "MÄRT KUNNUS",
    "ANDRES SONN",
    "ALEKSANDR KOSTIN",
    "Visahing OÜ", "OÜ VISAHING DESIGNER AGENCY", "ERKI KALLAS",
    # LHV foorumis visahing, Visahing OÜ
    "Haak OÜ",
    # Jaan Koort, Andres Koort
    "OÜ KAFIKOR",
    "RAFIKO OÜ",
    "SANDER KARU",
    "AMBIENT SOUND INVESTMENTS OÜ",
    "AVARON ARENEVA EUROOPA FOND",
    "OÜ NOTORIOUS", # Taavet Hinrikus fond
    #"Tekali", # Toivo Ninnas
    "Asrai Capital OÜ",
    # Kristi Saare
    "KIRSCHMANN OÜ",
    # Aare Kirsme, Kirke Kirsme
    "GALINDO BALTIC OÜ",
    "ÜLLE ELLER",
    # Ülle Eller
    "OSAÜHING VARMAGRUPP",
    "MARGUS RIHMA",
    "JAHMEK INVESTMENTS OÜ",
    "ALEKSANDER PAJURI",
    "M.C.E.FIDARSI OSAÜHING",
    "MADIS TALGRE",
    "RIGTOTRIP OÜ",
    "IT INVEST FOUND OÜ",
    "WITTYBOOKING OÜ",
    "OLEGS RADCENKO",
    # Olegs Radcenko
    "PETER ASSETS OÜ",
    "Peter Šaraškin",
    "TARMO TANILAS",
    "WERKDATA OÜ",  # Vahur Meus
    #"RIDGE CAPITAL AS", # Lauri Lind
    "OÜ ARMADIO", "OSAÜHING ARMADIO",
    "ARMADIO OÜ", "OSAÜHING GINARMA", "Teet Järvekülg",
    "OÜ KOHTLA-JÄRVE OMEGA",
    "ANTS AASMA",
    "ARTUR RASVA",
    "PEEP PAJUS",
    "ALO VALLIKIVI",
    "Lõhmus & Oolo Holdings OÜ",
    "Vantage Group OÜ",
    # Marko Oolo
    "OÜ Evispo",
    # Liisi Kirch
    "VIDAS JONAS ŠVABAUSKAS",
    "RELTO CAPITAL OÜ",
    "LAURI MEIDLA",
    # Lauri Meidla
    "VALLO MAIDLA",
    "Joonatan Uusväli",
    "TOOMAS KIVIMÄGI",
    "RANNA KONSULT OÜ",
    "AIVAR SÕERD",
    # Aivar Sõerd
    "MARIO LAMBING",
    #"MAIRO ALUMAA",
    "KAIDO LAURITS",
    "ERGO HIIUS",
    "PAAVO SIIMANN",
    "ERKKI REBANE",
    "RAINER MERI",
    "ANTS NOOT",
    "ANDRES LUME",
    "Toomas Taube",
    "osaühing Hausman & Toran", "OÜ HAUSMAN & TORAN", "IN INVEST OÜ", "Indrek Naur",
    "OÜ KINDLUSTUSMAAKLER TIINA NAUR",
    "AIGARS BARVIKS",
    "INGMAR KRISTJANSEN",
    "ANDRES LUME",
    "RENE JUNKUR",
    "EGERT KAAL",
    "MARGUS LILLO",
    "VALLO PALVADRE",
    "OÜ TRUST IN", # Indrek Neivelt
    "JÜRI ROSS",
    "PINORENA CAPITAL OÜ", "ILLIMAR MATTUS", 
    "MATTUS & CO AG", "INGMAR MATTUS",
    "LEMBIT TALPSEPP",
    "KML Invest OÜ", # Kristjan Liivamägi
    # "Marek Berkman",
    "LII KAARNA",
    "MATI KALME",
    #"SVEN-JARNO LULLU",
    "VAIDO VEEK",
    "JOONATAN UUSVÄLI",
    "UKU RUDISSAAR",
    # Rinja ?
    #"ILLIMAR MATTUS", "INGMAR MATTUS",
    "OÜ CUCULUS CONSULT",
    "OSAÜHING CUMULUS PROJEKT",
    # Arvo Iho
    "ESK GRUPP OÜ",
    "OÜ ESK GRUPP",
    # Elmo Somelar
    "PR INVEST OÜ",
    # Roman Raivet
    "ASM INVESTMENTS OÜ",
    # Alar Kroodo
    "GALIGO OÜ",
    "L-KAPITAL OÜ", # Lauri Paeveer
    "ÜLE OÜ",
    "UG INVESTMENTS OÜ", # Utilitas Group Investments
    "ZENTOP OÜ",
    "ING LUXEMBOURG S.A. AIF ACCOUNT",
    # KJK Sicaf-SIF fond (Soome, Kustaa Äimä ja Jaakko Salmelin)
    "SEB S.A. CLIENT ASSETS UCITS",
    # East Capital fond?
    "JPMORGAN CHASE BANK , NATIONAL ASSOCIATION ON BEHALF OF NON-TREATY CLIENTS",
    "BNYM AS AGT/CLTS",
    "BNYMSANV AS AGENT/CLIENTS BT G 1",
    # The Bank of New York Mellon
    "CACEIS BANK UCITS CLIENTS",
    "THE NORTHERN TRUST COMPANY / MONDRIAN EMERGING MARKETS SMALL CAP EQUITY FUND L.P.",
    "NORDEA BANK ABP/NON TREATY CLIENTS",
    "STATE STREET BANK AND TRUST OMNIBUS ACCOUNT A FUND NO OM01",
    "Clearstream Banking Luxembourg S.A. Clients",
    # E.Miroglio Finance S.A.
    "FIREBIRD REPUBLICS FUND LTD",
    "FIREBIRD AVRORA FUND, LTD.",
    "FIREBIRD FUND L.P.",
    "FIREBIRD NEW RUSSIA FUND",
    "LHV PENSIONIFOND EESTI",
    "FMI UABORION SECURITIES",
    "TRIGON BALTI FOND",
    "TRIGON DIVIDENDIFOND",
    "COMPENSA LIFE VIENNA INSURANCE GROUP SE",
    "AKTSIASELTS TAGABERGI KINNISVARA",
    # Raul Tagaväli
    "HAKK SOFTWARE OÜ",
    #"HZ VALDUSE OÜ", # Heiki Zupping
    "WOOD GROUSE CONSULTING OÜ",
    # Urmas Mõttus
    "KERME INVESTEERINGUD OÜ",
    # Peeter Mänd
    "MIEMMA HOLDING OÜ",
    # Viljar Arakas
    "JUHTMEJUKUD OÜ",
    # Erki Püve
    "OÜ JUSTKULL",
    # Erko Kundla (CV Keskus üks endine omanik)
    "TIIU LUMBERG", "TÕNU LUMBERG",
    "WOOD GROUSE CONSULTING OÜ",
    "VARMAGRUPP OÜ",
    "MARGUS RIHMA",
    "OSAÜHING SETH",
    # Marek Aigro
    "OÜ VIROMEENA", # Gunnar ja Märt Kraft(OÜ Viromeena)
    "Kakssada Kakskümmend Volti OÜ",
    "DESOKSÜRIBONUKLEIINHAPE DNA OÜ",
    "LAME MAAKERA OÜ",
    "KÕVER AEGRUUM OÜ",
    "KUU ON PÄIKE OÜ",  # Raivo Hein
    "OÜ TRAILBORG" # Peedo Pihlak
  )
  
  # get statements
  statements <-
    getStatements(reportDates =  reportDates,
                  shareholderName = shareholderName) %>%
    
    # summarise securities of shareholder by share symbols and dates
    group_by(shareholder, symbol, date) %>%
    summarise(securities = sum(securities), .groups = "drop_last") %>%
    
    # securities in different dates in separate columns
    spread(date, securities) %>% data.frame()
  
  # set missing securities to 0
  statements[is.na(statements)] <- 0
  
  #replace daily securities to daily differences from column 4
  statements <- securities2differences(statements, fromCol = 4)
  
  # securities changes
  statements <- statements %>%
    
    mutate(difference = .[[ncol(statements)]] - .[[3]])
  
  # compose table for printing
  #statements <- cbind(statements[1:3], statements[ncol(statements)-1], statements[ncol(statements)], statements[4:(ncol(statements)-2)])
  #statements <- select(statements, 1:3, ncol(statements)-1, ncol(statements), 4:(ncol(statements)-2))
  statements <-
    select(statements,
           1:3,
           last_col(offset = 1),
           last_col(),
           4:last_col(offset = 2))
  
  # update date column names so that year is last
  colnames(statements)[c(3, 4)] <-
    niceDateColumnName(colnames(statements)[c(3, 4)])
  if (ncol(statements) > 5) {
    colnames(statements)[c(6:ncol(statements))] <-
      niceDateColumnName(colnames(statements)[c(6:ncol(statements))])
  }
  
  # set zeroes to blanks
  statements[statements == 0] <- NA # "" for Google spreadsheet
  
  
  # # write sheet
  # sheetName <- "Shareholders"
  # write.xlsx(statements, file = "Statements_trial.xlsx", sheetName = sheetName,
  #            col.names = TRUE, row.names = FALSE,
  #            append = if (sheetName != shareSymbols[1]) TRUE else FALSE,
  #            showNA = FALSE,
  #            password = NULL
  # )
  
  
  # export into Excel sheet
  sheetName <- "Shareholders"
  sheet <- createSheet(wb, sheetName = sheetName)
  addDataFrame(
    statements,
    sheet = sheet,
    row.names = FALSE,
    colnamesStyle = csb
  )
  setColumnWidth(sheet, 1, 40)
  createFreezePane(sheet, 2, 6) # freeze from 2. row and 6.column = freeze first row and 5 first columns
  setZoom(sheet, numerator = 65, denominator = 100)
  
  # # Save to the worksheet of the selected shareholders
  # if (any(gs_ws_ls(ss) == sheetName)) {
  #   # delete existing worksheet
  #   ss <- gs_ws_delete(ss, ws = sheetName)
  # }
  # ss <- gs_ws_new(ss, ws_title = sheetName, input = statements, trim = TRUE)
  
  #} # end shareholders overview
  
  # Save Excel file
  print("Save workbook")
  saveWorkbook(wb, paste0(fileName, ".xlsx"))

} # end perRequest

# Report for all requested periods
walk(repRequests, perRequest)


dbDisconnect(con)

