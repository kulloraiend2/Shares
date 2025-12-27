library(RMySQL)
library(tidyverse)
library(dbplyr)
library(lubridate)
library(stringr)
library(googlesheets4)

# tidyverse_update() # update tidyverse packages
 
# install.packages("installr") # install R installer
# installr::updateR() # updating R. Running “updateR()” will detect if there is a new R version available, and if so it will download+install it (etc.).

source("SharesCommon.R")

# Days before holidays at the end of the month or week
before_holiday <- as.Date(c("2024-03-28"))

# Connect to DB
con <- dbConnect(
  RMySQL::MySQL(),
  user = localUserName,
  password = localUserPassword,
  dbname = dbName,
  host = host
) #,
#encoding = "native.enc")  # "UTF-8"
dbListTables(con)

share_insert <- "INSERT INTO share (symbol, company) VALUES
('EFT1T', 'EfTEN Real Estate Fund III'),
('HAE1T', 'Harju Elekter'),
('LHV1T', 'LHV Group'),
('MRK1T', 'Merko Ehitus'),
('NCN1T', 'Nordecon'),
('PRF1T', 'PRFoods'),
('SFG1T', 'Silvano Fashion Group'),
('TAL1T', 'Tallink Grupp'),
('TKM1T', 'Tallinna Kaubamaja Grupp'),
('TVEAT', 'Tallinna Vesi')
('TSM1T', 'Tallinna Sadam')"

share_insert <- "INSERT INTO share (symbol, company) VALUES
('ARC1T', 'Arco Vara'),
('BLT1T', 'Baltika'),
('EEG1T', 'Ekspress Grupp')"

share_insert <- "INSERT INTO share (symbol, company) VALUES
('CPA1T', 'Coop Pank')"

share_insert <- "INSERT INTO share (symbol, company) VALUES
('EGR1T', 'Enefit Green')"

share_insert <- "INSERT INTO share (symbol, company) VALUES
('HPR1T', 'Hepsor AS')"

share_insert <- "INSERT INTO share (symbol, company) VALUES
('INF1T', 'Infortar AS')"

#insertRec <- dbGetQuery(con, share_insert)

# update company
# updateRec <- dbGetQuery(con, "UPDATE share SET company='EfTEN Real Estate Fund AS' WHERE symbol='EFT1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='AS Harju Elekter Group' WHERE symbol='HAE1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='AS LHV Group' WHERE symbol='LHV1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='aktsiaselts MERKO EHITUS' WHERE symbol='MRK1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Nordecon AS' WHERE symbol='NCN1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='AS PRFoods' WHERE symbol='PRF1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Aktsiaselts Silvano Fashion Group' WHERE symbol='SFG1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Aktsiaselts Tallink Grupp' WHERE symbol='TAL1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='TKM Grupp AS' WHERE symbol='TKM1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='AS Tallinna Sadam' WHERE symbol='TSM1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Tallinna Vesi AS' WHERE symbol='TVEAT'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Arco Vara AS' WHERE symbol='ARC1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='AS Baltika' WHERE symbol='BLT1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Aktsiaselts Ekspress Grupp' WHERE symbol='EEG1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Coop Pank aktsiaselts' WHERE symbol='CPA1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Enefit Green AS' WHERE symbol='EGR1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Hepsor AS' WHERE symbol='HPR1T'")
# updateRec <- dbGetQuery(con, "UPDATE share SET company='Infortar AS' WHERE symbol='INF1T'")


# shares <- dbGetQuery(con, "SELECT * FROM share")

# Common statement date unknown at the beginning
commonStatementDate <- NA

# Get shereholder ids by names
getShareholderIds <- function(names) {
  #names <- statements$shareholderName
  
  # remove not allowed characters
  names <- str_remove_all(names, "[']")
  
  # get shareholder id by name
  getShID <- function(name) {
    # name <- names[1]
    # get shareholder id by name. Add new shareholder if missing
    repeat {
      
      # R 4.0.5 oli vaja kasutada fn enc2native
      # shareholderId <-
      #   dbGetQuery(con, enc2native(
      #     paste0("SELECT id FROM shareholder WHERE name='", name, "'")
      #   )) # kindlusta et paste(..) string oleks native (latin1) encoded
      
      shareholderId <-
        dbGetQuery(con, 
          paste0("SELECT id FROM shareholder WHERE name='", name, "'")
        ) # kindlusta et paste(..) string oleks native (latin1) encoded
      
      nrIds <- nrow(shareholderId)
      
      if (nrIds > 0) {
        break
      } # some ids found
      
      # add new shareholder
      
      # R 4.0.5 oli vaja kasutada fn enc2native
      # newSh <-
      #   dbGetQuery(con, enc2native(
      #     paste0("INSERT INTO shareholder (name) VALUES ('", name, "')")
      #   )) # kindlusta et paste(..) string oleks native (latin1) encoded
      
      newSh <-
        dbGetQuery(con,
          paste0("INSERT INTO shareholder (name) VALUES ('", name, "')")
        ) # kindlusta et paste(..) string oleks native (latin1) encoded
      
    }
    
    if (nrIds == 1) {
      # found one shareholder
      return(shareholderId$id[1])
      
    } else {
      # many shareholders with this name
      stop(paste(
        "getShareholderIds: ",
        nrIds,
        "shareholders with name",
        name
      ))
      
    }
    
  }
  
  # get ids for all names
  ids <- map_int(names, getShID)
  
}

# Save statements in the DB
saveStatementInDB <- function(statements, shareId, statementDate) {
  if (nrow(statements) == 0)
    return() # nothing to insert
  
  # delete all statement records for share_id where date=statementDate to allow repeated insert for the same date
  # For shareId = NULL all shares are deleted for statementDate
  deleteStatementInDB <- function(statementDate, shareId = NULL) {
    
    # shareId <- 4
    # statementDate <- as.Date("2025-05-26")
    
    # Number of records in a batch, batch sise
    limit <- 10000
    
    # Verify the existence of statement records for share_id where date=statementDate 
    statementsExist <- function(statementDate, shareId = NULL) {
    
      # Take statement records
      statementRecs <- tbl(con, in_schema(dbName, "statement")) %>% 
        
        # Constrain with statement date
        filter(date == statementDate) 
      
      # Share is defined
      if (!is.null(shareId)) {
        
        # constrain also share
        statementRecs <- statementRecs %>% 
          filter(share_id == shareId)
        
      }
      
      # Take some statement records 
      statementRecs <- statementRecs %>%  
        head(n = 1) %>% 
        # show_query() %>% 
        collect()
      
      # the existence of statement records
      nrow(statementRecs) > 0
    
    } # end statementsExist()
    
    # (statementsExist(as.Date("2025-05-26")))
    # (statementsExist(as.Date("2025-05-26"), shareId = 4))
    # (statementsExist(as.Date("2025-04-02")))
    # (statementsExist(as.Date("2025-04-02"), shareId = 4))
    
    # While statements exist
    while (statementsExist(statementDate, shareId = shareId)) {
      
      # Delete a batch of statements
      # Create SQL delete statement
      deleteSQL <- paste0(
        "DELETE FROM statement WHERE date='", statementDate, "'",
        if (!is.null(shareId)) paste0(" AND share_id =", shareId),
        
        # Constrain number of records 
        " LIMIT ", limit                  
      )
      delRec <- dbGetQuery(con, deleteSQL)
      
    } # end while()
    
  } # end deleteStatementInDB()
  
  # Delete all statements in one day
  # (deleteStatementInDB(as.Date("2025-08-22")))
  
  # delete all statement records for share_id where date=statementDate to allow repeated insert for the same date
  deleteStatementInDB(statementDate, shareId = shareId)
  
  # delete all statement records for share_id where date=statementDate to allow repeated insert for the same date
  # 02.04.2025:  Old version, ends with error
  # Error in .local(conn, statement, ...) :
  # could not run statement: The total number of locks exceeds the lock table size
  # 
  # delRec <- dbGetQuery(
  #   con,
  #   paste0(
  #     "DELETE FROM statement
  #                                  WHERE date='",
  #     statementDate,
  #     "' AND
  #                                       share_id =",
  #     shareId
  #   )
  # )
  
  # Delete all statements in one day
  # statementDate <- as.Date("2025-06-02")
  # delRec <- dbGetQuery(con, paste0("DELETE FROM statement WHERE date='", statementDate, "'"))
  
  # beginning of the Insert statement
  insert_beg <-
    "INSERT INTO statement (share_id, shareholder_id, securities, date) VALUES "
  
  # add values
  addValue <- function(id, securities) {
    paste0("(",
           shareId,
           ",",
           id,
           ",",
           securities,
           ",'",
           statementDate,
           "')")
    
  }
  
  values <-
    map2_chr(statements$shareholder_id, statements$securities, addValue)
  
  # concatenate insert statement
  insert <- paste0(insert_beg, str_c(values, collapse = ","))
  
  # execute the insert statement
  insertRec <- dbGetQuery(con, insert)
  
}

# Process sharesymbol spreadsheet
# shareSymbol <-  "ARC1T"   "HAE1T" 
processSpreadsheet <- function(shareSymbol) {
  
  # Retrieve spreadsheet-specific metadata
  # gs4_get(sheetURL)
  
  # read statements page of the share
  statements <-
    read_sheet(sheetURL, sheet = shareSymbol, col_names = FALSE) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    # Keep only the first column
    select(1) %>% 
    set_names( c("shareholderName")) %>% 
    mutate(
      shareholderName = str_trim(shareholderName)
    )
  
  # get the company from spreadsheet
  issuer <- filter(statements, str_detect(shareholderName, "Issuer ")) %>% 
    mutate(
      shareholderName = str_trim(str_remove(shareholderName, "Issuer ")) 
    ) %>% 
    pull(shareholderName)
  
  # wait to avoid error Too Many Requests (RFC 6585) (HTTP 429)
  Sys.sleep(3)
  
  # get the share
  share <-
    dbGetQuery(con,
               paste0("SELECT * FROM share WHERE symbol='", shareSymbol, "'"))
  
  # check the company
  if (issuer != share$company) {
    stop(paste(
      "Issuer",
      issuer,
      "does not match to the expected share",
      shareSymbol
    ),
    call. = FALSE)
  }
  
  # get shareId
  shareId <- share$id
  
  # get statement date
  statementDate <- statements %>% 
    filter(str_detect(shareholderName, "As of the end of the day ")) %>% 
    mutate(
      shareholderName = str_trim(str_remove(shareholderName, "As of the end of the day "))
    ) %>% 
    pull(shareholderName) %>% 
    as.Date(format = "%d.%m.%Y")
  
  # Increase special, or  statement dates before holiday
  if (statementDate %in% before_holiday) statementDate <- statementDate + 1
 
  # All statement dates should be equal
  if(!is.na(commonStatementDate) && statementDate != commonStatementDate) {
    
    stop("Statement date not equal to common statement date ", commonStatementDate, 
         ". Share = ", shareSymbol, call. = FALSE)
    
  }
  
  # Assign value by first share 
  if (is.na(commonStatementDate)) commonStatementDate <<- statementDate
  
  # Merge rows split by mistake
  for (j in seq_len(nrow(statements) - 1)) {
    
    # j <- 10025
    
    # Values in the current row and next row 
    value <- statements$shareholderName[j]
    value_next <- statements$shareholderName[j + 1]
    
    if (
        # value starts with sequence number and
        str_detect(value, "^[0-9]+. ") &&
        # does not end with %
        !str_detect(value, "%$") &&
        # Value in the next row does not start wit sequence number and
        !str_detect(value_next, "^[0-9]+. ") &&
        # ends with %
        str_detect(value_next, "%$") ) {
      
      # Merge two rows
      statements$shareholderName[j] <- paste(value, value_next)
      
      # Clear next not needed row
      statements$shareholderName[j + 1] <- ""
      
    }
    
  } # end for
  
  # prepare statement for inserting
  statements <- statements %>%
    
    # Exclude the table of statements
    # exclude blank rows
    filter(!is.na(shareholderName)) %>% 
    # exclude table header
    # rows that don't end with %
    filter(str_ends(shareholderName, "%")) %>%
    
    mutate(
      # remove %
      shareholderName = str_remove(shareholderName, " %$"),
      # remove sequence number from beginning
      shareholderName = str_remove(shareholderName, "^[0-9]*. "),
      # remove after space at the end
      shareholderName = str_remove(shareholderName, " [0-9]+[.]*[0-9]*$"),
      
      # extract number of securities 
      securities = str_extract(shareholderName, " [0-9,]+$"),
      # remove comma
      securities = str_remove_all(securities, ","),
      # convert number of securities to integer
      securities = as.integer(securities),
      
      # remove number of securities in the shareholder string
      # remove after space at the end 
      shareholderName = str_remove(shareholderName, " [0-9]+[[,]*[0-9]]*$"), 
      shareholderName = str_to_upper(str_trim(shareholderName)),

      # set shareholder id-s
      shareholder_id = getShareholderIds(shareholderName)
    ) %>%
    
    # summarize securities of a shareholder
    group_by(shareholder_id) %>%
    summarise(securities = sum(securities), .groups = "drop_last")
  
  # save statements
  saveStatementInDB(statements, shareId, statementDate)
  
}


# set locale to English
# Versioonis  R 4.0.5 oli vaja sättida, aga enam mitte. Vaikimisi locale sobib.
# locale_str <- Sys.setlocale("LC_ALL", "English")
#Sys.getlocale()

# statementDate <- today()#-1 # as the end of yesterday
# # if yesterday was weekend then take friday
# if (wday(statementDate, label = TRUE)=="L") {
#   statementDate <- statementDate -1
# } else if (wday(statementDate, label = TRUE)=="P") {
#   statementDate <- statementDate -2
# }


# googlesheets4 will, by default, help you interact with Sheets as an authenticated Google user. 
# If you don’t plan to write Sheets or to read private Sheets, use gs4_deauth() to indicate there is no
# need for a token. See the article googlesheets4 auth for more.
gs4_deauth()

# Statements spreadsheet
sheetURL <-
  "https://docs.google.com/spreadsheets/d/1r3LA2abVzEL6g-WyY_D1PaZGbQcZjXK8QY2OTVux-GQ/edit?usp=sharing"
  # "https://docs.google.com/spreadsheets/d/1r3LA2abVzEL6g-WyY_D1PaZGbQcZjXK8QY2OTVux-GQ/edit?gid=1864033360#gid=1864033360"

# Proov
# df <- read_sheet(sheetURL, sheet = "ARC1T")

# process spreadsheets for all shares
walk(shareSymbols, processSpreadsheet)


dbDisconnect(con)

# prepare statement differencies
print("Prepare statement differencies")
source("StatementDifferencies.R", encoding = 'UTF-8') # UTF-8 vajalik täpitähtede tõttu shareholderName väärtustes.


# # Tee aktsia split
# 
# # Viimane päev, millal aktsiate arv  vajab muutmist
# statementDate <- as.Date("2022-07-07") 
# # "LHV1T"
# share_id <- 3 
# 
# comp_statements <- dbGetQuery(con, 
#                               paste0("SELECT * FROM statement WHERE 
#                                      share_id = ", share_id, " and date = '", statementDate, "';")
#                               ) %>% 
#   arrange(desc(securities))
# 
# str(comp_statements)
# 
# # split 
# # split 1:split_multiplier
# split_multiplier <- 10 
# dbExecute(con, 
#           paste0("UPDATE statement SET securities = securities * ", split_multiplier, " WHERE
#                   share_id = ", share_id, " and date <= '", statementDate, "';")
#           )


# x <- tibble(
#   a = c(1, 2),
#   b = c(3, 4)
# )
# y <- tibble(
#   a = c(1, 1),
#   c = c(5, 6)
# )
# 
# left_join(x, y, by = "a")
