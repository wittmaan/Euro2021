
library(rvest)
library(data.table)
library(textclean)
library(stringr)

## fetch group data stuff

dataRaw <- read_html("https://www.uefa.com/uefaeuro-2020/standings/") %>%
  html_nodes(xpath = "//table[@class='table table--standings']") 

result <- NULL
for (ii in seq_along(dataRaw)) {
  country <- dataRaw[[ii]] %>% 
    html_table() %>%
    .[,1] %>%
    replace_non_ascii() %>%
    str_replace_all(" Playing now", "") %>%
    strsplit(" ")
  
  result <- rbind(result, data.table(
    group=LETTERS[ii],
    team=lapply(country, function(x) { 
      if (!is.na(x[3])) {
        paste(x[2], x[3])   
      } else {
        x[2] 
      }}) %>% unlist(),
    codes=lapply(country, function(x) { x[1] }) %>% unlist()
  ))
}

## fetch betting odds

dataRaw <- read_html("https://www.oddschecker.com/football/euro-2020/winner") %>%
  html_nodes(xpath = "//*[(@id = 'oddsTableContainer')]") 

dataRawOdds <- dataRaw %>%
  html_nodes(xpath = "table") %>%
  html_table() %>%
  as.data.table() 

dataProvider <- dataRaw %>% 
  html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'bk-logo-click', ' ' ))]") %>%
  html_attr("title") %>%
  unique()

indEmpty <- which(dataRawOdds$X1 == "")
dataRawOdds <- dataRawOdds[(indEmpty[length(indEmpty)]+1):nrow(dataRawOdds),]

# forth last column is just a separator, so remove it
dataRawOdds[, (ncol(dataRawOdds)-3) := NULL]

indEmptyCols <- colSums(dataRawOdds=="") == 24
dataRawOdds <- dataRawOdds[,!indEmptyCols, with=FALSE]

colnames(dataRawOdds) <- c("team", dataProvider[!indEmptyCols[-1]])
providerNames <- colnames(dataRawOdds)[-1]

dataRawOdds[, (providerNames) := lapply(.SD, function(x) {
  lapply(x, function(y) {
    if (nchar(y) > 0) {
      eval(parse(text = y)) 
    } else {
      NA
    }
  }) %>% unlist()
}), .SDcols = providerNames]


## skip bookmakers without entries
dataRawOdds <- dataRawOdds[, which(colSums(is.na(dataRawOdds)) < nrow(dataRawOdds)), with=FALSE]

## replace missing values with row means 
for (ii in seq_along(dataRawOdds)) {
  dataRawOddsRow <- dataRawOdds[ii,2:ncol(dataRawOdds)]
  indMissing <- which(is.na(dataRawOddsRow))
  if (length(indMissing) > 0) {
    set(dataRawOdds, i=ii, j=indMissing+1, value=round(mean(unlist(dataRawOddsRow), na.rm=T)))
  }
}

setkey(result, "team")
setkey(dataRawOdds, "team")

result <- result[dataRawOdds]
result <- result[order(group, team)]

fwrite(result, file = paste0("bettingOdds", format(Sys.Date(), "%Y%m%d"), ".csv"), sep = ";")
write.table(as.data.frame(result), paste0("bettingOdds", format(Sys.Date(), "%Y%m%d"), ".csv"), quote = FALSE, row.names = FALSE, sep = ";")
