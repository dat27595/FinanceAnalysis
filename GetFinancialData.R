# Credit to: https://stackoverflow.com/questions/50355094/how-to-get-financial-information-of-stocks-in-r
#Get Price Data
library(quantmod)
getSymbols("GOOG")
GOOG <- subset(GOOG, select = -GOOG.Adjusted)
# Get Financial Statement
getFin <- function(stock){
  if ("rvest" %in% installed.packages()) {
    library(rvest)
  }else{
    install.packages("rvest")
    library(rvest)
  }
  for (i in 1:length(stock)) {
    tryCatch(
      {
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/financials?p=",stock[i])
        wahis.session <- html_session(url)                                
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        IS <- p[[1]]
        colnames(IS) <- paste(IS[1,])
        IS <- IS[-c(1,5,12,20,25),]
        names_row <- paste(IS[,1])
        IS <- IS[,-1]
        IS <- apply(IS,2,function(x){gsub(",","",x)})
        IS <- as.data.frame(apply(IS,2,as.numeric))
        rownames(IS) <- paste(names_row)
        temp1 <- IS
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/balance-sheet?p=",stock[i])
        wahis.session <- html_session(url)
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        BS <- p[[1]]
        colnames(BS) <- BS[1,]
        BS <- BS[-c(1,2,17,28),]
        names_row <- BS[,1]
        BS <- BS[,-1] 
        BS <- apply(BS,2,function(x){gsub(",","",x)})
        BS <- as.data.frame(apply(BS,2,as.numeric))
        rownames(BS) <- paste(names_row)
        temp2 <- BS
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/cash-flow?p=",stock[i])
        wahis.session <- html_session(url)
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        CF <- p[[1]]
        colnames(CF) <- CF[1,]
        CF <- CF[-c(1,3,11,16),]
        names_row <- CF[,1]
        CF <- CF[,-1] 
        CF <- apply(CF,2,function(x){gsub(",","",x)})
        CF <- as.data.frame(apply(CF,2,as.numeric))
        rownames(CF) <- paste(names_row)
        temp3 <- CF
        assign(paste0(stock[i],'.f'),value = list(IS = temp1,BS = temp2,CF = temp3),envir = parent.frame())
        
      },
      error = function(cond){
        message(stock[i], "Give error ",cond)
      }
    )
  }
}
symbols <- c("NVDA", "GOOG", "GE")
getFin(symbols)

GOOG.f$IS
GOOG.f$BS
GOOG.f$CF

