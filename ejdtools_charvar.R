# ----- template.R.v160625s ----- #

# ========================== #
# ========================== #
# ===== Documentation. ===== #
# ========================== #
# ========================== #
#
# Project:      ejdtools_charvar
#
# Description:  This program creates a basic codebook of variables in a dataset by describing basic characteristics for each variable.
#
# 160625s [Daza_EJ] --- Created program.
#







# ============================ #
# ============================ #
# ===== Data Management. ===== #
# ============================ #
# ============================ #



### Characterize all variables.
ejdtools_charvar <- function(dataset.datain) {
  
  # Index variable names for use below.
  for (j in 1:dim(dataset.datain)[2]) {
    
    dataset.codebook.temp1 <- as.data.frame(table(dataset.datain[,j]))
    dataset.codebook.temp1$nameindex <- j
    if ( j == 1 ) dataset.codebook.temp2 <- dataset.codebook.temp1
    if ( j > 1 ) dataset.codebook.temp2 <- rbind(dataset.codebook.temp2, dataset.codebook.temp1)
    
  }
  names(dataset.codebook.temp2) <- c("value", "valuecount", "nameindex")
  
  # Construct variable characteristics.
  dataset.codebook.temp2$value <- as.character(dataset.codebook.temp2$value)
  dataset.codebook.temp2$min <- NA
  dataset.codebook.temp2$max <- NA
  for ( j in 1:length(names(dataset.datain)) ) {
    
    dataset.codebook.temp3 <- dataset.codebook.temp2[which(dataset.codebook.temp2$nameindex == j),]
    dataset.codebook.temp3$value <- as.character(dataset.codebook.temp3$value)
    dataset.codebook.temp3$freq <- dataset.codebook.temp3$valuecount / sum(dataset.codebook.temp3$valuecount)
    dataset.codebook.temp3$freqnoNA <- dataset.codebook.temp3$valuecount / sum(dataset.codebook.temp3$valuecount[dataset.codebook.temp3$value != ""])
    for ( i in 1:length(dataset.codebook.temp3$value) ) {
      if ( dataset.codebook.temp3$value[i] == "" ) dataset.codebook.temp3$freqnoNA[i] <- NA 
    }
    dataset.codebook.temp3$name <- names(dataset.datain)[j]
    dataset.codebook.temp3$type <- class(dataset.datain[,j])
    dataset.codebook.temp3$mean <- NA
    dataset.codebook.temp3$median <- NA
    dataset.codebook.temp3$min <- NA
    dataset.codebook.temp3$max <- NA
    dataset.codebook.temp3$sd <- NA
    if ( class(dataset.datain[,j]) != "character" & class(dataset.datain[,j]) != "factor" ) {
      
      dataset.codebook.temp3$mean <- mean(as.numeric(dataset.datain[,j]), na.rm=TRUE)
      dataset.codebook.temp3$median <- median(as.numeric(dataset.datain[,j]), na.rm=TRUE)
      dataset.codebook.temp3$min <- min(as.numeric(dataset.codebook.temp3$value), na.rm=TRUE)
      dataset.codebook.temp3$max <- max(as.numeric(dataset.codebook.temp3$value), na.rm=TRUE)
      dataset.codebook.temp3$sd <- sd(as.numeric(dataset.datain[,j]), na.rm=TRUE)
      
    }
    
    if ( j == 1 ) dataset.codebook <- dataset.codebook.temp3
    else dataset.codebook <- rbind(dataset.codebook, dataset.codebook.temp3)
    
  }
  for ( i in 1:length(dataset.codebook$value) ) {
    if ( dataset.codebook$value[i] == "" ) dataset.codebook$freqnoNA[i] <- NA
  }
  dataset.codebook <- dataset.codebook[
    c(
      "nameindex",
      "name",
      "type",
      "mean",
      "median",
      "min",
      "max",
      "sd",
      "value",
      "valuecount",
      "freq",
      "freqnoNA"
    )
    ]
  dataset.codebook$pct <- round(dataset.codebook$freq * 100, digits=2)
  dataset.codebook$pctnoNA <- round(dataset.codebook$freqnoNA * 100, digits=2)
  
  # Return codebook.
  return(dataset.codebook)
  
}







# ====================================================================== #
# ====================================================================== #
# ===== Miscellaneous (e.g., code, help files/websites, examples). ===== #
# ====================================================================== #
# ====================================================================== #