setwd("~/1.ECI/11.DT/13.Nodriza/Initial_replenishment")

library(tidyverse)
library(lattice)
library(caret)
library(purrr)
library(ggthemes)
library(ggrepel)
library(ggplot2)
library(lubridate)
library(aes)
library(grDevices)
library(readxl)

needs <- read_xlsx("~/1.ECI/11.DT/13.Nodriza/Initial_replenishment/Needs_per_reference.xlsx",1)
stock <- read_xlsx("~/1.ECI/11.DT/13.Nodriza/Initial_replenishment/Stock_available_in_each_center.xlsx",1)

ro <- nrow(needs)
co <- ncol(stock)

asign <- data.frame(matrix(nrow = ro, ncol = co))
count <- data.frame(matrix(nrow = ro, ncol = 1))
vec <- names(stock)
names(asign) <- vec
asign[is.na(asign)] <- 0 
count[is.na(count)] <- 0 
##names(count)<- "asign"
asign[,1] <- stock[,1]
for (i in 1:ro) {
  asign[i,2] <-min(needs[i,2],stock[i,2])
  count[i,1] <- asign[i,2]
}

for (j in 3:co) {
  for (i in 1:ro) {
    asign[i,j] <- min(stock[i,j], needs[i,2]-count[i,1])
    count[i,1] <- count[i,1] + asign[i,j]
  }
}
