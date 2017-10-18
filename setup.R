library(ggthemes)
library(readxl)
library(stringr)
library(tidyverse)
library(shiny)

remove(list = ls())

sheets <- excel_sheets("data.xlsx")
sheets <- sheets[-length(sheets)] # remove last sheet - take out of final code
sheets


for (i in seq_along(1:length(sheets))) {
  
  sheetIn <- read_xlsx("data.xlsx", sheet = i)
  sheetIn$wbs <- sheets[i]
  
  assign(str_c("sheet", i), sheetIn)
  
}

remove(i, sheetIn)

data <- bind_rows(sheet1, sheet2, sheet3, sheet4) # AUTOMATE

colnames(data) <- tolower(colnames(data))


