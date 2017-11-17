library(ggthemes)
library(scales)
library(readxl)
library(XLConnect)
library(lubridate)
library(DT)
library(stringr)
library(tidyverse)
library(shinyWidgets)
library(shiny)

remove(list = ls())

### Load Data
#data_file <- 'expData.xlsx'
# data_file <- '//10.6.100.235/tacnasa1$/Public/J8/Gardner/FY18 Budget Forecasting/app/expData.xlsx'
# expData <- read_xlsx(data_file, sheet = 1, trim_ws = TRUE)
expData <- readRDS("expData.RDS")


######

temp <- expData %>% filter(dir == "J3", fiscalYear==2017, dirfy17 != "J3")

######

# Filter down to 2017 J3 obligations
expData <- expData %>% filter(dir == "J3", fiscalYear == 2017)
expData <- expData %>% group_by(dirfy17, fiscalDay) %>% 
                       summarize(obligation = sum(obligation)) %>% 
                       complete(fiscalDay = 1:365,
                                fill      = list(obligation = 0)) %>% 
                       mutate(cumOblig = cumsum(obligation)) %>% 
                       select(dirfy17, obligation, cumOblig, everything()) %>% 
                       ungroup() %>% 
                       arrange(fiscalDay)
expData$dirfy17 <- factor(expData$dirfy17, levels = c("J35",
                                                      "J39",
                                                      "J3X",
                                                      "SOCCE SAM",
                                                      "SOCCE LCB",
                                                      "J3"))

# Build plot
plot <- ggplot(expData, aes(x = fiscalDay, y = cumOblig))
plot <- plot + geom_area(aes(color = dirfy17, fill = dirfy17)) +
               theme_light() +
               xlab("Day of Fiscal Year") + ylab("Obligations") + labs(title = "FY17 Obligations") +
  scale_y_continuous(label = dollar_format())
  
plot




# Sector <- rep(c("S01","S02","S03","S04","S05","S06","S07"),times=7)
# Year <- rep(c("1950","1960","1970","1980","1990","2000","2010"),each=7)
# Value <- runif(49, 10, 100)
# df <- data.frame(Sector,Year,Value)
# 
# gg <- ggplot(df, aes(x=as.numeric(as.character(Year)), y=Value))
# gg <- gg + geom_area(aes(colour=Sector, fill=Sector))
# gg
