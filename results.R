################ opg videnskabsteori 1 semester dataanalyse #################

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis,httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr, caret, lars, tidyverse, psych, dygraphs,
               vioplot, gapminder, nycflights13, gapminder, Lahman, ISLR2,
               hms, feather, haven, readxl, DBI, jsonlite, xml2, lvplot,dygraphs, vioplot 
)


## indlæser data
replies <- read.csv("data/reply.csv")
View(replies)


## omdøber kolonner
colnames(replies) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(replies)

##################### Kønsfordeling ###########################################

## udvælger kolonner til køns popolation
population <- dplyr::select(replies, gender)
View(population)


typeof(population$gender)


## finder antal mænd
gender_male <- as.logical(population$gender == "False") 
typeof(gender_male)
male <- sum(gender_male)
typeof(male)
male
## finder antal kinder
gender_female <- as.logical(population$gender == "True") 
typeof(gender_female)
female <- sum(gender_female)
typeof(female)
female

genders_text <- c("Mand", "Kvinde")
## laver dataframe
genders <- data.frame(
  Deltager = c("mand 45%","kvinde 55%"),
  value = c(male, female)
  )


### pie chart for kønsfordeling
  ggplot(genders, aes(x="", y=value, fill=Deltager)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start = 0) +
  labs(title = "Fordelingen mellem mænd og kvinder i undersøglsen i %") + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank() 
  )


#### digitalt indfødte og digitale invandre ##################################
  
  
  
  ## udvælger kolonner til digitalt indfødte  popolation
  population <- dplyr::select(replies, withOrWithout)
  View(population)
  typeof(population$withOrWithout)
  
  ## finder antal digitale invandre
  withOut <- as.logical(population$withOrWithout == "False") 
  typeof(withOut)
  without <- sum(withOut)
  typeof(without)
  without
  ## finder antal digitale indfødte
  with <- as.logical(population$withOrWithout == "True") 
  typeof(with)
  With <- sum(with)
  typeof(With)
  With
  

  ## laver dataframe
  withOrWithout <- data.frame(
    Deltager = c("Digitalt Indfødte 62.5%","Digitalt Invandre 37.5%"),
    value = c(without, With)
  )
  
  
  ### pie chart for fordeling af digitale indfødte og dem der ikke er
  ggplot(withOrWithout, aes(x="", y=value, fill=Deltager)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start = 0) +
    labs(title = "Fordelingen mellem Digitale Invandre og Digitale Indfødte i undersøglsen i %") + 
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank() 
    )
  
  
#################### kunstig intelligens analyse ALL ###############################
  
  
  replies <- read.csv("data/reply.csv")
  View(replies)
  
  
  ## omdøber kolonner
  colnames(replies) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
  View(replies)
  
  population <- dplyr::select(replies, svar_1, svar_2, svar_3)
  View(population)
  
  
  ## udvælger kolonner til svat_1 ##########################
  ## Ja
  allQ1Y <- as.logical(population$svar_1 == "True") 
  allQ1Y <- sum(allQ1Y)


## Nej
  allQ1N <- as.logical(population$svar_1 == "False") 
  allQ1N <- sum(allQ1N)


  ## udvælger kolonner til svat_2 ##########################
  ## Ja
  allQ2Y <- as.logical(population$svar_2 == "True") 
  allQ2Y <- sum(allQ2Y)

  ## Nej
  allQ2N <- as.logical(population$svar_2 == "False") 
  allQ2N <- sum(allQ2N)

  
  
  ## udvælger kolonner til svat_3 ##########################
  ## Ja
  allQ3Y <- as.logical(population$svar_3 == "True") 
  allQ3Y <- sum(allQ3Y)

  ## Nej
  allQ3N <- as.logical(population$svar_3 == "False") 
  allQ3N <- sum(allQ3N)


  ## laver dataframe og plot
  library(RColorBrewer)
    
    svar1 <- c(allQ1Y, allQ1N)
    svar2 <- c(allQ2Y, allQ2N)
    svar3 <- c(allQ3Y, allQ3N)
 allQ3 <- data.frame(svar1,svar2,svar3)
allQ3 <- as.matrix(allQ3)
coul <- brewer.pal(5, "Set2") 
barplot((allQ3),beside=TRUE,
        col = c("blue","red"),
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Alle",
        names.arg = c("7  -  33", "18  -  22", "12  -  28"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))


## svar 1
prct1y <- as.double(allQ1Y / 40 * 100)
prct1y
prct1n <- as.double(allQ1N / 40 * 100)
svar1 <- c(allQ1Y, allQ1N)
allQ3 <- data.frame(svar1)
allQ3 <- as.matrix(allQ3)
coul <- brewer.pal(5, "Set2") 
barplot((allQ3),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens svar på spørgsmål 1",
        ylab = "Antal deltagere",
        xlab = "1.	Er du tryk ved at computere tager beslutninger om forhold der gælder liv og død?",
        names.arg = c(prct1y, prct1n),
        legend = c("% tryg","% ikke tryk"), args.legend = list(x = "topright", ncol = 1))

## svar 2
allQ2Y

prct2y <- as.integer(allQ2Y / 40 * 100)
prct2y
prct2n <- as.integer(allQ2N / 40 * 100)
svar2 <- c(allQ2Y, allQ2N)
allQ3 <- data.frame(svar2)
allQ3 <- as.matrix(allQ3)
coul <- brewer.pal(5, "Set2") 
barplot((allQ3),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens svar på spørgsmål 2",
        ylab = "Antal deltagere",
        xlab = "2.	Vil du finde det sandsynligt at en computer ved en fejl vil kunne udløse en verdenskrig?",
        names.arg = c(prct2y, prct2n),
        legend = c("% sandsynligt","% ikke sandsynligt"), args.legend = list(x = "topright", ncol = 1))
  

## svar 3
prct3y <- as.integer(allQ3Y / 40 * 100)
prct3y
prct3n <- as.integer(allQ3N / 40 * 100)
svar3 <- c(allQ3Y, allQ3N)
allQ3 <- data.frame(svar3)
allQ3 <- as.matrix(allQ3)
coul <- brewer.pal(5, "Set2") 
barplot((allQ3),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens svar på spørgsmål 3",
        ylab = "Antal deltagere",
        xlab = "3.	Er du tryk ved at lade dig transportere i en førerløs bil kun styret af kunstig intelligens?",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))


### samled svar. svar_1 + svar_2 + svar_3 i prct  spørgsmål allQ2Y og allQ2N er byttet om for at af spejle det skeptiske
hellnowAll <- allQ1N + allQ2Y + allQ3N
hellyesAll <- allQ1Y + allQ2N + allQ3Y
hellnowAll <- hellnowAll / 120 * 100
hellyesAll <- hellyesAll  / 120 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for kunstig intelligens - set for hele gruppen",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de første 3 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

#################### kunstig intelligens analyse for digitale indfødte  ###############################


replies <- read.csv("data/reply.csv")
View(replies)


dinf <- filter(replies, replies$ReplyID & Reply_9 == "True") 
dinf
dinf <- sum(dinf)

## omdøber kolonner
colnames(dinf) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(dinf)

dinf <- dplyr::select(dinf, svar_1, svar_2, svar_3)
View(dinf)


dinfQ1Y <- as.logical(dinf$svar_1 == "True") 
dinfQ1Y <- sum(dinfQ1Y)
dinfQ1Y


dinfQ1N <- as.logical(dinf$svar_1 == "False") 
dinfQ1N <- sum(dinfQ1N)
dinfQ1N


dinfQ2Y <- as.logical(dinf$svar_2 == "True") 
dinfQ2Y <- sum(dinfQ2Y)
dinfQ2Y 

dinfQ2N <- as.logical(dinf$svar_2 == "False") 
dinfQ2N <- sum(dinfQ2N)
dinfQ2N

dinfQ3Y <- as.logical(dinf$svar_3 == "True") 
dinfQ3Y <- sum(dinfQ3Y)
dinfQ3Y

dinfQ3N <- as.logical(dinf$svar_3 == "False") 
dinfQ3N <- sum(dinfQ3N)
dinfQ3N

## laver dataframe og plot
library(RColorBrewer)

svar1 <- c(dinfQ1Y, dinfQ1N)
svar2 <- c(dinfQ2Y, dinfQ2N)
svar3 <- c(dinfQ3Y, dinfQ3N)
dinfQ3 <- data.frame(svar1,svar2,svar3)
dinfQ3 <- as.matrix(dinfQ3)
dinfQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ3),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Digitalt Indfødte",
        names.arg = c("5  -  10", "8  -  7", "4  -  11"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))

  

## svar 1a
a <- as.double(dinfQ1Y / 15 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinfQ1N / 15 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar1 <- c(dinfQ1Y, dinfQ1N)
dinfQ3 <- data.frame(svar1)
dinfQ3 <- as.matrix(dinfQ3)
dinfQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ3),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for kunstig intelligens svar på spørgsmål 1 Digitalt Indfødte",
        ylab = "Antal deltagere",
        xlab = "1.	Er du tryk ved at computere tager beslutninger om forhold der gælder liv og død?",
        names.arg = c(prct1y,prct1n),
        legend = c("% tryg", "% ikke tryg"), args.legend = list(x = "topright", ncol = 1))


## svar 2a

a <- as.double(dinfQ2Y / 15 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinfQ2N / 15 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar2 <- c(dinfQ2Y, dinfQ2N)
dinfQ3 <- data.frame(svar2)
dinfQ3 <- as.matrix(dinfQ3)
dinfQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ3),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for kunstig intelligens svar på spørgsmål 2 Digitalt Indfødte",
        ylab = "Antal deltagere",
        xlab = "2.	Vil du finde det sandsynligt at en computer ved en fejl vil kunne udløse en verdenskrig?",
        names.arg = c(prct1y,prct1n),
        legend = c("% sandsynligt", "% ikke sandsynligt"), args.legend = list(x = "topright", ncol = 1))


## svar 3a
a <- as.double(dinfQ3Y / 15 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinfQ3N / 15 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar3 <- c(dinfQ3Y, dinfQ3N)
dinfQ3 <- data.frame(svar3)
dinfQ3 <- as.matrix(dinfQ3)
dinfQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ3),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for kunstig intelligens svar på spørgsmål 3 Digitalt Indfødte",
        ylab = "Antal deltagere",
        xlab = "3.	Er du tryk ved at lade dig transportere i en førerløs bil kun styret af kunstig intelligens?",
        names.arg = c(prct1y,prct1n),
        legend = c("% tryg", "% ikke tryg"), args.legend = list(x = "topright", ncol = 1))


### samled svar. svar_1 + svar_2 + svar_3 i prct  spørgsmål allQ2Y og allQ2N er byttet om for at af spejle det skeptiske digitale indfødte
hellnowAll <- dinfQ1N + dinfQ2Y + dinfQ3N
hellyesAll <- dinfQ1Y + dinfQ2N + dinfQ3Y
hellnowAll <- hellnowAll / 45 * 100
hellyesAll <- hellyesAll  / 45 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for kunstig intelligens - set for hele gruppen af digitale indfødte",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de første 3 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))


#################### kunstig intelligens analyse for digitale indvandrere  ###############################

replies <- read.csv("data/reply.csv")
View(replies)


dinv <- filter(replies, replies$ReplyID & Reply_9 == "False") 
dinv
dinv <- sum(dinv)
## omdøber kolonner
colnames(dinv) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(dinv)

dinv <- dplyr::select(dinv, svar_1, svar_2, svar_3)
View(dinv)


dinvQ1Y <- as.logical(dinv$svar_1 == "True") 
dinvQ1Y <- sum(dinvQ1Y)
dinvQ1Y


dinvQ1N <- as.logical(dinv$svar_1 == "False") 
dinvQ1N <- sum(dinvQ1N)
dinvQ1N


dinvQ2Y <- as.logical(dinv$svar_2 == "True") 
dinvQ2Y <- sum(dinvQ2Y)
dinvQ2Y 

dinvQ2N <- as.logical(dinv$svar_2 == "False") 
dinvQ2N <- sum(dinvQ2N)
dinvQ2N

dinvQ3Y <- as.logical(dinv$svar_3 == "True") 
dinvQ3Y <- sum(dinvQ3Y)
dinvQ3Y

dinvQ3N <- as.logical(dinv$svar_3 == "False") 
dinvQ3N <- sum(dinvQ3N)
dinvQ3N

## laver dataframe og plot
library(RColorBrewer)

svar1 <- c(dinvQ1Y, dinvQ1N)
svar2 <- c(dinvQ2Y, dinvQ2N)
svar3 <- c(dinvQ3Y, dinvQ3N)
dinvQ3 <- data.frame(svar1,svar2,svar3)
dinvQ3 <- as.matrix(dinvQ3)
dinvQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ3),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Digitalt indvandrere",
        names.arg = c("2  -  23", "10  -  15", "8  -  17"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))


## svar 1b
a <- as.double(dinvQ1Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ1N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar1 <- c(dinvQ1Y, dinvQ1N)
dinvQ3 <- data.frame(svar1)
dinvQ3 <- as.matrix(dinvQ3)
dinvQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ3),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for kunstig intelligens svar på spørgsmål 1 Digitalt Indvandrere",
        ylab = "Antal deltagere",
        xlab = "1.	Er du tryk ved at computere tager beslutninger om forhold der gælder liv og død?",
        names.arg = c(prct1y, prct1n),
        legend = c("% ja","% nej"), args.legend = list(x = "topright", ncol = 1))

## svar 2b
a <- as.double(dinvQ2Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ2N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar2 <- c(dinvQ2Y, dinvQ2N)
dinvQ3 <- data.frame(svar2)
dinvQ3 <- as.matrix(dinvQ3)
dinvQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ3),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for kunstig intelligens svar på spørgsmål 2 Digitalt Indvandrere",
        ylab = "Antal deltagere",
        xlab = "2.	Vil du finde det sandsynligt at en computer ved en fejl vil kunne udløse en verdenskrig?",
        names.arg = c(prct1y, prct1n),
        legend = c("% sandsynligt","% ikke sandsynligt"), args.legend = list(x = "topright", ncol = 1))

## svar 3b
a <- as.double(dinvQ3Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ3N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar3 <- c(dinvQ3Y, dinvQ3N)
dinvQ3 <- data.frame(svar3)
dinvQ3 <- as.matrix(dinvQ3)
dinvQ3
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ3),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for kunstig intelligens svar på spørgsmål 3 Digitalt Indvandrere",
        ylab = "Antal deltagere",
        xlab = "3.	Er du tryk ved at lade dig transportere i en førerløs bil kun styret af kunstig intelligens? ",
        names.arg = c(prct1y, prct1n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

### samled svar. svar_1 + svar_2 + svar_3 i prct  spørgsmål allQ2Y og allQ2N er byttet om for at af spejle det skeptiske digitale indvandrere 
hellnowAll <- dinvQ1N + dinvQ2Y + dinvQ3N
hellyesAll <- dinvQ1Y + dinvQ2N + dinvQ3Y
hellnowAll <- hellnowAll / 75 * 100
hellyesAll <- hellyesAll  / 75 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for kunstig intelligens - set for hele gruppen af Digitale Indvandrere",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de første 3 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

#################### kunstig intelligens analyse for kvinder  ###############################
replies <- read.csv("data/reply.csv")
View(replies)


woman <- filter(replies, replies$ReplyID & Reply_10 == "True") 
woman
woman <- sum(woman)
## omdøber kolonner
colnames(woman) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(woman)

woman <- dplyr::select(woman, svar_1, svar_2, svar_3)
View(woman)


womanQ1Y <- as.logical(woman$svar_1 == "True") 
womanQ1Y <- sum(womanQ1Y)
womanQ1Y


womanQ1N <- as.logical(woman$svar_1 == "False") 
womanQ1N <- sum(womanQ1N)
womanQ1N


womanQ2Y <- as.logical(woman$svar_2 == "True") 
womanQ2Y <- sum(womanQ2Y)
womanQ2Y 

womanQ2N <- as.logical(woman$svar_2 == "False") 
womanQ2N <- sum(womanQ2N)
womanQ2N

womanQ3Y <- as.logical(woman$svar_3 == "True") 
womanQ3Y <- sum(womanQ3Y)
womanQ3Y

womanQ3N <- as.logical(woman$svar_3 == "False") 
womanQ3N <- sum(womanQ3N)
womanQ3N

## laver dataframe og plot
library(RColorBrewer)

svar1 <- c(womanQ1Y, womanQ1N)
svar2 <- c(womanQ2Y, womanQ2N)
svar3 <- c(womanQ3Y, womanQ3N)
womanQ3 <- data.frame(svar1,svar2,svar3)
womanQ3 <- as.matrix(womanQ3)
womanQ3
coul <- brewer.pal(5, "Set2") 
barplot((womanQ3),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Kvinder",
        names.arg = c("2  -  20", "11  -  11", "1  -  20"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))

## svar 1c
a <- as.double(womanQ1Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ1N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar1 <- c(womanQ1Y, womanQ1N)
womanQ3 <- data.frame(svar1)
womanQ3 <- as.matrix(womanQ3)
womanQ3
coul <- brewer.pal(5, "Set2") 
barplot((womanQ3),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for kunstig intelligens svar på spørgsmål 1 Kvinder",
        ylab = "Antal deltagere",
        xlab = "1.	Er du tryk ved at computere tager beslutninger om forhold der gælder liv og død?",
        names.arg = c(prct1y,prct1n),
        legend = c("% ja","% nej"), args.legend = list(x = "topright", ncol = 1))

## svar 2c
a <- as.double(womanQ2Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ2N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar2 <- c(womanQ2Y, womanQ2N)
womanQ3 <- data.frame(svar2)
womanQ3 <- as.matrix(womanQ3)
womanQ3
coul <- brewer.pal(5, "Set2") 
barplot((womanQ3),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for kunstig intelligens svar på spørgsmål 2 Kvinder",
        ylab = "Antal deltagere",
        xlab = "2.	Vil du finde det sandsynligt at en computer ved en fejl vil kunne udløse en verdenskrig?",
        names.arg = c(prct1y,prct1n),
        legend = c("% sandsynligt","% ikke sandsynligt"), args.legend = list(x = "topright", ncol = 1))


## svar 3c
a <- as.double(womanQ3Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ3N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar3 <- c(womanQ3Y, womanQ3N)
womanQ3 <- data.frame(svar3)
womanQ3 <- as.matrix(womanQ3)
womanQ3
coul <- brewer.pal(5, "Set2") 
barplot((womanQ3),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for kunstig intelligens svar på spørgsmål 3 Kvinder",
        ylab = "Antal deltagere",
        xlab = "3.	Er du tryk ved at lade dig transportere i en førerløs bil kun styret af kunstig intelligens?",
        names.arg = c(prct1y,prct1n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

### samled svar. svar_1 + svar_2 + svar_3 i prct  spørgsmål allQ2Y og allQ2N er byttet om for at af spejle det skeptiske kvinder 
hellnowAll <- womanQ1N + womanQ2Y + womanQ3N
hellyesAll <- womanQ1Y + womanQ2N + womanQ3Y
hellnowAll <- hellnowAll / 66 * 100
hellyesAll <- hellyesAll  / 66 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for kunstig intelligens - set for hele gruppen af Kvinder",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de første 3 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))


#################### kunstig intelligens analyse for Mænd  ###############################

replies <- read.csv("data/reply.csv")
View(replies)


man <- filter(replies, replies$ReplyID & Reply_10 == "False") 
man
man <- sum(man)
## omdøber kolonner
colnames(man) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(man)

man <- dplyr::select(man, svar_1, svar_2, svar_3)
View(man)


manQ1Y <- as.logical(man$svar_1 == "True") 
manQ1Y <- sum(manQ1Y)
manQ1Y


manQ1N <- as.logical(man$svar_1 == "False") 
manQ1N <- sum(manQ1N)
manQ1N


manQ2Y <- as.logical(man$svar_2 == "True") 
manQ2Y <- sum(manQ2Y)
manQ2Y 

manQ2N <- as.logical(man$svar_2 == "False") 
manQ2N <- sum(manQ2N)
manQ2N

manQ3Y <- as.logical(man$svar_3 == "True") 
manQ3Y <- sum(manQ3Y)
manQ3Y

manQ3N <- as.logical(man$svar_3 == "False") 
manQ3N <- sum(manQ3N)
manQ3N

## laver dataframe og plot
library(RColorBrewer)

svar1 <- c(manQ1Y, manQ1N)
svar2 <- c(manQ2Y, manQ2N)
svar3 <- c(manQ3Y, manQ3N)
manQ3 <- data.frame(svar1,svar2,svar3)
manQ3 <- as.matrix(manQ3)
manQ3
coul <- brewer.pal(5, "Set2") 
barplot((manQ3),beside=TRUE,
        col = c("blue","red"),
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Mænd",
        names.arg = c("5  -  13", "7  -  11", "10  -  8"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))


## svar 1d
a <- as.double(manQ1Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ1N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar1 <- c(manQ1Y, manQ1N)
manQ3 <- data.frame(svar1)
manQ3 <- as.matrix(manQ3)
manQ3
coul <- brewer.pal(5, "Set2") 
barplot((manQ3),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for kunstig intelligens svar på spørgsmål 1 Mænd",
        ylab = "Antal deltagere",
        xlab = "1.	Er du tryk ved at computere tager beslutninger om forhold der gælder liv og død?",
        names.arg = c(prct1y,prct1n),
        legend = c("% ja","% nej"), args.legend = list(x = "topright", ncol = 1))

## svar 2d
a <- as.double(manQ2Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ2N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar2 <- c(manQ2Y, manQ2N)
manQ3 <- data.frame(svar2)
manQ3 <- as.matrix(manQ3)
manQ3
coul <- brewer.pal(5, "Set2") 
barplot((manQ3),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for kunstig intelligens svar på spørgsmål 2 Mænd",
        ylab = "Antal deltagere",
        xlab = "2.	Vil du finde det sandsynligt at en computer ved en fejl vil kunne udløse en verdenskrig?",
        names.arg = c(prct1y,prct1n),
        legend = c("% sandsynligt","% ikke sandsynligt"), args.legend = list(x = "topright", ncol = 1))

## svar 3d

a <- as.double(manQ3Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ3N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar3 <- c(manQ3Y, manQ3N)
manQ3 <- data.frame(svar3)
manQ3 <- as.matrix(manQ3)
manQ3
coul <- brewer.pal(5, "Set2") 
barplot((manQ3),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for kunstig intelligens svar på spørgsmål 3 Mænd",
        ylab = "Antal deltagere",
        xlab = "3.	Er du tryk ved at lade dig transportere i en førerløs bil kun styret af kunstig intelligens?",
        names.arg = c(prct1y,prct1n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

### samled svar. svar_1 + svar_2 + svar_3 i prct  spørgsmål allQ2Y og allQ2N er byttet om for at af spejle det skeptiske kvinder 
hellnowAll <- manQ1N + manQ2Y + manQ3N
hellyesAll <- manQ1Y + manQ2N + manQ3Y
hellnowAll <- hellnowAll / 54 * 100
hellyesAll <- hellyesAll  / 54 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for kunstig intelligens - set for hele gruppen af Mænd",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de første 3 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

#################### Frygt for misbrug af data ALL ###############################


replies <- read.csv("data/reply.csv")
View(replies)


## omdøber kolonner
colnames(replies) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(replies)

population <- dplyr::select(replies, svar_4, svar_5, svar_6, svar_7, svar_8)
View(population)


allQ4Y <- as.logical(population$svar_4 == "True") 
allQ4Y <- sum(allQ4Y)
allQ4Y

allQ4N <- as.logical(population$svar_4 == "False") 
allQ4N <- sum(allQ4N)
allQ4N

allQ5Y <- as.logical(population$svar_5 == "True") 
allQ5Y <- sum(allQ5Y)
allQ5Y

allQ5N <- as.logical(population$svar_5 == "False") 
allQ5N <- sum(allQ5N)
allQ5N

allQ6Y <- as.logical(population$svar_6 == "True") 
allQ6Y <- sum(allQ6Y)
allQ6Y
allQ6N <- as.logical(population$svar_6 == "False") 
allQ6N <- sum(allQ6N)
allQ6N

## laver dataframe og plot
library(RColorBrewer)

svar4 <- c(allQ4Y, allQ4N)
svar5 <- c(allQ5Y, allQ5N)
svar6 <- c(allQ6Y, allQ6N)
svar7 <- c(allQ7Y, allQ7N)
svar8 <- c(allQ8Y, allQ8N)
allQ5 <- data.frame(svar4,svar5,svar6)
allQ5
allQ5 <- as.matrix(allQ5)
allQ5
coul <- brewer.pal(5, "Set2") 
barplot((allQ5),beside=TRUE,
        col = c("blue","red"),
        ylim = c(0, 40),
        main = "Frygt for misbrug for vores data svar 4 - 6",
        ylab = "Antal deltagere",
        xlab = "Alle",
        names.arg = c("4  -  36", "16  -  24", "4  -  36"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 2))


#### 7 - 8



allQ7Y <- as.logical(population$svar_7 == "True") 
allQ7Y <- sum(allQ7Y)
allQ7Y

allQ7N <- as.logical(population$svar_7 == "False") 
allQ7N <- sum(allQ7N)
allQ7N

allQ8Y <- as.logical(population$svar_8 == "True") 
allQ8Y <- sum(allQ8Y)
allQ8Y

allQ8N <- as.logical(population$svar_8 == "False") 
allQ8N <- sum(allQ8N)
allQ8N
## laver dataframe og plot
library(RColorBrewer)


svar7 <- c(allQ7Y, allQ7N)
svar8 <- c(allQ8Y, allQ8N)
allQ5 <- data.frame(svar7,svar8)
allQ5
allQ5 <- as.matrix(allQ5)
allQ5
coul <- brewer.pal(5, "Set2") 
barplot((allQ5),beside=TRUE,
        col = c("blue","red"),
        ylim = c(0, 40),
        main = "Frygt for misbrug for vores data svar 7 - 8",
        ylab = "Antal deltagere",
        xlab = "Alle",
        names.arg = c("3  -  37", "15  -  25"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 2))



#################### Frygt for misbrug af data ALL enkeltvis ###############################
replies <- read.csv("data/reply.csv")
View(replies)


## omdøber kolonner
colnames(replies) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(replies)

population <- dplyr::select(replies, svar_4, svar_5, svar_6, svar_7, svar_8)
View(population)


allQ4Y <- as.logical(population$svar_4 == "True") 
allQ4Y <- sum(allQ4Y)
allQ4Y

allQ4N <- as.logical(population$svar_4 == "False") 
allQ4N <- sum(allQ4N)
allQ4N

allQ5Y <- as.logical(population$svar_5 == "True") 
allQ5Y <- sum(allQ5Y)
allQ5Y

allQ5N <- as.logical(population$svar_5 == "False") 
allQ5N <- sum(allQ5N)
allQ5N

allQ6Y <- as.logical(population$svar_6 == "True") 
allQ6Y <- sum(allQ6Y)
allQ6Y

allQ6N <- as.logical(population$svar_6 == "False") 
allQ6N <- sum(allQ6N)
allQ6N

allQ7Y <- as.logical(population$svar_7 == "True") 
allQ7Y <- sum(allQ7Y)
allQ7Y

allQ7N <- as.logical(population$svar_7 == "False") 
allQ7N <- sum(allQ7N)
allQ7N

allQ8Y <- as.logical(population$svar_8 == "True") 
allQ8Y <- sum(allQ8Y)
allQ8Y

allQ8N <- as.logical(population$svar_8 == "False") 
allQ8N <- sum(allQ8N)
allQ8N
## laver dataframe og plot
library(RColorBrewer)



## svar 4
prct1y <- as.double(allQ4Y / 40 * 100)
prct1y
prct1n <- as.double(allQ4N / 40 * 100)
svar4 <- c(allQ4Y, allQ4N)
allQ4 <- data.frame(svar4)
allQ4 <- as.matrix(allQ4)
coul <- brewer.pal(5, "Set2") 
barplot((allQ4),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for misbrug af vores data spørgsmål 4",
        ylab = "Antal deltagere",
        xlab = "4.	Er du ok med at internationale Tech-Giganter indsamler dine data og bruger dem kommercielt?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 5
prct1y <- as.double(allQ5Y / 40 * 100)
prct1y
prct1n <- as.double(allQ5N / 40 * 100)
svar5 <- c(allQ5Y, allQ5N)
allQ5 <- data.frame(svar5)
allQ5 <- as.matrix(allQ5)
coul <- brewer.pal(5, "Set2") 
barplot((allQ5),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for misbrug af vores data spørgsmål 5",
        ylab = "Antal deltagere",
        xlab = "5.	Er du ok med at nationale myndigheder kan indsamle dine data?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 6
prct1y <- as.double(allQ6Y / 40 * 100)
prct1y
prct1n <- as.double(allQ6N / 40 * 100)
svar6 <- c(allQ6Y, allQ6N)
allQ6 <- data.frame(svar6)
allQ6 <- as.matrix(allQ6)
coul <- brewer.pal(5, "Set2") 
barplot((allQ6),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for misbrug af vores data spørgsmål 6",
        ylab = "Antal deltagere",
        xlab = "6.	Er du ok med at nationale myndigheder kan købe data fra private aktører?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))


## svar 7
prct1y <- as.double(allQ7Y / 40 * 100)
prct1y
prct1n <- as.double(allQ7N / 40 * 100)
svar7 <- c(allQ7Y, allQ7N)
allQ7 <- data.frame(svar7)
allQ7 <- as.matrix(allQ7)
coul <- brewer.pal(5, "Set2") 
barplot((allQ7),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for misbrug af vores data spørgsmål 7",
        ylab = "Antal deltagere",
        xlab = "7.	Er du ok med at private aktører køber og sælger dine data for profittens skyld?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 8
prct1y <- as.double(allQ8Y / 40 * 100)
prct1y
prct1n <- as.double(allQ8N / 40 * 100)
svar8 <- c(allQ8Y, allQ8N)
allQ8 <- data.frame(svar8)
allQ8 <- as.matrix(allQ8)
coul <- brewer.pal(5, "Set2") 
barplot((allQ8),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 40),
        main = "Frygt for misbrug af vores data spørgsmål 8",
        ylab = "Antal deltagere",
        xlab = "8.	frygter du at dine data vil indgå i en overvågnings kultur?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

### samled svar. svar_4 + svar_5 + svar_6 + svar_7 + svar_8 i prct
##spørgsmål allQ8Y og allQ8N er byttet om for at afspejle det skeptiske 
#Frygt for misbrug af vores data - set for hele gruppen 
hellnowAll <- allQ4N + allQ5N + allQ6N + allQ7N + allQ8Y
hellyesAll <- allQ4Y + allQ5Y + allQ6Y + allQ7Y + allQ8N
hellnowAll <- hellnowAll / 200 * 100
hellyesAll <- hellyesAll  / 200 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("lightblue","#f59042"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for misbrug af vores data - set for hele gruppen",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de sidste 5 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

#################### Frygt for misbrug af data analyse for digitale indfødte  ###############################


replies <- read.csv("data/reply.csv")
View(replies)


dinf <- filter(replies, replies$ReplyID & Reply_9 == "True") 
dinf
dinf <- sum(dinf)

## omdøber kolonner
colnames(dinf) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(dinf)

dinf <- dplyr::select(dinf, svar_4, svar_5, svar_6, svar_7, svar_8)
View(dinf)


dinfQ4Y <- as.logical(dinf$svar_4 == "True") 
dinfQ4Y <- sum(dinfQ4Y)
dinfQ4Y


dinfQ4N <- as.logical(dinf$svar_4 == "False") 
dinfQ4N <- sum(dinfQ4N)
dinfQ4N


dinfQ5Y <- as.logical(dinf$svar_5 == "True") 
dinfQ5Y <- sum(dinfQ5Y)
dinfQ5Y 

dinfQ5N <- as.logical(dinf$svar_5 == "False") 
dinfQ5N <- sum(dinfQ5N)
dinfQ5N

dinfQ6Y <- as.logical(dinf$svar_6 == "True") 
dinfQ6Y <- sum(dinfQ6Y)
dinfQ6Y

dinfQ6N <- as.logical(dinf$svar_6 == "False") 
dinfQ6N <- sum(dinfQ6N)
dinfQ6N

dinfQ7Y <- as.logical(dinf$svar_7 == "True") 
dinfQ7Y <- sum(dinfQ7Y)
dinfQ7Y

dinfQ7N <- as.logical(dinf$svar_7 == "False") 
dinfQ7N <- sum(dinfQ7N)
dinfQ7N

dinfQ8Y <- as.logical(dinf$svar_8 == "True") 
dinfQ8Y <- sum(dinfQ8Y)
dinfQ8Y

dinfQ8N <- as.logical(dinf$svar_8 == "False") 
dinfQ8N <- sum(dinfQ8N)
dinfQ8N
library(RColorBrewer)
## svar 4
prct1y <- as.double(dinfQ4Y / 15 * 100)
prct1y
prct1n <- as.double(dinfQ4N / 15 * 100)
svar4 <- c(dinfQ4Y, dinfQ4N)
dinfQ4 <- data.frame(svar4)
dinfQ4 <- as.matrix(dinfQ4)
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ4),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for misbrug af vores data spørgsmål 4 digitale indfødte ",
        ylab = "Antal deltagere",
        xlab = "4.	Er du ok med at internationale Tech-Giganter indsamler dine data og bruger dem kommercielt?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))


## svar 5
a <- as.double(dinfQ5Y / 15 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinfQ5N / 15 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar5 <- c(dinfQ5Y, dinfQ5N)
dinfQ5 <- data.frame(svar5)
dinfQ5 <- as.matrix(dinfQ5)
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ5),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for misbrug af vores data spørgsmål 5 digitale indfødte ",
        ylab = "Antal deltagere",
        xlab = "5.	Er du ok med at nationale myndigheder kan indsamle dine data?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 6
a <- as.double(dinfQ6Y / 15 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinfQ6N / 15 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar6 <- c(dinfQ6Y, dinfQ6N)
dinfQ6 <- data.frame(svar6)
dinfQ6 <- as.matrix(dinfQ6)
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ6),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for misbrug af vores data spørgsmål 6 digitale indfødte ",
        ylab = "Antal deltagere",
        xlab = "6.	Er du ok med at nationale myndigheder kan købe data fra private aktører?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 7
a <- as.double(dinfQ7Y / 15 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinfQ7N / 15 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar7 <- c(dinfQ7Y, dinfQ7N)
dinfQ7 <- data.frame(svar7)
dinfQ7 <- as.matrix(dinfQ7)
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ7),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for misbrug af vores data spørgsmål 7 digitale indfødte ",
        ylab = "Antal deltagere",
        xlab = "7.	Er du ok med at private aktører køber og sælger dine data for profittens skyld?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))


## svar 8
a <- as.double(dinfQ8Y / 15 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinfQ8N / 15 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar8 <- c(dinfQ8Y, dinfQ8N)
dinfQ8 <- data.frame(svar8)
dinfQ8 <- as.matrix(dinfQ8)
coul <- brewer.pal(5, "Set2") 
barplot((dinfQ8),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 16),
        main = "Frygt for misbrug af vores data spørgsmål 8 digitale indfødte ",
        ylab = "Antal deltagere",
        xlab = "8.	frygter du at dine data vil indgå i en overvågnings kultur?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

### samled svar. svar_4 + svar_5 + svar_6 + svar_7 + svar_8 i prct
##spørgsmål allQ8Y og allQ8N er byttet om for at afspejle det skeptiske 
#Frygt for misbrug af vores data - digitale indfødte
hellnowAll <- dinfQ4N + dinfQ5N + dinfQ6N + dinfQ7N + dinfQ8Y
hellyesAll <- dinfQ4Y + dinfQ5Y + dinfQ6Y + dinfQ7Y + dinfQ8N
hellnowAll <- hellnowAll / 75 * 100
hellyesAll <- hellyesAll  / 75 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("#adf542","#f55142"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for misbrug af vores data - set for digitale indfødte",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de sidste 5 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

#################### Frygt for misbrug af data analyse for digitale indvandrere   ###############################


replies <- read.csv("data/reply.csv")
View(replies)


dinv <- filter(replies, replies$ReplyID & Reply_9 == "False") 
dinv
dinv <- sum(dinv)
## omdøber kolonner
colnames(dinv) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(dinv)

dinv <- dplyr::select(dinv, svar_4, svar_5, svar_6, svar_7, svar_8)
View(dinv)


dinvQ4Y <- as.logical(dinv$svar_4 == "True") 
dinvQ4Y <- sum(dinvQ4Y)
dinvQ4Y


dinvQ4N <- as.logical(dinv$svar_4 == "False") 
dinvQ4N <- sum(dinvQ4N)
dinvQ4N


dinvQ5Y <- as.logical(dinv$svar_5 == "True") 
dinvQ5Y <- sum(dinvQ5Y)
dinvQ5Y 

dinvQ5N <- as.logical(dinv$svar_5 == "False") 
dinvQ5N <- sum(dinvQ5N)
dinvQ5N

dinvQ6Y <- as.logical(dinv$svar_6 == "True") 
dinvQ6Y <- sum(dinvQ6Y)
dinvQ6Y

dinvQ6N <- as.logical(dinv$svar_6 == "False") 
dinvQ6N <- sum(dinvQ6N)
dinvQ6N

dinvQ7Y <- as.logical(dinv$svar_7 == "True") 
dinvQ7Y <- sum(dinvQ7Y)
dinvQ7Y 

dinvQ7N <- as.logical(dinv$svar_7 == "False") 
dinvQ7N <- sum(dinvQ7N)
dinvQ7N

dinvQ8Y <- as.logical(dinv$svar_8 == "True") 
dinvQ8Y <- sum(dinvQ8Y)
dinvQ8Y

dinvQ8N <- as.logical(dinv$svar_8 == "False") 
dinvQ8N <- sum(dinvQ8N)
dinvQ8N


## svar 4
a <- as.double(dinvQ4Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ4N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar4 <- c(dinvQ4Y, dinvQ4N)
dinvQ4 <- data.frame(svar4)
dinvQ4 <- as.matrix(dinvQ4)
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ4),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for misbrug af vores data spørgsmål 4 digitale indvandrere ",
        ylab = "Antal deltagere",
        xlab = "4.	Er du ok med at internationale Tech-Giganter indsamler dine data og bruger dem kommercielt?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 5
a <- as.double(dinvQ5Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ5N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar5 <- c(dinvQ5Y, dinvQ5N)
dinvQ5 <- data.frame(svar5)
dinvQ5 <- as.matrix(dinvQ5)
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ5),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for misbrug af vores data spørgsmål 5 digitale indvandrere ",
        ylab = "Antal deltagere",
        xlab = "5.	Er du ok med at nationale myndigheder kan indsamle dine data?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 6
a <- as.double(dinvQ6Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ6N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar6 <- c(dinvQ6Y, dinvQ6N)
dinvQ6 <- data.frame(svar6)
dinvQ6 <- as.matrix(dinvQ6)
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ6),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for misbrug af vores data spørgsmål 6 digitale indvandrere ",
        ylab = "Antal deltagere",
        xlab = "6.	Er du ok med at nationale myndigheder kan købe data fra private aktører?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 7
a <- as.double(dinvQ7Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ7N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar7 <- c(dinvQ7Y, dinvQ7N)
dinvQ7 <- data.frame(svar7)
dinvQ7 <- as.matrix(dinvQ7)
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ7),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for misbrug af vores data spørgsmål 7 digitale indvandrere ",
        ylab = "Antal deltagere",
        xlab = "7.	Er du ok med at private aktører køber og sælger dine data for profittens skyld?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 8
a <- as.double(dinvQ8Y / 25 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(dinvQ8N / 25 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar8 <- c(dinvQ8Y, dinvQ8N)
dinvQ8 <- data.frame(svar8)
dinvQ8 <- as.matrix(dinvQ8)
coul <- brewer.pal(5, "Set2") 
barplot((dinvQ8),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 26),
        main = "Frygt for misbrug af vores data spørgsmål 8 digitale indvandrere ",
        ylab = "Antal deltagere",
        xlab = "8.	frygter du at dine data vil indgå i en overvågnings kultur?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))
### samled svar. svar_4 + svar_5 + svar_6 + svar_7 + svar_8 i prct
##spørgsmål allQ8Y og allQ8N er byttet om for at afspejle det skeptiske 
#Frygt for misbrug af vores data -digitale indvandrere 
hellnowAll <- dinvQ4N + dinvQ5N + dinvQ6N + dinvQ7N + dinvQ8Y
hellyesAll <- dinvQ4Y + dinvQ5Y + dinvQ6Y + dinvQ7Y + dinvQ8N
hellnowAll <- hellnowAll / 125 * 100
hellyesAll <- hellyesAll  / 125 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("#4296f5","#f5ec42"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for misbrug af vores data - set for digitale indvandrere",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de sidste 5 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

#################### Frygt for misbrug af data analyse for kvinder   ###############################

replies <- read.csv("data/reply.csv")
View(replies)


woman <- filter(replies, replies$ReplyID & Reply_10 == "True") 
woman
woman <- sum(woman)
## omdøber kolonner
colnames(woman) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(woman)

woman <- dplyr::select(woman, svar_4, svar_5, svar_6, svar_7, svar_8)
View(woman)


womanQ4Y <- as.logical(woman$svar_4 == "True") 
womanQ4Y <- sum(womanQ4Y)
womanQ4Y


womanQ4N <- as.logical(woman$svar_4 == "False") 
womanQ4N <- sum(womanQ4N)
womanQ4N


womanQ5Y <- as.logical(woman$svar_5 == "True") 
womanQ5Y <- sum(womanQ5Y)
womanQ5Y 

womanQ5N <- as.logical(woman$svar_5 == "False") 
womanQ5N <- sum(womanQ5N)
womanQ5N

womanQ6Y <- as.logical(woman$svar_6 == "True") 
womanQ6Y <- sum(womanQ6Y)
womanQ6Y

womanQ6N <- as.logical(woman$svar_6 == "False") 
womanQ6N <- sum(womanQ6N)
womanQ6N

womanQ7Y <- as.logical(woman$svar_7 == "True") 
womanQ7Y <- sum(womanQ7Y)
womanQ7Y 

womanQ7N <- as.logical(woman$svar_7 == "False") 
womanQ7N <- sum(womanQ7N)
womanQ7N

womanQ8Y <- as.logical(woman$svar_8 == "True") 
womanQ8Y <- sum(womanQ8Y)
womanQ8Y

womanQ8N <- as.logical(woman$svar_8 == "False") 
womanQ8N <- sum(womanQ8N)
womanQ8N

## svar 4
a <- as.double(womanQ4Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ4N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar4 <- c(womanQ4Y, womanQ4N)
womanQ4 <- data.frame(svar4)
womanQ4 <- as.matrix(womanQ4)
coul <- brewer.pal(5, "Set2") 
barplot((womanQ4),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for misbrug af vores data spørgsmål 4 Kvinder ",
        ylab = "Antal deltagere",
        xlab = "4.	Er du ok med at internationale Tech-Giganter indsamler dine data og bruger dem kommercielt?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 5
a <- as.double(womanQ5Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ5N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar5 <- c(womanQ5Y, womanQ5N)
womanQ5 <- data.frame(svar5)
womanQ5 <- as.matrix(womanQ5)
coul <- brewer.pal(5, "Set2") 
barplot((womanQ5),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for misbrug af vores data spørgsmål 5 Kvinder ",
        ylab = "Antal deltagere",
        xlab = "5.	Er du ok med at nationale myndigheder kan indsamle dine data?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 6
a <- as.double(womanQ6Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ6N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar6 <- c(womanQ6Y, womanQ6N)
womanQ6 <- data.frame(svar6)
womanQ6 <- as.matrix(womanQ6)
coul <- brewer.pal(5, "Set2") 
barplot((womanQ6),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for misbrug af vores data spørgsmål 6 Kvinder ",
        ylab = "Antal deltagere",
        xlab = "6.	Er du ok med at nationale myndigheder kan købe data fra private aktører?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 7
a <- as.double(womanQ7Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ7N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar7 <- c(womanQ7Y, womanQ7N)
womanQ7 <- data.frame(svar7)
womanQ7 <- as.matrix(womanQ7)
coul <- brewer.pal(5, "Set2") 
barplot((womanQ7),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for misbrug af vores data spørgsmål 7 Kvinder ",
        ylab = "Antal deltagere",
        xlab = "7.	Er du ok med at private aktører køber og sælger dine data for profittens skyld?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 8
a <- as.double(womanQ8Y / 22 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(womanQ8N / 22 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar8  <- c(womanQ8Y, womanQ8N)
womanQ8 <- data.frame(svar8)
womanQ8 <- as.matrix(womanQ8)
coul <- brewer.pal(5, "Set2") 
barplot((womanQ8),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 23),
        main = "Frygt for misbrug af vores data spørgsmål 8 Kvinder ",
        ylab = "Antal deltagere",
        xlab = "8.	frygter du at dine data vil indgå i en overvågnings kultur? ",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

### samled svar. svar_4 + svar_5 + svar_6 + svar_7 + svar_8 i prct
##spørgsmål allQ8Y og allQ8N er byttet om for at afspejle det skeptiske 
#Frygt for misbrug af vores data - kvinder
hellnowAll <- womanQ4N + womanQ5N + womanQ6N + womanQ7N + womanQ8Y
hellyesAll <- womanQ4Y + womanQ5Y + womanQ6Y + womanQ7Y + womanQ8N
hellnowAll <- hellnowAll / 110 * 100
hellyesAll <- hellyesAll  / 110 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("#42f599","#7836eb"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for misbrug af vores data - set for Kvinder",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de sidste 5 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))

#################### Frygt for misbrug af data analyse for Mænd   ###############################
replies <- read.csv("data/reply.csv")
View(replies)


man <- filter(replies, replies$ReplyID & Reply_10 == "False") 
man
man <- sum(man)
## omdøber kolonner
colnames(man) <- c("svarID","svar_1","svar_2","svar_3","svar_4","svar_5","svar_6","svar_7","svar_8","withOrWithout","gender")
View(man)

man <- dplyr::select(man, svar_4, svar_5, svar_6, svar_7, svar_8)
View(man)


manQ4Y <- as.logical(man$svar_4 == "True") 
manQ4Y <- sum(manQ4Y)
manQ4Y


manQ4N <- as.logical(man$svar_4 == "False") 
manQ4N <- sum(manQ4N)
manQ4N


manQ5Y <- as.logical(man$svar_5 == "True") 
manQ5Y <- sum(manQ5Y)
manQ5Y 

manQ5N <- as.logical(man$svar_5 == "False") 
manQ5N <- sum(manQ5N)
manQ5N

manQ6Y <- as.logical(man$svar_6 == "True") 
manQ6Y <- sum(manQ6Y)
manQ6Y

manQ6N <- as.logical(man$svar_6 == "False") 
manQ6N <- sum(manQ6N)
manQ6N

manQ7Y <- as.logical(man$svar_7 == "True") 
manQ7Y <- sum(manQ7Y)
manQ7Y 

manQ7N <- as.logical(man$svar_7 == "False") 
manQ7N <- sum(manQ7N)
manQ7N

manQ8Y <- as.logical(man$svar_8 == "True") 
manQ8Y <- sum(manQ8Y)
manQ8Y

manQ8N <- as.logical(man$svar_8 == "False") 
manQ8N <- sum(manQ8N)
manQ8N

## svar 4
a <- as.double(manQ4Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ4N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar4  <- c(manQ4Y, manQ4N)
manQ4 <- data.frame(svar4)
manQ4 <- as.matrix(manQ4)
coul <- brewer.pal(5, "Set2") 
barplot((manQ4),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for misbrug af vores data spørgsmål 4 Mænd ",
        ylab = "Antal deltagere",
        xlab = "4.	Er du ok med at internationale Tech-Giganter indsamler dine data og bruger dem kommercielt? ",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 5
a <- as.double(manQ5Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ5N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar5  <- c(manQ5Y, manQ5N)
manQ5 <- data.frame(svar5)
manQ5 <- as.matrix(manQ5)
coul <- brewer.pal(5, "Set2") 
barplot((manQ5),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for misbrug af vores data spørgsmål 5 Mænd ",
        ylab = "Antal deltagere",
        xlab = "5.	Er du ok med at nationale myndigheder kan indsamle dine data?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 6
a <- as.double(manQ6Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ6N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar6  <- c(manQ6Y, manQ6N)
manQ6 <- data.frame(svar6)
manQ6 <- as.matrix(manQ6)
coul <- brewer.pal(5, "Set2") 
barplot((manQ6),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for misbrug af vores data spørgsmål 6 Mænd ",
        ylab = "Antal deltagere",
        xlab = "6.	Er du ok med at nationale myndigheder kan købe data fra private aktører?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 7
a <- as.double(manQ7Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ7N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar7  <- c(manQ7Y, manQ7N)
manQ7 <- data.frame(svar7)
manQ7 <- as.matrix(manQ7)
coul <- brewer.pal(5, "Set2") 
barplot((manQ7),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for misbrug af vores data spørgsmål 7 Mænd ",
        ylab = "Antal deltagere",
        xlab = "7.	Er du ok med at private aktører køber og sælger dine data for profittens skyld?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))

## svar 8
a <- as.double(manQ8Y / 18 * 100)
prct1y <- format(round(a, 2), nsmall = 2)
prct1y
b <- as.double(manQ8N / 18 * 100)
prct1n <- format(round(b, 2), nsmall = 2)
svar8  <- c(manQ8Y, manQ8N)
manQ8 <- data.frame(svar8)
manQ8 <- as.matrix(manQ8)
coul <- brewer.pal(5, "Set2") 
barplot((manQ8),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 19),
        main = "Frygt for misbrug af vores data spørgsmål 8 Mænd ",
        ylab = "Antal deltagere",
        xlab = "8.	frygter du at dine data vil indgå i en overvågnings kultur?",
        names.arg = c(prct1y, prct1n),
        legend = c("% Ja","% Nej"), args.legend = list(x = "topleft", ncol = 1))


### samled svar. svar_4 + svar_5 + svar_6 + svar_7 + svar_8 i prct
##spørgsmål allQ8Y og allQ8N er byttet om for at afspejle det skeptiske 
#Frygt for misbrug af vores data - mænd
hellnowAll <- manQ4N + manQ5N + manQ6N + manQ7N + manQ8Y
hellyesAll <- manQ4Y + manQ5Y + manQ6Y + manQ7Y + manQ8N
hellnowAll <- hellnowAll / 90 * 100
hellyesAll <- hellyesAll  / 90 * 100
prct3y <- format(round(hellyesAll, 2), nsmall = 2)
prct3y
prct3n <- format(round(hellnowAll, 2), nsmall = 2)
svarCompl <- c(hellyesAll, hellnowAll)
allQCompl <- data.frame(svarCompl)
allQCompl <- as.matrix(allQCompl)
coul <- brewer.pal(5, "Set2") 
barplot((allQCompl),beside=TRUE,
        col = c("blue","red"),
        space = 1,
        ylim = c(0, 100),
        main = "Frygt for misbrug af vores data - set for Mænd",
        ylab = "Antal deltagere i %",
        xlab = "baseret på de sidste 5 spørgsmål",
        names.arg = c(prct3y, prct3n),
        legend = c("% tryg","% ikke tryg"), args.legend = list(x = "topright", ncol = 1))


# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)