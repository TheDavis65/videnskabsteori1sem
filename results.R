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
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Digitalt Indfødte",
        names.arg = c("5  -  10", "8  -  7", "4  -  11"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))

  

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
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Digitalt indvandrere",
        names.arg = c("2  -  23", "10  -  15", "8  -  17"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))

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
        ylim = c(0, 40),
        main = "Frygt for kunstig intelligens",
        ylab = "Antal deltagere",
        xlab = "Kvinder",
        names.arg = c("2  -  20", "11  -  11", "1  -  20"),
        legend = c("ja","nej"), args.legend = list(x = "topright", ncol = 1))


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