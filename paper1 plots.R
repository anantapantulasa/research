########
# load data files
devtools::install_github("daniel1noble/orchaRd", force = TRUE)
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, 
               emmeans, ape, phytools, flextable)

library(readr)
library(orchaRd)
library(metafor)
library(devtools)
library(dplyr)
library(pacman)
library(knitr)

########
# load the data 
data <- read.csv(file.choose())
View(data)

#plantGraph <- read.csv(file.choose())

# calculate effect sizes
dataES <- escalc(measure = "ROM",
                 n1i = n_t,
                 sd1i = sd_impu_t,
                 m1i = mean_t,
                 n2i = n_c,
                 sd2i = sd_impu_c,
                 m2i = mean_c,
                 var.names = c("lnRR", "vlnRR"),
                 data = data)
#dataES$vlnRR <- sqrt(dataES$vlnRR)

# res <- metafor::rma.mv(yi = lnRR, V = vlnRR, 
                       #random = ~1 |study/exp_id,
                       #data = dataES,
                       #method = "REML")
# funnel(res, yaxis = "sei")

View(dataES)

fsn(yi=lnRR, vi=vlnRR, data=dataES, type = "Rosenthal")

# order the different data types
dataES$res_group <- with(dataES, factor(res_group, 
                                        levels = c("off-flavor", "microcystin", 
                                                   "pigments", "cell density")))

dataES$cide_group <- with(dataES, factor(cide_group, 
                                        levels = c("plant", "physical", 
                                                   "chemical", "bacteria")))
# make sure the algaecides are in alphabetical order
dataES$algaecide <- factor(as.character(dataES$algaecide))
dataES$algaecide <- factor(dataES$algaecide, levels = rev(levels(dataES$algaecide)))

dataES$algaecide <- with(dataES, factor(algaecide, 
                                         levels = c("Barley straw", 
                                                    "Phoslock",
                                                    "Dry-till",
                                                    "Deep well circulation",
                                                    "Terbutryn",
                                                    "Simazine",
                                                    "Sanguinarine",
                                                    "Quinone",
                                                    "Potassium ricinoleate",
                                                    "Potassium permanganate",
                                                    "PH4062",
                                                    "Peracetic acid",
                                                    "Juglone",
                                                    "Hydrogen peroxide",
                                                    "Gypsum",
                                                    "Fluridone",
                                                    "Ferulic acid",
                                                    "Dry-till + Gypsum",
                                                    "Diuron",
                                                    "Diquat",
                                                    "Copper + Phoslock",
                                                    "Copper",
                                                    "SK09 + APVAS carriers",
                                                    "SK09",
                                                    "LakePak",
                                                    "BactaPur",
                                                    "Aqua5",
                                                    "APVAS")))

#levels(dataES$algaecide) # to confirm 

# split data set
#end <- dplyr::filter(dataES, end == "LAST")
#big <- dplyr::filter(dataES, big == "MIN")
#DTGend <- dplyr::filter(end, algaecide == "Dry-till + Gypsum")
#DTGbig <- dplyr::filter(big, algaecide == "Dry-till + Gypsum")
#View(DTGend)
#View(DTGbig)

# make sure everything is correct
View(dataES)
View(end)
View(big)
plant <- dplyr::filter(dataES, cide_group == "plant")
View(plantGraph)

########
# model (multivariate)
mode <- metafor::rma.mv(yi = lnRR, V = vlnRR, 
                         random = ~1 |study/exp_id,
                         mods = ~mod-1, ####
                         data = dplyr::filter(plantGraph, big == "MIN"), ####
                         method = "REML")
# summary of model - copy into text editor
summary(mode)

# modsEnd
orchaRd::orchard_plot(mode,
                       mod = "mod", ###
                       data = dplyr::filter(plantGraph, end == "LAST"), ###
                       group = "study",
                       xlab = "lnRR",
                       legend.pos = c("none"), # can be "none" or "top.left" or "bottom.right"
                       angle = 0, # this changes the angle of the labels on the moderators 
                       k = TRUE,
                       cb = FALSE)




#######
bacteriaBig <- dplyr::filter(big, cide_group == "bacteria")
chemBig <- dplyr::filter(big, cide_group == "chemical")
physicalBig <- dplyr::filter(big, cide_group == "physical")
plantBig <- dplyr::filter(big, cide_group == "plant")
plant <- dplyr::filter(dataES, cide_group == "plant")
View(plant)

bacteriaEnd <- dplyr::filter(end, cide_group == "bacteria")
chemEnd <- dplyr::filter(end, cide_group == "chemical")
physicalEnd <- dplyr::filter(end, cide_group == "physical")
plantEnd <- dplyr::filter(end, cide_group == "plant")

#######
# model (multivariate)
mode <- metafor::rma.mv(yi = lnRR, V = vlnRR, 
                           random = ~1 |study/exp_id,
                           #mods = ~res_group-1,
                           data = plantEnd,
                           method = "REML")
# summary of model - copy into text editor
summary(mode)

# modsEn
orchaRd::orchard_plot(mode,
                      mod = "res_group", ###
                      data = physicalBig, ###
                      group = "study",
                      xlab = "lnRR",
                      legend.pos = c("top.left"), # can be "none" or "top.left" or "bottom.right"
                      angle = 0, # this changes the angle of the labels on the moderators 
                      k = FALSE,
                      cb = FALSE)


#######
# finding and keeping I squared values
#copper <- dplyr::filter(end, algaecide == "Copper")
model <- metafor::rma.mv(yi = lnRR, V = vlnRR, 
                        random = ~1 |study/exp_id,
                        #mods = ~algaecide-1, ###
                        data = dataES, ###
                        method = "REML")
summary(model)
print(orchaRd::i2_ml(model))

orchaRd::orchard_plot(model,
                      #mod = "algaecide", ###
                      data = dataES, ###
                      group = "study",
                      xlab = "lnRR")
