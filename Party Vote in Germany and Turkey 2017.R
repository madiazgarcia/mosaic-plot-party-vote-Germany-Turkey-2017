# This script can be used to replicate the Mosaic Plot of Votes in Turkey and Germany 
# of the working paper "The Political Behaviour of a Large and Fast-Growing Group of 
# Immigrant-Origin Voters in a Reluctant Country of Immigration: Germans of Turkish Descent
# between 1999 and 2021" 

# Authors: Manuel Diaz Garcia
# Date: 2024/02/06
# Notes:
  # (1) Please take note of authors who created packages used in this script.

##### Installing and loading necessary packages #####



# This section installs and loads all necessary packages for the data preparation
# and analyses.


install.packages("tidyverse")
install.packages("haven")
install.packages("foreign")
install.packages("ggpubr")
install.packages("Hmisc")
install.packages("readxl")
install.packages("sjPlot")
install.packages("devtools")
install.packages("weights")
install.packages("mice", type = "binary")

library(mice)
library(weights)
library(devtools)
library(sjPlot)
library(readxl)
library(Hmisc)
library(ggpubr)
library(foreign)
library(haven)
library(tidyverse)

devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)



##### Importing Necessary Data #####



# This section imports the dataset of the Immigrant German Election Study I as
# provided at GESIS (https://search.gesis.org/research_data/ZA7495), and 
# the manifesto project dataset from 2022 (Lehmann, Pola / Burst, Tobias / Matthieß, Theres / Regel, Sven / Volkens, Andrea / Weßels, Bernhard / Zehnter, Lisa (2022): The Manifesto Data Collection. Manifesto Project (MRG/CMP/MARPOR). Version 2022a. Berlin: Wissenschaftszentrum Berlin für Sozialforschung (WZB). https://doi.org/10.25522/manifesto.mpds.2022a).

#IMGES I data
imges_gesis <- read_dta("") ### INSERT FILE PATH OF IMGES DATA HERE


#Manifesto Project dataset from 2022
marpor <- read_dta("") ### INSERT FILE PATH OF MANIFESTO PROJECT DATA HERE


#####Mosaic Plot#####

################################################################Data Preparation


# Preparation of dataset for transnational turnout
imges_transnat <- imges_gesis %>%
  filter(sample_screen == 1) %>%
  mutate(turnout_germany = case_when(turnout == 1 ~ "Voted in Germany",
                                     turnout == 0 ~ "Did not vote in Germany"),
         turnout_turkey_dub = case_when(partyvote_dubcitizenship > 92 & partyvote_dubcitizenship < 500 ~ "Would vote in Turkey",
                                        partyvote_dubcitizenship == 92 | partyvote_dubcitizenship == 998 ~ "Would not vote in Turkey"),
         turnout_turkey_hyp = case_when(hyppartyvote_orig > 92 & hyppartyvote_orig < 500 ~ "Would vote in Turkey",
                                        hyppartyvote_orig == 92 | hyppartyvote_orig == 998 ~ "Would not vote in Turkey"),
         turnout_turkey_all = case_when(partyvote_dubcitizenship > 92 & partyvote_dubcitizenship < 500 ~ "Would vote in Turkey",
                                        partyvote_dubcitizenship == 92 | partyvote_dubcitizenship == 998 ~ "Would not vote in Turkey",
                                        hyppartyvote_orig > 92 & hyppartyvote_orig < 500 ~ "Would vote in Turkey",
                                        hyppartyvote_orig == 92 | hyppartyvote_orig == 998 ~ "Would not vote in Turkey"))


# Merging Imges data with Manifesto Data Set
imges_transnat_marpor <- imges_transnat %>%
  mutate(btwsecondvote_new = case_when(btwsecondvote == 2 ~ "CDU/CSU",
                                       btwsecondvote == 3 ~ "CDU/CSU",
                                       btwsecondvote == 4 ~ "SPD",
                                       btwsecondvote == 5 ~ "LINKE",
                                       btwsecondvote == 6 ~ "90/Greens",
                                       btwsecondvote == 7 ~ "FDP",
                                       btwsecondvote == 8 ~ "AfD"),
         hyppartyvote_orig_new = case_when(hyppartyvote_orig == 303 ~ "HDP",
                                           hyppartyvote_orig == 302 ~ "CHP",
                                           hyppartyvote_orig == 301 ~ "AKP",
                                           hyppartyvote_orig == 304 ~ "MHP"),
         partyvote_dubcitizenship_new = case_when(partyvote_dubcitizenship == 303 ~ "HDP",
                                                  partyvote_dubcitizenship == 302 ~ "CHP",
                                                  partyvote_dubcitizenship == 301 ~ "AKP",
                                                  partyvote_dubcitizenship == 304 ~ "MHP"),
         partyvote_turkey_all = case_when(hyppartyvote_orig == 303 ~ "HDP",
                                          hyppartyvote_orig == 302 ~ "CHP",
                                          hyppartyvote_orig == 301 ~ "AKP",
                                          hyppartyvote_orig == 304 ~ "MHP",
                                          partyvote_dubcitizenship == 303 ~ "HDP",
                                          partyvote_dubcitizenship == 302 ~ "CHP",
                                          partyvote_dubcitizenship == 301 ~ "AKP",
                                          partyvote_dubcitizenship == 304 ~ "MHP")) %>%
  left_join(marpor, by = c("btwsecondvote_new" = "partyabbrev")) %>%
  left_join(marpor, by = c("hyppartyvote_orig_new" = "partyabbrev")) %>%
  left_join(marpor, by = c("partyvote_dubcitizenship_new" = "partyabbrev")) %>%
  left_join(marpor, by = c("partyvote_turkey_all" = "partyabbrev")) %>%
  rename(rile_btwsecondvote = rile.x,
         rile_hyppartyvote_orig = rile.y,
         rile_partyvote_dubcitizenship = rile.x.x,
         rile_partyvote_turkey_all = rile.y.y) %>%
  select(1:858, rile_btwsecondvote, rile_hyppartyvote_orig, rile_partyvote_dubcitizenship, rile_partyvote_turkey_all)


#####################################################################Mosaic Plot

party_mosaic <- ggplot(data = imges_transnat_marpor) +
  geom_mosaic(aes(weight = dweight_adj2, x = product(btwsecondvote_new, partyvote_turkey_all), fill = btwsecondvote_new), na.rm = TRUE) +   
  labs(y="Party Voted for in Germany", x="Hypothetical Vote Choice in Turkey") +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.x = element_text(size = 30, margin = margin(r=0, l=0, b=0,t = 10, unit = "pt")),
        text=element_text(family="Book Antiqua", size=30),
        legend.key.size = unit(1.5, 'cm'),
        legend.text=element_text(size = 30)) +
  scale_fill_manual(values = c("#1AA037", "#0489DB", "#000000", "#FFEF00", "#FF00FF", "#E3000F")) +
  guides(fill=guide_legend(title = "Party Voted for in Germany", reverse = TRUE))
party_mosaic