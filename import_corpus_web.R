# ----------- Les bibliothèques

library(tm)
library(tm.plugin.webmining)
library(SnowballC)
library(XML)



# ----------- Mettre des pages wiki dans un corpus ------------
#Une longue procédure pour avoir une liste d'articles dans un string
# ATTENION aux noms des pages, utilisez des URL (URlEncoded)
url_start<- "http://edutechwiki.unige.ch/fmediawiki/api.php?action=parse&page="
url_end <- "&format=xml"
titles <- c("1066", "Activate", "Alice", "Argument_Wars", "CeeBot_4", "Chevron", "Cité_romaine", 
            "Citéjob-négo", "Cyberbudget", "Darfur_is_dying", "E-psych", "Elude", "Energy_City",
            "Envers_et_contre_tout", "Eonautes", "FacteurAcademy", "Foodforce", "Get_the_glass", 
            "Glucifer", "Halte_aux_catastrophes", "Happy_Night", "I-progress", "ICE-D", "InfinITy",
            "Ivy%E2%80%99s_Meadow", "J%27apprends_J%27entreprends", "K-ROBOT", 
            "Mon_entretien_d%27embauche", "MySQLgame", "Oiligarchy", "Orbitrunner","Petits_Détectives", 
            "Phun", "Play_the_news", "Real_Lives", "RobotProg", "ScienceMuseum", "September_12th", 
            "StarBankTheGame", "Super_Kimy", "SuperBetter", "TechnoCity", "The_Great_Flu", 
            "The_Traveler_IQ_Challenge", "Timeout", "Tree_Frog", "Typershark", 
            "Une_journée_au_fil_de_l%27eau")

# un vecteur qui contient 12 strings vides ("")
article_list <- character(length(titles))
# on remplace par les URLs ci-dessus
for (i in 1:length(titles)) {
  article_list[i] <- (paste (url_start,titles[i],url_end, sep=""))
}
#Vérification
article_list

# On construit le corpus en utilisant un reader fait par nous et
# Cette fonction extrait juste l'élément XML "text" (voir l'API des mediawiki)
readMWXML <-readXML (spec = list (content = list ("node", "//text"),
                                  heading = list ("attribute", "//parse/@title")),
                     doc=PlainTextDocument())

# Attention : Casse (notamment sous Ubuntu) si les URLs ne sont pas correctement encodés
wiki.source<- VCorpus(URISource(article_list, encoding="UTF-8"), readerControl=list(reader=readMWXML, language="fr"))

# On change les "id"  des metadonnés de chaque document (titres à la place d'URLs illisibles)
for (j in seq.int (wiki.source)) {
  
  meta(wiki.source[[j]],"id") <- titles[j]
  
}
# Ajouter une balise html autour du tout
wiki.source <- tm_map (wiki.source, encloseHTML)
# Le corpus final
corpus<- wiki.source


