#' Titre : Scripts motifs - Fonctions
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## ## ## ## ## ## ## ## ## ## ## ##  LISTE DES FONCTIONS ## ## ## ## ## ## ## ## ## ## ## ## ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 


# Fonction Étiquetage (màj 15 mai 2021) : 

annotation_udpipe <- function(path = "~/Desktop/Motifs/Corpus/", 
                              model = "~/Desktop/Motifs/model_udpipe/french-gsd-ud-2.5-191206.udpipe"){
  
  # Librairies: 
  require("udpipe")
  require("tidyverse")
  require("vroom")
  require("stringr")
  # Modèle
  udmodel_french <- udpipe_load_model(file = model)
  setwd(path)
  
  # Fichiers txt :
  list_of_files <- list.files(recursive = TRUE,
                              pattern = "*.txt", 
                              full.names = TRUE)
  
  # Lecture : 
  df <- vroom(list_of_files, id = "FileName", delim = "\n", col_names = "mots", progress = F)
  
  # Correction : ajout d'un saut de ligne en bout pour éviter erreurs étiquetage :
  # Correction encodage apostrophes :
  
  df <- df %>%
    mutate(mots = stringr::str_replace_all(.$mots, "$", "\n")) %>%
    mutate(mots = stringr::str_replace_all(.$mots, "’", "'")) %>%
    mutate(mots = stringr::str_replace_all(.$mots, "'", "'"))
  
  # Retrait des NA dans la colonne mots : 
  
  df = df %>%
    na.omit(df$mots)
  
  # Annotation :
  
  corpus_annote <- udpipe_annotate(udmodel_french, x = df$mots, tagger = "default", parser = "none", trace = TRUE, doc_id = df$FileName)
  
  # Transformation en df : 
  corpus_annote <- as.data.frame(corpus_annote)
  
  # Correction de l'identifiant : retrait "./" au début :
  corpus_annote$doc_id <- gsub("\\.\\/", "", corpus_annote$doc_id)
  
  # Sélection des colonnes qui nous intéressent : 
  corpus_annote_cols <- corpus_annote[,c("token", "lemma", "upos", "feats", "doc_id")]
  colnames(corpus_annote_cols) <- c("mots", "lemmes", "POS", "feats", "Oeuvre")
  
  head(corpus_annote_cols)
  
  # Exportation csv : 
  write.csv(corpus_annote_cols, "UDPipe_corpus_complet.csv", fileEncoding = "UTF-8")
  
}

# Fonction Transformation en motifs UDPpipe (màj octobre 2021) :

regex_corpus_entier_UDPipe <- function(path = "~/Desktop/Motifs/", corpus = "UDPipe_corpus_complet.csv"){
  
  # Librairies : 
  
  require("stringr")
  require("dplyr")
  require("readr")
  require("data.table")
  setwd(path)
  
  ## Importation du corpus : 
  
  corpus = fread(corpus, encoding = "UTF-8", header = TRUE)
  
  ## Vérification que les colonnes sont les bonnes : 
  
  corpus <- corpus[,c('mots', 'lemmes', 'POS', 'feats', 'Oeuvre')] 
  
  # Auxiliaires : 
  
  corpus <- corpus %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>% # Auxiliaires
    mutate(POS = replace(POS, lemmes == "avoir", "avoir")) # Auxiliaires
  
  # Remplacement des feats avoir et être pour qu'ils ne soient pas transformés :
  # (on garde les auxiliaires)
  
  corpus <- corpus %>%
    mutate(feats = replace(feats, lemmes == "avoir", "avoir")) %>%
    mutate(feats = replace(feats, lemmes == "être", "être"))
  
  # Infinitifs :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Typo=Yes|VerbForm=Inf", "INF")) %>%
    mutate(lemmes = replace(lemmes, feats == "VerbForm=Inf", "INF")) %>%
    mutate(lemmes = replace(lemmes, feats == "Typo=No|VerbForm=Inf", "INF"))
  
  # Participes : 
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Tense=Past|Typo=Yes|VerbForm=Part", "PPAS")) %>% # PPsé masc sing
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part", "PPAS")) %>% 
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Tense=Past|Typo=Yes|VerbForm=Part", "PPAS")) %>% # PPsé fem sin
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Tense=Past|Typo=Yes|VerbForm=Part", "PPAS")) %>% ## PPsé mas plu
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Plur|Tense=Past|VerbForm=Part", "PPAS")) %>% # PPsé fem plu
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Tense=Past|VerbForm=Part", "PPAS")) %>% # PPsé fem sing
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Tense=Past|VerbForm=Part", "PPAS")) %>% # PPsé masc plu
    mutate(lemmes = replace(lemmes, feats == "Tense=Pres|VerbForm=Part", "PPRES")) # Pprésnt.
  
  
  
  
  ############ Remplacement des verbes : ##
  
  # Subjonctif présent :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin", "VSUBP"))
  
  # Subjonctif imparfait :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=1|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=2|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=3|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=1|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=2|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=3|Tense=Imp|VerbForm=Fin", "VSUBI"))
  
  # Impératif présent :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Imp|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "IMP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Imp|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "IMP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Imp|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "IMP"))
  
  # Conditionnel :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin", "VCOND"))
  
  # Indicatif présent :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin", "PRES"))
  
  # Imparfait :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Imp|VerbForm=Fin", "VIMP"))
  
  # Passé simple : 
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Past|VerbForm=Fin", "VPS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Past|VerbForm=Fin", "VPS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin", "VPS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Past|VerbForm=Fin", "VPS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Past|VerbForm=Fin", "VPS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Past|VerbForm=Fin", "VPS"))
  
  # Futur :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Fut|VerbForm=Fin", "VF")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Fut|VerbForm=Fin", "VF")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Fut|VerbForm=Fin", "VF")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Fut|VerbForm=Fin", "VF")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Fut|VerbForm=Fin", "VF")) %>%
    mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Fut|VerbForm=Fin", "VF"))
  
  # Déterminants possessifs :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DEPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS"))
  
  
  # Retrait colonne morphologie :
  
  corpus <- corpus[,-4]
  
  ## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 
  
  # REGEX #
  
  ## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 
  
  # Mots invariables :
  
  # Transformation des lemmes en motifs :
  # Il faut ici se fonder sur les lemmes puis transformer les POS pour qu'ils ne soient pas 
  # changés ensuite au cours des transformations futures. Ex : on veut garder le verbe être
  # pour qu'il demeure dans les motifs. Pour cela, obligation de transformer le POS.
  
  corpus <- corpus %>%
    #mutate(POS = replace(POS, lemmes == "être", "être")) %>% # Auxiliaires
    # mutate(POS = replace(POS, lemmes == "avoir", "avoir")) %>% # Auxiliaires
    mutate(POS = replace(POS, lemmes == "ne", "ne")) %>%
    mutate(POS = replace(POS, lemmes == "plusieurs", "plusieurs")) %>% # Noms communs
    mutate(POS = replace(POS, lemmes == "fois", "fois")) %>%
    mutate(POS = replace(POS, lemmes == "façon", "façon")) %>%
    mutate(POS = replace(POS, lemmes == "sorte", "sorte")) %>%
    mutate(POS = replace(POS, lemmes == "espèce", "espèce")) %>%
    mutate(POS = replace(POS, lemmes == "jour", "jour")) %>%
    mutate(POS = replace(POS, lemmes == "nuit", "nuit")) %>%
    mutate(POS = replace(POS, lemmes == "tel", "tel")) %>%
    mutate(POS = replace(POS, lemmes == "pas vrai", "pas vrai")) %>%
    mutate(POS = replace(POS, lemmes == "à peu près", "à peu près")) %>%
    mutate(POS = replace(POS, lemmes == "de le côté du", "de le côté de le")) %>%
    mutate(POS = replace(POS, lemmes == "tenir", "tenir")) %>%
    mutate(POS = replace(POS, lemmes == "prendre", "prendre")) %>%
    mutate(POS = replace(POS, lemmes == "pouvoir", "pouvoir")) %>%
    mutate(POS = replace(POS, lemmes == "passer", "passer")) %>%
    mutate(POS = replace(POS, lemmes == "parler", "parler")) %>%
    mutate(POS = replace(POS, lemmes == "laisser", "laisser")) %>%
    mutate(POS = replace(POS, lemmes == "donner", "donner")) %>%
    mutate(POS = replace(POS, lemmes == "penser", "penser")) %>%
    mutate(POS = replace(POS, lemmes == "arriver", "arriver")) %>%
    mutate(POS = replace(POS, lemmes == "mettre", "mettre")) %>%
    #mutate(POS = replace(POS, lemmes == "faire", "faire")) %>%
    #mutate(POS = replace(POS, lemmes == "aller", "aller")) %>%
    mutate(POS = replace(POS, lemmes == "falloir", "falloir")) %>%
    mutate(POS = replace(POS, lemmes == "finir", "finir")) %>%
    mutate(POS = replace(POS, lemmes == "commencer", "commencer")) %>%
    mutate(POS = replace(POS, lemmes == "venir", "venir")) %>%
    mutate(POS = replace(POS, lemmes == "devoir", "devoir")) %>%
    mutate(POS = replace(POS, lemmes == "paraître", "paraître")) %>%
    mutate(POS = replace(POS, lemmes == "sembler", "sembler")) %>%
    mutate(POS = replace(POS, lemmes == "rester", "rester")) %>%
    mutate(POS = replace(POS, lemmes == "devenir", "devenir")) %>%
    mutate(POS = replace(POS, lemmes == "meilleur", "meilleur")) %>% # adjectifs
    mutate(POS = replace(POS, lemmes == "vrai", "vrai")) %>%
    mutate(POS = replace(POS, lemmes == "véritable", "véritable")) %>%
    mutate(POS = replace(POS, lemmes == "autre", "autre")) %>%
    mutate(POS = replace(POS, lemmes == "même", "même")) %>%
    mutate(POS = replace(POS, lemmes == "au contraire", "au contraire")) %>%
    mutate(POS = replace(POS, lemmes == "est-ce que", "est-ce que")) %>%
    mutate(POS = replace(POS, lemmes == "mal", "mal")) %>%
    mutate(POS = replace(POS, lemmes == "plutôt", "plutôt")) %>%
    mutate(POS = replace(POS, lemmes == "à demi", "à demi")) %>%
    mutate(POS = replace(POS, lemmes == "à peu près", "à peu près")) %>%
    mutate(POS = replace(POS, lemmes == "ainsi", "ainsi")) %>% # ADV
    mutate(POS = replace(POS, lemmes == "alors", "alors")) %>%
    mutate(POS = replace(POS, lemmes == "aussi", "aussi")) %>%
    mutate(POS = replace(POS, lemmes == "autant", "autant")) %>%
    mutate(POS = replace(POS, lemmes == "aucunement", "aucunement")) %>%
    mutate(POS = replace(POS, lemmes == "autrement", "autrement")) %>%
    mutate(POS = replace(POS, lemmes == "avant", "avant")) %>%
    mutate(POS = replace(POS, lemmes == "beaucoup", "beaucoup")) %>%
    mutate(POS = replace(POS, lemmes == "bien plus", "bien plus")) %>%
    mutate(POS = replace(POS, lemmes == "bien moins", "bien moins")) %>%
    mutate(POS = replace(POS, lemmes == "bien", "bien")) %>%
    mutate(POS = replace(POS, lemmes == "cependant", "cependant")) %>%
    mutate(POS = replace(POS, lemmes == "combien", "combien")) %>%
    mutate(POS = replace(POS, lemmes == "comment", "comment")) %>%
    mutate(POS = replace(POS, lemmes == "davantage", "davantage")) %>%
    mutate(POS = replace(POS, lemmes == "déjà", "déjà")) %>%
    mutate(POS = replace(POS, lemmes == "depuis", "depuis")) %>%
    mutate(POS = replace(POS, lemmes == "de très près", "de très près")) %>%
    mutate(POS = replace(POS, lemmes == "dorénavant", "dorénavant")) %>%
    mutate(POS = replace(POS, lemmes == "encore", "encore")) %>%
    mutate(POS = replace(POS, lemmes == "en plus", "en plus")) %>%
    mutate(POS = replace(POS, lemmes == "ensemble", "ensemble")) %>%
    mutate(POS = replace(POS, lemmes == "environ", "environ")) %>%
    mutate(POS = replace(POS, lemmes == "delà", "delà")) %>%
    mutate(POS = replace(POS, lemmes == "tandis", "tandis")) %>%
    mutate(POS = replace(POS, lemmes == "fort", "fort")) %>%
    mutate(POS = replace(POS, lemmes == "grandement", "grandement")) %>%
    mutate(POS = replace(POS, lemmes == "guère", "guère")) %>%
    mutate(POS = replace(POS, lemmes == "longtemps", "longtemps")) %>%
    mutate(POS = replace(POS, lemmes == "sous", "sous")) %>%
    mutate(POS = replace(POS, lemmes == "lors", "lors")) %>%
    mutate(POS = replace(POS, lemmes == "parfois", "parfois")) %>%
    mutate(POS = replace(POS, lemmes == "ici", "ici")) %>%
    mutate(POS = replace(POS, lemmes == "travers", "travers")) %>%
    mutate(POS = replace(POS, lemmes == "où", "où")) %>%
    mutate(POS = replace(POS, lemmes == "là", "là")) %>%
    mutate(POS = replace(POS, lemmes == "puis", "puis")) %>%
    mutate(POS = replace(POS, lemmes == "au-dessus", "au-dessus")) %>%
    mutate(POS = replace(POS, lemmes == "jusque", "jusque")) %>%
    mutate(POS = replace(POS, lemmes == "au-dessous", "au-dessous")) %>%
    mutate(POS = replace(POS, lemmes == "an", "ans")) %>%
    mutate(POS = replace(POS, lemmes == "mais", "mais")) %>%
    mutate(POS = replace(POS, lemmes == "mieux", "mieux")) %>%
    mutate(POS = replace(POS, lemmes == "moins", "moins")) %>%
    mutate(POS = replace(POS, lemmes == "nullement", "nullement")) %>%
    mutate(POS = replace(POS, lemmes == "par conséquent", "par conséquent")) %>%
    mutate(POS = replace(POS, lemmes == "par hasard", "par hasard")) %>%
    mutate(POS = replace(POS, lemmes == "pas", "pas")) %>%
    mutate(POS = replace(POS, lemmes == "plus", "plus")) %>%
    # mutate(POS = replace(POS, lemmes == "point", "point")) %>% # ATTENTION : ici pb car si NC ne fonctionne pas...
    mutate(POS = replace(POS, lemmes == "presque", "presque")) %>%
    mutate(POS = replace(POS, lemmes == "prou", "prou")) %>%
    mutate(POS = replace(POS, lemmes == "pourquoi", "pourquoi")) %>%
    mutate(POS = replace(POS, lemmes == "quasi", "quasi")) %>%
    mutate(POS = replace(POS, lemmes == "quasiment", "quasiment")) %>%
    mutate(POS = replace(POS, lemmes == "quelque", "quelque")) %>%
    mutate(POS = replace(POS, lemmes == "soudain", "soudain")) %>%
    mutate(POS = replace(POS, lemmes == "tant de", "tant de")) %>%
    mutate(POS = replace(POS, lemmes == "tard", "tard")) %>%
    mutate(POS = replace(POS, lemmes == "tôt", "tôt")) %>%
    mutate(POS = replace(POS, lemmes == "bientôt", "bientôt")) %>%
    mutate(POS = replace(POS, lemmes == "aussitôt", "aussitôt")) %>%
    mutate(POS = replace(POS, lemmes == "non plus", "non plus")) %>%
    mutate(POS = replace(POS, lemmes == "tout", "tout")) %>%
    mutate(POS = replace(POS, lemmes == "tout à coup", "tout à coup")) %>%
    mutate(POS = replace(POS, lemmes == "très", "très")) %>%
    mutate(POS = replace(POS, lemmes == "trop", "trop")) %>%
    mutate(POS = replace(POS, lemmes == "après", "après")) %>% 
    mutate(POS = replace(POS, lemmes == "voire", "voire")) %>%
    mutate(POS = replace(POS, lemmes == "peu", "peu")) %>%
    mutate(POS = replace(POS, lemmes == "heureusement", "heureusement")) %>%
    mutate(POS = replace(POS, lemmes == "malheureusement", "malheureusement")) %>%
    mutate(POS = replace(POS, lemmes == "également", "également")) %>%
    mutate(POS = replace(POS, lemmes == "pourquoi", "pourquoi")) %>%
    mutate(POS = replace(POS, lemmes == "par trop", "par trop")) %>%
    mutate(POS = replace(POS, lemmes == "tous les jours", "tous les jours")) %>%
    mutate(POS = replace(POS, lemmes == "de loin", "de loin")) %>%
    mutate(POS = replace(POS, lemmes == "comme toujours", "comme toujours")) %>%
    mutate(POS = replace(POS, lemmes == "d'ailleurs", "d'ailleurs")) %>%
    mutate(POS = replace(POS, lemmes == "dans l'ensemble", "dans l'ensemble")) %>%
    mutate(POS = replace(POS, lemmes == "surtout", "surtout")) %>%
    mutate(POS = replace(POS, lemmes == "pourtant", "pourtant")) %>%
    mutate(POS = replace(POS, lemmes == "réellement", "réellement")) %>%
    mutate(POS = replace(POS, lemmes == "à la fin", "à la fin")) %>%
    mutate(POS = replace(POS, lemmes == "à peine", "à peine")) %>%
    mutate(POS = replace(POS, lemmes == "au début", "au début")) %>%
    mutate(POS = replace(POS, lemmes == "d'abord", "d'abord")) %>%
    mutate(POS = replace(POS, lemmes == "tout d'abord", "tout d'abord")) %>%
    mutate(POS = replace(POS, lemmes == "enfin", "enfin")) %>%
    mutate(POS = replace(POS, lemmes == "ensuite", "ensuite")) %>%
    mutate(POS = replace(POS, lemmes == "jamais", "jamais")) %>%
    mutate(POS = replace(POS, lemmes == "toujours", "toujours")) %>%
    mutate(POS = replace(POS, lemmes == "maintenant", "maintenant")) %>%
    mutate(POS = replace(POS, lemmes == "si", "si")) %>%
    mutate(POS = replace(POS, lemmes == "oui", "oui")) %>%
    mutate(POS = replace(POS, lemmes == "non", "non")) %>%
    mutate(POS = replace(POS, lemmes == "peut-être", "peut-être")) %>%
    mutate(POS = replace(POS, lemmes == "monsieur", "monsieur")) %>%
    mutate(POS = replace(POS, lemmes == "quel", "quel")) %>%
    mutate(POS = replace(POS, lemmes == "chose", "chose")) %>%
    mutate(POS = replace(POS, lemmes == "tant", "tant")) %>%
    mutate(POS = replace(POS, lemmes == "milieu", "milieu")) %>%
    mutate(POS = replace(POS, lemmes == "madame", "monsieur"))
  
  
  # if lemmes == x
  # replace pos by lemmes ce qui permet 
  # de sauver ensuite le motifs de la transformation.
  
  # Précisions / insertions POS :
  # Remplacement conditionnels : si corpus$lemmes == c(), remplacement des POS et lemmes en ADVTOT, etc.
  
  corpus[corpus$lemmes %in% c("absolument", "complètement", "entièrement", "incomplètement", "intégralement", "parfaitement", "partiellement", "
                              pleinement", "quasiment", "radicalement", "rigoureusement", "strictement", "totalement"), c("lemmes", "POS")] <- "ADVTOT"
  
  
  corpus[corpus$lemmes %in% c("actuellement", "adultérieurement", "anciennement", "antécédement", "antérieurement", "antiquement", "
                              dernièrement", "diurnement", "fraîchement", "futurement", "imminementimminent", "incessamment", "initialement", "nouvellement", "
                              nuitamment", "originairement", "originellement", "postérieurement", "posthumément", "préalablement", "précédemment", "
                              précocement", "préliminairement", "prématurément", "présentement", "primitivement", "prochainement", "
                              récemment", "tardivement", "ultérieurement", "ultimement"), c("lemmes", "POS")] <- "ADVPHA"
  
  
  corpus[corpus$lemmes %in% c("accidentellement", "annuellement", "bihebdomadairement", "bimensuellement", "bimestriellement", 
                              "biquotidiennement", "bisannuellementcasuellement", "chroniquement", "constamment", "continuellement", "épisodiquement", "
                              exceptionnellement", "fréquemment", "hebdomadairement", "irrégulièrement", "journellement", "mensuellement", "
                              occasionnellement", "périodiquement", "perpétuellement", "pluriannuellement", "quotidiennement", "rarement", "
                              rarissimement", "régulièrement", "saisonnièrement", "séculairement", "semestriellement", "sempiternellement", "
                              sporadiquement", "trimestriellement", "trisannuellement"), c("lemmes", "POS")] <- "ADVFRE"
  
  corpus[corpus$lemmes %in% c("abominablement", "abondamment", "admirablement", "adorablement", "
                              affreusement", "amplement", "anormalement", "appréciablement", "ardemment", "
                              astronomiquement", "atrocement", "autrement", "bigrement", "bougrement", "
                              capitalement", "catastrophiquement", "célestement", "chichement", "chouettement
                              ", "colossalement", "considérablement", "convenablement", "copieusement", "cruellement
                              ", "cuisamment", "dangereusement", "délicieusement", "démentiellement", "démesurément
                              ", "déplorablement", "dérisoirement", "désastreusement", "désespéramment", "désespérément
                              ", "désolamment", "détestablement", "diablement", "diaboliquement", "diamétralement", "
                              diantrement", "disproportionnément", "divinement", "doublement", "draconiennement
                              ", "drastiquement", "drôlement", "durement", "éclatamment", "effrayamment", "
                              effroyablement", "éhontément", "éminemment", "énormément", "épatamment", "éperdument", "
                              épouvantablement", "étonnamment", "exagérément", "excédentairement", "excellemment", "
                              exceptionnellement", "excessivement", "exécrablement", "exorbitamment", "exquisément", "
                              extraordinairement", "extrêmement", "fabuleusement", "faiblement", "fameusement", "
                              fantastiquement", "faramineusement", "farouchement", "férocement", "fichtrement", "
                              fichument", "follement", "formidablement", "fortement", "foutrement", "foutument", "
                              franchement", "furieusement", "génialement", "gigantesquement", "grandement", "grassement", "
                              gravement", "grièvement", "haïssablement", "hautement", "hideusement", "horriblement", "
                              idéalement", "immaculément", "immensément", "immodérément", "impayablement", "
                              impeccablement", "imperceptiblement", "implacablement", "impressivement", "
                              inadmissiblement", "inappréciablement", "incalculablement", "incommensurablement", "
                              incomparablement", "inconcevablement", "incorrigiblement", "incorruptiblement", "
                              incroyablement", "incurablement", "indécrottablement", "indéfectiblement", "
                              indémontablement", "indépassablement", "indéracinablement", "indescriptiblement", "
                              indestructiblement", "indiciblement", "indomptablement", "inébranlablement", "
                              ineffablement", "inégalablement", "inénarrablement", "inépuisablement", "inestimablement", "
                              inexpiablement", "inexprimablement", "inextinguiblement", "inextirpablement", "
                              inextricablement", "infernalement", "infiniment", "inguérissablement", "inimaginablement", "
                              inimitablement", "innombrablement", "inoubliablement", "inqualifiablement", "insatiablement", "
                              insignement", "insondablement", "insoupçonnablement", "insoutenablement", "instamment", "
                              insuffisamment", "insupportablement", "insurmontablement", "insurpassablement", "intarissablement", "
                              intenablement", "intensément", "intensivement", "intolérablement", "invinciblement", "inviolablement", "
                              invraisemblablement", "invulnérablement", "irréconciliablement", "irrécusablement", "irréductiblement", "
                              irréfragablement", "irréfutablement", "irrémédiablement", "irrémissiblement", "irréparablement", "
                              irrépressiblement", "irrésistiblement", "irrespirablement", "joliment", "lamentablement", "largement", "
                              légèrement", "littéralement", "magnifiquement", "maigrement", "maximalement", "médiocrement", "merveilleusement", "
                              minimement", "mirifiquement", "mirobolamment", "modérément", "modiquement", "monstrueusement", "
                              monumentalement", "mortellement", "moyennement", "multiplement", "nettement", "notablement", "
                              outrageusement", "outrancièrement", "particulièrement", "passablement", "passionnément", "
                              phénoménalement", "plantureusement", "plénièrement", "pléthoriquement", "positivement", "
                              prodigieusement", "profondément", "puissamment", "quadruplement", "quellement", "radieusement", "
                              raisonnablement", "redoutablement", "relativement", "remarquablement", "résolument", "ridiculement", "
                              rudement", "sacrément", "satisfaisamment", "scandaleusement", "sensationnellement", "sensiblement", "
                              sérieusement", "significativement", "singulièrement", "souverainement", "spécialement", "spectaculairement", "
                              splendidement", "sublimement", "substantiellement", "suffisamment", "superbement", "supérieurement", "
                              superlativement", "suprêmement", "surabondamment", "surhumainement", "surréellement", "tellement", "
                              terriblement", "triplement", "vachement", "vertigineusement", "viscéralement", "vivement"), c("lemmes", "POS")] <- "ADVINT"
  
  corpus[corpus$lemmes %in% c("communément", "coutumièrement", "généralement", "habituellement", "invariablement", "
                              normalement", "ordinairement", "rituellement", "traditionnellement", "usuellement"), c("lemmes", "POS")] <- "ADVHAB"
  
  corpus[corpus$lemmes %in% c("apparemment", "assurément", "certainement", "effectivement", "éventuellement", "
                              évidemment", "fatalement", "forcément", "immanquablement", "incontestablement", "indéniablement", "
                              indiscutablement", "indubitablement", "inéluctablement", "inévitablement", "infailliblement", "
                              manifestement", "naturellement", "nécessairement", "obligatoirement", "plausiblement", "possiblement", "
                              présumablement", "probablement", "supposément", "sûrement", "visiblement", "vraisemblablement", "vraiment", "
                              véritablement", "bien sûr", "certes", "sans doute", "sans aucun doute", "sans nul doute", "certes"), c("lemmes", "POS")] <- "ADVMOD"
  
  
  corpus[corpus$lemmes %in% c("abjectement", "abruptement", "abstraitement", "abstrusément", "absurdement", "académiquement", "
 acariâtrement", "accortement", "acerbement", "acidement", "âcrement", "acrimonieusement", "activement", "adipeusement", "
 admirativement", "adroitement", "affablement", "affaireusement", "affectionnément", "affectueusement", "agilement", "
 agressivement", "aguicheusement", "aigrement", "aimablement", "alertement", "allègrement", "allusivement", "altièrement", "
 ambigument", "ambitieusement", "amènement", "amèrement", "amicalement", "amiteusement", "amoralement", "amoureusement", "
 amusamment", "amusément", "angéliquement", "antipathiquement", "antisportivement", "anxieusement", "apathiquement", "
 âprement", "archaïquement", "arrogamment", "artificieusement", "artistement", "ascétiquement", "assidûment", "astucieusement", "
 attentivement", "audacieusement", "austèrement", "autonomement", "autoritairement", "avarement", "avaricieusement", "
 aventureusement", "avidement", "badaudement", "badinement", "balourdement", "banalement", "barbarement", "baroquement", "
 bassement", "batailleusement", "bavardement", "baveusement", "béatement", "bégueulement", "belliqueusement", "bénévolement", "
 bénignement", "benoîtement", "béotiennement", "besogneusement", "bestialement", "bêtement", "bienheureusement", "bienveillament", "
 bigotement", "bileusement", "bilieusement", "bizarement", "blagueusement", "blâmablement", "bonassement", "bordéliquement", "
 boudeusement", "bouffonement", "bougonnement", "boulimiquement", "bourgeoisement", "bravachement", "bravement", "bredouillement", "
 brièvement", "brillamment", "brouillonnement", "brumeusement", "brutalement", "bruyament", "burlesquement", "byzantinement", "
 cabotinement", "cachottièrement", "cafardeusement", "cafouilleusement", "cajoleusement", "câlinement", "calmement", "calomnieusement", "
 canaillement", "candidement", "capricieusement", "captieusement", "caractériellement", "cartésiennement", "casanièrement", "
 caustiquement", "cauteleusement", "cavalièrement", "célibatairement", "cérémonieusement", "chafouinement", "chagrinement", "
 chaleureusement", "chanceusement", "charismatiquement", "charitablement", "charitablement", "chastement", "chattement", "chaudement", "
 chauvinement", "chevaleresquement", "chicanièrement", "chichiteusement", "chinoisement", "chipoteusement", "chiquement", "
 chrétiennement", "circonspectement", "citadinement", "civilement", "clairement", "classiquement", "cocassement", "cochonnement", "
 coércitivement", "coléreusement", "colériquement", "comiquement", "compendieusement", "complaisamment", "compréhensivement", "
 concisément", "concussionnairement", "condamnablement", "confusément", "connement", "consciemment", "consciencieusement", "
 conséquemment", "considérément", "contemplativement", "concrètement", "coquettement", "coquinement", "cordialement", "
 coriacement", "corrosivement", "couardement", "coupablement", "courageusement", "courtoisement", "craintivement", "crânement", "
 crapuleusement", "crédiblement", "crédulement", "crétinement", "criminellement", "critiquablement", "critiquement", "cruellement", "
 crûment", "cuistrement", "cupidement", "curieusement", "cyniquement", "damnablement", "débilement", "débonnairement", "décemment", "
 décidément", "dédaigneusement", "défavorablement", "dégoûtamment", "degueulassement", "délicatement", "déloyalement", "
 démoniaquement", "dépendamment", "déplaisement", "déraisonnablement", "désapprobativement", "désespérément", "déshonnêtement", "
 désinvoltement", "désobligeamment", "désordonnément", "despotiquement", "déterminément", "dévotement", "dévotieusement", "
 dextrement", "dignement", "diligemment", "dinguement", "discourtoisement", "discrètement", "disertement", "disgracieusement", "
 dissolument", "distraitement", "dithyrambiquement", "docilement", "doctement", "doctoralement", "doctrinairement", "dogmatiquement", "
 dolemment", "dolentement", "domestiquement", "doucement", "doucereusement", "droitement", "drôlement", "dubitativement", "durement", "
 dynamiquement", "économement", "efficacement", "effrénément", "effrontément", "égocentriquement", "égoïstement", "égrillardement", "
 élégamment", "élitistement", "élogieusement", "éloquemment", "emphatiquement", "énergiquement", "enfantinement", "enigmatiquement", "
 enjôleusement", "ennuyeusement", "ensorceleusement", "enthousiastement", "envieusement", "épicuriennement", "épileptiquement", "
 équitablement", "équivoquement", "érotiquement", "éruditement", "ésotériquement", "espièglement", "estimablement", "étourdiment", "
 étrangement", "euphoriquement", "évasivement", "exactement", "exaspérément", "excentriquement", "exclusivement", "exemplairement", "
 expansivement", "expéditivement", "expertement", "explicitement", "facétieusement", "factieusement", "fadement", "fallacieusement", "
 falotement", "faméliquement", "familièrement", "fanatiquement", "fanfaronnement", "fantaisistement", "fantasquement", "faraudement", "
 fascistement", "fashionablement", "fastidieusement", "fastueusement", "fautivement", "favorablement", "fébrilement", "félinement", "
 félonnement", "fémininement", "fermement", "férocement", "fervemment", "fiablement", "fidèlement", "fielleusement", "fièrement", "
 fiévreusement", "finaudement", "finement", "flagorneusement", "flâneusement", "flatteusement", "flegmatiquement", "fofollement", "
 folâtrement", "folkloriquement", "follement", "fougueusement", "fourbement", "franchement", "fraternellement", "frêlement", "
 frénétiquement", "frigidement", "frileusement", "friponnement", "frivolement", "froidement", "froussardement", "frustement", "
 fumeusement", "funestement", "furibondement", "furieusement", "futilement", "gaillardement", "gaîment", "galamment", "gallicanement", "
 gaminement", "gâteusement", "gauchement", "gauloisement", "geignardement", "généreusement", "gentement", "gentiment", "glacialement", "
 glorieusement", "gloutonnement", "godichement", "goguenardement", "gouilleusement", "goujatement", "goulûment", "gourdement", "
 gourmandement", "gracieusement", "graveleusement", "gravement", "grincheusement", "grivoisement", "grossièrement", "grotesquement", "
 guillerettement", "habilement", "hagardement", "haineusement", "haïssablement", "hardiment", "hargneusement", "hautainement", "
 hérétiquement", "héroïquement", "hilarement", "honnêtement", "hospitalièrement", "hostilement", "humainement", "humblement", "
 hyperboliquement", "hypocondriaquement", "hypocritement", "hystériquement", "idéalistement", "idiotement", "idolâtrement", "
 ignarement", "ignoblement", "ignoramment", "imbécilement", "immodestement", "immondement", "immoralement", "immortellement", "
 impardonnablement", "impartialement", "impassiblement", "impatiemment", "impavidement", "impénétrablement", "impérieusement", "
 impersonnellement", "impertinemment", "imperturbablement", "impétueusement", "impitoyablement", "implacablement", "impoliment", "
 impopulairement", "importunément", "imprudemment", "impudemment", "impudiquement", "impulsivement", "impurement", "inactivement", "
 inamicalement", "inanomoviblement", "inattaquablement", "inattentivement", "inauthentiquement", "incapablement", "incestueusement", "
 incisivement", "incivilement", "inciviquement", "incompétemment", "incongrûment", "inconsciemment", "inconséquemment", "inconsolablement", "
 inconstamment", "incorrectement", "incrédulement", "increvablement", "indécemment", "indélicatement", "indépendamment", "indévotement", "
 indigemment", "indiscrètement", "indocilement", "indolemment", "indulgemment", "industrieusement", "inélégamment", "inertement", "
 inexcusablement", "inexpertement", "infâmement", "infatigablement", "infectement", "infidèlement", "inflexiblement", "ingénieusement", "
 ingénument", "inglorieusement", "ingratement", "inhabilement", "inhospitalièrement", "inhumainement", "inintelligemment", "iniquement", "
 injurieusement", "injustement", "inlassablement", "innocemment", "inoffensivement", "inquiètement", "insanement", "insensément", "insincèrement", "
 insipidement", "insolemment", "insouciamment", "insoucieusement", "intègrement", "intelligemment", "interlopement", "intraitablement", "
 intrépidement", "inventivement", "irasciblement", "ironiquement", "irraisonnablement", "irrationnellement", "irrépréhensiblement", "
 irréprochablement", "irrésolument", "irrespectueusement", "irresponsablement", "irrévérencieusement", "jacobinement", "jalousement", "
 janséniquement", "jésuitement", "jobardement", "jovialement", "joyeusement", "judicieusement", "justement", "lâchement", "laconiquement", "
 ladrement", "langoureusement", "lascivement", "lassement", "légèrement", "lestement", "léthargiquement", "libéralement", "libertinement", "
 libidineusement", "librement", "licencieusement", "lisiblement", "logiquement", "longanimement", "loquacement", "louangeusement", "
 louchement", "loufoquement", "lourdaudement", "lourdement", "loyalement", "lubriquement", "lucidement", "lunatiquement", "luxurieusement", "
 lymphatiquement", "lyriquement", "macabrement", "machiavéliquement", "magistralement", "magnanimement", "majestueusement", "
 maladroitement", "malgracieusement", "malhabilement", "malheusement", "malhonnêtement", "malicieusement", "malignement", "
 malproprement", "malveillament", "maniaquement", "marginalement", "marmiteusement", "martialement", "masculinement", "
 masochistement", "matérialistement", "maternellement", "matoisement", "maupiteusement", "maussadement", "méchamment", "
 méditativement", "mélancoliquement", "menteusement", "méphistophéliquement", "méprisablement", "mesquinement", "
 mesurément", "méthodiquement", "méticuleusement", "mielleusement", "mièvrement", "mignardement", "mignonnement", "
 minutieusement", "misérablement", "miséreusement", "miséricordieusement", "misogynement", "mochement", "modestement", "
 mollassement", "mollement", "moqueusement", "moralement", "morosement", "morveusement", "muettement", "mutinement", "
 naïvement", "narcissiquement", "narquoisement", "naturellement", "nébuleusement", "négligemment", "nerveusement", "
 nettement", "niaisement", "nigaudement", "noblement", "nonchalamment", "nostalgiquement", "obèsement", "objectivement", "
 obligeamment", "obséquieusement", "obtusément", "odieusement", "oisivement", "ombrageusement", "onctueusement", "
 opiniâtrement", "opportunistement", "optimistement", "ordurièrement", "orgueilleusement", "originalement", "orthodoxement", "
 oublieusement", "outrecuidament", "pacifiquement", "païennement", "paillardement", "paisiblement", "papelardement", "
 paradoxalement", "paranoïaquement", "parcimonieusement", "paresseusement", "partialement", "passivement", "pataudement", "
 patelinement", "paternellement", "pathétiquement", "patibulairement", "patiemment", "patriarcalement", "pauvrement", "
 paysannement", "pédamment", "pédantement", "peinardement", "peineusement", "pénardement", "penaudement", "pendablement", "
 pensivement", "pépèrement", "péremptoirement", "perfidement", "permissivement", "pernicieusement", "perplexement", "
 persévéramment", "persifleusement", "perspicacement", "persuasivement", "perversement", "pesamment", "pessimistement", "
 petitement", "peureusement", "philistinement", "phobiquement", "pieusement", "pinailleusement", "pingrement", "piteusement", "
 pitoyablement", "placidement", "plaignardement", "plaintivement", "plaisamment", "platement", "plébéiennement", "
 pleignardement", "pleutrement", "pointilleusement", "poliment", "polissonnement", "poltronnement", "pompeusement", "
 ponctuellement", "pondérément", "populairement", "posément", "possessivement", "poussivement", "pragmatiquement", "
 précautionneusement", "précieusement", "précisément", "présomptueusement", "prestement", "prétentieusement", "primairement", "
 primesautièrement", "probement", "prodiguement", "profanement", "prolétairement", "prolixement", "proprement", "provincialement", "
 prudement", "prudemment", "prudhommesquement", "pudibondement", "pudiquement", "puérilement", "pugnacement", "purement", "
 puritainement", "pusillanimement", "quiètement", "racoleusement", "radinement", "rageusement", "raidement", "railleusement", "
 raisonnablement", "rapacement", "réactionnairement", "réalistement", "rébarbativement", "répressivement", "résolument", "
 respectablement", "respectueusement", "revêchement", "révérement", "révérencieusement", "rêveusement", "ridiculement", "
 rieusement", "rigoureusement", "ringardement", "robustement", "roidement", "romainement", "romanesquement", "roturièrement", "
 roublardement", "rudement", "rustaudement", "rustrement", "sacrilègement", "sadiquement", "sagacement", "sagement", "saintement", "
 salacement", "salaudement", "salement", "sanguinairement", "sarcastiquement", "sataniquement", "sauvagement", "savamment", "
 scabreusement", "scélératement", "sceptiquement", "schématiquement", "scolairement", "scrupuleusement", "sèchement", "
 sectairement", "sédentairement", "séditieusement", "sénilement", "sensément", "sensuellement", "sentencieusement", "
 sentimentalement", "sereinement", "sérieusement", "serviablement", "servilement", "sévèrement", "sibyllinement", "
 silencieusement", "simplement", "sincèrement", "singulièrement", "sinistrement", "siouxement", "sobrement", "sociablement", "
 soigneusement", "solitairement", "sombrement", "songeusement", "sottement", "soucieusement", "soupçonneusement", "souplement", "
 sourcilleusement", "sournoisement", "souverainement", "spartiatement", "spirituellement", "spontanément", "sportivement", "
 staliniennement", "stoïquement", "strictement", "studieusement", "stupidement", "subtilement", "subversivement", "
 succinctement", "suicidairement", "superficiellement", "superfinement", "supersitieusement", "sûrement", "suspicieusement", "
 sympatiquement", "taciturnement", "talentueusement", "taquinement", "tatillonnement", "teigneusement", "témérairement", "
 tenacement", "tendrement", "ténébreusement", "tièdement", "timidement", "tortueusement", "tracassièrement", "tranquillement", "
 tristement", "trivialement", "trouillardement", "turpidement", "tyranniquement", "urbainement", "vaillammment", "valeureusement", "
 vaniteusement", "vantardement", "vaseusement", "véhémentement", "velléitairement", "venimeusement", "verbeusement", "
 versatilement", "vertueusement", "vétilleusement", "veulement", "vicieusement", "victorieusement", "vieillottement", "
 vigilamment", "vigoureusement", "vilainement", "vilement", "vindicativement", "violemment", "virilement", "virulemment", "
 vivement", "volagement", "volubilement", "voluptueusement", "voracement", "vulgairement", "abjectement", "adroitement", "
 audacieusement", "charitablement", "connement", "courageusement", "cruellement", "égoïstement", "généreusement", "
 habilement", "héroïquement", "imbécilement", "imprudemment", "inintelligemment", "intelligemment", "lâchement", "
 magnanimement", "prudemment", "sadiquement", "sagement", "témérairement", "astucieusement", "bassement", "bêtement", "
 criminellement", "idiotement", "judicieusement", "perversement", "sottement", "stupidement", "bizarrement", "
 curieusement", "étonnamment", "étrangement", "fâcheusement", "inexplicablement", "paradoxalement", "regrettablement"), c("lemmes", "POS")] <- "ADVMAN"
  
  
  ## Cas où l'on part des POS pour changer les lemmes :
  ## Ici, transformation des adverbes restants en ADV
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADV", "ADV"))
  
  ## Noms communs :
  
  corpus[corpus$lemmes %in% c("bien-être", "accablement", "acuité", "admiration", "affection", "joie", "alacrité", "allégresse", 
                              "amitié", "amour", "tristesse", "angoisse", "animosité", "anxiété", "apaisement", "appréhension", "attendrissement",
                              "attirance", "attraction", "douleur", "peur", "aversion", "plaisir", "bonheur", "sensation", "calme", 
                              "désenchantement", "plaisir", "réconfort", "ennui", "orgueil", "bonheur", "écoeurement", "soulagement", "malaise",
                              "sentiment", "horreur", "inquiétude", "émotion", "gêne", "hostilité", "paresse", "fierté", "honte", "sympathie",
                              "satisfaction", "tendresse", "retenue", "hostilité", "joie", "jouissance", "chagrin", "consolation", "consternation",
                              "contentement", "contrariété", "crainte", "déception", "regret", "curiosité", "douleur", "doute", "dégoût", "déchirement",
                              "délectation", "délivrance", "dépit", "déplaisir", "désarroi", "désespoir", "désir", "détachement", "détresse", "embarras"
                              ,"engourdissement", "ennui", "exaltation", "exaspération", "excitation", "fatigue", "frisson", "frémissement", "félicité",
                              "goût", "plaisir", "soulagement", "saisissement", "estime", "réconfort", "soulagement", "honte", "peine", "frayeur",
                              "humiliation", "déception", "répugnance", "paresse", "impatience", "pitié", "insouciance", "peine", "satisfaction",
                              "lassitude", "stupeur", "amertume", "solidarité", "souffrance", "indignation", "gratitude", "humiliation", "inquiétude",
                              "irritation", "ivresse", "mélancolie", "mépris", "nostalgie", "désappointement", "passion", "perplexité", "sensation",
                              "ravissement", "reconnaissance", "remord", "respect", "répugnance", "répulsion", "pressentiment", "sentiment",
                              "humiliation", "tentation", "chagrin", "beauté", "charme", "coquetterie", "courage", "courtoisie", "douceur", "ennui",
                              "gaîté", "gentillesse", "laideur", "piété", "imprudence", "prudence", "sagesse", "sensibilité", "insincérité", "sincérité"
                              , "sérénité", "timidité", "tristesse", "violence", "abnégation", "ambition", "amoralité", "angélisme", "ardeur",
                              "combativité", "audace", "autoritarisme", "aptitude", "entrain", "ingéniosité", "finesse", "doigté", "tact", "patience",
                              "courage", "volonté", "complaisance", "compréhension", "incompétence", "compétence", "connerie", "bêtise", "curiosité",
                              "célérité", "discernement", "discrétion", "défaitisme", "dévouement", "esprit", "faiblesse", "fantaisie", "fermeté",
                              "finesse", "grossièreté", "générosité", "humanité", "humilité", "ignorance", "imagination", "impartialité", "impatience",
                              "imprudence", "imprévoyance", "incapacité", "inconscience", "indifférence", "indiscipline", "indulgence", "indépendance",
                              "initiative", "insensibilité", "intelligence", "intrépidité", "irrespect", "jovialité", "modestie", "obstination",
                              "optimisme", "originalité", "outrecuidance", "partialité", "impatience", "patience", "patriotisme", "persuasion",
                              "pessimisme", "objectivité", "nonchalance", "modération", "impudeur", "pudeur", "puérilité", "rapidité", "rigueur",
                              "incompétence", "souplesse", "stoïcisme", "prévoyance", "cynisme", "tolérance", "arrogance", "talent", "zèle", "charisme",
                              "sang-froid", "aisance", "bonté", "bravoure", "dilection", "discrétion", "délicatesse", "efficacité", "endurance",
                              "exagération", "élégance", "intelligence", "intrépidité", "intuition", "lucidité", "lâcheté", "mansuétude", "naïveté",
                              "paresse", "impolitesse", "politesse", "résignation", "résistance", "fantaisie", "abnégation", "ambition", "amoralité",
                              "sentiment", "combativité", "audace", "autoritarisme", "autorité", "dévouement", "hospitalité", "impatience",
                              "imprudence", "imprévoyance", "inconscience", "indulgence", "insensibilité", "intrépidité", "intégrité", "irrespect",
                              "mansuétude", "obstination", "optimisme", "outrecuidance", "impartialité", "partialité", "puérilité", "pessimisme",
                              "qualité", "incompétence", "stoïcisme", "dynamisme", "négligence", "avarice", "intrépidité", "conscience", 
                              "accointance", "adoration", "affection", "affinité", "amitié", "amour", "camaraderie", "charité", "copinerie",
                              "cordialité", "fidélité", "flirt", "fraternité", "gratitude", "passion", "piété", "préférence", "sympathie", 
                              "vénération", "xénophilie", "attachement", "béguin", "communion", "dévotion", "idolâtrie", "kiffe", 
                              "petit faible", "prosternation", "reconnaissance", "ambition", "aspiration", "avidité", "caprice", "concupiscence",
                              "convoitise", "curiosité", "désir", "envie", "érotisme", "espérance", "penchant", "souhait", "acclamation", "admiration",
                              "adulation", "applaudissement", "approbation", "émerveillement", "engouement", "enjôlement", "entichement", "flatterie",
                              "gloire", "ovation", "aménité", "charme", "intérêt", "séduction", "subjugation", "tentation", "affriolement",
                              "aimantation", "appât", "attirance", "captation", "enchantement", "ensorcellement", "envoûtement", "fascination",
                              "humanité", "accortise", "affabilité", "agrément", "altruisme", "amabilité", "apitoiement", "bonté", "clémence",
                              "commisération", "compassion", "complaisance", "empathie", "générosité", "gentillesse", "magnanimité", 
                              "miséricorde", "obligeance", "pitié", "prévenance", "tendresse", "délicatesse", "bienveillance", "cajolerie", 
                              "câlinerie", "indulgence", "mansuétude", "tolérance", "patience", "longanimité", "humilité", "modestie", 
                              "allégresse", "bonheur", "euphorie", "exultation", "heureux", "joie", "jubilation", "liesse", "béatitude", "extase",
                              "félicité", "nirvana", "triomphe", "victoire", "hilarité", "marrade", "rigolade", "bien-être", "équilibre", 
                              "raison", "lucidité", "clairvoyance", "pondération", "sagesse", "gaieté", "amusement", "badiner", "humeur",
                              "divertissement", "ébaudissement", "égaiement", "émoustillement", "enjouement", "folâtrerie", "folichonnerie", 
                              "gaieté", "jovialité", "optimisme", "alacrité", "animation", "dynamisme", "empressement", "émulation", "enivrement",
                              "enthousiasme", "entrain", "exaltation", "ferveur", "fougue", "impétuosité", "ivresse", "pétulance", 
                              "ragaillardissement", "rétablissement", "revitalisation", "revivification", "stimulation", "vigueur", "vitalité",
                              "vivacité", "volonté", "zèle", "énergie", "élan", "ressort", "tonicité", "résurrection", "activité", "emballement",
                              "éveil", "vigilance", "réactivation", "réanimation", "régénération", "réveil", "apaisement", "assagissement", 
                              "assistance", "bienfait", "consolation", "étayage", "guérison", "radoucissement", "rassérénement", "rassurement",
                              "réconciliation", "réconfort", "secours", "soulagement", "soutien", "déculpabilisation", "franchise", 
                              "honnêteté", "innocence", "loyauté", "probité", "sincérité", "délivrance", "libération", "affront", 
                              "complexe ", "déshonneur", "honte", "humiliation", "ignominie", "infamie", "irrespect", "mortification", "offense",
                              "outrage", "résignation", "soumission", "vexation", "abaissement", "agenouillement", "agonir", "assassiner",
                              "asservissement", "assujettissement", "avanie", "avilissement", "brimade", "camouflet", "froissement", "grossièreté",
                              "injure", "insulte", "piqué au vif", "profanation", "rabaissement", "ravalement", "soufflet", "souillure", 
                              "componction", "contrition", "culpabilité", "faute", "improbité", "regret", "remords", "repentance", "reproche",
                              "résipiscence", "attrition", "blocage", "convulsion", "crise", "décontenancement", "doute", "hésitation",
                              "hypersensiblité", "indécision", "malaise", "mésaise", "périclitation", "perplexité", "perturbation", 
                              "pressentiment", "problème", "scandale", "sensiblerie", "tourneboulement", "aberration", "accès", "anomalie",
                              "anormalité", "aria", "autodestruction", "aveuglement", "bouleversement", "bourrèlement", "branle-bas",
                              "brouillement", "chambardement", "chaos", "choc", "confusion", "contrecoup", "coup dur", "déboussolement", 
                              "dépaysement", "déphasage", "dérangement", "désarroi", "désintégration", "désorganisation", "déstabilisation",
                              "dissonance", "ébranlement", "égarement", "embrouillement", "envahissement", "état de choc", "état second", 
                              "étouffement", "étranglement", "fébrilité", "incertitude", "instabilité", "suffoquement", "tintamarre", "tourmente",
                              "tumulte", "turbulence", "calamité", "drame", "infortune", "malheur", "accident", "cataclysme", "catastrophe", 
                              "cruauté du sort", "désastre", "disgrâce", "thanatos", "tragédie", "alarme", "alerte", "appréhension", "contraction",
                              "crainte", "danger", "défiance", "inquiétude", "méfiance", "préoccupation", "qui-vive", "redouter", "souci", 
                              "soupçonneux", "stress", "suspens", "tension", "mauvais sang", "mineux", "mouron", "spectre", "transissement",
                              "affres", "affreux", "angoisse", "anxiété", "détresse", "oppression", "transe", "pression", "serrement", "affolement",
                              "effarement", "panique", "complexe", "farouche", "introversion", "pusillanimité", "timidité", "timoré", "embarras",
                              "gaucherie", "gêne", "piège", "repli", "couardise", "trac", "déballonner", "effarouchement", "frousse", "insécurité",
                              "intimidation", "lâcheté", "péril", "pétoche", "peur", "phobie", "pleutrerie", "poltronnerie", "trouille",
                              "trouillomètre", "abomination", "cauchemar", "effroi", "epouvante", "horreur", "sinistre", "souleur", "terreur",
                              "ahurissement", "ébahissement", "étonnement", "sidération", "soubresaut", "surprise", "sursaut", "tressaillement",
                              "tressautement", "abasourdissement", "bluff", "épatement", "renversement", "alexithymie", "ataraxie", "impassibilité",
                              "impavidité", "imperturbabilité", "indifférence", "indolence", "insensibilité", "neutralité", "stoïque", "anesthésie",
                              "aridité", "chloroformer", "de glace", "de marbre", "désaffection", "dessèchement", "détachement", "froideur",
                              "imperméabilité", "racornissement", "sans âme", "sans sourciller", "sèchement", "sécheresse", "considération",
                              "déférence", "dignité", "égard", "estime", "honneur", "respect", "révérence", "assouvissement", "plénitude",
                              "rassasiement", "épanouissement", "contentement", "délectation", "délice", "jouissance", "plaisir", "ravissement",
                              "régal", "réjouissance", "satisfaction", "sensualité", "volupté", "abréaction", "catharsis", "défoulement",
                              "désinhibition", "exutoire", "flegme", "harmonie", "placidité", "prudence", "quiétude", "sérénité", "sang froid",
                              "self control", "zen", "tête froide", "accalmie", "accoisement", "paisibilité", "repos", "tranquillité",
                              "décrispation", "décompression", "délassement", "décontraction", "désénervement", "relâchement", "détente", 
                              "répit", "insouciance", "trêve", "rapaisement", "relaxation", "audace", "bravoure", "courage", "cran",
                              "intrépidité", "toupet", "vaillance", "fanfaronnade", "aplomb", "confiance", "confiance en soi", 
                              "détermination", "hardiesse", "témérité", "assurance", "aisance", "extraversion", "autolâtrie", 
                              "complexe de supériorité", "égocentrisme", "élation", "fatuité", "forfanterie", "immodestie", "infatuation",
                              "orgueil", "résomption", "suffisance", "surestimation", "susceptibilité", "vanité", "vantardise",
                              "bouffissure", "cabotinage", "colère", "courroux", "fâcherie", "indignation", "irascibilité", "emportement",
                              "expectoration", "agression", "agressivité", "fureur", "hargne", "intolérance", "invective", "malmenage",
                              "menace", "provocation", "rage", "raptus", "rudoiement", "violence", "vitupération", "brutalité", "coups et blessures",
                              "déchaînement", "dureté", "flagellation", "fulmination", "heurt", "virulence", "atrocité", "barbarie",
                              "cruauté", "dénaturer", "férocité", "impitoyabilité", "implacabilité", "inhumanité", "malveillance", 
                              "mauvaiseté", "méchanceté", "monstruosité", "malignité", "tyrannie", "acariâtreté", "bouderie", "bougonner", 
                              "éréthisme", "exaspération", "excéder", "grognassement", "grogne", "grommellement", "hérisser", "importun",
                              "irritation", "humeur", "mécontentement", "nervosité", "outrepassement", "renfrognement", "ronchonnement",
                              "taquinerie", "agacement", "horripilation", "lancinement", "anathème", "arrogance", "bafouement", "blâme",
                              "condescendance", "critique", "déconsidération", "dédain", "désintérêt", "égoïsme", "égotisme", "fustigation",
                              "hâblerie", "inconsidération", "mépris","mésestime", "morgue", "outrecuidance", "persiflage", "piètrerie",
                              "réprimande", "ricanerie", "ridiculisation", "risée", "discrédit", "impiété", "stigmatisation", "abjection", 
                              "aversion", "blasement", "dégoût", "dégueulasserie", "ignobilité", "répugnance", "vilenie", "affadissement", 
                              "écoeurement", "hideur", "indigestion", "naupathie", "nausée", "poisse", "puanteur", "rejet", "repoussement", 
                              "répulsion", "soulèvement", "vomissement", "animadversion", "animosité", "antipathie", "conflit", "détestation",
                              "dispute", "engueulade", "exécration", "haine", "hostilité", "ingratitude", "inimitié", "jalousie", "querelle",
                              "racisme", "rancoeur", "rancune", "ressentiment", "vendetta", "vengeance", "vindicte", "xénophobie", "acerbité",
                              "acescence", "acidité", "âcreté", "acrimonie", "aigreur", "allergie", "amertume", "âpreté", "discrétion", 
                              "dissimulation", "inhibition", "pudeur", "refoulement", "réserve", "contenance", "refrènement",
                              "renfermement", "renfoncement", "répression", "retenue", "contrariété", "déception", "déconvenue", "dépit",
                              "déplaisir", "désagrément", "désappointement", "désenchantement", "désillusion", "embêtement", "incommodité",
                              "revers", "besoin", "carence", "contrainte", "frustration", "inapaisement", "inassouvissement", "insatisfaction",
                              "irrassasiable", "manque", "insatiabilité", "privation", "sentiment", "abrutissement", "alanguissement", 
                              "amorphe", "apathie", "atonie", "ensommeillement", "étourdissement", "évanouissement", "hébétement", 
                              "inconscient", "lymphatique", "marasme", "nonchalance", "paresse", "passivité", "perclus", 
                              "perdre connaissance", "sommeil", "somnolence", "tétanie", "torpeur", "torpidité", "alourdissement", 
                              "ankylose", "assoupissement", "aveulissement", "catalepsie", "coma", "endormissement", "engourdissement", 
                              "estourbir", "figé", "gel", "glacé", "inanimation", "inertie", "léthargie", "mollesse", "morbidesse", 
                              "paralysie", "pétrification", "raideur", "ralentissement", "sclérose", "sommeil de plomb", "sonné", 
                              "soporifique", "stagnation", "statufication", "syncope", "tiède", "tomber dans les pommes", "végétement", 
                              "abrutissement", "alanguissement", "amorphe", "apathie", "atonie", "ensommeillement", "état d absence", 
                              "étourdissement", "évanouissement", "hébétement", "inconscient", "lymphatique", "marasme", "nonchalance", 
                              "paresse", "passivité", "perclus", "perdre connaissance", "sommeil", "somnolence", "tétanie", "torpeur", "torpidité",
                              "abattement", "aboulie", "accablement", "adynamie", "affaiblissement", "affalement", "amoindrissement", 
                              "asthénie", "collapsus", "consomption", "consternation", "débilitation", "déconfort", "découragement", 
                              "défaillance", "démoralisation", "déprime", "désabusement", "épuisement", "éreintement", "fatigue", "harassement",
                              "impuissance", "inexpressivité", "langueur", "lassitude", "neurasthénie", "prostration", "psychasthénie", 
                              "surmenage", "ternissement", "vulnérabilité", "affaissement", "altération", "amollissement", "anémie", 
                              "aplatissement", "assèchement", "assommement", "assourdissement", "atrophie", "atterrement", "avachissement",
                              "avarier", "bousillage", "bras ballants", "breakdown", "brisement", "cassement", "décadence", "décatissement", 
                              "déchéance", "déclin", "décomposition", "décrépitude", "défiguration", "dégénérescence", "déglinguage",
                              "délabrement", "déliquescence", "démolition", "déperdition", "dépérissement", "dépression", "destruction", 
                              "dévastation", "diminution", "dissolution", "écrasement", "écroulement", "effondrement", "effritement", "émoussement",
                              "massacre", "mutilation", "phtisie", "pourrissement", "poussif", "rabougrissement", "ramollissement", 
                              "ratatinement", "ravage", "refroidissement", "submersion", "affectation", "chagrin", "délaissement", 
                              "déréliction", "désespoir", "ennui", "esseulement", "inconsolation", "isolement", "lugubre", "lypémanie", "maussaderie",
                              "mélancolie", "morne", "morosité", "navrement", "nostalgie", "peine", "pessimisme", "solitude", "spleen", 
                              "tristesse", "abandon", "assombrissement", "blues", "bourdon", "cafard", "désolation", "élégie", "fadeur", 
                              "funèbre", "noirceur", "pessimisme", "rembrunissement", "pensée", "viduité", "aliénation", "confusion",
                              "délire", "démence", "déraison", "déséquilibré", "divagation", "extravagance", "folie", "embrouillement", 
                              "larmoiement", "éplorement", "affliction", "aggravation", "agonie", "contorsion", "crève-coeur", "déploration",
                              "deuil", "doléance", "dolence", "douleur", "élancement", "gémissement", "insupportabilité", "invivable",
                              "lamentation", "maltraitance", "morfondement", "pénibilité", "plainte", "ressassement", "rumination", "souffrance",
                              "supplice", "tourment", "tracas", "anéantissement", "arrachement", "complainte", "contusion", "damnation",
                              "déchirement", "difficulté", "dislocation", "hantise", "harcèlement", "irrespirabilité","martyrisation",
                              "obsession", "persécution", "pesanteur", "tenaillement", "tiraillement", "traque", "ulcération"), c("lemmes", "POS")] <- "NCABS"
  
  corpus[corpus$lemmes %in% c("visage", "figure", "cheveu", "front", "sourcil", "yeux", "cil", "nez", "oeil", "bouche", "lèvre", "dos", "ventre",
                              "menton", "joue", "oreille", "gorge", "poil", "bras", "main", "doigt", "jambe", "cuisse", "tête", "cou", "épaule",
                              "coude", "avant-bras", "poignet", "hanche", "genou", "cheville", "front", "orteil", "thorax", "abdomen", "barbe",
                              "moustache", "duvet", "langue", "dent", "corps","tempe", "fesse", "pied"), c("lemmes", "POS")] <- "NCCOR"
  
  ## Cas où l'on part des POS pour changer les lemmes :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADJ", "ADJ")) %>%
    mutate(lemmes = replace(lemmes, POS == "NUM", "NUM")) %>%  
    mutate(lemmes = replace(lemmes, POS == "DETPOSS", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, POS == "NOUN", "NC")) %>%
    mutate(lemmes = replace(lemmes, POS == "PROPN", "NP")) %>%
    mutate(lemmes = replace(lemmes, POS == "INTJ", "INTJ")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "«", '"')) %>% # Remplacement des guillemets français en anglais.
    mutate(lemmes = replace(lemmes, lemmes == "»", '"'))
  
  # Les pronoms personnels et réfléchis : on part des mots pour changer les lemmes : 
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, mots == "je", "je")) %>%
    mutate(lemmes = replace(lemmes, mots == "Je", "je")) %>%
    mutate(lemmes = replace(lemmes, mots == "j'", "je")) %>%
    mutate(lemmes = replace(lemmes, mots == "J'", "je")) %>%
    mutate(lemmes = replace(lemmes, mots == "me", "me")) %>%
    mutate(lemmes = replace(lemmes, mots == "Me", "me")) %>%
    mutate(lemmes = replace(lemmes, mots == "tu", "tu")) %>%
    mutate(lemmes = replace(lemmes, mots == "Tu", "tu")) %>%
    mutate(lemmes = replace(lemmes, mots == "te", "te")) %>%
    mutate(lemmes = replace(lemmes, mots == "Te", "te")) %>%
    mutate(lemmes = replace(lemmes, mots == "Il", "il")) %>%
    mutate(lemmes = replace(lemmes, mots == "il", "il")) %>%
    mutate(lemmes = replace(lemmes, mots == "Elle", "elle")) %>%
    mutate(lemmes = replace(lemmes, mots == "elle", "elle")) %>%
    mutate(lemmes = replace(lemmes, mots == "Se", "se")) %>%
    mutate(lemmes = replace(lemmes, mots == "se", "se")) %>%
    mutate(lemmes = replace(lemmes, mots == "Nous", "nous")) %>%
    mutate(lemmes = replace(lemmes, mots == "nous", "nous")) %>%
    mutate(lemmes = replace(lemmes, mots == "Vous", "vous")) %>%
    mutate(lemmes = replace(lemmes, mots == "vous", "vous")) %>%
    mutate(lemmes = replace(lemmes, mots == "Ils", "ils")) %>%
    mutate(lemmes = replace(lemmes, mots == "ils", "ils")) %>%
    mutate(lemmes = replace(lemmes, mots == "Elles", "elles")) %>%
    mutate(lemmes = replace(lemmes, mots == "elles", "elles"))
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Retrait des lignes vides :
  corpus <- as_tibble(corpus)
  
  
  # Dernières vérifications :
  
  t <- which(corpus$mots == "aux")
  e <- which(corpus$mots == "du")
  f <- which(corpus$mots == "des")
  g <- which(corpus$mots == "au")
  d <- which(corpus$lemmes == "")
  
  if(length(d) > 0) {
    corpus = corpus[-d,1:4] 
  }
  
  retrait_na <- which(corpus$mots == "NA")
  
  if(length(retrait_na) > 0){
    corpus <- corpus[-retrait_na,1:4]
  }
  
  if(length(t) > 0){
    corpus <- corpus[-t,1:4]
  }
  
  if(length(e) > 0){
    corpus <- corpus[-e,1:4]
  }
  
  if(length(f) > 0){
    corpus <- corpus[-f,1:4]
  }
  
  if(length(g) > 0){
    corpus <- corpus[-g,1:4]
  }
  
  # 
  # if(length(t) > 0){
  #   corpus <- corpus %>%
  #     mutate(lemmes = replace(lemmes, mots == "aux", "à_le"))
  # }
  # 
  # if(length(e) > 0){
  #   corpus <- corpus %>%
  #     mutate(lemmes = replace(lemmes, mots == "du", "de_le"))
  # }
  # 
  # if(length(f) > 0){
  #   corpus <- corpus %>%
  #     mutate(lemmes = replace(lemmes, mots == "des", "de_le"))
  # }
  # 
  # if(length(g) > 0){
  #   corpus <- corpus %>%
  #     mutate(lemmes = replace(lemmes, mots == "au", "à_le"))
  # }
  
  
  
  ## Exportation de la première et 3ème colonne csv :
  
  corpus <- corpus[-3] # Suppression colonne POS
  
  ## Retrait des lignes vides :
  
  corpus <- corpus[complete.cases(corpus),]
  
  ## Renommer la colonne motifs :
  
  names(corpus) <- c("mots", "motifs", "Oeuvre")
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Export du corpus pour le retour aux textes :
  
  # export
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Export des motifs simples :
  
  
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Corpus_motifs_UDPipe.csv', tapez 1 et enter \n
                                Dans une variable R corpus_motifs, tapez 2 et enter")))
  if(toprint==1){
    write.csv(corpus, "Corpus_motifs_UDPipe.csv", fileEncoding = "UTF-8")
  }
  if(toprint==2){
    corpus_motifs <<- corpus
  }
}

# Fonction pour le choix du nombre de ngrams (màj 15 mai 2021) : 

choix_nb_ngrams <- function(path = "~/Dropbox/2020-2021/Motifs/",
                            csv = "Corpus_motifs_UDPipe.csv") {
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  
  corpus_spec <-
    fread(
      csv,
      encoding = "UTF-8",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  
  # Vérification okazou :
  
  corpus_spec <- corpus_spec[, c('mots', 'motifs', 'Oeuvre')]
  
  ## Retrait des cases vides okazou :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  # Choix du nombre de ngrams :
  
  choix_nb_grams <-
    as.numeric(readline("Sélectionner le nombre de ngrams (2 à 7) et tapez enter"))
  
  if (choix_nb_grams == 2) {
    # bigrams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(next_motif = lead(motifs)) %>%
      filter(!is.na(next_motif)) %>%
      mutate(ngrammotif = paste(motifs, next_motif))
    
    # bigrams mots :
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(next_word = lead(mots)) %>%
      filter(!is.na(next_word)) %>%
      mutate(ngrammot = paste(mots, next_word))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
    
  }
  
  if (choix_nb_grams == 3) {
    # 3-grams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(next_motif = lead(motifs),
             next_motif2 = lead(motifs, 2)) %>%
      filter(!is.na(next_motif), !is.na(next_motif2)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2))
    
    # 3-grams mots : 
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(next_word = lead(mots),
             next_word2 = lead(mots, 2)) %>%
      filter(!is.na(next_word), !is.na(next_word2)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  if (choix_nb_grams == 4) {
    # 4-grams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3)
      ) %>%
      filter(!is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3))
    
    # 4-grams mots :
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3)
      ) %>%
      filter(!is.na(next_word),!is.na(next_word2),!is.na(next_word3)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2, next_word3))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  if (choix_nb_grams == 5) {
    # Fivegrams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3),
        next_motif4 = lead(motifs, 4)
      ) %>%
      filter(!is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3, next_motif4))
    
    # Fivegrams mots : 
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3),
        next_word4 = lead(mots, 4)
      ) %>%
      filter(!is.na(next_word),!is.na(next_word2),!is.na(next_word3),!is.na(next_word4)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2, next_word3, next_word4))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
    
  }
  
  if (choix_nb_grams == 6) {
    # Sixgrams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3),
        next_motif4 = lead(motifs, 4),
        next_motif5 = lead(motifs, 5)
      ) %>%
      filter(
        !is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4),!is.na(next_motif5)
      ) %>%
      mutate(ngrammotif = paste(
        motifs,
        next_motif,
        next_motif2,
        next_motif3,
        next_motif4,
        next_motif5
      ))
    
    # Sixgrams mots :
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3),
        next_word4 = lead(mots, 4),
        next_word5 = lead(mots, 5)
      ) %>%
      filter(
        !is.na(next_word),!is.na(next_word2),!is.na(next_word3),!is.na(next_word4),!is.na(next_word5)
      ) %>%
      mutate(ngrammot = paste(
        mots,
        next_word,
        next_word2,
        next_word3,
        next_word4,
        next_word5
      ))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  if (choix_nb_grams == 7) {
    # 7-grams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3),
        next_motif4 = lead(motifs, 4),
        next_motif5 = lead(motifs, 5),
        next_motif6 = lead(motifs, 6)
      ) %>%
      filter(
        !is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4),!is.na(next_motif5),!is.na(next_motif6)
      ) %>%
      mutate(
        ngrammotif = paste(
          motifs,
          next_motif,
          next_motif2,
          next_motif3,
          next_motif4,
          next_motif5,
          next_motif6
        )
      )
    
    # 7-grams mots :
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3),
        next_word4 = lead(mots, 4),
        next_word5 = lead(mots, 5),
        next_word6 = lead(mots, 6)
      ) %>%
      filter(
        !is.na(next_word),!is.na(next_word2),!is.na(next_word3),!is.na(next_word4),!is.na(next_word5),!is.na(next_word6)
      ) %>%
      mutate(
        ngrammot = paste(
          mots,
          next_word,
          next_word2,
          next_word3,
          next_word4,
          next_word5,
          next_word6
        )
      )
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  write.csv(corpus_spec_punct, "corpus_motifs_grams.csv", fileEncoding = "UTF-8")
  
}


# Fonction Nuage de mots (màj : 15 mai 2021) : 

motifs_nuage <- function(path = "~/Dropbox/2020-2021/Corpus-test-motifs/", 
                         csv = "Corpus_motifs_UDPipe.csv", nmots = 25){
  
  # Librairies :
  
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("ggwordcloud")
  require("RColorBrewer")
  require("reshape2")
  require("ggsci")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus_spec[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus <- corpus %>%
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  ## Visualisation sur les fréquences absolues :
  
  plot_abs <- ggplot(
    corpus[1:nmots,], # TOdo : changer 50 par une variable dans la fonction
    aes(
      label = motifs, size = n,
      x = Oeuvre, color = Oeuvre, fill = Oeuvre 
    )
  ) +
    geom_text_wordcloud_area(shape = "diamond") +
    scale_size_area(max_size = 15) +
    scale_x_discrete() +
    theme_minimal()
  
  ## Visualisation sur les fréquences relatives :
  
  plot_freq <- ggplot(
    corpus_words_ngrams[1:nmots,], # Choix du nombre de motifs à faire apparaître
    aes(
      label = motifs, size = rel_freq,
      x = Oeuvre, color = Oeuvre, fill = Oeuvre 
    )
  ) +
    geom_text_wordcloud_area(shape = "diamond") +
    scale_size_area(max_size = 15) + # à moduler suivant le nb de motifs
    scale_x_discrete() +
    theme_minimal()
  
  
  freq_rel <- as.numeric(readline("Fréquences relatives, tapez 1 et enter \n Fréquences absolues, tapez 2 et enter"))
  
  if(freq_rel == 1){
    return(plot_freq)
    
  }
  
  
  if(freq_rel == 2){
    return(plot_abs)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères binaires proposés...!")
  }
  
}

# Fonction Histogramme (màj : 15 mai 2021) : 

motifs_histograms <- function(path = "~/Dropbox/2020-2021/Motifs/", 
                              csv = "corpus_motifs_grams.csv", nmots = 25){
  
  # Librairies :
  
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("RColorBrewer")
  require("reshape2")
  require("ggsci")
  require("data.table")
  require("ggpubr")
  
  # Lecture des données :
  
  setwd(path)
  corpus_grams <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_grams <- corpus_grams %>%
    count(motifs, Oeuvre, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_grams %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_grams, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  # Visualisation en histogrammes : 
  
  ## Pour visualisation par fréquences décroissantes :
  
  #Turn your 'treatment' column into a character vector
  corpus_words_ngrams$motifs <- as.character(corpus_words_ngrams$motifs)
  #Then turn it back into a factor with the levels in the correct order
  corpus_words_ngrams$motifs <- factor(corpus_words_ngrams$motifs, levels=unique(corpus_words_ngrams$motifs))
  
  ## Fréquences absolues :
  
  plot_abs <- ggplot(data = corpus_words_ngrams[1:nmots,],
                     aes(x = motifs, y = n, fill = Oeuvre)) +
    geom_histogram(stat = "identity", position = "dodge", show.legend = T) + 
    theme_minimal() +
    theme(legend.position="top")
  
  plot_abs <- plot_abs + theme(axis.text.x= element_text(angle = 50, hjust = 1))
  
  ## Fréquences relatives
  
  # Réordonnancement de la df par fréquences relatives :
  
  df_freq_rel <- corpus_words_ngrams %>%
    arrange(-rel_freq)
  
  # Visualisation :
  
  plot_freq <- ggplot(data = df_freq_rel[1:nmots,],
                      aes(x = motifs, y = rel_freq, fill = Oeuvre)) +
    geom_histogram(stat = "identity", position = "dodge", show.legend = T) + 
    theme_minimal() +
    theme(legend.position="top")
  
  plot_freq <- plot_freq + theme(axis.text.x= element_text(angle = 50, hjust = 1))
  
  freq_rel <- as.numeric(readline("Fréquences relatives, tapez 1 et enter \n Fréquences absolues, tapez 2 et enter"))
  
  if(freq_rel == 1){
    return(plot_freq)
    
  }
  
  
  if(freq_rel == 2){
    return(plot_abs)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères binaires proposés...!")
  }
  
}


# Fonction TF-IDF (màj : 15 mai 2021) :

tf_idf_motifs <- function(path = "~/Dropbox/2020-2021/Motifs/",
                          csv = "corpus_motifs_grams.csv", nombre_motifs = 20){
  
  ## Importation des librairies : 
  
  require("tidytext")
  require("tidyverse")
  require("dplyr")
  # require("slider")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_grams <- fread(csv, encoding = "UTF-8", 
                        header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_words_ngrams <- corpus_grams %>%
    count(Oeuvre, motifs, sort = TRUE) %>%
    filter(n > 1)
  
  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  
  
  # TF-IDF : 
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_words_ngrams %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_words_ngrams, total_words) 
  
  corpus_words_ngrams <- corpus_words_ngrams %>%
    bind_tf_idf(motifs, Oeuvre, n)
  
  corpus_words_ngrams %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  tf_idf_export <- corpus_words_ngrams %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  # Visualisation :
  
  tf_idf_grid <- corpus_words_ngrams %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
    group_by(Oeuvre) %>% 
    top_n(nombre_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
    ungroup %>%
    ggplot(aes(motifs, tf_idf, fill = Oeuvre)) +
    geom_col(show.legend = T) +
    labs(x = NULL, y = "TF-IDF") +
    facet_wrap(~Oeuvre, ncol = 2, scales = "free") +
    coord_flip() +
    theme_minimal()
  
  ## Autre visualisation : ##
  
  tf_idf_all <- corpus_words_ngrams %>% 
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
    top_n(nombre_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
    ungroup() %>%
    mutate(word = reorder(motifs, tf_idf)) %>%
    ggplot(aes(motifs, tf_idf, fill = Oeuvre)) +
    geom_bar(stat = "identity") +
    ylab("TF-IDF") +
    coord_flip() +
    theme_minimal()
  
  plot_tfidf <- as.numeric(readline("Visualisation séparée, tapez 1 et enter \n Visualisation groupée, tapez 2 et enter \n Sauvergarde dans un csv, tapez 3"))
  
  if(plot_tfidf == 1){
    return(tf_idf_grid)
    
  }
  
  if(plot_tfidf == 2){
    return(tf_idf_all)
  }
  
  if(plot_tfidf == 3){
    write_csv(tf_idf_export, "Tf-idf.csv")
  }
  
  else{
    print("Votre choix ne correspond pas aux critères ternaires proposés...!")
  }
  
}

# Fonction Analyse en composante principale (màj : 28 mai 2021) :

motifs_acp <- function(path = "~/Dropbox/2020-2021/Motifs/", csv = "corpus_motifs_grams.csv", 
                       freq_filter = 1, n_obs = 50){
  
  # Librairies : 
  
  require("tidyverse")
  require("tidytext")
  require("dplyr")
  # require("slider")
  require("FactoMineR")
  require("ggplot2")
  require("ggrepel")
  require("ca")
  require("factoextra")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_grams <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]
  
  ## Dénombrement + filtrage des données pour alléger les normalisations :
  corpus_grams <- corpus_grams %>%
    dplyr::count(Oeuvre, motifs, sort = TRUE) %>%
    filter(n > freq_filter)
  
  ## Préparation des données pour normalisation : 
  ## lignes = motifs
  ## colonnes = corpus
  ## Réf : https://stackoverflow.com/questions/19346066/r-re-arrange-dataframe-some-rows-to-columns
  
  corpus_lexical_table <- xtabs(n~motifs+Oeuvre, corpus_grams)
  
  ## Ré-ordonnancement : 
  
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  head(corpus_lexical_table)
  tail(corpus_lexical_table)
  
  ## Normalisations (zscores)
  ## Cf.
  
  # Z-scores sur les fréquences de motifs
  ZTransf = function(x){
    for(i in 1:nrow(x)){
      x[i,] = ( x[i,] - mean(x[i,]) )  / sd(x[i,])
    }
    return(x)
  }
  
  corpus_norm <- ZTransf(corpus_lexical_table)
  
  head(corpus_norm)
  
  # Check na and infinite values : 
  
  a <- is.infinite(corpus_norm)
  b <- which(a == TRUE)
  
  if(length(b) > 0){
    corpus_norm <- corpus_norm[-b,]
  }
  
  c <- is.na(corpus_norm)
  d <- which(c == TRUE)
  
  if(length(d) > 0){
    corpus_norm <- corpus_norm[-d,]
  }
  
  # And now PCA : 
  
  if(n_obs == "all"){
    corpus_PCA <- prcomp(corpus_norm[1:nrow(corpus_norm),], scale. = FALSE)
  }
  else {
    corpus_PCA <- prcomp(corpus_norm[1:n_obs,], scale. = FALSE)
  }
  
  
  # Impression des composants
  
  fviz_eig(corpus_PCA)
  
  # Plot observations !
  
  plot_obs <- fviz_pca_ind(corpus_PCA,
                           col.ind = "coord", # Colorer par le cos2
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = FALSE, 
  )
  
  # Plot variables ! 
  
  plot_var <- fviz_pca_var(corpus_PCA,
                           col.var = "coord", 
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE     
  )
  
  # Plot variables + obs ! 
  
  plot_bis <- fviz_pca_biplot(corpus_PCA, repel = TRUE,
                              col.var = "#2E9FDF", 
                              col.ind = "#696969"  
  )
  
  msg <- as.numeric(readline("Plot variables, tapez 1 et enter \n , Plot motifs tapez 2 et enter \n Plot motifs + variables, tapez 3 et enter"))
  
  if(msg == 1){
    fviz_eig(corpus_PCA)
    return(plot_var)
  }
  if(msg == 2){
    fviz_eig(corpus_PCA)
    return(plot_obs)
  }
  if(msg == 3){
    fviz_eig(corpus_PCA)
    return(plot_bis)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères ternaires proposés...!")
  }
  
}

# Fonction Calcul de spécificités (màj : 15 mai 2021) :

calcul_de_specificites <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                   csv = "corpus_motifs_grams.csv",
                                   retrait_frequence_1 = TRUE){
  
  ## Librairies :
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  
  ## Répertoire de travail :
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus_spec <- corpus_spec[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec <- corpus_spec %>%
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_spec <- left_join(corpus_spec, total_words)
  
  ## Calcul de la fréquence relative :
  
  corpus_spec$rel_freq <- corpus_spec$n / corpus_spec$total
  
  corpus_words_ngrams_spec <- corpus_spec
  
  ## Reshaping the data : colonnes = corpus, lignes = mots et freq
  corpus_lexical_table <- xtabs(n~motifs+Oeuvre, corpus_words_ngrams_spec)
  
  ## Ré-ordonnancement : 
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  # Retrait des lignes contenant des ngrams qui ne sont pas dans tous les textes :
  
  # Cela veut dire : toutes les valeurs de ngrams qui sont uniques (qui contienne 0)
  
  #row_substract <- apply(corpus_lexical_table, 1, function(row) all(row !=0 ))
  
  ## Subset :
  #corpus_clean <- corpus_lexical_table[row_substract,]
  #corpus_clean <- as.matrix(corpus_clean)
  
  corpus_clean <- corpus_lexical_table
  
  ### CALCUL SPÉCIFICITÉS : ###
  
  # lexicaltable = a matrix of nrow parts and ncol type where :
  # f : fréquence absolu dans un corpus 
  # t : nombre de mots du sub-corpus
  # F : nombre d'apparition du mots dans tout le corpus :
  # T : nombre total de mots dans le corpus
  
  `specificites` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      spe <- specificites.probabilities(lexicaltable, types, parts);
      #dim(spe);
      spelog <- matrix(0, nrow=nrow(spe), ncol=ncol(spe));
      spelog[spe < 0.5] <- log10(spe[spe < 0.5]);
      spelog[spe > 0.5] <- abs(log10(1 - spe[spe > 0.5]));
      spelog[spe == 0.5] <- 0;
      spelog[is.infinite(spe)] <- 0;
      spelog <- round(spelog, digits=4);
      rownames(spelog) <- rownames(spe);
      colnames(spelog) <- colnames(spe);
      class(spelog) <- "specificites";
      attr(spelog, "frequency.table") <- lexicaltable;
      attr(spelog, "types") <- types;
      attr(spelog, "parts") <- parts;
      attr(spelog, "corpussize") <- attr(spe, "F");
      
      return(spelog);
    }
  
  # lexicaltable = a matrix of nrow parts and ncol type
  `specificites.probabilities` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      
      #if (!is.numeric(lexicaltable)) stop("The lexical table must contain numeric values.");
      
      colMargin <- colSums(lexicaltable); # or "F" (the total frequency of all the types).
      rowMargin <- rowSums(lexicaltable); # or "T" (the size of the parts).
      F <- sum(colMargin);             # The grand total (number of tokens in the corpus).
      
      if (! is.null(types)) {      # Filter on tokens to be considered.
        if(is.character(types)) {  # convert the name of types given with "types" into row index numbers.
          if (is.null(rownames(lexicaltable))) {
            stop("The lexical table has no row names and the \"types\" argument is a character vector.");
          }
          if (! all(types %in% rownames(lexicaltable))) stop(paste(
            "Some requested types are not known in the lexical table: ",
            paste(types[! (types %in% rownames(lexicaltable))], collapse=" ")
          )
          ); 
        } else {
          if (any(types < 1)) stop("The row index must be greater than 0.");
          if (max(types) > nrow(lexicaltable)) stop("Row index must be smaller than the number of rows.");
        }
        lexicaltable <- lexicaltable[ , types, drop = FALSE];
        colMargin <- colMargin[types];
      }
      
      if (! is.null(parts)) {      # Filter on parts to be considered.
        if(is.character(parts)) {  # convert the name of parts given with "parts" into col index numbers.
          if (is.null(colnames(lexicaltable))) {
            stop("The lexical table has no col names and the \"parts\" argument is a character vector.");
          }
          if (! all(parts %in% colnames(lexicaltable))) stop(paste(
            "Some requested parts are not known in the lexical table: ",
            paste(parts[! (parts %in% colnames(lexicaltable))], collapse=" "))
          ); 
        } else {
          if (max(parts) > ncol(lexicaltable)) stop("Column index must be smaller than the number of cols.");
          if (any(parts < 1)) stop("The col index must be greater than 0.");
        }
        lexicaltable <- lexicaltable[parts, , drop=FALSE];
        rowMargin <- rowMargin[parts];
      }
      
      if (nrow(lexicaltable) == 0 | ncol(lexicaltable) == 0) {
        stop("The lexical table must contains at least one row and one column.");
      }
      
      specif <- matrix(0.0, nrow=nrow(lexicaltable), ncol=ncol(lexicaltable));
      
      for(i in 1:nrow(lexicaltable)) {    # We proceed the whole lexical table by row (i.e. by part).
        
        whiteDrawn <- lexicaltable[i,];  # The frequencies observed in this part for each type.
        white <- colMargin;     # The total frequencies in the corpus for each type.
        black <- F-white;       # The total complement frequency in the corpus for each type.
        drawn <- rowMargin[i];  # The total number of occurrences in the part.
        
        independance    <- (white * drawn) / F;         # The theoretic frequency of each type.
        specif_negative <- whiteDrawn <  independance;  # index of observed frequencies below the theoretic frequencies.
        specif_positive <- whiteDrawn >= independance;  # index of observed frequencies above the theoretic frequencies.
        
        specif[i, specif_negative] <- phyper (
          whiteDrawn[specif_negative], white[specif_negative], black[specif_negative], drawn
        );
        
        specif[i, specif_positive] <- phyper (
          whiteDrawn[specif_positive] - 1, white[specif_positive], black[specif_positive], drawn
        );
      }
      
      dimnames(specif) <- dimnames(lexicaltable);
      
      attr(specif, "F") <- F;
      return(specif);
    }
  
  calcul_spec <- specificites(corpus_clean)
  
  ##
  
  calcul_spec_test <- specificites.probabilities(corpus_clean)
  
  head(calcul_spec_test)
  
  #####
  
  calcul_spec <- as.data.frame.matrix(calcul_spec)
  
  # Transformation des lignes dans la variable motifs :
  
  calcul_spec <- setDT(calcul_spec, keep.rownames = "motifs")[]
  
  ## Ajout de la table de fréquences :
  
  colnames(corpus_words_ngrams_spec) <- c("Oeuvre", "motifs", "n", "total", "nrel")
  
  # Fusion des dataframes :
  calcul_spec_freq <- inner_join(corpus_words_ngrams_spec, calcul_spec)
  
  # Retrait éventuel des fréquences < 1 pour réduction de la taille du corpus : 
  
  if(retrait_frequence_1 == TRUE){
    
    calcul_spec_freq <- calcul_spec_freq %>%
      filter(n > 1)
    
  }
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Corpus_motifs_specificites.csv', tapez 1 et enter\nSauvegarder les résulats avec fréquences 'Corpus_spec_freq' (pour retour au texte) tapez 2\nSavegarder les résultats dans une variable 'res', tapez 3")))
  if(toprint==1){
    write.csv(calcul_spec, "Corpus_motifs_specificites.csv", fileEncoding = "UTF-8")
  }
  if(toprint==2){
    write.csv(calcul_spec_freq, "Corpus_spec_freq.csv", fileEncoding = "UTF-8")
  }
  if(toprint==3){
    res <<- calcul_spec_freq
  }
  
}

# Fonction calcul de densité (màj : 15 mai 2021) :

motifs_densite <- function(path = "~/Dropbox/2020-2021/Motifs/", 
                           csv = "corpus_motifs_grams.csv", 
                           filtre = "Flaubert-Bovary.txt", 
                           motif1 = "le NC de le NC",
                           motif2 = "NC de le NC ,",
                           motif3 = "le NC de DETPOSS NC",
                           motif4 = "à le NC de le",
                           motif5 = "NC de le NC .",
                           bd = 4000,
                           titre_graphique = "Densité sur cinq motifs - Madame Bovary"){
  
  require("dplyr")
  require("reshape2")
  require("readr")
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("ggridges")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  # Filtre d'une oeuvre
  
  corpus <- corpus %>%
    filter(Oeuvre == filtre)
  
  # Changement de nom de colonnes pour coller au script : 
  
  corpus_dens <- corpus
  
  names(corpus_dens) <- c('mots', 'ngrammotif', 'Oeuvre')
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_dens <- corpus_dens[,c("mots", "ngrammotif")]
  
  # Extraction des motifs pertinents :
  
  corpus_dens$m1 <- corpus_dens$ngrammotif == motif1
  corpus_dens$m2 <- corpus_dens$ngrammotif == motif2
  corpus_dens$m3 <- corpus_dens$ngrammotif == motif3
  corpus_dens$m4 <- corpus_dens$ngrammotif == motif4
  corpus_dens$m5 <- corpus_dens$ngrammotif == motif5
  
  # Renommer les motifs :
  
  # Transformation des TRUE en rownumber :
  # == Transformer les TRUE en la valeur de l'index correspondante...
  
  true_to_rownb1 = which(corpus_dens$m1 == TRUE)
  corpus_dens$m1[corpus_dens$m1 == TRUE] <- true_to_rownb1
  
  true_to_rownb2 = which(corpus_dens$m2 == TRUE)
  corpus_dens$m2[corpus_dens$m2 == TRUE] <- true_to_rownb2
  
  true_to_rownb3 = which(corpus_dens$m3 == TRUE)
  corpus_dens$m3[corpus_dens$m3 == TRUE] <- true_to_rownb3
  
  true_to_rownb4 = which(corpus_dens$m4 == TRUE)
  corpus_dens$m4[corpus_dens$m4 == TRUE] <- true_to_rownb4
  
  true_to_rownb5 = which(corpus_dens$m5 == TRUE)
  corpus_dens$m5[corpus_dens$m5 == TRUE] <- true_to_rownb5
  
  # Transformer les FALSE en NA :
  
  corpus_dens$m1[corpus_dens$m1 == FALSE] <- 0
  corpus_dens$m2[corpus_dens$m2 == FALSE] <- 0
  corpus_dens$m3[corpus_dens$m3 == FALSE] <- 0
  corpus_dens$m4[corpus_dens$m4 == FALSE] <- 0
  corpus_dens$m5[corpus_dens$m5 == FALSE] <- 0
  
  # Retrait colonne mots :
  
  corpus_dens <- corpus_dens[,-1]
  
  # Renommer les colonnes pour que les motifs soient affichés dans le graphique
  
  names(corpus_dens) <- c("ngrammotifs", as.character(motif1), as.character(motif2), as.character(motif3),
                          as.character(motif4), as.character(motif5))
  
  # Transformation des données :
  
  corpus_melt <- melt(corpus_dens, id.var = "ngrammotifs")
  
  names(corpus_melt) <- c("ngrammotifs", "motifs", "value")
  
  # Transformation des 0 en NA
  
  corpus_melt$value[corpus_melt$value == 0] <- NA
  
  # Ajout d'une colonne index pour x : 1:n où n est le nb de mots dans l'oeuvre :
  # Jouer avec le paramètre bandwith pour faire varier les courbes de densité et mettre à 
  # la bonne échelle.
  
  ggplot(corpus_melt, aes(x = `value`, y = `motifs`, fill = `motifs`)) +
    stat_density_ridges(bandwidth = bd, na.rm = T) +
    scale_fill_brewer() +
    labs(title = titre_graphique) +
    theme_bw()
  
}

# Fonction pour statistiques générales (màj : 15 mai 2021) :

stats_motifs <- function(path = "~/Dropbox/2020-2021/Motifs/", 
                         csv = "corpus_motifs_grams.csv"){
  
  ## Librairies :
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  
  ## Répertoire de travail :
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus_spec <- corpus_spec[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## ## ##  BARYCENTRES ## ## ## ## ## ## ## ## ## ## ## ## ## #
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  corpus_punct <- corpus_spec
  
  corpus_punct$index <- cumsum(!duplicated(corpus_punct$Oeuvre))
  
  # Correction d'un léger bug sur la nature de l'objet.
  # int => num
  
  corpus_punct$index <- as.numeric(corpus_punct$index)
  
  # str(corpus_punct)
  
  corpus_punct_n <- corpus_punct %>% 
    dplyr::count(motifs, index, sort = T)
  
  corpus_punct_total <- corpus_punct
  
  corpus_punct_total <- corpus_punct_total %>%
    dplyr::ungroup() %>%
    dplyr::count(motifs, sort = T)
  
  names(corpus_punct_total) <- c("motifs", "n_total")
  
  corpus_baryc <- inner_join(corpus_punct_n, corpus_punct_total)
  ## Colonnes de fréquences relative et absolues. ## 
  
  # Somme du nombre d'occ * indice / Nombre total d'occurrence de la forme.
  
  corpus_baryc <- corpus_baryc %>%
    ungroup() %>%
    mutate(barycentre = n * index / n_total)
  
  # Barycentres qui vont de 0 à 3 :
  # 3 : le motif représente toutes les occurrences totales : correspond donc à un motif qui n'est présent que dans une oeuvre.
  # 1.5 : le motif représente la moitié des occurrences totales : il est donc présent deux fois plus que dans le reste du corpus.
  # 0,1 : le motif est quasi-absent de l'oeuvre, mais très présent dans d'autres.
  
  round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  baryc_arrondi <- round_df(corpus_baryc$barycentre, 3)
  
  # Changement des valeurs dans la dataframe :
  
  corpus_baryc$barycentre <- baryc_arrondi
  
  # Ordonnancement : 
  
  corpus_baryc <- corpus_baryc[order(-corpus_baryc$n_total),]
  
  # Transformation en pourcentage :
  
  corpus_barycentre_pourcentage <- corpus_baryc %>%
    mutate(pourcentage = n / n_total * 100)
  
  poucentage_arrondi <- round_df(corpus_barycentre_pourcentage$pourcentage, 2)
  
  # Changement des valeurs dans la dataframe :
  
  corpus_barycentre_pourcentage$pourcentage <- poucentage_arrondi
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## ## # SPÉCIFICITÉS # ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  
  corpus_spec <- corpus_spec %>%
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_spec <- left_join(corpus_spec, total_words)
  
  ## Calcul de la fréquence relative :
  
  corpus_spec$rel_freq <- corpus_spec$n / corpus_spec$total
  
  corpus_words_ngrams_spec <- corpus_spec
  
  ## Reshaping the data : colonnes = corpus, lignes = motifs et freq
  corpus_lexical_table <- xtabs(n~motifs+Oeuvre, corpus_words_ngrams_spec)
  
  ## Ré-ordonnancement : 
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  # Retrait des lignes contenant des ngrams qui ne sont pas dans tous les textes :
  # Cela veut dire : toutes les valeurs de ngrams qui sont uniques (qui contienne 0)
  
  #row_substract <- apply(corpus_lexical_table, 1, function(row) all(row !=0 ))
  
  ## Subset :
  #corpus_clean <- corpus_lexical_table[row_substract,]
  #corpus_clean <- as.matrix(corpus_clean)
  
  corpus_clean <- corpus_lexical_table
  
  ### CALCUL SPÉCIFICITÉS : ###
  
  # lexicaltable = a matrix of nrow parts and ncol type where :
  # f : fréquence absolu dans un corpus 
  # t : nombre de mots du sub-corpus
  # F : nombre d'apparition du mots dans tout le corpus :
  # T : nombre total de mots dans le corpus
  
  `specificites` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      spe <- specificites.probabilities(lexicaltable, types, parts);
      #dim(spe);
      spelog <- matrix(0, nrow=nrow(spe), ncol=ncol(spe));
      spelog[spe < 0.5] <- log10(spe[spe < 0.5]);
      spelog[spe > 0.5] <- abs(log10(1 - spe[spe > 0.5]));
      spelog[spe == 0.5] <- 0;
      spelog[is.infinite(spe)] <- 0;
      spelog <- round(spelog, digits=4);
      rownames(spelog) <- rownames(spe);
      colnames(spelog) <- colnames(spe);
      class(spelog) <- "specificites";
      attr(spelog, "frequency.table") <- lexicaltable;
      attr(spelog, "types") <- types;
      attr(spelog, "parts") <- parts;
      attr(spelog, "corpussize") <- attr(spe, "F");
      
      return(spelog);
    }
  
  # lexicaltable = a matrix of nrow parts and ncol type
  `specificites.probabilities` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      
      #if (!is.numeric(lexicaltable)) stop("The lexical table must contain numeric values.");
      
      colMargin <- colSums(lexicaltable); # or "F" (the total frequency of all the types).
      rowMargin <- rowSums(lexicaltable); # or "T" (the size of the parts).
      F <- sum(colMargin);             # The grand total (number of tokens in the corpus).
      
      if (! is.null(types)) {      # Filter on tokens to be considered.
        if(is.character(types)) {  # convert the name of types given with "types" into row index numbers.
          if (is.null(rownames(lexicaltable))) {
            stop("The lexical table has no row names and the \"types\" argument is a character vector.");
          }
          if (! all(types %in% rownames(lexicaltable))) stop(paste(
            "Some requested types are not known in the lexical table: ",
            paste(types[! (types %in% rownames(lexicaltable))], collapse=" ")
          )
          ); 
        } else {
          if (any(types < 1)) stop("The row index must be greater than 0.");
          if (max(types) > nrow(lexicaltable)) stop("Row index must be smaller than the number of rows.");
        }
        lexicaltable <- lexicaltable[ , types, drop = FALSE];
        colMargin <- colMargin[types];
      }
      
      if (! is.null(parts)) {      # Filter on parts to be considered.
        if(is.character(parts)) {  # convert the name of parts given with "parts" into col index numbers.
          if (is.null(colnames(lexicaltable))) {
            stop("The lexical table has no col names and the \"parts\" argument is a character vector.");
          }
          if (! all(parts %in% colnames(lexicaltable))) stop(paste(
            "Some requested parts are not known in the lexical table: ",
            paste(parts[! (parts %in% colnames(lexicaltable))], collapse=" "))
          ); 
        } else {
          if (max(parts) > ncol(lexicaltable)) stop("Column index must be smaller than the number of cols.");
          if (any(parts < 1)) stop("The col index must be greater than 0.");
        }
        lexicaltable <- lexicaltable[parts, , drop=FALSE];
        rowMargin <- rowMargin[parts];
      }
      
      if (nrow(lexicaltable) == 0 | ncol(lexicaltable) == 0) {
        stop("The lexical table must contains at least one row and one column.");
      }
      
      specif <- matrix(0.0, nrow=nrow(lexicaltable), ncol=ncol(lexicaltable));
      
      for(i in 1:nrow(lexicaltable)) {    # We proceed the whole lexical table by row (i.e. by part).
        
        whiteDrawn <- lexicaltable[i,];  # The frequencies observed in this part for each type.
        white <- colMargin;     # The total frequencies in the corpus for each type.
        black <- F-white;       # The total complement frequency in the corpus for each type.
        drawn <- rowMargin[i];  # The total number of occurrences in the part.
        
        independance    <- (white * drawn) / F;         # The theoretic frequency of each type.
        specif_negative <- whiteDrawn <  independance;  # index of observed frequencies below the theoretic frequencies.
        specif_positive <- whiteDrawn >= independance;  # index of observed frequencies above the theoretic frequencies.
        
        specif[i, specif_negative] <- phyper (
          whiteDrawn[specif_negative], white[specif_negative], black[specif_negative], drawn
        );
        
        specif[i, specif_positive] <- phyper (
          whiteDrawn[specif_positive] - 1, white[specif_positive], black[specif_positive], drawn
        );
      }
      
      dimnames(specif) <- dimnames(lexicaltable);
      
      attr(specif, "F") <- F;
      return(specif);
    }
  
  calcul_spec <- specificites(corpus_clean)
  
  calcul_spec <- as.data.frame.matrix(calcul_spec)
  
  # Transformation des lignes dans la variable motifs :
  
  calcul_spec <- setDT(calcul_spec, keep.rownames = "motifs")[]
  
  ## Ajout de la table de fréquences :
  
  colnames(corpus_words_ngrams_spec) <- c("Oeuvre", "motifs", "n", "nb_total_mots", "n_rel")
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## FUSION DES DATAFRAMES ## ## ## ## ## ## ## ## ## ## ## ## #
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  
  # Fusion calcul de spécificités avec fréquences absolues, nombre total de mots dans le corpus, frequences relatives, specificité.
  
  calcul_spec_freq <- inner_join(corpus_words_ngrams_spec, calcul_spec)
  
  # Ajout des barycentres :
  
  corpus_final <- inner_join(calcul_spec_freq, corpus_barycentre_pourcentage)
  
  corpus_final <- subset(corpus_final, select=-c(index))
  
  
  
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Motifs_statistisques.csv', tapez 1 et enter \n Sauvegarder les résultats dans une variable R corpus_final, tapez 2 et enter")))
  if(toprint==1){
    write.csv(corpus_final, "Motifs_statistisques.csv", fileEncoding = "UTF-8")
  }
  if(toprint==2){
    result_df_stats <<- corpus_final
  }
  
}


# Fonction de retour aux textes (màj : 15 mai 2021) : 

retour_texte_specificites <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                      csv_corpus_motifs = "corpus_motifs_grams.csv",
                                      csv_corpus_specificites = "Corpus_spec_freq.csv", 
                                      frequence = 150){
  
  
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  require("dplyr")
  
  # Chargement des deux corpus :
  
  corpus_spec <- fread(csv_corpus_specificites, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Suppression colonne index : 
  
  corpus_spec <- corpus_spec[,-c("V1")]
  
  corpus <- fread(csv_corpus_motifs, encoding = "UTF-8", 
                  header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  # Réduction du corpus_spec à nombre_motifs : évite de produire des trop grand csv,
  # réduit le temps de génération, inutile d'analyser des motifs à très basse fréquence...
  
  corpus_spec <- corpus_spec %>%
    dplyr::filter(n > frequence)
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  
  
  
  retour_aux_textes <- function(corpus_spec){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    #keyword<- (readline("Entrez le motif : \n"))
    hits <- which(corpus$motifs %in% corpus_spec$motifs)
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+as.numeric(longueur_motif) # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus$ngrammot[hits[h]], collapse=" "), 
                     paste(corpus$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus$motifs[hits[h]], collapse = " "))
        result<-rbind(result,myrow)
        
      }
      colnames(result)<-c("id", "contexte_gauche", "motif", "contexte_droit", "Oeuvre", "motifs")
      result <- as_tibble(result)
      result <- inner_join(result, corpus_spec)
      result <- result[order(result$nrel),]
      toprint<-as.numeric((readline("Sauvegarder les résultats en csv, tapez 1 et enter \n, Sauvegarder dans un objet R result_df, tapez 2 \n")))
      if(toprint==1){
        write.csv(result, "Retour_aux_textes_corpus_specificites.csv", fileEncoding = "UTF-8")
      }
      if(toprint==2){
        result_df <<- result
      }
    }
    else {
      print("Votre motif n'a pas été trouvé")
    }
  }
  
  retour_aux_textes(corpus_spec)
  
}

# Fonction de retour aux textes pour un motif spécifique (màj : 15 mai 2021) :

retour_texte_specificites_un_motif <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                               csv_corpus_motifs = "corpus_motifs_grams.csv",
                                               csv_corpus_specificites = "Corpus_spec_freq.csv",
                                               motif_cible = "de le NC de le NC ,"){
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  library("dplyr")
  
  # Chargement des deux corpus :
  
  corpus_spec <- fread(csv_corpus_specificites, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Suppression colonne index : 
  
  corpus_spec <- corpus_spec[,-c("V1")]
  
  corpus <- fread(csv_corpus_motifs, encoding = "UTF-8", 
                  header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  retour_aux_textes <- function(corpus){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    keyword<- as.character(motif_cible)
    hits <- which(corpus$motifs == as.character(keyword))
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+as.numeric(longueur_motif) # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus$ngrammot[hits[h]], collapse=" "), 
                     paste(corpus$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus$motifs[hits[h]], collapse = " "))
        result<-rbind(result,myrow)
        
      }
      colnames(result)<-c("id", "contexte_gauche", "motif", "contexte_droit", "Oeuvre", "motifs")
      result <- as_tibble(result)
      result <- inner_join(result, corpus_spec)
      result <- result[order(result$nrel),]
      toprint<-as.numeric((readline("Sauvegarder les résultats en csv, tapez 1 et enter \n, affichez dans le terminal tapez 2 et enter\n Sauvegarder dans un objet R result_df, tapez 3 \n")))
      if(toprint==1){
        write.csv(result, paste(keyword,"_In_", context, ".csv"), fileEncoding = "UTF-8")
      }
      if(toprint==2){
        return(result)
      }
      if(toprint==3){
        result_df <<- as_tibble(result)
      }
    } 
    else {
      print("Votre motif n'a pas été trouvé")
    }
  }
  
  retour_aux_textes(corpus = corpus)
  
}
