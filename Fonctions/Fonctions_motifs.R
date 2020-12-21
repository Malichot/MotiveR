## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## ## ## ## ## ## ## ## ## ## ## ##  LISTE DES FONCTIONS ## ## ## ## ## ## ## ## ## ## ## ## ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 


# Fonction Étiquetage : 


annotation_udpipe <- function(path = "~/Dropbox/2019-2020/Stage/Test/", 
                              model = "~/Dropbox/2019-2020/Stage/french-gsd-ud-2.4-190531.udpipe"){
  
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
  
  # Correction apostrophes :
  
  df = df %>%
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

# Fonction Transformation en motifs UDPpipe :

regex_corpus_entier_UDPipe <- function(path = "~/Dropbox/2019-2020/Stage/Test/", corpus = "UDPipe_corpus_complet.csv"){
  
  # Librairies : 
  
  require("stringr")
  # require("plyr")
  require("dplyr")
  require("readr")
  require("data.table")
  setwd(path)
  
  ## Importation du corpus : 
  
  corpus = fread(corpus, encoding = "UTF-8")
  
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
  
  # Démonstratifs :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DEPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS"))
  
  
  # Retrait colonne morphologie :
  
  corpus <- corpus[,-4]
  
  ## Retrait des lignes vides :
  
  corpus <- corpus[complete.cases(corpus),]
  
  ## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 
  
  # REGEX #
  
  ## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 
  
  # Corrections des lemmes mal étiquetés :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "^e$", "AEFFACER")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "vener", "venir")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu dees", "au milieu de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu deu", "au milieu de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu dee", "au milieu de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "est-ce qu", "est-ce que")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "une peu d", "un peu de")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu d", "au milieu de")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "tant d", "tant de")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "tell", "tel")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "nulle", "nul")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "de le côté du", " de le côté de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "^-", "AEFFACER")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "l'une", "le un")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "l'un", "le un")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "quelques-unes", "quelques-uns"))
  
  # Transformation des lemmes en motifs :
  # Il faut ici se fonder sur les lemmes puis transformer les POS pour qu'ils ne soient pas 
  # changer ensuite au cours des transformations futures. Ex : on veut garder le verbe être
  # pour qu'il demeure dans les motifs. Pour cela, obligationd de transformer le POS.
  
  corpus <- corpus %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>% # Auxiliaires
    mutate(POS = replace(POS, lemmes == "avoir", "avoir")) %>% # Auxiliaires
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
    mutate(POS = replace(POS, lemmes == "savoir", "savoir")) %>% # Verbes
    mutate(POS = replace(POS, lemmes == "permettre", "permettre")) %>%
    mutate(POS = replace(POS, lemmes == "sentir", "sentir")) %>%
    mutate(POS = replace(POS, lemmes == "regarder", "regarder")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "écouter", "écouter")) %>%
    mutate(POS = replace(POS, lemmes == "voir", "voir")) %>%
    mutate(POS = replace(POS, lemmes == "vouloir", "vouloir")) %>%
    mutate(POS = replace(POS, lemmes == "tenir", "tenir")) %>%
    mutate(POS = replace(POS, lemmes == "prendre", "prendre")) %>%
    mutate(POS = replace(POS, lemmes == "répondre", "répondre")) %>%
    mutate(POS = replace(POS, lemmes == "pouvoir", "pouvoir")) %>%
    mutate(POS = replace(POS, lemmes == "passer", "passer")) %>%
    mutate(POS = replace(POS, lemmes == "parler", "parler")) %>%
    mutate(POS = replace(POS, lemmes == "laisser", "laisser")) %>%
    mutate(POS = replace(POS, lemmes == "donner", "donner")) %>%
    mutate(POS = replace(POS, lemmes == "croire", "croire")) %>%
    mutate(POS = replace(POS, lemmes == "penser", "penser")) %>%
    mutate(POS = replace(POS, lemmes == "arriver", "arriver")) %>%
    mutate(POS = replace(POS, lemmes == "dire", "dire")) %>%
    mutate(POS = replace(POS, lemmes == "mettre", "mettre")) %>%
    mutate(POS = replace(POS, lemmes == "faire", "faire")) %>%
    mutate(POS = replace(POS, lemmes == "aller", "aller")) %>%
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
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "même", "même")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "au contraire", "au contraire")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "est-ce que", "est-ce que")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "mal", "mal")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
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
    mutate(POS = replace(POS, lemmes == "fort", "fort")) %>%
    mutate(POS = replace(POS, lemmes == "grandement", "grandement")) %>%
    mutate(POS = replace(POS, lemmes == "guère", "guère")) %>%
    mutate(POS = replace(POS, lemmes == "longtemps", "longtemps")) %>%
    mutate(POS = replace(POS, lemmes == "lors", "lors")) %>%
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
    mutate(POS = replace(POS, lemmes == "peut-être", "peut-être"))
  
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
                              ", "colossalement", "considérablement", "onvenablement", "copieusement", "cruellement
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
                              indiscutablement", "indubitablement", "inéluctablement", "inévitablement ", "infailliblement", "
                              manifestement", "naturellement", "nécessairement", "obligatoirement", "plausiblement", "possiblement", "
                              présumablement", "probablement", "supposément", "sûrement", "visiblement", "vraisemblablement", "vraiment", "
                              véritablement", "bien sûr", "certes", "sans doute", "sans aucun doute", "sans nul doute", "certes"), c("lemmes", "POS")] <- "ADVMOD"
  
  ## Retrait des lignes vides :
  corpus <- corpus[complete.cases(corpus),]
  
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
  
  ## Retrait des lignes vides :
  corpus <- corpus[complete.cases(corpus),]
  
  ## Cas où l'on part des POS pour changer les lemmes :
  ## Ici, transformation des adverbes restants en ADV
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADV", "ADV"))
  ## Noms communs :
  
  corpus[corpus$lemmes %in% c("bien-être", "accablement", "acuité", "admiration", "affection", "joie", "alacrité", "allégresse", "
                              amitié", "amour", "tristesse", "angoisse", "animosité", "anxiété", "apaisement", "appréhension", "attendrissement", "
                              attirance", "attraction", "douleur", "peur", "aversion", "plaisir", "bonheur", "sensation", "calme", "désenchantement", "
                              plaisir", "réconfort", "ennui", "orgueil", "bonheur", "écoeurement", "soulagement", "malaise", "trouble", "sentiment", "
                              horreur", "inquiétude", "émotion", "gêne", "hostilité", "paresse", "fierté", "honte", "sympathie", "satisfaction", "
                              tendresse", "retenue", "hostilité", "joie", "jouissance", "chagrin", "consolation", "consternation", "contentement", "
                              contrariété", "crainte", "déception", "regret", "curiosité", "douleur", "doute", "dégoût", "déchirement", "délectation", "
                              délivrance", "dépit", "déplaisir", "désarroi", "désespoir", "désir", "détachement", "détresse", "embarras", "engourdissement", "
                              ennui", "exaltation", "exaspération", "excitation", "fatigue", "frisson", "frémissement", "félicité", "goût", "plaisir", "trouble", "
                              soulagement", "saisissement", "estime", "réconfort", "soulagement", "honte", "peine", "frayeur", "humiliation", "déception", "
                              répugnance", "paresse", "impatience", "pitié", "insouciance", "peine", "satisfaction", "lassitude", "stupeur", "amertume", "
                              solidarité", "souffrance", "indignation", "gratitude", "humiliation", "inquiétude", "irritation", "ivresse", "mélancolie", "
                              mépris", "nostalgie", "désappointement", "passion", "perplexité", "sensation", "ravissement", "reconnaissance", "remord", "
                              respect", "répugnance", "répulsion", "pressentiment", "sentiment", "humiliation", "tentation", "chagrin", "beauté", "charme", "
                              coquetterie", "courage", "courtoisie", "douceur", "ennui", "gaîté", "gentillesse", "laideur", "piété", "imprudence", "prudence", "
                              sagesse", "sensibilité", "insincérité", "sincérité", "sérénité", "timidité", "tristesse", "violence", "abnégation", "ambition", "
                              amoralité", "angélisme", "ardeur", "combativité", "audace", "autoritarisme", "aptitude", "entrain", "ingéniosité", "finesse", "
                              doigté", "tact", "patience", "courage", "volonté", "complaisance", "compréhension", "incompétence", "compétence", "connerie", "
                              bêtise", "curiosité", "célérité", "discernement", "discrétion", "défaitisme", "dévouement", "esprit", "faiblesse", "fantaisie", "
                              fermeté", "finesse", "grossièreté", "générosité", "humanité", "humilité", "ignorance", "imagination", "impartialité", "impatience", "
                              imprudence", "imprévoyance", "incapacité", "inconscience", "indifférence", "indiscipline", "indulgence", "indépendance", "
                              initiative", "insensibilité", "intelligence", "intrépidité", "irrespect", "jovialité", "modestie", "obstination", "optimisme", "
                              originalité", "outrecuidance", "partialité", "impatience", "patience", "patriotisme", "persuasion", "pessimisme", "objectivité", "
                              nonchalance", "modération", "impudeur", "pudeur", "puérilité", "rapidité", "rigueur", "incompétence", "souplesse", "stoïcisme", "
                              prévoyance", "cynisme", "tolérance", "arrogance", "talent", "zèle", "charisme", "sang-froid", "aisance", "bonté", "bravoure", "dilection", "
                              discrétion", "délicatesse", "efficacité", "endurance", "exagération", "élégance", "intelligence", "intrépidité", "intuition", "
                              lucidité", "lâcheté", "mansuétude", "naïveté", "paresse", "impolitesse", "politesse", "résignation", "résistance", "fantaisie", "
                              abnégation", "ambition", "amoralité", "sentiment", "combativité", "audace", "autoritarisme", "autorité", "dévouement", "hospitalité", "
                              impatience", "imprudence", "imprévoyance", "inconscience", "indulgence", "insensibilité", "intrépidité", "intégrité", "irrespect", "
                              mansuétude", "obstination", "optimisme", "outrecuidance", "impartialité", "partialité", "puérilité", "pessimisme", "qualité", "
                              incompétence", "stoïcisme", "dynamisme", "négligence", "avarice", "intrépidité"), c("lemmes", "POS")] <- "NCABS"
  
  corpus[corpus$lemmes %in% c("visage", "figure", "cheveu", "front", "sourcil", "yeux", "cil", "nez", "oeil", "bouche", "lèvre", 
                              "menton", "joue", "oreille", "gorge", "poil", "bras", "main", "doigt", "jambe", "cuisse", "tête", "cou", "épaule", "coude", 
                              "avant-bras", "poignet", "hanche", "genou", "cheville", "front", "orteil", "thorax", "abdomen", "barbe", "moustache", "duvet", 
                              "langue", "dent", "tempe", "fesse", "pied"), c("lemmes", "POS")] <- "NCCOR"
  
  
  ## Simplification des POS avec regex : 
  ## NB : non prise en compte des regex dans les fonctions qui suivent...
  ## Donc obliger de simplifier avant.
  # corpus <- corpus %>%
  #   mutate(POS = str_replace_all(.$POS, "NC.+", "NC")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VPARPM.+", "VPARPM")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VPARPF.+", "VPARPF")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VINDPS.+", "VINDPS")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VINDP3S", "VINDPS")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VSUBP.+", "VSUBP")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VSUBI.+", "VSUBI")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VCOND.+", "VCOND")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VCONP.+", "VCONP")) %>%
  #   mutate(POS = str_replace_all(.$POS, "VINDI.+", "VINDI")) %>%
  #   mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
  #   mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
  #   mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
  #   mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
  #   mutate(POS = str_replace_all(.$POS, "NP.+", "NP"))
  
  # TODO : les regex qui précèdent ne servent à rien dans l'étiquetage UDPIPE, non? 
  
  ## Cas où l'on part des POS pour changer les lemmes :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADJ", "ADJ")) %>%
    mutate(lemmes = replace(lemmes, POS == "NUM", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "DETPOSS", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "elle", "il")) %>%
    mutate(lemmes = replace(lemmes, POS == "NOUN", "NC")) %>%
    mutate(lemmes = replace(lemmes, POS == "PROPN", "NP")) %>%
    mutate(lemmes = replace(lemmes, POS == "INTJ", "INTJ")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "«", "AEFFACER")) %>% # TODO : vérifier si c'est pertinent de garder cela ??
    mutate(lemmes = replace(lemmes, lemmes == "»", "AEFFACER"))
  
  # Manque :  
  
  #^\bque\b\tPRI > que_PR
  #^\bque\b\tSUB > que_SUB
  #^\bque\b\tADV > que_ADV
  
  # Retrait des AEFFACER :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "AEFFACER", "")) %>%
    mutate(lemmes = replace(lemmes, POS == "AEFFACER", ""))
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## FIN DU SCRIPT PERL expressions2.perl ##
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Retrait des lignes vides :
  corpus <- as_tibble(corpus)
  corpus <- corpus[complete.cases(corpus),]
  
  # Vérification :
  
  which(corpus$lemmes == "AEFFACER")
  which(corpus$POS == "AEFFACER")
  which(corpus$lemmes == "")
  which(corpus$POS == "")
  
  t <- which(corpus$mots == "aux")
  e <- which(corpus$mots == "du")
  f <- which(corpus$mots == "des")
  g <- which(corpus$mots == "au")
  d <- which(corpus$lemmes == "")
  # expl <- which(corpus$POS == "PART") # Suppression explétifs.
  
  if(length(d) > 0) {
    corpus = corpus[-d,1:4] 
  }
  
  retrait_na <- which(corpus$mots == "NA")
  
  if(length(retrait_na) > 0){
    corpus <- corpus[-retrait_na,1:4]
  }
  
  if(length(t) > 0){
    corpus <- corpus %>%
      mutate(lemmes = replace(lemmes, mots == "aux", "à_le"))
  }
  
  if(length(e) > 0){
    corpus <- corpus %>%
      mutate(lemmes = replace(lemmes, mots == "du", "de_le"))
  }
  
  if(length(f) > 0){
    corpus <- corpus %>%
      mutate(lemmes = replace(lemmes, mots == "des", "de_le"))
  }
  
  if(length(g) > 0){
    corpus <- corpus %>%
      mutate(lemmes = replace(lemmes, mots == "au", "à_le"))
  }
  
  
  #corpus = corpus[-expl,1:4]
  
  #corpus = corpus[-t,1:4]
  #corpus = corpus[-e,1:4]
  #corpus = corpus[-f,1:4]
  #corpus = corpus[-g,1:4]
  
  ## Exportation de la première et 3ème colonne csv :
  
  corpus <- corpus[-3] # Suppression colonne POS
  
  ## Renommer la colonne motifs :
  
  names(corpus) <- c("mots", "motifs", "Oeuvre")
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Export du corpus pour le retour aux textes :
  
  # export
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Export des motifs simples :
  
  
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Corpus_motifs_UDPipe.csv', tapez 1 et enter")))
  if(toprint==1){
    write.csv(corpus, "Corpus_motifs_UDPipe.csv", fileEncoding = "UTF-8")
  }
}


# Transformation en motifs (Cordial) : 

regex_corpus_entier_Cordial <- function(path = "~/Dropbox/2019-2020/Stage/corpus_test/"){
  
  # Librairies : 
  
  require("stringr")
  require("plyr")
  require("dplyr")
  require("readr")
  
  # Répertoire de travail :
  
  setwd(path)
  
  ## Importation du corpus : 
  
  corpus = plyr::ldply(list.files(pattern = "*.cnr|*.csv"), function(filename) { # ou .csv
    dum = read.csv(filename, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
    #If you want to add the filename as well on the column
    dum$Oeuvre = filename
    return(dum)
  })
  
  ## Retrait des lignes vides :
  
  corpus <- corpus[complete.cases(corpus),]
  
  ## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 
  
  # REGEX #
  
  ## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 
  
  # Corrections des lemmes mal étiquetés :
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "^e$", "AEFFACER")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "vener", "venir")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu dees", "au milieu de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu deu", "au milieu de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu dee", "au milieu de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "est-ce qu", "est-ce que")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "une peu d", "un peu de")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "au milieu d", "au milieu de")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "tant d", "tant de")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "tell", "tel")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "nulle", "nul")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "de le côté du", " de le côté de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "^-", "AEFFACER")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "l'une", "le un")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "l'un", "le un")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "quelques-unes", "quelques-uns"))
  
  # Transformation des lemmes en motifs :
  # Il faut ici se fonder sur les lemmes puis transformer les POS pour qu'ils ne soient pas 
  # changer ensuite au cours des transformations futures. Ex : on veut garder le verbe être
  # pour qu'il demeure dans les motifs. Pour cela, obligationd de transformer le POS.
  
  corpus <- corpus %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>% # Auxiliaires
    mutate(POS = replace(POS, lemmes == "avoir", "avoir")) %>% # Auxiliaires
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
    mutate(POS = replace(POS, lemmes == "savoir", "savoir")) %>% # Verbes
    mutate(POS = replace(POS, lemmes == "permettre", "permettre")) %>%
    mutate(POS = replace(POS, lemmes == "sentir", "sentir")) %>%
    mutate(POS = replace(POS, lemmes == "regarder", "regarder")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "écouter", "écouter")) %>%
    mutate(POS = replace(POS, lemmes == "voir", "voir")) %>%
    mutate(POS = replace(POS, lemmes == "vouloir", "vouloir")) %>%
    mutate(POS = replace(POS, lemmes == "tenir", "tenir")) %>%
    mutate(POS = replace(POS, lemmes == "prendre", "prendre")) %>%
    mutate(POS = replace(POS, lemmes == "répondre", "répondre")) %>%
    mutate(POS = replace(POS, lemmes == "pouvoir", "pouvoir")) %>%
    mutate(POS = replace(POS, lemmes == "passer", "passer")) %>%
    mutate(POS = replace(POS, lemmes == "parler", "parler")) %>%
    mutate(POS = replace(POS, lemmes == "laisser", "laisser")) %>%
    mutate(POS = replace(POS, lemmes == "donner", "donner")) %>%
    mutate(POS = replace(POS, lemmes == "croire", "croire")) %>%
    mutate(POS = replace(POS, lemmes == "penser", "penser")) %>%
    mutate(POS = replace(POS, lemmes == "arriver", "arriver")) %>%
    mutate(POS = replace(POS, lemmes == "dire", "dire")) %>%
    mutate(POS = replace(POS, lemmes == "mettre", "mettre")) %>%
    mutate(POS = replace(POS, lemmes == "faire", "faire")) %>%
    mutate(POS = replace(POS, lemmes == "aller", "aller")) %>%
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
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "même", "même")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "au contraire", "au contraire")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "est-ce que", "est-ce que")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
    mutate(POS = replace(POS, lemmes == "mal", "mal")) %>%
    mutate(POS = replace(POS, lemmes == "être", "être")) %>%
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
    mutate(POS = replace(POS, lemmes == "fort", "fort")) %>%
    mutate(POS = replace(POS, lemmes == "grandement", "grandement")) %>%
    mutate(POS = replace(POS, lemmes == "guère", "guère")) %>%
    mutate(POS = replace(POS, lemmes == "longtemps", "longtemps")) %>%
    mutate(POS = replace(POS, lemmes == "lors", "lors")) %>%
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
    mutate(POS = replace(POS, lemmes == "peut-être", "peut-être"))
  
  
  # if lemmes == x
  # replace pos by lemmes ce qui permet 
  # de sauver ensuite le motifs de la transformation.
  
  
  
  ## Retrait des lignes vides :
  
  corpus <- corpus[complete.cases(corpus),]
  
  ## Suite :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "vener", "venir"))
  
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
                              ", "colossalement", "considérablement", "onvenablement", "copieusement", "cruellement
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
                              indiscutablement", "indubitablement", "inéluctablement", "inévitablement ", "infailliblement", "
                              manifestement", "naturellement", "nécessairement", "obligatoirement", "plausiblement", "possiblement", "
                              présumablement", "probablement", "supposément", "sûrement", "visiblement", "vraisemblablement", "vraiment", "
                              véritablement", "bien sûr", "certes", "sans doute", "sans aucun doute", "sans nul doute", "certes"), c("lemmes", "POS")] <- "ADVMOD"
  
  ## Retrait des lignes vides :
  corpus <- corpus[complete.cases(corpus),]
  
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
  
  ## Retrait des lignes vides :
  corpus <- corpus[complete.cases(corpus),]
  
  ## Cas où l'on part des POS pour changer les lemmes :
  ## Ici, transformation des adverbes restants en ADV
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADV", "ADV"))
  ## Noms communs :
  
  corpus[corpus$lemmes %in% c("bien-être", "accablement", "acuité", "admiration", "affection", "joie", "alacrité", "allégresse", "
                              amitié", "amour", "tristesse", "angoisse", "animosité", "anxiété", "apaisement", "appréhension", "attendrissement", "
                              attirance", "attraction", "douleur", "peur", "aversion", "plaisir", "bonheur", "sensation", "calme", "désenchantement", "
                              plaisir", "réconfort", "ennui", "orgueil", "bonheur", "écoeurement", "soulagement", "malaise", "trouble", "sentiment", "
                              horreur", "inquiétude", "émotion", "gêne", "hostilité", "paresse", "fierté", "honte", "sympathie", "satisfaction", "
                              tendresse", "retenue", "hostilité", "joie", "jouissance", "chagrin", "consolation", "consternation", "contentement", "
                              contrariété", "crainte", "déception", "regret", "curiosité", "douleur", "doute", "dégoût", "déchirement", "délectation", "
                              délivrance", "dépit", "déplaisir", "désarroi", "désespoir", "désir", "détachement", "détresse", "embarras", "engourdissement", "
                              ennui", "exaltation", "exaspération", "excitation", "fatigue", "frisson", "frémissement", "félicité", "goût", "plaisir", "trouble", "
                              soulagement", "saisissement", "estime", "réconfort", "soulagement", "honte", "peine", "frayeur", "humiliation", "déception", "
                              répugnance", "paresse", "impatience", "pitié", "insouciance", "peine", "satisfaction", "lassitude", "stupeur", "amertume", "
                              solidarité", "souffrance", "indignation", "gratitude", "humiliation", "inquiétude", "irritation", "ivresse", "mélancolie", "
                              mépris", "nostalgie", "désappointement", "passion", "perplexité", "sensation", "ravissement", "reconnaissance", "remord", "
                              respect", "répugnance", "répulsion", "pressentiment", "sentiment", "humiliation", "tentation", "chagrin", "beauté", "charme", "
                              coquetterie", "courage", "courtoisie", "douceur", "ennui", "gaîté", "gentillesse", "laideur", "piété", "imprudence", "prudence", "
                              sagesse", "sensibilité", "insincérité", "sincérité", "sérénité", "timidité", "tristesse", "violence", "abnégation", "ambition", "
                              amoralité", "angélisme", "ardeur", "combativité", "audace", "autoritarisme", "aptitude", "entrain", "ingéniosité", "finesse", "
                              doigté", "tact", "patience", "courage", "volonté", "complaisance", "compréhension", "incompétence", "compétence", "connerie", "
                              bêtise", "curiosité", "célérité", "discernement", "discrétion", "défaitisme", "dévouement", "esprit", "faiblesse", "fantaisie", "
                              fermeté", "finesse", "grossièreté", "générosité", "humanité", "humilité", "ignorance", "imagination", "impartialité", "impatience", "
                              imprudence", "imprévoyance", "incapacité", "inconscience", "indifférence", "indiscipline", "indulgence", "indépendance", "
                              initiative", "insensibilité", "intelligence", "intrépidité", "irrespect", "jovialité", "modestie", "obstination", "optimisme", "
                              originalité", "outrecuidance", "partialité", "impatience", "patience", "patriotisme", "persuasion", "pessimisme", "objectivité", "
                              nonchalance", "modération", "impudeur", "pudeur", "puérilité", "rapidité", "rigueur", "incompétence", "souplesse", "stoïcisme", "
                              prévoyance", "cynisme", "tolérance", "arrogance", "talent", "zèle", "charisme", "sang-froid", "aisance", "bonté", "bravoure", "dilection", "
                              discrétion", "délicatesse", "efficacité", "endurance", "exagération", "élégance", "intelligence", "intrépidité", "intuition", "
                              lucidité", "lâcheté", "mansuétude", "naïveté", "paresse", "impolitesse", "politesse", "résignation", "résistance", "fantaisie", "
                              abnégation", "ambition", "amoralité", "sentiment", "combativité", "audace", "autoritarisme", "autorité", "dévouement", "hospitalité", "
                              impatience", "imprudence", "imprévoyance", "inconscience", "indulgence", "insensibilité", "intrépidité", "intégrité", "irrespect", "
                              mansuétude", "obstination", "optimisme", "outrecuidance", "impartialité", "partialité", "puérilité", "pessimisme", "qualité", "
                              incompétence", "stoïcisme", "dynamisme", "négligence", "avarice", "intrépidité"), c("lemmes", "POS")] <- "NCABS"
  
  corpus[corpus$lemmes %in% c("visage", "figure", "cheveu", "front", "sourcil", "yeux", "cil", "nez", "oeil", "bouche", "lèvre", 
                              "menton", "joue", "oreille", "gorge", "poil", "bras", "main", "doigt", "jambe", "cuisse", "tête", "cou", "épaule", "coude", 
                              "avant-bras", "poignet", "hanche", "genou", "cheville", "front", "orteil", "thorax", "abdomen", "barbe", "moustache", "duvet", 
                              "langue", "dent", "tempe", "fesse", "pied"), c("lemmes", "POS")] <- "NCCOR"
  
  ## Simplification des POS avec regex : 
  ## NB : non prise en compte des regex dans les fonctions qui suivent...
  ## Donc obliger de simplifier avant.
  corpus <- corpus %>%
    mutate(POS = str_replace_all(.$POS, "NC.+", "NC")) %>%
    mutate(POS = str_replace_all(.$POS, "VPARPM.+", "VPARPM")) %>%
    mutate(POS = str_replace_all(.$POS, "VPARPF.+", "VPARPF")) %>%
    mutate(POS = str_replace_all(.$POS, "VINDPS.+", "VINDPS")) %>%
    mutate(POS = str_replace_all(.$POS, "VINDP3S", "VINDPS")) %>%
    mutate(POS = str_replace_all(.$POS, "VSUBP.+", "VSUBP")) %>%
    mutate(POS = str_replace_all(.$POS, "VSUBI.+", "VSUBI")) %>%
    mutate(POS = str_replace_all(.$POS, "VCOND.+", "VCOND")) %>%
    mutate(POS = str_replace_all(.$POS, "VCONP.+", "VCONP")) %>%
    mutate(POS = str_replace_all(.$POS, "VINDI.+", "VINDI")) %>%
    mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
    mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
    mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
    mutate(POS = str_replace_all(.$POS, "NH.+", "NH")) %>%
    mutate(POS = str_replace_all(.$POS, "NP.+", "NP"))
  
  # Introduire une transformation des POS en NCCOR, NCABS, ADVMAN, ADVHAB, ADVINT, ADVPHA, ADVFRE
  # Pour les conserver dans l'étape suivante :
  
  corpus <- corpus %>%
    mutate(POS = replace(POS, lemmes == "NCCOR", "NCCOR")) %>%
    mutate(POS = replace(POS, lemmes == "NCABS", "NCABS")) %>%
    mutate(POS = replace(POS, lemmes == "ADVMAN", "ADVMAN")) %>%
    mutate(POS = replace(POS, lemmes == "ADVMOD", "ADVMOD")) %>%
    mutate(POS = replace(POS, lemmes == "ADVINT", "ADVINT")) %>%
    mutate(POS = replace(POS, lemmes == "ADVHAB", "ADVHAB")) %>%
    mutate(POS = replace(POS, lemmes == "ADVFRE", "ADVFRE")) %>%
    mutate(POS = replace(POS, lemmes == "ADVPHA", "ADVPHA")) %>%
    mutate(POS = replace(POS, lemmes == "ADVTOT", "ADVTOT"))
  
  ## Cas où l'on part des POS pour changer les lemmes :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADJSIG", "ADJ")) %>%
    mutate(lemmes = replace(lemmes, POS == "ADJPIG", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJMIN", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJFIN", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJMS", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJPS", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJFS", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJFP", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJMP", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJHFS", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJHMS", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "AHMS", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "ADJHSIG", "ADJ")) %>%
    mutate(lemmes = replace(lemmes, POS == "ADJNUM", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "NCMIN", "DATE")) %>%
    mutate(lemmes = replace(lemmes, POS == "ADJORD", "ADJORD")) %>%
    mutate(lemmes = replace(lemmes, POS == "DETPOSS", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "elle", "il")) %>%
    mutate(lemmes = replace(lemmes, POS == "VINF", "INF")) %>%
    mutate(lemmes = replace(lemmes, POS == "VIMP", "IMP")) %>%
    mutate(lemmes = replace(lemmes, POS == "VPARPRES", "PRES")) %>%
    mutate(lemmes = replace(lemmes, POS == "VPARPM", "PASS")) %>%
    mutate(lemmes = replace(lemmes, POS == "VPARPF", "PASS")) %>%
    mutate(lemmes = replace(lemmes, POS == "VINDPS", "VPS")) %>%
    mutate(lemmes = replace(lemmes, POS == "VINDF", "VF")) %>%
    mutate(lemmes = replace(lemmes, POS == "VSUBP", "VSUBP")) %>%
    mutate(lemmes = replace(lemmes, POS == "VSUBI", "VSUBI")) %>%
    mutate(lemmes = replace(lemmes, POS == "VCOND", "COND")) %>%
    mutate(lemmes = replace(lemmes, POS == "VCONP", "COND")) %>%
    mutate(lemmes = replace(lemmes, POS == "VINDI", "VIMP")) %>% #### PROBLEME ?
    mutate(lemmes = replace(lemmes, POS == "NC", "NC")) %>%
    mutate(lemmes = replace(lemmes, POS == "NH", "NC")) %>%
    mutate(lemmes = replace(lemmes, POS == "NP", "NP")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "du", "de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "des+", "de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "dees", "de le")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "dee", "de")) %>%
    mutate(lemmes = replace(lemmes, POS == "INT", "INT")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "«", "AEFFACER")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "»", "AEFFACER"))
  
  # Manque :  
  
  #^\bque\b\tPRI > que_PR
  #^\bque\b\tSUB > que_SUB
  #^\bque\b\tADV > que_ADV
  
  # Retrait des AEFFACER :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "AEFFACER", "")) %>%
    mutate(lemmes = replace(lemmes, POS == "AEFFACER", ""))
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## FIN DU SCRIPT PERL expressions2.perl ##
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Retrait des lignes vides :
  corpus <- as_tibble(corpus)
  corpus <- corpus[complete.cases(corpus),]
  
  # Vérification :
  
  which(corpus$lemmes == "AEFFACER")
  which(corpus$POS == "AEFFACER")
  which(corpus$lemmes == "")
  which(corpus$POS == "")
  
  # Suppression des blancs :
  
  d <- which(corpus$lemmes == "")
  
  # Conditionnellement sans cela supprime tout :
  
  if(length(d) > 0) {
    corpus = corpus[-d,1:4] 
  }
  
  ## Exportation de la première et 3ème colonne csv :
  
  corpus <- corpus[-3] # Suppression colonne POS
  
  ## Renommer la colonne motifs :
  
  names(corpus) <- c("mots", "motifs", "Oeuvre")
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Export des motifs simples :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Corpus_motifs_Cordial.csv', tapez 1 et enter")))
  if(toprint==1){
    write.csv(corpus, "Corpus_motifs_Cordial.csv", fileEncoding = "UTF-8")
  }
}


# Fonction Nuage de mots : 

motifs_nuage <- function(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", 
                         csv = "Corpus_motifs_UDPipe.csv", nmots = 55){
  
  # Librairies :
  
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("ggwordcloud")
  require("RColorBrewer")
  require("reshape2")
  require("ggsci")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Fivegrams :
  corpus_spec_punct <- corpus_spec  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec_punct <- corpus_spec_punct %>%
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec_punct %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_spec_punct, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  ## Visualisation sur les fréquences absolues :
  
  plot_abs <- ggplot(
    corpus_spec_punct[1:nmots,], # TOdo : changer 50 par une variable dans la fonction
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

# Fonction TF-IDF :

tf_idf_motifs <- function(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/",
                          csv = "Corpus_motifs_UDPipe.csv", nombre_motifs = 20){
  
  ## Importation des librairies : 
  
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Fivegrams :
  corpus_spec_punct <- corpus_spec  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec_punct <- corpus_spec_punct %>%
    count(Oeuvre, motifs, sort = TRUE) %>%
    filter(n > 1)
  
  corpus_words_ngrams <- corpus_spec_punct
  
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
    labs(x = NULL, y = "tf-idf") +
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
  
  plot_tfidf <- as.numeric(readline("Visualisation séparée, tapez 1 et enter \n Visualisation groupée, tapez 2 et enter"))
  
  if(plot_tfidf == 1){
    return(tf_idf_grid)
    
  }
  
  
  if(plot_tfidf == 2){
    return(tf_idf_all)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères binaires proposés...!")
  }
  
}

# Fonction Analyse factorielle des correspondances : 

motifs_afc <- function(path = "~/Dropbox/2019-2020/Stage/Test/", csv = "UDPipe_corpus_complet.csv", nombre_oeuvres = 2, 
                       nmotifs = 30, nombre_dimensions = 5, une_oeuvre = "Rigodon.cnr"){
  
  # Librairies : 
  
  require("tidyverse")
  require("tidytext")
  require("FactoMineR")
  require("ggplot2")
  require("ggrepel")
  require("ca")
  require("factoextra")
  require("data.table")
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Fivegrams :
  corpus_spec_punct <- corpus_spec  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec_punct <- corpus_spec_punct %>%
    dplyr::count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec_punct %>%
    group_by(Oeuvre) %>%
    dplyr::summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_spec_punct, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  ## Reshaping the data : colonnes = corpus, lignes = mots et freq
  # Réf : https://stackoverflow.com/questions/19346066/r-re-arrange-dataframe-some-rows-to-columns
  
  corpus_lexical_table <- xtabs(rel_freq~motifs+Oeuvre, corpus_words_ngrams)
  
  ## Ré-ordonnancement : 
  
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  head(corpus_lexical_table)
  tail(corpus_lexical_table)
  
  
  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  
  
  # Retrait des lignes contenant des ngrams qui ne sont pas dans tous les textes :
  # Cela veut dire : toutes les valeurs de ngrams qui sont uniques (qui contienne 0)
  # Renversement de la dataframe avec variable = corpus
  # rows = motifs
  # Retrait des motifs où une valeur = 0.
  
  ## Nouvelle matrice nettoyée : 
  
  row_substract <- apply(corpus_lexical_table, 1, function(row) all(row !=0 ))
  
  ## Subset :
  
  corpus_clean <- corpus_lexical_table[row_substract,]
  corpus_clean <- as.matrix(corpus_clean)
  head(corpus_clean)
  
  ## Visualisation : 
  
  
  maCA <- CA(corpus_clean, ncp = nombre_dimensions, row.sup = NULL, col.sup = NULL, 
             quanti.sup=NULL, quali.sup = NULL, graph = T, 
             axes = c(1,2), row.w = NULL, excl=NULL)
  
  
  
  # fviz_ca_biplot(maCA, title = "Analyse Factorielle des Correspondances")
  
  plot_ca <- fviz_ca_biplot(maCA, map ="rowprincipal", repel = T, select.row = list(contrib = nmotifs),
                            title = "Analyse Factorielle des Correspondances")
  
  # Avec gradient de couleur en fonction des coordonnées :
  
  plot_grad <- fviz_ca(maCA, map ="rowprincipal", repel = T, select.row = list(contrib = nmotifs), 
                       col.row = "coord", title = "Analyse Factorielle des Correspondances")
  
  # Une oeuvre particulière : 
  
  une_ca <- fviz_ca_biplot(maCA, map ="rowprincipal", repel = T, select.row = list(contrib = nmotifs), 
                           select.col = list(name = une_oeuvre), title = "Analyse Factorielle des Correspondances")
  
  visualisation <- as.numeric(readline("Visualisation, tapez 1 et enter \n Avec gradient de couleurs, tapez 2 \n Une oeuvre particulière, vérifiez que vous l'avez entrée dans le paramètre une_oeuvre et tapez 3"))
  
  if(visualisation == 1){
    return(plot_ca)
    
  }
  
  if(visualisation == 2){
    return(plot_grad)
  }
  
  if(visualisation == 3){
    return(une_ca)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères ternaires proposés...!")
  }
  
}

# Fonction Calcul de spécificités :

calcul_de_specificites <- function(path = "~/Dropbox/2019-2020/Stage/Corpus/", csv = "Corpus_motifs_UDPipe.csv"){
  
  ## Librairies :
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  
  ## Répertoire de travail :
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  corpus_spec <- as_tibble(corpus_spec) %>%
    group_by(Oeuvre)
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Fivegrams :
  corpus_spec_punct <- corpus_spec  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec_punct <- corpus_spec_punct %>%
    count(motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec_punct %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_spec_punct <- left_join(corpus_spec_punct, total_words)
  
  ## Calcul de la fréquence relative :
  
  corpus_spec_punct$rel_freq <- corpus_spec_punct$n / corpus_spec_punct$total
  
  corpus_words_ngrams_spec <- corpus_spec_punct
  
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
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Corpus_motifs_specificites.csv', tapez 1 et enter")))
  if(toprint==1){
    write.csv(calcul_spec_freq, "Corpus_motifs_specificites.csv", fileEncoding = "UTF-8")
  }
  
}

# Fonction Calcul des barycentres : 

barycentre <- function(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", csv = "Corpus_motifs_UDPipe.csv"){
  
  require("dplyr")
  require("readr")
  require("data.table")
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  corpus_spec <- as_tibble(corpus_spec) %>%
    group_by(Oeuvre)
  
  corpus <- corpus_spec
  
  # Vérification okazou :
  names(corpus) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  ## Fivegrams :
  corpus_punct <- corpus  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    #filter(!is.na(next_word), !is.na(next_word2), !is.na(next_word3), !is.na(next_word4)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_punct <- corpus_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_punct) = c("motifs", "Oeuvre")
  
  # il faudrait tout d'abord pouvoir numéroter les périodes par un indice augmentant de +1 à chaque changement de période.
  # La formule utilisée pour calculer le barycentre est alors : B = (Somme pour tout les i de 
  # ( Nombre d'occurences  à la période i * indice de la période i)) divisée par (Nombre total d'occurences de la forme)
  
  # Ajout d'une colonne index pour numéroter les périodes :
  # 1 période = 1 oeuvre :
  
  corpus_punct$index <- cumsum(!duplicated(corpus_punct$Oeuvre))
  
  corpus_punct_n <- corpus_punct %>% 
    dplyr::count(motifs, index, sort = T)
  
  corpus_punct_total <- corpus_punct
  
  corpus_punct_total <- corpus_punct_total %>%
    ungroup(corpus_punct_total) %>%
    count(motifs, sort = T)
  
  names(corpus_punct_total) <- c("motifs", "n_total")
  
  corpus_baryc <- inner_join(corpus_punct_n, corpus_punct_total) ## TODO Ajouter cette fonctionnalité innerjoin aux retours aux textes. ##
  ## Colonnes de fréquences relative et absolues. ## 
  
  # Somme du nombre d'occ * indice / Nombre total d'occurrence de la forme.
  
  corpus_baryc <- corpus_baryc %>%
    ungroup(corpus_baryc) %>%
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
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Barycentre_motifs.csv', tapez 1 et enter \n dans une variable R 'corpus_barycentre', tapez 2")))
  if(toprint==1){
    write.csv(corpus_barycentre_pourcentage, "Barycentre_motifs.csv", fileEncoding = "UTF-8")
  }
  if(toprint==2){
    corpus_barycentre <<- corpus_barycentre_pourcentage
  } 
}

# Fonction calcul de densité :

motifs_densite <- function(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", csv = "Corpus_motifs_UDPipe.csv", 
                           filtre = "13_germinal.txt", motif1 = "NC à le NC de", motif2 = "NC de le NC de",
                           motif3 = "le NC et le NC", motif4 = "le ADJ NC de le", motif5 = "à le NC ce être",
                           bd = 4000, titre_graphique = "Densité sur cinq motifs - Germinal"){
  
  require("dplyr")
  require("reshape2")
  require("readr")
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("ggridges")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  # Filtre d'une oeuvre
  
  corpus_spec <- corpus_spec %>%
    filter(Oeuvre == filtre)
  
  ## Fivegrams :
  corpus_spec_punct <- corpus_spec  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("mots", "ngrammotif")]
  
  # Extraction des motifs pertinents :
  
  corpus_spec_punct$m1 <- corpus_spec_punct$ngrammotif == "NC à le NC de"
  corpus_spec_punct$m2 <- corpus_spec_punct$ngrammotif == "NC de le NC de"
  corpus_spec_punct$m3 <- corpus_spec_punct$ngrammotif == "le NC et le NC"
  corpus_spec_punct$m4 <- corpus_spec_punct$ngrammotif == "le ADJ NC de le"
  corpus_spec_punct$m5 <- corpus_spec_punct$ngrammotif == "à le NC ce être"
  
  # Renommer les motifs :
  
  # Transformation des TRUE en rownumber :
  # == Transformer les TRUE en la valeur de l'index correspondante...
  
  true_to_rownb1 = which(corpus_spec_punct$m1 == TRUE)
  corpus_spec_punct$m1[corpus_spec_punct$m1 == TRUE] <- true_to_rownb1
  
  true_to_rownb2 = which(corpus_spec_punct$m2 == TRUE)
  corpus_spec_punct$m2[corpus_spec_punct$m2 == TRUE] <- true_to_rownb2
  
  true_to_rownb3 = which(corpus_spec_punct$m3 == TRUE)
  corpus_spec_punct$m3[corpus_spec_punct$m3 == TRUE] <- true_to_rownb3
  
  true_to_rownb4 = which(corpus_spec_punct$m4 == TRUE)
  corpus_spec_punct$m4[corpus_spec_punct$m4 == TRUE] <- true_to_rownb4
  
  true_to_rownb5 = which(corpus_spec_punct$m5 == TRUE)
  corpus_spec_punct$m5[corpus_spec_punct$m5 == TRUE] <- true_to_rownb5
  
  # Transformer les FALSE en NA :
  
  corpus_spec_punct$m1[corpus_spec_punct$m1 == FALSE] <- 0
  corpus_spec_punct$m2[corpus_spec_punct$m2 == FALSE] <- 0
  corpus_spec_punct$m3[corpus_spec_punct$m3 == FALSE] <- 0
  corpus_spec_punct$m4[corpus_spec_punct$m4 == FALSE] <- 0
  corpus_spec_punct$m5[corpus_spec_punct$m5 == FALSE] <- 0
  
  # Retrait colonne mots :
  
  corpus_spec_punct <- corpus_spec_punct[-1]
  
  # Renommer les colonnes pour que les motifs soient affichés dans le graphique
  
  names(corpus_spec_punct) <- c("ngrammotifs", as.character(motif1), as.character(motif2), as.character(motif3),
                                as.character(motif4), as.character(motif5))
  
  # Transformation des données :
  
  corpus_melt <- melt(corpus_spec_punct, id.var = "ngrammotifs")
  
  names(corpus_melt) <- c("ngrammotifs", "motifs", "value")
  
  # Transformation des 0 en NA
  
  corpus_melt$value[corpus_melt$value == 0] <- NA
  
  # Ajout d'une colonne index pour x : 1:n où n est le nb de mots dans l'oeuvre :
  # Jouer avec le paramètre bandwith pour faire varier les courbes de densité et mettre à 
  # la bonne échelle.
  
  ggplot(corpus_melt, aes(x = `value`, y = `motifs`, fill = `motifs`)) +
    stat_density_ridges(bandwidth = bd, na.rm = T) +
    scale_fill_viridis_d() +
    labs(title = titre_graphique) +
    theme_bw()
  
}

# Fonction pour statistiques générales :

stats_motifs <- function(path = "~/Dropbox/2019-2020/Stage/Corpus/", csv = "Corpus_motifs_UDPipe.csv"){
  
  ## Librairies :
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  
  ## Répertoire de travail :
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  corpus_spec <- as_tibble(corpus_spec) %>%
    group_by(Oeuvre)
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Fivegrams :
  
  corpus_spec_punct <- corpus_spec  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2), !is.na(next_word3), !is.na(next_word4)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## ## ##  BARYCENTRES ## ## ## ## ## ## ## ## ## ## ## ## ## #
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  corpus_barycentre <- corpus_spec_punct
  
  # il faudrait tout d'abord pouvoir numéroter les périodes par un indice augmentant de +1 à chaque changement de période.
  # La formule utilisée pour calculer le barycentre est alors : B = (Somme pour tout les i de 
  # ( Nombre d'occurences  à la période i * indice de la période i)) divisée par (Nombre total d'occurences de la forme)
  
  # Ajout d'une colonne index pour numéroter les périodes :
  # 1 période = 1 oeuvre :
  
  corpus_barycentre$index <- cumsum(!duplicated(corpus_barycentre$Oeuvre))
  
  corpus_barycentre_n <- corpus_barycentre %>% 
    dplyr::count(motifs, index, sort = T)
  
  corpus_punct_total <- corpus_barycentre
  
  corpus_punct_total <- corpus_punct_total %>%
    ungroup(corpus_punct_total) %>%
    count(motifs, sort = T)
  
  names(corpus_punct_total) <- c("motifs", "n_total")
  
  corpus_baryc <- inner_join(corpus_barycentre_n, corpus_punct_total) ## TODO Ajouter cette fonctionnalité innerjoin aux retours aux textes. ##
  ## Colonnes de fréquences relative et absolues. ## 
  
  # Somme du nombre d'occ * indice / Nombre total d'occurrence de la forme.
  
  corpus_baryc <- corpus_baryc %>%
    ungroup(corpus_baryc) %>%
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
  
  corpus_barycentre_pourcentage$pourcentage <- poucentage_arrondi
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## ## # SPÉCIFICITÉS # ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  
  corpus_spec_punct <- corpus_spec_punct %>%
    count(motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec_punct %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_spec_punct <- left_join(corpus_spec_punct, total_words)
  
  ## Calcul de la fréquence relative :
  
  corpus_spec_punct$rel_freq <- corpus_spec_punct$n / corpus_spec_punct$total
  
  corpus_words_ngrams_spec <- corpus_spec_punct
  
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
  
  colnames(corpus_words_ngrams_spec) <- c("Oeuvre", "motifs", "n", "n_rel", "nb_total_mots")
  
  
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

# Fonction de retour aux textes : 

retour_texte_specificites <- function(csv_corpus_motifs = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_UDPipe.csv",
                                      csv_corpus_specificites = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_specificites.csv", 
                                      frequence = 10){
  
  
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  library("dplyr")
  
  corpus_spec <- fread(csv_corpus_specificites, encoding = "UTF-8")
  corpus <- fread(csv_corpus_motifs)
  
  ## Fivegrams de motifs :
  
  corpus_five <- corpus  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotifs = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  ## Fivegrams texte : 
  
  corpus_five <- corpus_five %>%
    group_by(Oeuvre) %>%
    mutate(next_word = lead(mots),
           next_word2 = lead(mots, 2),
           next_word3 = lead(mots, 3),
           next_word4 = lead(mots, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammots = paste(mots, next_word, next_word2, next_word3, next_word4))
  
  corpus_five <- corpus_five[,c("mots", "ngrammots", "ngrammotifs", "Oeuvre")]
  
  # Réduction du corpus_spec à nombre_motifs : évite de produire des trop grand csv,
  # réduit le temps de génération, inutile d'analyser des motifs à très basse fréquence...
  
  corpus_spec <- corpus_spec %>%
    dplyr::filter(n > frequence)
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  
  retour_aux_textes <- function(corpus_five){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    #keyword<- (readline("Entrez le motif : \n"))
    hits <- which(corpus_five$ngrammotifs %in% corpus_spec$motifs)
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+5 # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus_five$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus_five$ngrammots[hits[h]], collapse=" "), 
                     paste(corpus_five$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus_five$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus_five$ngrammotifs[hits[h]], collapse = " "))
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
  
  retour_aux_textes(corpus_five)
  
}

# Fonction de retour aux textes pour un motif spécifique :

retour_texte_specificites_un_motif <- function(csv_corpus_motifs = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_UDPipe.csv",
                                               csv_corpus_specificites = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_specificites.csv", 
                                               motif_cible = "le NC de le NC"){
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  library("dplyr")
  
  corpus_spec <- fread(csv_corpus_specificites)
  corpus <- fread(csv_corpus_motifs, encoding = "UTF-8")
  
  ## Fivegrams de motifs :
  
  corpus_five <- corpus  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotifs = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  ## Fivegrams texte : 
  
  corpus_five <- corpus_five %>%
    group_by(Oeuvre) %>%
    mutate(next_word = lead(mots),
           next_word2 = lead(mots, 2),
           next_word3 = lead(mots, 3),
           next_word4 = lead(mots, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammots = paste(mots, next_word, next_word2, next_word3, next_word4))
  
  corpus_five <- corpus_five[,c("mots", "ngrammots", "ngrammotifs", "Oeuvre")]
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  retour_aux_textes <- function(corpus_five){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    keyword<- as.character(motif_cible)
    hits <- which(corpus_five$ngrammotifs == as.character(keyword))
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+5 # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus_five$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus_five$ngrammots[hits[h]], collapse=" "), 
                     paste(corpus_five$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus_five$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus_five$ngrammotifs[hits[h]], collapse = " "))
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
  
  retour_aux_textes(corpus_five = corpus_five)
  
}


