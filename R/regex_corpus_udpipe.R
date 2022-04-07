#' Regex corpus UDPipe
#'
#' Transformation en motifs UDPpipe
#'
#' @param corpus data.frame contenant les différents corpus.
#'
#' @param corpus_path string Chemin du dossier contenant les différents corpus.
#' 
#' @param save_output boolean: Sauvegarde les résultats
#' 
#' @param save_path string: Chemin du fichier de sauvergarde
#' 
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#' @return DataFrame: corpus_motifs motifs pour chaque corpus
#'
#' @example
#' corpus_motifs <- regex_corpus_UDPipe("./output/UDPipe_corpus_complet.csv", save_output = TRUE)
#'
#' @export
regex_corpus_udpipe <- function(corpus = NULL, corpus_path = NULL, save_output = FALSE, save_path = NULL, overwrite=FALSE){
  ## Importation du corpus : 
  corpus = import_corpus(corpus, corpus_path, func_name="regex_corpus_udpipe")
  ## Vérification que les colonnes sont les bonnes :
  corpus <- corpus[,c('mots', 'lemmes', 'POS', 'feats', 'Oeuvre')] 
  
  # Auxiliaires :
  corpus <- corpus %>%
    dplyr::mutate(POS = replace(POS, lemmes == "être", "être")) %>% # Auxiliaires
    dplyr::mutate(POS = replace(POS, lemmes == "avoir", "avoir")) # Auxiliaires
  
  # Remplacement des feats avoir et être pour qu'ils ne soient pas transformés :
  # (on garde les auxiliaires)
  
  corpus <- corpus %>%
    dplyr::mutate(feats = replace(feats, lemmes == "avoir", "avoir")) %>%
    dplyr::mutate(feats = replace(feats, lemmes == "être", "être"))
  
  # Infinitifs :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Typo=Yes|VerbForm=Inf", "INF")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "VerbForm=Inf", "INF")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Typo=No|VerbForm=Inf", "INF"))
  
  # Participes : 
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Tense=Past|Typo=Yes|VerbForm=Part", "PPAS")) %>% # PPsé masc sing
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part", "PPAS")) %>% 
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Tense=Past|Typo=Yes|VerbForm=Part", "PPAS")) %>% # PPsé fem sin
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Tense=Past|Typo=Yes|VerbForm=Part", "PPAS")) %>% ## PPsé mas plu
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Plur|Tense=Past|VerbForm=Part", "PPAS")) %>% # PPsé fem plu
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Tense=Past|VerbForm=Part", "PPAS")) %>% # PPsé fem sing
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Tense=Past|VerbForm=Part", "PPAS")) %>% # PPsé masc plu
    dplyr::mutate(lemmes = replace(lemmes, feats == "Tense=Pres|VerbForm=Part", "PPRES")) # Pprésnt.
  
  
  
  
  ############ Remplacement des verbes : ##
  
  # Subjonctif présent :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "VSUBP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin", "VSUBP"))
  
  # Subjonctif imparfait :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=1|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=2|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Sing|Person=3|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=1|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=2|Tense=Imp|VerbForm=Fin", "VSUBI")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Sub|Number=Plur|Person=3|Tense=Imp|VerbForm=Fin", "VSUBI"))
  
  # Impératif présent :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Imp|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "IMP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Imp|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "IMP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Imp|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "IMP"))
  
  # Conditionnel :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "VCOND")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Cnd|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin", "VCOND"))
  
  # Indicatif présent :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin", "PRES")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin", "PRES"))
  
  # Imparfait :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Imp|VerbForm=Fin", "VIMP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Imp|VerbForm=Fin", "VIMP"))
  
  # Passé simple : 
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Past|VerbForm=Fin", "VPS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Past|VerbForm=Fin", "VPS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin", "VPS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Past|VerbForm=Fin", "VPS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Past|VerbForm=Fin", "VPS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Past|VerbForm=Fin", "VPS"))
  
  # Futur :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=1|Tense=Fut|VerbForm=Fin", "VF")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=2|Tense=Fut|VerbForm=Fin", "VF")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Sing|Person=3|Tense=Fut|VerbForm=Fin", "VF")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=1|Tense=Fut|VerbForm=Fin", "VF")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=2|Tense=Fut|VerbForm=Fin", "VF")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Mood=Ind|Number=Plur|Person=3|Tense=Fut|VerbForm=Fin", "VF"))
  
  # Déterminants possessifs :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DEPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS"))
  
  
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
    #dplyr::mutate(POS = replace(POS, lemmes == "être", "être")) %>% # Auxiliaires
    # dplyr::mutate(POS = replace(POS, lemmes == "avoir", "avoir")) %>% # Auxiliaires
    dplyr::mutate(POS = replace(POS, lemmes == "ne", "ne")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "plusieurs", "plusieurs")) %>% # Noms communs
    dplyr::mutate(POS = replace(POS, lemmes == "fois", "fois")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "façon", "façon")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "sorte", "sorte")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "espèce", "espèce")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "jour", "jour")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "nuit", "nuit")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tel", "tel")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "pas vrai", "pas vrai")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "à peu près", "à peu près")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "de le côté du", "de le côté de le")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tenir", "tenir")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "prendre", "prendre")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "pouvoir", "pouvoir")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "passer", "passer")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "parler", "parler")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "laisser", "laisser")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "donner", "donner")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "penser", "penser")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "arriver", "arriver")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "mettre", "mettre")) %>%
    #dplyr::mutate(POS = replace(POS, lemmes == "faire", "faire")) %>%
    #dplyr::mutate(POS = replace(POS, lemmes == "aller", "aller")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "falloir", "falloir")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "finir", "finir")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "commencer", "commencer")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "venir", "venir")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "devoir", "devoir")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "paraître", "paraître")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "sembler", "sembler")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "rester", "rester")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "devenir", "devenir")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "meilleur", "meilleur")) %>% # adjectifs
    dplyr::mutate(POS = replace(POS, lemmes == "vrai", "vrai")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "véritable", "véritable")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "autre", "autre")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "même", "même")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "au contraire", "au contraire")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "est-ce que", "est-ce que")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "mal", "mal")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "plutôt", "plutôt")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "à demi", "à demi")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "à peu près", "à peu près")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "ainsi", "ainsi")) %>% # ADV
    dplyr::mutate(POS = replace(POS, lemmes == "alors", "alors")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "aussi", "aussi")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "autant", "autant")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "aucunement", "aucunement")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "autrement", "autrement")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "avant", "avant")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "beaucoup", "beaucoup")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "bien plus", "bien plus")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "bien moins", "bien moins")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "bien", "bien")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "cependant", "cependant")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "combien", "combien")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "comment", "comment")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "davantage", "davantage")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "déjà", "déjà")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "depuis", "depuis")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "de très près", "de très près")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "dorénavant", "dorénavant")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "encore", "encore")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "en plus", "en plus")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "ensemble", "ensemble")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "environ", "environ")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "delà", "delà")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tandis", "tandis")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "fort", "fort")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "grandement", "grandement")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "guère", "guère")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "longtemps", "longtemps")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "sous", "sous")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "lors", "lors")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "parfois", "parfois")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "ici", "ici")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "travers", "travers")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "où", "où")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "là", "là")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "puis", "puis")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "au-dessus", "au-dessus")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "jusque", "jusque")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "au-dessous", "au-dessous")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "an", "ans")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "mais", "mais")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "mieux", "mieux")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "moins", "moins")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "nullement", "nullement")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "par conséquent", "par conséquent")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "par hasard", "par hasard")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "pas", "pas")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "plus", "plus")) %>%
    # dplyr::mutate(POS = replace(POS, lemmes == "point", "point")) %>% # ATTENTION : ici pb car si NC ne fonctionne pas...
    dplyr::mutate(POS = replace(POS, lemmes == "presque", "presque")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "prou", "prou")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "pourquoi", "pourquoi")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "quasi", "quasi")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "quasiment", "quasiment")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "quelque", "quelque")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "soudain", "soudain")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tant de", "tant de")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tard", "tard")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tôt", "tôt")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "bientôt", "bientôt")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "aussitôt", "aussitôt")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "non plus", "non plus")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tout", "tout")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tout à coup", "tout à coup")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "très", "très")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "trop", "trop")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "après", "après")) %>% 
    dplyr::mutate(POS = replace(POS, lemmes == "voire", "voire")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "peu", "peu")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "heureusement", "heureusement")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "malheureusement", "malheureusement")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "également", "également")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "pourquoi", "pourquoi")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "par trop", "par trop")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tous les jours", "tous les jours")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "de loin", "de loin")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "comme toujours", "comme toujours")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "d'ailleurs", "d'ailleurs")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "dans l'ensemble", "dans l'ensemble")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "surtout", "surtout")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "pourtant", "pourtant")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "réellement", "réellement")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "à la fin", "à la fin")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "à peine", "à peine")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "au début", "au début")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "d'abord", "d'abord")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tout d'abord", "tout d'abord")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "enfin", "enfin")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "ensuite", "ensuite")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "jamais", "jamais")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "toujours", "toujours")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "maintenant", "maintenant")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "si", "si")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "oui", "oui")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "non", "non")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "peut-être", "peut-être")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "monsieur", "monsieur")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "quel", "quel")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "chose", "chose")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "tant", "tant")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "milieu", "milieu")) %>%
    dplyr::mutate(POS = replace(POS, lemmes == "madame", "monsieur"))
  
  
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
    dplyr::mutate(lemmes = replace(lemmes, POS == "ADV", "ADV"))
  
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
    dplyr::mutate(lemmes = replace(lemmes, POS == "ADJ", "ADJ")) %>%
    dplyr::mutate(lemmes = replace(lemmes, POS == "NUM", "NUM")) %>%  
    dplyr::mutate(lemmes = replace(lemmes, POS == "DETPOSS", "DETPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, POS == "NOUN", "NC")) %>%
    dplyr::mutate(lemmes = replace(lemmes, POS == "PROPN", "NP")) %>%
    dplyr::mutate(lemmes = replace(lemmes, POS == "INTJ", "INTJ")) %>%
    dplyr::mutate(lemmes = replace(lemmes, lemmes == "«", '"')) %>% # Remplacement des guillemets français en anglais.
    dplyr::mutate(lemmes = replace(lemmes, lemmes == "»", '"'))
  
  # Les pronoms personnels et réfléchis : on part des mots pour changer les lemmes : 
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "je", "je")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Je", "je")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "j'", "je")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "J'", "je")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "me", "me")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Me", "me")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "tu", "tu")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Tu", "tu")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "te", "te")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Te", "te")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Il", "il")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "il", "il")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Elle", "elle")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "elle", "elle")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Se", "se")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "se", "se")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Nous", "nous")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "nous", "nous")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Vous", "vous")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "vous", "vous")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Ils", "ils")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "ils", "ils")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "Elles", "elles")) %>%
    dplyr::mutate(lemmes = replace(lemmes, mots == "elles", "elles"))
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Retrait des lignes vides :
  corpus <- dplyr::as_tibble(corpus)
  
  
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
  #     dplyr::mutate(lemmes = replace(lemmes, mots == "aux", "à_le"))
  # }
  # 
  # if(length(e) > 0){
  #   corpus <- corpus %>%
  #     dplyr::mutate(lemmes = replace(lemmes, mots == "du", "de_le"))
  # }
  # 
  # if(length(f) > 0){
  #   corpus <- corpus %>%
  #     dplyr::mutate(lemmes = replace(lemmes, mots == "des", "de_le"))
  # }
  # 
  # if(length(g) > 0){
  #   corpus <- corpus %>%
  #     dplyr::mutate(lemmes = replace(lemmes, mots == "au", "à_le"))
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
  
  
  # Exportation csv :
  if (!is.null(save_path) | save_output){
    save_data_to_csv(corpus, "regex_corpus_udpipe", save_path, fileEncoding = "UTF-8", overwrite = overwrite)
  }
  return(corpus)

}
