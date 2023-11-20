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
#' corpus_motifs <- regex_corpus_UDPipe(corpus_path="./output/udpipe_corpus_complet.csv", save_output = TRUE)
#'
#' @export
regex_corpus_udpipe <- function(corpus = NULL, corpus_path = NULL, 
                                save_output = FALSE, save_path = NULL, 
                                overwrite=FALSE){
  # For R CMD check "no visible binding for global variable"
  POS <- lemmes <- feats <- mots <- NULL
  
  # Lecture des donnees :
  check_object_param(corpus, corpus_path)
  if (is.null(corpus)){
    corpus = import_table(corpus_path, file_name = "udpipe_corpus_complet.csv")
  }
  ## Verification que les colonnes sont les bonnes :
  corpus <- corpus[,c('mots', 'lemmes', 'POS', 'feats', 'Oeuvre')] 
  
  
  #### AUXILIAIRES ####
  
  # ajout du path pour les regles de transformation : 
  path_to_aux <- system.file("extdata", "regle_transformation_motifs", "auxiliaires.txt", package = "MotiveR")
  
  aux <- read.csv(path_to_aux)
  
  # Auxiliaire index : 
  id_aux <- which(corpus$lemmes %in% aux$mots)
  
  # replace: 
  corpus$POS[id_aux] <- corpus$lemmes[id_aux]
  corpus$feats[id_aux] <- corpus$lemmes[id_aux]
  
  #### MORPH ####
  
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
  
  # Subjonctif present :
  
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
  
  # Imperatif present :
  
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
  
  # Indicatif present :
  
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
  
  # Passe simple : 
  
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
  
  # Determinants possessifs :
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DEPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    dplyr::mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS"))
  
  # Retrait colonne morphologie :
  
  corpus <- corpus[,-4]
  
  #### Mots invariables ####
  
  path_to_inv <- system.file("extdata", "regle_transformation_motifs", "mots_invariables.txt", package = "MotiveR")
  
  inv <- read.csv(path_to_inv)
  
  # mots invariables index : 
  id_inv <- which(corpus$lemmes %in% inv$mots)
  
  # replace: 
  corpus$POS[id_inv] <- corpus$lemmes[id_inv]
  
  #### Adverbes totalité ####
  
  path_to_advtot <- system.file("extdata", "regle_transformation_motifs", "adverbes_tot.txt", package = "MotiveR")
  
  advtot <- read.csv(path_to_advtot)
  
  # adv tot index : 
  id_advtot <- which(corpus$lemmes %in% advtot$mots)
  
  # replace: 
  corpus$POS[id_advtot] <- "ADVTOT"
  corpus$lemmes[id_advtot] <- "ADVTOT"
  
  #### Adverbes phase ####
  
  path_to_advpha <- system.file("extdata", "regle_transformation_motifs", "adverbes_phase.txt", package = "MotiveR")
  
  advpha <- read.csv(path_to_advpha)
  
  # adv phase index : 
  id_advpha <- which(corpus$lemmes %in% advpha$mots)
  
  # replace: 
  corpus$POS[id_advpha] <- "ADVPHA"
  corpus$lemmes[id_advpha] <- "ADVPHA"
  
  #### Adverbes de frequence #### 
  
  path_to_advfre <- system.file("extdata", "regle_transformation_motifs", "adverbes_freq.txt", package = "MotiveR")
  
  advfre <- read.csv(path_to_advfre)
  
  # adv freq index : 
  id_advfre <- which(corpus$lemmes %in% advfre$mots)
  
  # replace: 
  corpus$POS[id_advfre] <- "ADVFRE"
  corpus$lemmes[id_advfre] <- "ADVFRE"
  
  #### Adverbe intensite ####
  
  path_to_advint <- system.file("extdata", "regle_transformation_motifs", "adverbes_intensite.txt", package = "MotiveR")
  
  advint <- read.csv(path_to_advint)
  
  # adv int index : 
  id_advint <- which(corpus$lemmes %in% advint$mots)
  
  # replace: 
  corpus$POS[id_advint] <- "ADVINT"
  corpus$lemmes[id_advint] <- "ADVINT"
  
  #### Adverbes habitude ####
  
  path_to_advhab <- system.file("extdata", "regle_transformation_motifs", "adverbes_habitude.txt", package = "MotiveR")
  
  advhab <- read.csv(path_to_advhab)
  
  # adv hab index : 
  id_advhab <- which(corpus$lemmes %in% advhab$mots)
  
  # replace: 
  corpus$POS[id_advhab] <- "ADVHAB"
  corpus$lemmes[id_advhab] <- "ADVHAB"
  
  #### Adverbes modaux ####
  
  path_to_advmod <- system.file("extdata", "regle_transformation_motifs", "adverbes_modaux.txt", package = "MotiveR")
  
  advmod <- read.csv(path_to_advmod)
  
  # adv hab index : 
  id_advmod <- which(corpus$lemmes %in% advmod$mots)
  
  # replace: 
  corpus$POS[id_advmod] <- "ADVMOD"
  corpus$lemmes[id_advmod] <- "ADVMOD"
  
  #### Adverbes maniere ####
  
  path_to_advman <- system.file("extdata", "regle_transformation_motifs", "adverbes_maniere.txt", package = "MotiveR")
  
  advman <- read.csv(path_to_advman)
  
  # adv man index : 
  id_advman <- which(corpus$lemmes %in% advman$mots)
  
  # replace: 
  corpus$POS[id_advman] <- "ADVMAN"
  corpus$lemmes[id_advman] <- "ADVMAN"
  
  
  #### Transformation des adverbes restants en ADV ####
  
  corpus <- corpus %>%
    dplyr::mutate(lemmes = replace(lemmes, POS == "ADV", "ADV"))
  
  #### Noms communs abstraits ####
  
  path_to_ncabs <- system.file("extdata", "regle_transformation_motifs", "noms_abstraits.txt", package = "MotiveR")
  
  ncabs <- read.csv(path_to_ncabs)
  
  # ncabs index :
  id_ncabs <- which(corpus$lemmes %in% ncabs$mots)

  # replace: 
  corpus$POS[id_ncabs] <- "NCABS"
  corpus$lemmes[id_ncabs] <- "NCABS"
  
  #### Noms communs corporels ####
  
  path_to_nccor <- system.file("extdata", "regle_transformation_motifs", "parties_corpus.txt", package = "MotiveR")
  
  nccor <- read.csv(path_to_nccor)
  
  # nccor index :
  id_nccor <- which(corpus$lemmes %in% nccor$mots)
  
  # replace: 
  corpus$POS[id_nccor] <- "NCCOR"
  corpus$lemmes[id_nccor] <- "NCCOR"
  
  #### ADJ, NUM, DETPOSS, NC, INTJ
  
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
  
  #### PRONOMS ####
  
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
  
  # Dernières verifications (== a le, de le)
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
  
  ## Exportation de la premiere et 3eme colonne csv :
  
  corpus <- corpus[-3] # Suppression colonne POS
  
  ## Retrait des lignes vides :
  
  corpus <- corpus[complete.cases(corpus),]
  
  ## Renommer la colonne motifs :
  
  names(corpus) <- c("mots", "motifs", "Oeuvre")
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Exportation csv :
  if (!is.null(save_path) | save_output){
    save_data_to_csv(corpus, "udpipe_corpus_motifs.csv", save_path, fileEncoding = "UTF-8", overwrite = overwrite)
  }
  corpus = data.table::as.data.table(corpus)
  
  return(corpus)
  
}
