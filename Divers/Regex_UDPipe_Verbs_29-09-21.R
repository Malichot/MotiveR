# Fonction Transformation en motifs UDPpipe (màj septembre 2021) :

path = "~/Motifs/Test/"
corpus = "UDPipe_corpus_complet.csv"

regex_corpus_entier_UDPipe <- function(path = "~/Dropbox/2020-2021/Motifs/", corpus = "UDPipe_corpus_complet.csv"){
  
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
  
  # Remplacement des POS et feats verbes courants, vb de cognition, perception, diction,
  # pour qu'ils ne soient pas transformés :
  
  corpus <- corpus %>%
    mutate(POS = replace(POS, lemmes == "faire", "faire"))%>% 
    mutate(POS = replace(POS, lemmes == "aller", "aller"))%>% 
    mutate(POS = replace(POS, lemmes == "venir", "venir"))%>% 
    mutate(POS = replace(POS, lemmes == "devenir", "devenir"))%>%
    mutate(POS = replace(POS, lemmes == "pouvoir", "pouvoir"))%>% 
    mutate(POS = replace(POS, lemmes == "devoir", "devoir"))%>% 
    mutate(POS = replace(POS, lemmes == "arriver", "arriver"))%>% 
    mutate(POS = replace(POS, lemmes == "mettre", "mettre"))%>% 
    mutate(POS = replace(POS, lemmes == "permettre", "permettre"))%>% 
    mutate(POS = replace(POS, lemmes == "paraître", "paraître"))%>% 
    mutate(POS = replace(POS, lemmes == "sembler", "sembler"))%>% 
    mutate(POS = replace(POS, lemmes == "laisser", "laisser"))%>% 
    mutate(POS = replace(POS, lemmes == "aller", "aller"))%>% 
    mutate(POS = replace(POS, lemmes == "apercevoir", "VCOG"))%>% 
    mutate(POS = replace(POS, lemmes == "voir", "VCOG"))%>% 
    mutate(POS = replace(POS,lemmes ==  "chuchoter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "crier" , "VDIC%"))%>% 
    mutate(POS = replace(POS,lemmes ==  "dire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "écrire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "envoyer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "glisser" , "VDIC"))%>%
    mutate(POS = replace(POS,lemmes ==  "gueuler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "hurler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "jacter " , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "jurer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "lâcher" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "lancer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "lire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "murmurer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "rapporter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "recrier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "récrire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réécrire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "répliquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "roucouler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "rugir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "souffler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "susurrer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "bigophoner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "câbler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "crayonner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "faxer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "radiotélégraphier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "recâbler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "retélégraphier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "retéléphoner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "télédicter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "télégraphier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "téléphoner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "télésignaliser" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "télexer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "affirmer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "alléguer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "annoncer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "apprendre" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "assener" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "asséner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "assurer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "avouer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "celer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "certifier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "communiquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "concéder" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "confesser" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "confier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "confirmer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "déclarer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "découvrir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "démontrer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "dévoiler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "dissimuler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "excuser" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "expliquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "exposer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "exprimer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "garantir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "improuver" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "inculquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "indémontrer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "indiquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "insinuer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "jurer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "maintenir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "manifester" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "marquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "masquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "mentionner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "narrer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "nier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "notifier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "objecter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "opposer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "pardonner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "parier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "polliciter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "préannoncer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "préciser" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "prédire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "promettre" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "pronostiquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "prophétiser" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "prouver" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "raconter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "rappeler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "rapporter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réaffirmer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réalléguer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réannoncer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "recertifier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "recommuniquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "reconcéder" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "reconfesser" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "reconfirmer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "recorder" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "redéclarer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "redémontrer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "redépeindre" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "redévoiler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "redire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "redissimuler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réexpliquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réexprimer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "regarantir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réinculquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réitérer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "rejurer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "relater" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "remanifester" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "remasquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "remémorer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "renotifier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "réobjecter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "répéter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==   "répondre" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "repréciser" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "représenter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "reprocher" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "repromettre" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "reprouver" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "resignaler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "resignifier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "ressortir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "rétorquer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "retracer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "révéler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "riposter" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "seriner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "signifier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "souligner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "soutenir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "spécifier" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "stipuler" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "taire" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "témoigner" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "jurer" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "proposer" , "VDIC"))%>%  
    mutate(POS = replace(POS,lemmes ==  "autopersuader" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "persuader " , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "geignir" , "VDIC"))%>% 
    mutate(POS = replace(POS,lemmes ==  "prouver", "VDIC"))%>%
    mutate(POS = replace(POS,lemmes == "accroire" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "admettre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "apercevoir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "apprendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "appréhender" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "avaler" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "bouquiner" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "calculer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "chercher" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "comploter" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "comprendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "concevoir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "concocter" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "connaître " , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "conspirer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes ==  "croire" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "deviner" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "discerner" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "distinguer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "déchiffrer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "décoder" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "découvrir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "décrypter" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes ==  "délibérer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "dépister" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "entendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "entendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "entendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes ==  "entendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "entraver" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "entrevoir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "envisager" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "espérer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "excuser" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "flairer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "gamberger" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "gober" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "identifier" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "ignorer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "imaginer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "interpréter" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "intuitionner" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "inventer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "inventer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "lire" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "mécomprendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "méconnaître" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "mécroire" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "mésinterpréter" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "omettre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "oublier" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "ouïr" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "penser" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "percevoir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "piger" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "pourpenser" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "pressentir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "préconcevoir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "rapprendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "reconnaître" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "regarder" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "relire" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "renifler" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "repenser" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "repiger" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "repérer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "retrouver" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "revoir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "réaliser" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "réapprendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "réentendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "réflexionner" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "réimaginer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "réinventer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "résoudre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "rêver" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "saisir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "savoir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "sentir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "soudre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "soupçonner" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "subodorer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "supposer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "surprendre" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "suspecter" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "trouver" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "voir" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "écouter" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "élucider" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "élucubrer" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "éprouver" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "étudier" , "VCOG"))%>%
    mutate(POS = replace(POS,lemmes == "éventer", "VCOG"))
  
  corpus <- corpus %>%
    mutate(feats = replace(feats, lemmes == "faire", "faire"))%>% 
    mutate(feats = replace(feats, lemmes == "aller", "aller"))%>% 
    mutate(feats = replace(feats, lemmes == "venir", "venir"))%>% 
    mutate(feats = replace(feats, lemmes == "devenir", "devenir"))%>%
    mutate(feats = replace(feats, lemmes == "pouvoir", "pouvoir"))%>% 
    mutate(feats = replace(feats, lemmes == "devoir", "devoir"))%>% 
    mutate(feats = replace(feats, lemmes == "arriver", "arriver"))%>% 
    mutate(feats = replace(feats, lemmes == "mettre", "mettre"))%>% 
    mutate(feats = replace(feats, lemmes == "permettre", "permettre"))%>% 
    mutate(feats = replace(feats, lemmes == "paraître", "paraître"))%>% 
    mutate(feats = replace(feats, lemmes == "sembler", "sembler"))%>% 
    mutate(feats = replace(feats, lemmes == "laisser", "laisser"))%>% 
    mutate(feats = replace(feats, lemmes == "aller", "aller"))%>% 
    mutate(feats = replace(feats, lemmes == "apercevoir", "VCOG"))%>% 
    mutate(feats = replace(feats, lemmes == "voir", "VCOG"))%>% 
    mutate(feats = replace(feats,lemmes ==  "chuchoter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "crier" , "VDIC%"))%>% 
    mutate(feats = replace(feats,lemmes ==  "dire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "écrire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "envoyer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "glisser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "gribouiller" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "griffonner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "gueuler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "hurler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "jacter " , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "jurer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "lâcher" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "lancer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "lire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "murmurer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "rapporter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "recrier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "récrire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réécrire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "répliquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "roucouler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "rugir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "souffler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "susurrer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "bigophoner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "câbler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "crayonner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "faxer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "radiotélégraphier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "recâbler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "retélégraphier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "retéléphoner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "télédicter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "télégraphier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "téléphoner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "télésignaliser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "télexer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "affirmer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "alléguer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "annoncer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "apprendre" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "assener" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "asséner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "assurer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "avouer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "celer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "certifier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "communiquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "concéder" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "confesser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "confier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "confirmer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "déclarer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "découvrir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "démontrer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "dévoiler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "dissimuler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "excuser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "expliquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "exposer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "exprimer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "garantir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "improuver" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "inculquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "indémontrer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "indiquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "insinuer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "jurer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "maintenir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "manifester" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "marquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "masquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "mentionner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "narrer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "nier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "notifier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "objecter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "opposer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "pardonner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "parier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "polliciter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "préannoncer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "préciser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "prédire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "promettre" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "pronostiquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "prophétiser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "prouver" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "raconter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "rappeler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "rapporter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réaffirmer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réalléguer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réannoncer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "recertifier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "recommuniquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "reconcéder" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "reconfesser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "reconfirmer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "recorder" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "redéclarer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "redémontrer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "redépeindre" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "redévoiler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "redire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "redissimuler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réexpliquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réexprimer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "regarantir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réinculquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réitérer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "rejurer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "relater" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "remanifester" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "remasquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "remémorer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "renotifier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "réobjecter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "répéter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==   "répondre" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "repréciser" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "représenter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "reprocher" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "repromettre" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "reprouver" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "resignaler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "resignifier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "ressortir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "rétorquer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "retracer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "révéler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "riposter" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "seriner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "signifier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "souligner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "soutenir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "spécifier" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "stipuler" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "taire" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "témoigner" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "jurer" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "proposer" , "VDIC"))%>%  
    mutate(feats = replace(feats,lemmes ==  "autopersuader" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "persuader " , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "geignir" , "VDIC"))%>% 
    mutate(feats = replace(feats,lemmes ==  "prouver", "VDIC"))%>%
    mutate(feats = replace(feats,lemmes == "accroire" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "admettre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "apercevoir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "apprendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "appréhender" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "avaler" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "bouquiner" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "calculer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "chercher" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "comploter" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "comprendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "concevoir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "concocter" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "connaître " , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "conspirer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes ==  "croire" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "deviner" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "discerner" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "distinguer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "déchiffrer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "décoder" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "découvrir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "décrypter" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes ==  "délibérer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "dépister" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "entendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "entendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "entendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes ==  "entendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "entraver" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "entrevoir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "envisager" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "espérer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "excuser" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "flairer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "gamberger" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "gober" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "identifier" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "ignorer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "imaginer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "interpréter" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "intuitionner" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "inventer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "inventer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "lire" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "mécomprendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "méconnaître" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "mécroire" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "mésinterpréter" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "omettre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "oublier" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "ouïr" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "penser" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "percevoir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "piger" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "pourpenser" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "pressentir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "préconcevoir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "rapprendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "reconnaître" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "regarder" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "relire" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "renifler" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "repenser" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "repiger" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "repérer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "retrouver" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "revoir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "réaliser" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "réapprendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "réentendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "réflexionner" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "réimaginer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "réinventer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "résoudre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "rêver" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "saisir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "savoir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "sentir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "soudre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "soupçonner" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "subodorer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "supposer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "surprendre" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "suspecter" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "trouver" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "voir" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "écouter" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "élucider" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "élucubrer" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "éprouver" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "étudier" , "VCOG"))%>%
    mutate(feats = replace(feats,lemmes == "éventer", "VCOG")) 
  
  
  
  
  corpus[corpus$lemmes %in% c("accroire", "admettre", "apercevoir", "apprendre", "appréhender", "avaler", "bouquiner", "calculer", "chercher", "comploter", "comprendre", 
                              "concevoir", "concocter", "connaître ", "conspirer",  "croire", "deviner", "discerner", "distinguer", "déchiffrer", "décoder", "découvrir", "décrypter", 
                              "délibérer", "dépister", "entendre", "entendre", "entendre",  "entendre", "entraver", "entrevoir", "envisager", "espérer", "excuser", "flairer", 
                              "gamberger", "gober", "identifier", "ignorer", "imaginer", "interpréter", "intuitionner", "inventer", "inventer", "lire", "mécomprendre", "méconnaître", "mécroire", 
                              "mésinterpréter", "omettre", "oublier", "ouïr", "penser", "percevoir", "piger", "pourpenser", "pressentir", "préconcevoir", "rapprendre", "reconnaître", "reconstruire",
                              "regarder", "relire", "renifler", "repenser", "repiger", "repérer", "retrouver", "revoir", "rouler", "réaliser", "réapprendre", "réentendre", "réflexionner", 
                              "réimaginer", "réinventer", "résoudre", "rêver", "saisir", "savoir", "sentir", "soudre", "soupçonner", "subodorer", "supposer", "surprendre", "suspecter", "trouver", "voir", 
                              "écouter", "élucider", "élucubrer", "éprouver", "étudier", "éventer"), c("lemmes", "POS", "feats")] <- "VCOG"
  
  corpus[corpus$lemmes %in% c( "bafouiller", "balancer", "balbutier", "baragouiner", "bégayer", "bredouiller", "cafarder", "cafeter", "cafter", "chuchoter", "corner", "crier", "dire", "écrire", 
                               "envoyer", "glisser", "gribouiller", "griffonner", "gueuler", "hurler", "jacter ", "jurer", "lâcher", "lancer", "lire", "murmurer", "rapporter", "recrier", "récrire", "réécrire", "répliquer", 
                               "roucouler", "rugir", "souffler", "susurrer", "bigophoner", "câbler", "crayonner", "faxer", "radiotélégraphier", "recâbler", "retélégraphier", "retéléphoner", "télédicter", "télégraphier", "téléindiquer",
                               "télémater", "téléphoner", "télésignaliser", "télexer", "affirmer", "alléguer", "annoncer", "apprendre", "assener", "asséner", "assurer", "avouer", "celer", "certifier", "communiquer", "concéder", 
                               "confesser", "confier", "confirmer", "déclarer", "découvrir", "démontrer", "dévoiler", "dissimuler", "excuser", "expliquer", "exposer", "exprimer", "garantir", "improuver", "inculquer", "indémontrer", "indiquer", 
                               "insinuer", "jurer", "maintenir", "manifester", "marquer", "masquer", "mentionner", "narrer", "nier", "notifier", "objecter", "opposer", "pardonner", "parier", "polliciter", "préannoncer", "préciser", "prédire", 
                               "promettre", "pronostiquer", "prophétiser", "prouver", "raconter", "rappeler", "rapporter", "réaffirmer", "réalléguer", "réannoncer", "recertifier", "recommuniquer", "reconcéder", "reconfesser", "reconfirmer", "recorder", "redéclarer", 
                               "redémontrer", "redépeindre", "redévoiler", "redire", "redissimuler", "réexpliquer", "réexprimer", "regarantir", "réinculquer", "réitérer", "rejurer", "relater", "remanifester", "remasquer", "remémorer", "renotifier", "réobjecter", "répéter", 
                               "répondre", "repréciser", "représenter", "reprocher", "repromettre", "reprouver", "resignaler", "resignifier", "ressortir", "rétorquer", "retracer", "révéler", "riposter", "seriner", "signifier",  "souligner", "soutenir", "spécifier", "stipuler", 
                               "taire", "témoigner", "jurer",  "proposer", "autopersuader", "persuader ", "geignir", "prouver"), c("lemmes", "POS", "feats")] <- "VDIC"  
  
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

regex_corpus_entier_UDPipe(path = "~/Motifs/Test/", corpus = "UDPipe_corpus_complet.csv")

