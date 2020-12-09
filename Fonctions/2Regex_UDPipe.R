## Legallois - Fonction motifs regex dans R sur UDPipe ##

## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 

# Fonction pour un corpus étiqueté avec des fichiers .cnr ou .csv
# Entrée : corpus = UDPipe_corpus_complet.csv : mots || lemmes || POS || feats || Oeuvre (sortie du script UDPipe_Fonction.R)
# Sortie : Corpus_motifs_UDPipe.csv avec : mots || motifs || Oeuvre.

## ## ## ## ## ## ##  ## ## ## ## ## ##  ## ## ##  ## ## ## 
path = "~/Dropbox/2019-2020/Stage/Zola/"
corpus = "UDPipe_Fortune.csv"
regex_corpus_entier <- function(path = "~/Dropbox/2019-2020/Stage/Test/", corpus = "UDPipe_corpus_complet.csv"){
  
  # Librairies : 
  
  require("stringr")
 # require("plyr")
  require("dplyr")
  require("readr")
  require("data.table")
  setwd(path)
  
  ## Importation du corpus : 
  
  corpus = fread(corpus)
  
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


  # Remplacement des AUX par AUX en feats :
  
  corpus <- corpus %>%
    mutate(feats = replace(feats, lemmes == "AUX", "AUX"))

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
    mutate(lemmes = replace(lemmes, feats == "  Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DEPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Sing|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Masc|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, feats == "Gender=Fem|Number=Plur|Poss=Yes|PronType=Prs", "DETPOSS"))
  

  # Retrait colonne morphologie :
  
  corpus <- corpus[,-4]
  
  ## Retrait des lignes vides :

  corpus <- corpus[complete.cases(corpus),]
  
  ## Suite :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "différent.+", "ADJ")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "vener", "venir")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "absolument|complètement|entièrement|incomplètement|intégralement|parfaitement|partiellement|
                            pleinement|quasiment|radicalement|rigoureusement|strictement|totalement", "ADVTOT")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "actuellement|adultérieurement|anciennement|antécédement|antérieurement|antiquement|
                            dernièrement|diurnement|fraîchement|futurement|imminementimminent|incessamment|initialement|nouvellement|
                            nuitamment|originairement|originellement|postérieurement|posthumément|préalablement|précédemment|
                            précocementprécoce|préliminairement|prématurémentprématuré|présentement|primitivement|prochainement|
                            récemment|tardivementtardif|ultérieurement|ultimement", "ADVPHA")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "accidentellement|annuellement|bihebdomadairement|bimensuellement|bimestriellement
                            |biquotidiennement|bisannuellementcasuellement|chroniquement|constamment|continuellement|épisodiquement|
                            exceptionnellement|fréquemment|hebdomadairement|irrégulièrement|journellement|mensuellement|
                            occasionnellement|périodiquement|perpétuellement|pluriannuellement|quotidiennement|rarement|
                            rarissimement|régulièrement|saisonnièrement|séculairement|semestriellement|sempiternellement|
                            sporadiquement|trimestriellement|trisannuellement", "ADVFRE")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "abominablement|abondamment|admirablement|adorablement|
                            affreusement|amplement|anormalement|appréciablement|ardemment|
                            astronomiquement|atrocement|autrement|bigrement|bougrement|
                            capitalement|catastrophiquement|célestement|chichement|chouettement
                            |colossalement|considérablement|onvenablement|copieusement|cruellement
                            |cuisamment|dangereusement|délicieusement|démentiellement|démesurément
                            |déplorablement|dérisoirement|désastreusement|désespéramment|désespérément
                            |désolamment|détestablement|diablement|diaboliquement|diamétralement|
                            diantrement|disproportionnément|divinement|doublement|draconiennement
                            |drastiquement|drôlement|durement|éclatamment|effrayamment|
                            effroyablement|éhontément|éminemment|énormément|épatamment|éperdument|
                            épouvantablement|étonnamment|exagérément|excédentairement|excellemment|
                            exceptionnellement|excessivement|exécrablement|exorbitamment|exquisément|
                            extraordinairement|extrêmement|fabuleusement|faiblement|fameusement|
                            fantastiquement|faramineusement|farouchement|férocement|fichtrement|
                            fichument|follement|formidablement|fortement|foutrement|foutument|
                            franchement|furieusement|génialement|gigantesquement|grandement|grassement|
                            gravement|grièvement|haïssablement|hautement|hideusement|horriblement|
                            idéalement|immaculément|immensément|immodérément|impayablement|
                            impeccablement|imperceptiblement|implacablement|impressivement|
                            inadmissiblement|inappréciablement|incalculablement|incommensurablement|
                            incomparablement|inconcevablement|incorrigiblement|incorruptiblement|
                            incroyablement|incurablement|indécrottablement|indéfectiblement|
                            indémontablement|indépassablement|indéracinablement|indescriptiblement|
                            indestructiblement|indiciblement|indomptablement|inébranlablement|
                            ineffablement|inégalablement|inénarrablement|inépuisablement|inestimablement|
                            inexpiablement|inexprimablement|inextinguiblement|inextirpablement|
                            inextricablement|infernalement|infiniment|inguérissablement|inimaginablement|
                            inimitablement|innombrablement|inoubliablement|inqualifiablement|insatiablement|
                            insignement|insondablement|insoupçonnablement|insoutenablement|instamment|
                            insuffisamment|insupportablement|insurmontablement|insurpassablement|intarissablement|
                            intenablement|intensément|intensivement|intolérablement|invinciblement|inviolablement|
                            invraisemblablement|invulnérablement|irréconciliablement|irrécusablement|irréductiblement|
                            irréfragablement|irréfutablement|irrémédiablement|irrémissiblement|irréparablement|
                            irrépressiblement|irrésistiblement|irrespirablement|joliment|lamentablement|largement|
                            légèrement|littéralement|magnifiquement|maigrement|maximalement|médiocrement|merveilleusement|
                            minimement|mirifiquement|mirobolamment|modérément|modiquement|monstrueusement|
                            monumentalement|mortellement|moyennement|multiplement|nettement|notablement|
                            outrageusement|outrancièrement|particulièrement|passablement|passionnément|
                            phénoménalement|plantureusement|plénièrement|pléthoriquement|positivement|
                            prodigieusement|profondément|puissamment|quadruplement|quellement|radieusement|
                            raisonnablement|redoutablement|relativement|remarquablement|résolument|ridiculement|
                            rudement|sacrément|satisfaisamment|scandaleusement|sensationnellement|sensiblement|
                            sérieusement|significativement|singulièrement|souverainement|spécialement|spectaculairement|
                            splendidement|sublimement|substantiellement|suffisamment|superbement|supérieurement|
                            superlativement|suprêmement|surabondamment|surhumainement|surréellement|tellement|
                            terriblement|triplement|vachement|vertigineusement|viscéralement|vivement", "ADVINT")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "communément|coutumièrement|généralement|habituellement|invariablement|
                            normalement|ordinairement|rituellement|traditionnellement|usuellement", "ADVHAB")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "apparemment|assurément|certainement|effectivement|éventuellement|
                            évidemment|fatalement|forcément|immanquablement|incontestablement|indéniablement|
                            indiscutablement|indubitablement|inéluctablement|inévitablement |infailliblement|
                            manifestement|naturellement|nécessairement|obligatoirement|plausiblement|possiblement|
                            présumablement|probablement|supposément|sûrement|visiblement|vraisemblablement|vraiment|
                            véritablement|bien sûr|certes|sans doute|sans aucun doute|sans nul doute|certes", "ADVMOD"))
  
  ## Retrait des lignes vides :
  corpus <- corpus[complete.cases(corpus),]
  
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "abjectement|abruptement|abstraitement|abstrusément|absurdement|académiquement|
                            acariâtrement|accortement|acerbement|acidement|âcrement|acrimonieusement|activement|adipeusement|
                            admirativement|adroitement|affablement|affaireusement|affectionnément|affectueusement|agilement|
                            agressivement|aguicheusement|aigrement|aimablement|alertement|allègrement|allusivement|altièrement|
                            ambigument|ambitieusement|amènement|amèrement|amicalement|amiteusement|amoralement|amoureusement|
                            amusamment|amusément|angéliquement|antipathiquement|antisportivement|anxieusement|apathiquement|
                            âprement|archaïquement|arrogamment|artificieusement|artistement|ascétiquement|assidûment|astucieusement|
                            attentivement|audacieusement|austèrement|autonomement|autoritairement|avarement|avaricieusement|
                            aventureusement|avidement|badaudement|badinement|balourdement|banalement|barbarement|baroquement|
                            bassement|batailleusement|bavardement|baveusement|béatement|bégueulement|belliqueusement|bénévolement|
                            bénignement|benoîtement|béotiennement|besogneusement|bestialement|bêtement|bienheureusement|bienveillament|
                            bigotement|bileusement|bilieusement|bizarement|blagueusement|blâmablement|bonassement|bordéliquement|
                            boudeusement|bouffonement|bougonnement|boulimiquement|bourgeoisement|bravachement|bravement|bredouillement|
                            brièvement|brillamment|brouillonnement|brumeusement|brutalement|bruyament|burlesquement|byzantinement|
                            cabotinement|cachottièrement|cafardeusement|cafouilleusement|cajoleusement|câlinement|calmement|calomnieusement|
                            canaillement|candidement|capricieusement|captieusement|caractériellement|cartésiennement|casanièrement|
                            caustiquement|cauteleusement|cavalièrement|célibatairement|cérémonieusement|chafouinement|chagrinement|
                            chaleureusement|chanceusement|charismatiquement|charitablement|charitablement|chastement|chattement|chaudement|
                            chauvinement|chevaleresquement|chicanièrement|chichiteusement|chinoisement|chipoteusement|chiquement|
                            chrétiennement|circonspectement|citadinement|civilement|clairement|classiquement|cocassement|cochonnement|
                            coércitivement|coléreusement|colériquement|comiquement|compendieusement|complaisamment|compréhensivement|
                            concisément|concussionnairement|condamnablement|confusément|connement|consciemment|consciencieusement|
                            conséquemment|considérément|contemplativement|concrètement|coquettement|coquinement|cordialement|
                            coriacement|corrosivement|couardement|coupablement|courageusement|courtoisement|craintivement|crânement|
                            crapuleusement|crédiblement|crédulement|crétinement|criminellement|critiquablement|critiquement|cruellement|
                            crûment|cuistrement|cupidement|curieusement|cyniquement|damnablement|débilement|débonnairement|décemment|
                            décidément|dédaigneusement|défavorablement|dégoûtamment|degueulassement|délicatement|déloyalement|
                            démoniaquement|dépendamment|déplaisement|déraisonnablement|désapprobativement|désespérément|déshonnêtement|
                            désinvoltement|désobligeamment|désordonnément|despotiquement|déterminément|dévotement|dévotieusement|
                            dextrement|dignement|diligemment|dinguement|discourtoisement|discrètement|disertement|disgracieusement|
                            dissolument|distraitement|dithyrambiquement|docilement|doctement|doctoralement|doctrinairement|dogmatiquement|
                            dolemment|dolentement|domestiquement|doucement|doucereusement|droitement|drôlement|dubitativement|durement|
                            dynamiquement|économement|efficacement|effrénément|effrontément|égocentriquement|égoïstement|égrillardement|
                            élégamment|élitistement|élogieusement|éloquemment|emphatiquement|énergiquement|enfantinement|enigmatiquement|
                            enjôleusement|ennuyeusement|ensorceleusement|enthousiastement|envieusement|épicuriennement|épileptiquement|
                            équitablement|équivoquement|érotiquement|éruditement|ésotériquement|espièglement|estimablement|étourdiment|
                            étrangement|euphoriquement|évasivement|exactement|exaspérément|excentriquement|exclusivement|exemplairement|
                            expansivement|expéditivement|expertement|explicitement|facétieusement|factieusement|fadement|fallacieusement|
                            falotement|faméliquement|familièrement|fanatiquement|fanfaronnement|fantaisistement|fantasquement|faraudement|
                            fascistement|fashionablement|fastidieusement|fastueusement|fautivement|favorablement|fébrilement|félinement|
                            félonnement|fémininement|fermement|férocement|fervemment|fiablement|fidèlement|fielleusement|fièrement|
                            fiévreusement|finaudement|finement|flagorneusement|flâneusement|flatteusement|flegmatiquement|fofollement|
                            folâtrement|folkloriquement|follement|fougueusement|fourbement|franchement|fraternellement|frêlement|
                            frénétiquement|frigidement|frileusement|friponnement|frivolement|froidement|froussardement|frustement|
                            fumeusement|funestement|furibondement|furieusement|futilement|gaillardement|gaîment|galamment|gallicanement|
                            gaminement|gâteusement|gauchement|gauloisement|geignardement|généreusement|gentement|gentiment|glacialement|
                            glorieusement|gloutonnement|godichement|goguenardement|gouilleusement|goujatement|goulûment|gourdement|
                            gourmandement|gracieusement|graveleusement|gravement|grincheusement|grivoisement|grossièrement|grotesquement|
                            guillerettement|habilement|hagardement|haineusement|haïssablement|hardiment|hargneusement|hautainement|
                            hérétiquement|héroïquement|hilarement|honnêtement|hospitalièrement|hostilement|humainement|humblement|
                            hyperboliquement|hypocondriaquement|hypocritement|hystériquement|idéalistement|idiotement|idolâtrement|
                            ignarement|ignoblement|ignoramment|imbécilement|immodestement|immondement|immoralement|immortellement|
                            impardonnablement|impartialement|impassiblement|impatiemment|impavidement|impénétrablement|impérieusement|
                            impersonnellement|impertinemment|imperturbablement|impétueusement|impitoyablement|implacablement|impoliment|
                            impopulairement|importunément|imprudemment|impudemment|impudiquement|impulsivement|impurement|inactivement|
                            inamicalement|inanomoviblement|inattaquablement|inattentivement|inauthentiquement|incapablement|incestueusement|
                            incisivement|incivilement|inciviquement|incompétemment|incongrûment|inconsciemment|inconséquemment|inconsolablement|
                            inconstamment|incorrectement|incrédulement|increvablement|indécemment|indélicatement|indépendamment|indévotement|
                            indigemment|indiscrètement|indocilement|indolemment|indulgemment|industrieusement|inélégamment|inertement|
                            inexcusablement|inexpertement|infâmement|infatigablement|infectement|infidèlement|inflexiblement|ingénieusement|
                            ingénument|inglorieusement|ingratement|inhabilement|inhospitalièrement|inhumainement|inintelligemment|iniquement|
                            injurieusement|injustement|inlassablement|innocemment|inoffensivement|inquiètement|insanement|insensément|insincèrement|
                            insipidement|insolemment|insouciamment|insoucieusement|intègrement|intelligemment|interlopement|intraitablement|
                            intrépidement|inventivement|irasciblement|ironiquement|irraisonnablement|irrationnellement|irrépréhensiblement|
                            irréprochablement|irrésolument|irrespectueusement|irresponsablement|irrévérencieusement|jacobinement|jalousement|
                            janséniquement|jésuitement|jobardement|jovialement|joyeusement|judicieusement|justement|lâchement|laconiquement|
                            ladrement|langoureusement|lascivement|lassement|légèrement|lestement|léthargiquement|libéralement|libertinement|
                            libidineusement|librement|licencieusement|lisiblement|logiquement|longanimement|loquacement|louangeusement|
                            louchement|loufoquement|lourdaudement|lourdement|loyalement|lubriquement|lucidement|lunatiquement|luxurieusement|
                            lymphatiquement|lyriquement|macabrement|machiavéliquement|magistralement|magnanimement|majestueusement|
                            maladroitement|malgracieusement|malhabilement|malheusement|malhonnêtement|malicieusement|malignement|
                            malproprement|malveillament|maniaquement|marginalement|marmiteusement|martialement|masculinement|
                            masochistement|matérialistement|maternellement|matoisement|maupiteusement|maussadement|méchamment|
                            méditativement|mélancoliquement|menteusement|méphistophéliquement|méprisablement|mesquinement|
                            mesurément|méthodiquement|méticuleusement|mielleusement|mièvrement|mignardement|mignonnement|
                            minutieusement|misérablement|miséreusement|miséricordieusement|misogynement|mochement|modestement|
                            mollassement|mollement|moqueusement|moralement|morosement|morveusement|muettement|mutinement|
                            naïvement|narcissiquement|narquoisement|naturellement|nébuleusement|négligemment|nerveusement|
                            nettement|niaisement|nigaudement|noblement|nonchalamment|nostalgiquement|obèsement|objectivement|
                            obligeamment|obséquieusement|obtusément|odieusement|oisivement|ombrageusement|onctueusement|
                            opiniâtrement|opportunistement|optimistement|ordurièrement|orgueilleusement|originalement|orthodoxement|
                            oublieusement|outrecuidament|pacifiquement|païennement|paillardement|paisiblement|papelardement|
                            paradoxalement|paranoïaquement|parcimonieusement|paresseusement|partialement|passivement|pataudement|
                            patelinement|paternellement|pathétiquement|patibulairement|patiemment|patriarcalement|pauvrement|
                            paysannement|pédamment|pédantement|peinardement|peineusement|pénardement|penaudement|pendablement|
                            pensivement|pépèrement|péremptoirement|perfidement|permissivement|pernicieusement|perplexement|
                            persévéramment|persifleusement|perspicacement|persuasivement|perversement|pesamment|pessimistement|
                            petitement|peureusement|philistinement|phobiquement|pieusement|pinailleusement|pingrement|piteusement|
                            pitoyablement|placidement|plaignardement|plaintivement|plaisamment|platement|plébéiennement|
                            pleignardement|pleutrement|pointilleusement|poliment|polissonnement|poltronnement|pompeusement|
                            ponctuellement|pondérément|populairement|posément|possessivement|poussivement|pragmatiquement|
                            précautionneusement|précieusement|précisément|présomptueusement|prestement|prétentieusement|primairement|
                            primesautièrement|probement|prodiguement|profanement|prolétairement|prolixement|proprement|provincialement|
                            prudement|prudemment|prudhommesquement|pudibondement|pudiquement|puérilement|pugnacement|purement|
                            puritainement|pusillanimement|quiètement|racoleusement|radinement|rageusement|raidement|railleusement|
                            raisonnablement|rapacement|réactionnairement|réalistement|rébarbativement|répressivement|résolument|
                            respectablement|respectueusement|revêchement|révérement|révérencieusement|rêveusement|ridiculement|
                            rieusement|rigoureusement|ringardement|robustement|roidement|romainement|romanesquement|roturièrement|
                            roublardement|rudement|rustaudement|rustrement|sacrilègement|sadiquement|sagacement|sagement|saintement|
                            salacement|salaudement|salement|sanguinairement|sarcastiquement|sataniquement|sauvagement|savamment|
                            scabreusement|scélératement|sceptiquement|schématiquement|scolairement|scrupuleusement|sèchement|
                            sectairement|sédentairement|séditieusement|sénilement|sensément|sensuellement|sentencieusement|
                            sentimentalement|sereinement|sérieusement|serviablement|servilement|sévèrement|sibyllinement|
                            silencieusement|simplement|sincèrement|singulièrement|sinistrement|siouxement|sobrement|sociablement|
                            soigneusement|solitairement|sombrement|songeusement|sottement|soucieusement|soupçonneusement|souplement|
                            sourcilleusement|sournoisement|souverainement|spartiatement|spirituellement|spontanément|sportivement|
                            staliniennement|stoïquement|strictement|studieusement|stupidement|subtilement|subversivement|
                            succinctement|suicidairement|superficiellement|superfinement|supersitieusement|sûrement|suspicieusement|
                            sympatiquement|taciturnement|talentueusement|taquinement|tatillonnement|teigneusement|témérairement|
                            tenacement|tendrement|ténébreusement|tièdement|timidement|tortueusement|tracassièrement|tranquillement|
                            tristement|trivialement|trouillardement|turpidement|tyranniquement|urbainement|vaillammment|valeureusement|
                            vaniteusement|vantardement|vaseusement|véhémentement|velléitairement|venimeusement|verbeusement|
                            versatilement|vertueusement|vétilleusement|veulement|vicieusement|victorieusement|vieillottement|
                            vigilamment|vigoureusement|vilainement|vilement|vindicativement|violemment|virilement|virulemment|
                            vivement|volagement|volubilement|voluptueusement|voracement|vulgairement|abjectement|adroitement|
                            audacieusement|charitablement|connement|courageusement|cruellement|égoïstement|généreusement|
                            habilement|héroïquement|imbécilement|imprudemment|inintelligemment|intelligemment|lâchement|
                            magnanimement|prudemment|sadiquement|sagement|témérairement|astucieusement|bassement|bêtement|
                            criminellement|idiotement|judicieusement|perversement|sottement|stupidement|bizarrement|
                            curieusement|étonnamment|étrangement|fâcheusement|inexplicablement|paradoxalement|regrettablement", "ADVMAN"))
  
  ## Retrait des lignes vides :
  corpus <- corpus[complete.cases(corpus),]
  
  ## Cas où l'on part des POS pour changer les lemmes :
  ## Ici, transformation des adverbes restants en ADV
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADV", "ADV"))
  ## Noms communs :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, lemmes == "bien-être|accablement|acuité|admiration|affection|joie|alacrité|allégresse|
                            amitié|amour|tristesse|angoisse|animosité|anxiété|apaisement|appréhension|attendrissement|
                            attirance|attraction|douleur|peur|aversion|plaisir|bonheur|sensation|calme|désenchantement|
                            plaisir|réconfort|ennui|orgueil|bonheur|écoeurement|soulagement|malaise|trouble|sentiment|
                            horreur|inquiétude|émotion|gêne|hostilité|paresse|fierté|honte|sympathie|satisfaction|
                            tendresse|retenue|hostilité|joie|jouissance|chagrin|consolation|consternation|contentement|
                            contrariété|crainte|déception|regret|curiosité|douleur|doute|dégoût|déchirement|délectation|
                            délivrance|dépit|déplaisir|désarroi|désespoir|désir|détachement|détresse|embarras|engourdissement|
                            ennui|exaltation|exaspération|excitation|fatigue|frisson|frémissement|félicité|goût|plaisir|trouble|
                            soulagement|saisissement|estime|réconfort|soulagement|honte|peine|frayeur|humiliation|déception|
                            répugnance|paresse|impatience|pitié|insouciance|peine|satisfaction|lassitude|stupeur|amertume|
                            solidarité|souffrance|indignation|gratitude|humiliation|inquiétude|irritation|ivresse|mélancolie|
                            mépris|nostalgie|désappointement|passion|perplexité|sensation|ravissement|reconnaissance|remord|
                            respect|répugnance|répulsion|pressentiment|sentiment|humiliation|tentation|chagrin|beauté|charme|
                            coquetterie|courage|courtoisie|douceur|ennui|gaîté|gentillesse|laideur|piété|imprudence|prudence|
                            sagesse|sensibilité|insincérité|sincérité|sérénité|timidité|tristesse|violence|abnégation|ambition|
                            amoralité|angélisme|ardeur|combativité|audace|autoritarisme|aptitude|entrain|ingéniosité|finesse|
                            doigté|tact|patience|courage|volonté|complaisance|compréhension|incompétence|compétence|connerie|
                            bêtise|curiosité|célérité|discernement|discrétion|défaitisme|dévouement|esprit|faiblesse|fantaisie|
                            fermeté|finesse|grossièreté|générosité|humanité|humilité|ignorance|imagination|impartialité|impatience|
                            imprudence|imprévoyance|incapacité|inconscience|indifférence|indiscipline|indulgence|indépendance|
                            initiative|insensibilité|intelligence|intrépidité|irrespect|jovialité|modestie|obstination|optimisme|
                            originalité|outrecuidance|partialité|impatience|patience|patriotisme|persuasion|pessimisme|objectivité|
                            nonchalance|modération|impudeur|pudeur|puérilité|rapidité|rigueur|incompétence|souplesse|stoïcisme|
                            prévoyance|cynisme|tolérance|arrogance|talent|zèle|charisme|sang-froid|aisance|bonté|bravoure|dilection|
                            discrétion|délicatesse|efficacité|endurance|exagération|élégance|intelligence|intrépidité|intuition|
                            lucidité|lâcheté|mansuétude|naïveté|paresse|impolitesse|politesse|résignation|résistance|fantaisie|
                            abnégation|ambition|amoralité|sentiment|combativité|audace|autoritarisme|autorité|dévouement|hospitalité|
                            impatience|imprudence|imprévoyance|inconscience|indulgence|insensibilité|intrépidité|intégrité|irrespect|
                            mansuétude|obstination|optimisme|outrecuidance|impartialité|partialité|puérilité|pessimisme|qualité|
                            incompétence|stoïcisme|dynamisme|négligence|avarice|intrépidité", "NCABS")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "visage|figure|cheveu|front|sourcil|yeux|cil|nez|oeil|bouche|lèvre|menton|joue|oreille|
                            gorge|poil|bras|main|doigt|jambe|cuisse|tête|cou|épaule|coude|avant-bras|poignet|hanche|genou|cheville|
                            front|orteil|thorax|abdomen|barbe|moustache|duvet|langue|dent|tempe|fesse|pied", "NCCOR"))
  
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
  
  
  
  ## Cas où l'on part des POS pour changer les lemmes :
  corpus <- corpus %>%
    mutate(lemmes = replace(lemmes, POS == "ADJ", "ADJ")) %>%
    mutate(lemmes = replace(lemmes, POS == "NUM", "ADJ")) %>%  
    mutate(lemmes = replace(lemmes, POS == "DETPOSS", "DETPOSS")) %>%
    mutate(lemmes = replace(lemmes, lemmes == "elle", "il")) %>%
    mutate(lemmes = replace(lemmes, POS == "NOUN", "NC")) %>%
    mutate(lemmes = replace(lemmes, POS == "PROPN", "NP")) %>%
    mutate(lemmes = replace(lemmes, POS == "INTJ", "INTJ")) %>%
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
    write_csv(corpus, "Corpus_motifs_UDPipe.csv")
  }
}

regex_corpus_entier(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", corpus = "UDPipe_corpus_complet.csv")

 