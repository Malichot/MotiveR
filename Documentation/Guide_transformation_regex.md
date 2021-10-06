### Guide de transformation regex : 

#### On garde les auxiliaires :
 
- changement des POS à partir des lemmes.
- changement des feats (morphologie) pour qu’ils ne soient pas transformés ensuite.

#### On garde certains verbes courants :

- changement des POS à partir des lemmes.
- changement des feats pour qu’ils ne soient pas transformés ensuite.

#### Transformation VDIC, VCOG : 

**N.B.** : Présent dans une version spécifique des scripts car manque d'homogénéité statistique ensuite, mais une piste à creuser.

- Changement des POS à partir des lemmes. 
- Changement des feats pour qu’ils ne soient pas transformés ensuite.

#### Transformation des verbes : 

- Remplacement des lemmes par les feats : INF, PPAS, PPRES, VSUBP, SSUBI, IMP, VCOND, PRES, VIMP, VPS, VF.

#### Transformation des déterminants possessifs :

- DETPOSS

#### Mots invariables : 

- Certains ne font pas vraiment sens : « jour », « nuit », des verbes (prendre, falloir, finir, …), des adjectifs (vrai, véritable…), des mots composés qui ne seront de toute façon pas pris en compte lors de la tokénisation (« bien plus », « bien moins »). À revoir ? À systématiser ?

#### Adverbes :

- On garde certains adverbes et on les qualifie : ADVTOT (?), ADVPHA (?), ADVFRE (fréquence), ADVINT (intensité), ADVHAB (habitude), ADVMOD (modaux), ADVMAN (manière). Remplacement conditionnel : si le lemme = « mot », remplacement du POS par le mot pour qu’il apparaisse dans les motifs.

Remplacement des autres ADV en ADV dans les motifs.

#### Noms communs : 

- NCABS (abstrait), NCCOR (partie du corps). 

#### Remplacement des ADJ, NUM, DETPOSS, NOUN = NC, PROPN = NP.

#### Remplacement des guillemets français en anglais :

Ça dépend trop de l’édition pour que ce soit pertinent de les garder. Notamment avec les textes récupérés sur internet, en epub, etc.

#### Les pronoms personnels et réfléchis : on garde les formes (avec et sans majuscule) :

- je / j' / me / Je / J' / Me
- tu / Tu / Te / te 
- il / elle / se / Il / Elle / Se
- nous / nous / Nous
- vous / vous / Vous
- ils / elles / se / Ils / Elles / Se

#### Dernières vérifications : 

- retrait « aux », « du », « des », « au », « NA ».
- retrait des éventuelles lignes vides.
- suppression de la colonne POS et extraction csv : mots / motifs / Oeuvre.