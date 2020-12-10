# Motifs

 Développement sous R d’une méthode d’analyse textuelle fondée sur l’identification de motifs lexico-grammaticaux, méthode dite des « motifs ».


## Préparation des fichiers.

Encodage et noms de fichiers.

* Les fichiers doivent tous être encodés en UTF-8.

* Les titres des fichiers ne doivent pas contenir de caractères spéciaux (accents, espaces, etc.). Ex : mon_texte.txt

* Les fichiers doivent être du texte brut, sous format .txt.


## Processus général.


### 1. Étiquetage morphosyntaxique.

__Script__ : Etiquetage_UDPipe_Fonction.R

__Paramètres__ : 

    path  = chemin vers les .txt

    model = chemin vers le modèle.

__Sortie__ : UDPipe_corpus_complet.csv


### 2. Transformation en motifs (UDPipe) : 

__Script__ : Regex_UDPipe.R

__Paramètres__ : 

    path  = chemin vers le csv produit par le script précédent.

    corpus = UDPipe_corpus_complet.csv

__Sortie__ : Corpus_motifs_UDPipe.csv


### 3. Transformation en motifs (Cordial) : 

__Script__ : Regex_Cordial.R

__Paramètres__ : 

    path  = chemin vers les .cnr produits par l’étiquetage Cordial.

__Sortie__ : Corpus_motifs_Cordial.csv

### 4. Wordcloud : 

Fonction permettant de générer des nuages de mots sur les fréquences relatives et absolues des n premiers motifs du corpus.

__Script__ : Wordcloud_Fonction.R

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_UDPipe.csv

    nmots = nombre de mots à afficher dans les nuages. 

__Sortie__ : Visualisations fréquences relatives ou absolues.


### 5. TF-IDF : 

Fonction permettant de pratiquer une analyse TF-IDF sur n motifs du corpus.

__Script__ : TF_IDF_Function.R

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_UDPipe.csv

    nmots = nombre de mots à afficher dans le TF-IDF. 

__Sortie__ : Visualisations groupées ou séparées.

### 6. AFC : 

Fonction permettant de générer une Analyse factorielle des correspondances sur les n premiers motifs du corpus.

__Script__ : AFC_Fonction.R

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_UDPipe.csv

    nombre_oeuvres = nombre d’oeuvres dans le corpus analysé.

    nmotifs : nombre de motifs à prendre en compte dans l’AFC et dans la visualisation. 

    nombre_dimensions : nombre de dimension à conserver dans le calcul de l’AFC.

    Optionnel : une_oeuvre = affichage d’une seule oeuvre dans l’AFC.  

__Sortie__ : Visualisations simple, en gradient de couleurs, ou avec l’affichage d’une seule oeuvre.

### 7. Calcul de spécificités :

Fonction permettant de pratiquer un calcul de spécificité sur les motifs du corpus. La spécificité “Inf” correspond à une spécificité positive maximale.

__Script__ : Calcul_Specificite_Fonction.R

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_UDPipe.csv

__Sortie__ : Corpus_motifs_specificites.csv avec : Oeuvre || motifs || n (fréq abs) || total (nb de mots dans l'oeuvre) || nrel (fréq relative) || Spécificités par oeuvres.

### 8. Barycentres et pourcentage d’apparition : 

Fonction permettant de pratiquer un calcul du barycentre des motifs (répartition dans le corpus) ainsi que le pourcentage de présence du motif dans l’oeuvre par rapport au corpus. 

__Script__ : Barycentre_Function.R

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_UDPipe.csv

__Sortie__ : Barycentre_motifs.csv avec : Oeuvre || motifs || index || n (fréq abs) || n_total (nb de mots dans l'oeuvre) || barycentre || pourcentage (présence du motif par rapport au reste du corpus)

### 9. Densité de cinq motifs dans une oeuvre :

Fonction permettant d’analyser la densité d’apparition de cinq motifs différents dans une oeuvre. Attention à bien noter que l’échelle de chaque motif est propre au motif : il faut donc prendre des motifs ayant une fréquence d’apparition proche, sans cela les échelles sont faussées.

__Script__ : Densite_Function.R

__Paramètres__ :

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_UDPipe.csv : corpus de motifs.

    filtre = Nom de l’oeuvre tel qu’elle apparaît dans la colonne du fichier Corpus_motifs_UDPipe.csv.

    motif1, motif2, 3, 4, 5 : ex : “le NC de le NC” : motifs à analyser. Nécessairement 5.

    bandwidth : degré de correspondance entre la densité et la fréquence. Par défaut, 4000. 

    titre_graphique = titre du graphique affiché.

__Sortie__ : Visualisation.

### 10. Statistiques générales :

Script permettant de générer un csv avec tous les calculs précédents dans un tableau.

__Script__ : Stats_motifs.R

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_UDPipe.csv

__Sortie__ : Motifs_statistisques.csv avec : Oeuvre || motifs || n (fréq absolue) || nb_total_mots (dans l'oeuvre) || n_rel (fréquence relative) || spécificités oeuvre par oeuvre || n_total || barycentre || pourcentage.

__N.B.__ : Possibilité affichage dans le terminal ou dans une variable R result_df_stats.


### 11. Retour aux textes : 

Fonction permettant de revenir aux textes à partir des motifs que l’on souhaite examiner. La fonction peut prendre beaucoup de temps (plusieurs minutes, ou une dizaine de minutes) à être exécutée, le nombre de motifs étant très élevé suivant la taille de votre corpus. C’est pourquoi il est possible d’ajouter un filtre de fréquence, pour ne prendre en compte que les motifs supérieur à n occurrences (en fréquence absolue).

__Script__ : Retour_aux_textes_Specificites_Fonction.R

__Paramètres__ : 

    csv_corpus_motifs  = chemin vers le csv produit par le script de transformation en motifs.

    csv_corpus_specificites = Corpus_motifs_specificites.csv

    frequence = 25 (choix du filtre n > 25 occurrences).

__Choix__ :

    Contexte à afficher en nombre de mots à gauche et droite du motif.

    Longueur du motifs, en nombre de mots (5).


__Sortie__ : Retour_aux_textes_corpus_specificites.csv avec : id || contexte_gauche || motif (texte) || contexte_droit || Oeuvre || motifs || n || total || nrel || spécificités pour chaque oeuvre.


### 12. Retour aux textes sur un motif particulier : 

Même fonctionnalité que la précédente, mais en ne revenant au texte que pour un motif spécifique que l’on entre.

__Script__ : Retour_aux_textes_un_motif.R

__Paramètres__ : 

    csv_corpus_motifs = chemin vers le csv produit par le script de transformation en motifs.

    csv_corpus_specificites = Corpus_motifs_specificites.csv

    motif cible = “PRES et on ADV PRES”.

__Choix__ :

    Contexte à afficher en nombre de mots à gauche et droite du motif.

    Longueur du motifs, en nombre de mots (5).


__Sortie__ : motif_in_context.csv avec : id || contexte_gauche || motif (texte) || contexte_droit || Oeuvre || motifs || n || total || nrel || spécificités pour chaque oeuvre. Possibilité affichage dans le terminal ou dans une variable R result_df.