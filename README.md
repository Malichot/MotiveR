# MotiveR

 Développement sous R d’une méthode d’analyse textuelle fondée sur l’identification de motifs lexico-grammaticaux, méthode dite des « motifs » par Dominique Legallois (Université Sorbonne Nouvelle Paris III) et Antoine de Sacy. 
 
 Ce projet a reçu l'aide de l'Association Humanistica (http://www.humanisti.ca/) à travers la bourse octroyée en 2021-2022 et a été lauréat du DARIAH Theme Call 2023-2024.

# Installation


``` r
library(devtools)
install_github("Malichot/Motifs")
```


## Préparation des fichiers.

Encodage et noms de fichiers.

* Les fichiers doivent tous être encodés en UTF-8.

* Les titres des fichiers ne doivent pas contenir de caractères spéciaux (accents, espaces, etc.). Ex : mon_texte.txt

* Les fichiers doivent être du texte brut, sous format .txt.

Si vous travaillez sur Windows, l'encodage de vos fichiers sera sûrement windows-1252. Pour détecter et transformer rapidement votre encodage, une petite fonction bash à exécuter dans un terminal peut vous être utile :

- Ouvrir un terminal.
- Aller dans le répertoire où se trouvent les .txt et lancez la commande : chardetect nom\_du\_fichier.txt
- Pour convertir les fichiers en utf-8, lancez la commande : for f in *.txt; do iconv -f windows-1252 -t utf-8 $f > $f-ut8.txt; done
- Les fichiers d'origine sont conservés, de nouveaux fichiers sont générés avec l'extension -ut8.txt.


## Usage:

``` r
library(Motifs)
# Params
path = "./Corpus-torun" # chemin du corpus
save_output = TRUE # Sauvegarde résultats
overwrite = TRUE # Écrase résultats précédents
n_grams = 4 # n-gram encodage
frequence = 3
len_context = 4
 
# UDpipe annotation
corpus_annote = annotation_udpipe(path = "./Corpus-torun",
                                  save_output = save_output,
                                  overwrite = overwrite)
# Étiquetage du corpus
corpus_motifs = regex_corpus_udpipe(corpus = corpus_annote,
                                    save_output = save_output,
                                    overwrite = overwrite)

# Ou pipeline entière
corpus_motifs = tag_motif_pipeline(path = "./Corpus-torun",
                                   save_output = save_output,
                                   overwrite = overwrite)

# Choix du nombre de ngrams :
corpus_grams =  choix_nb_ngrams(
  n_grams,
  corpus = corpus_motifs,
  save_output = save_output,
  overwrite = overwrite
)

# Wordcloud :
motifs_nuage(corpus_grams = corpus_grams,
             nmots = 10,
             freq = "rel")

# Histogrammes :
motifs_histogram(corpus_grams = corpus_grams,
                 nmots = 10,
                 freq = "rel")

# TF-IDF :
motifs_tf_idf(
  n_motifs = 10,
  plot_type = "sep",
  corpus_grams = corpus_grams,
  save_output = FALSE,
  overwrite = TRUE
)

# ACP :
motifs_acp(plot_type = "var", corpus_grams = corpus_grams)
motifs_acp(plot_type = "motif", corpus_grams = corpus_grams)
motifs_acp(plot_type = "var+motif", corpus_grams = corpus_grams)

# Stats
df_stats = motifs_stats(
  corpus_grams = corpus_grams,
  save_output = save_output,
  overwrite = overwrite
)

# Calcul de spécificités
calcul_spec_freq = calcul_specificites(
  save_freq = TRUE,
  retrait_frequence_1 = TRUE,
  corpus_grams = corpus_grams,
  save_output = save_output,
  overwrite = overwrite
)


# Retour aux textes
calcul_spec_freq = retour_texte_specificites(
  frequence = frequence,
  len_context = len_context,
  n_grams = n_grams,
  corpus_grams = corpus_grams,
  corpus_spec = calcul_spec_freq,
  save_output = save_output,
  overwrite = overwrite
)

# Retour aux textes à partir d'un motif
calcul_spec_freq = retour_texte_specificites_un_motif(
  motif_cible = "le NC de le",
  len_context = len_context,
  n_grams = n_grams,
  corpus_grams = corpus_grams,
  corpus_spec = calcul_spec_freq,
  save_output = save_output,
  overwrite = overwrite
)
```


### Étiquetage morphosyntaxique.

__Fonction__ : annotation_udpipe()

__Paramètres__ : 

    path  = chemin vers le répertoire contenant les .txt

    model = chemin vers le modèle.

__Sortie__ : UDPipe_corpus_complet.csv


### Transformation en motifs (UDPipe) : 

__Fonction__ : regex_corpus_entier_UDPipe()

__Paramètres__ : 

    path  = chemin vers le csv produit par le script précédent.

    corpus = UDPipe_corpus_complet.csv

__Sortie__ : Corpus_motifs_UDPipe.csv

### Choix nombre n-grams :

__Fonction__ : choix_nb_ngrams()

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs
        
    csv = Corpus_motifs_UDPipe.csv

__Sortie__ : Corpus_motifs_grams.csv

### Histogramme : 

Fonction permettant de générer des histogrammes sur les fréquences relatives et absolues des n premiers motifs du corpus.

__Fonction__ : motifs_histograms()

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation en motifs.

    csv = Corpus_motifs_grams.csv

    nmots = nombre de mots à afficher dans l'histogramme. 

__Sortie__ : Visualisations fréquences relatives ou absolues.

### TF-IDF : 

Fonction permettant de pratiquer une analyse TF-IDF sur n motifs du corpus.

__Fonction__ : tf_idf_motifs()

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de transformation ngrams.

    csv = Corpus_motifs_grams.csv

    nmots = nombre de mots à afficher dans le TF-IDF. 

__Sortie__ : Visualisations groupées ou séparées.

### ACP : 

Fonction permettant de générer une Analyse en composante principale.

__Fonction__ : motifs_acp()

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de choix de ngrams.

	csv = "corpus_motifs_grams.csv"
	
    freq_filter = seuil de fréquences des observations prises en compte 

    n_obs = nombre d'observations à garder dans l'ACP, si "all", toutes prises en compte.

__Sortie__ : Visualisations multiples, variables, observations, les deux + % des composantes.

### Calcul de spécificités :

Fonction permettant de pratiquer un calcul de spécificité sur les motifs du corpus. La spécificité “Inf” correspond à une spécificité positive maximale.

__Fonction__ : calcul_de_specificites()

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de choix de ngrams.

	csv = "corpus_motifs_grams.csv"
	
__Sortie__ : Corpus_motifs_specificites.csv avec : Oeuvre || motifs || n (fréq abs) || total (nb de mots dans l'oeuvre) || nrel (fréq relative) || Spécificités par oeuvres. Pour le retour aux textes, tapez 2.

### Densité de cinq motifs dans une oeuvre :

Fonction permettant d’analyser la densité d’apparition de cinq motifs différents dans une oeuvre. Attention à bien noter que l’échelle de chaque motif est propre au motif : il faut donc prendre des motifs ayant une fréquence d’apparition proche, sans cela les échelles sont faussées.

__Fonction__ : motifs_densite()

__Paramètres__ :

    path  = chemin vers le csv produit par le script de choix de ngrams.

    csv = corpus_motifs_grams.csv.

    filtre = Nom de l’oeuvre tel qu’elle apparaît dans la colonne du fichier Corpus_motifs_UDPipe.csv. Ex : filtre = "Flaubert-Bovary.txt"

    motif1, motif2, 3, 4, 5 : ex : “le NC de le NC” : motifs à analyser. Nécessairement 5.

    bandwidth : degré de correspondance entre la densité et la fréquence. Par défaut, 4000. 

    titre_graphique = titre du graphique affiché.

__Sortie__ : Visualisation.

### Statistiques générales :

Script permettant de générer un csv avec tous les calculs précédents dans un tableau.

__Fonction__ : stats_motifs()

__Paramètres__ : 

    path  = chemin vers le csv produit par le script de choix du ngrams.

    csv = corpus_motifs_grams.csv

__Sortie__ : Motifs_statistisques.csv avec : Oeuvre || motifs || n (fréq absolue) || nb_total_mots (dans l'oeuvre) || n_rel (fréquence relative) || spécificités oeuvre par oeuvre || n_total || barycentre || pourcentage.

__N.B.__ : Possibilité affichage dans le terminal ou dans une variable R result_df_stats.


### Retour aux textes : 

Fonction permettant de revenir aux textes à partir des motifs que l’on souhaite examiner. La fonction peut prendre beaucoup de temps (plusieurs minutes, ou une dizaine de minutes) à être exécutée, le nombre de motifs étant très élevé suivant la taille de votre corpus. C’est pourquoi il est possible d’ajouter un filtre de fréquence, pour ne prendre en compte que les motifs supérieur à n occurrences (en fréquence absolue).

__Fonction__ : retour_texte_specificites()

__Paramètres__ : 

    csv_corpus_motifs  = chemin vers le csv produit par le script de choix du nombre de ngrams.

    csv_corpus_specificites = Corpus_motifs_specificites.csv

    frequence = 25 (choix du filtre n > 25 occurrences).

__Choix__ :

    Contexte à afficher en nombre de mots à gauche et droite du motif.

    Longueur du motifs, en nombre de mots.


__Sortie__ : Retour_aux_textes_corpus_specificites.csv avec : id || contexte_gauche || motif (texte) || contexte_droit || Oeuvre || motifs || n || total || nrel || spécificités pour chaque oeuvre.


### Retour aux textes sur un motif particulier : 

Même fonctionnalité que la précédente, mais en ne revenant au texte que pour un motif spécifique que l’on entre.

__Fonction__ : retour_texte_specificites_un_motif()

__Paramètres__ : 

    csv_corpus_motifs = chemin vers le csv produit par le script de choix du ngrams.

    csv_corpus_specificites = Corpus_motifs_specificites.csv

    motif cible = ex : “PRES et on ADV PRES”.

__Choix__ :

    Contexte à afficher en nombre de mots à gauche et droite du motif.

    Longueur du motifs, en nombre de mots (2-7).


__Sortie__ : motif_in_context.csv avec : id || contexte_gauche || motif (texte) || contexte_droit || Oeuvre || motifs || n || total || nrel || spécificités pour chaque oeuvre. Possibilité affichage dans le terminal ou dans une variable R result_df.
