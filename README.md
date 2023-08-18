# MotiveR

 Développement sous R d’une méthode d’analyse textuelle fondée sur l’identification de motifs lexico-grammaticaux, méthode dite des « motifs » par Dominique Legallois (Université Sorbonne Nouvelle Paris III) et Antoine de Sacy. 
 
 Ce projet a reçu l'aide de l'Association Humanistica (http://www.humanisti.ca/) à travers la bourse octroyée en 2021-2022 et a été lauréat du DARIAH Theme Call 2023-2024.

# Installation

En attendant la pulication sur CRAN, vous pouvez installer le package 
directement depuis Github avec:

```r
devtools::install_github("Malichot/MotiveR")
```

avec les vignettes:

```r
devtools::install_github("Malichot/MotiveR", build_vignettes = TRUE, force = TRUE)
```

## Préparation des fichiers

Encodage et noms de fichiers.

* Les fichiers doivent tous être encodés en UTF-8.

* Les titres des fichiers ne doivent pas contenir de caractères spéciaux (accents, espaces, etc.). Ex : mon_texte.txt

* Les fichiers doivent être du texte brut, sous format .txt.

Si vous travaillez sur Windows, l'encodage de vos fichiers sera sûrement windows-1252. Pour détecter et transformer rapidement votre encodage, une petite fonction bash à exécuter dans un terminal peut vous être utile :

- Ouvrir un terminal.
- Aller dans le répertoire où se trouvent les .txt et lancez la commande : chardetect nom\_du\_fichier.txt
- Pour convertir les fichiers en utf-8, lancez la commande : for f in *.txt; do iconv -f windows-1252 -t utf-8 $f > $f-ut8.txt; done
- Les fichiers d'origine sont conservés, de nouveaux fichiers sont générés avec l'extension -ut8.txt.

## Documentation

Après installation du package, accédez à la documentation avec `?Motifs`

## Usage

Se référer aux vignettes dans le dossier "doc" où le package est installé.
