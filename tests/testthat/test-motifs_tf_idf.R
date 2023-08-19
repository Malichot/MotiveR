test_that("motif_tf_idf works", {
  mots = c("Honoré", "Honoré", "Honoré", "Honoré",
           "le",  "le",  "le", 
           "par","par",
           "Madame", "Madame", "Madame", 
           "le", 
           "FLAUBERT", "FLAUBERT",
           "À", "À", "À", 
           "par", "par", "par", "par", "par"
  )
  
  ngrammot = c("Honoré de Balzac Le", "Honoré de Balzac Le", "Honoré de Balzac Le", "Honoré de Balzac Le",
               "le grand et illustre", "le grand et illustre", "le grand et illustre",
               "par les quelques portraits", "par les quelques portraits",
               "Madame Bovary GUSTAVE FLAUBERT", "Madame Bovary GUSTAVE FLAUBERT", "Madame Bovary GUSTAVE FLAUBERT",
               "le grand et illustre",
               "FLAUBERT Paris , 12", "FLAUBERT Paris , 12",
               "À Rebours Huysmans ,", "À Rebours Huysmans ,", "À Rebours Huysmans ,",
               "par les quelques portraits", "par les quelques portraits", "par les quelques portraits", "par les quelques portraits",
               "par les quelques portraits"
  )
  
  motifs = c("PPAS de NP le", "PPAS de NP le", "PPAS de NP le", "PPAS de NP le",
             "le ADJ et ADJ", "le ADJ et ADJ", "le ADJ et ADJ",
             "par le quelque NC", "par le quelque NC", 
             "madame NP NP NP", "madame NP NP NP", "madame NP NP NP", 
             "le ADJ et ADJ",
             "NP NP , NUM", "NP NP , NUM", 
             "à NP NP ,",  "à NP NP ,",  "à NP NP ,", 
             "par le quelque NC", "par le quelque NC", "par le quelque NC", "par le quelque NC","par le quelque NC"
  )
  
  Oeuvre = c("Balzac-Goriot","Balzac-Goriot","Balzac-Goriot","Balzac-Goriot",
             "Balzac-Goriot",  "Balzac-Goriot","Balzac-Goriot",
             "Balzac-Goriot","Balzac-Goriot",
             "Flaubert-Bovary", "Flaubert-Bovary","Flaubert-Bovary",
             "Flaubert-Bovary","Flaubert-Bovary", "Flaubert-Bovary",
             "Huysmans-Rebours","Huysmans-Rebours","Huysmans-Rebours",
             "Huysmans-Rebours", "Huysmans-Rebours","Huysmans-Rebours","Huysmans-Rebours","Huysmans-Rebours")
  
  corpus_grams = data.table::data.table(mots = mots, ngrammot = ngrammot, motifs = motifs, Oeuvre = Oeuvre)
  
  # TF-IDF :
  corpus_words_ngrams = motifs_tf_idf(corpus_grams = corpus_grams)
  expect_true(all(corpus_words_ngrams$tf_idf >= 0) & all(corpus_words_ngrams$tf_idf <= 1))
  
  tf_idf = c(0.25341569, 0.48827213, 0.36620410, 0.65916737, 0.41197961, 0.09010336, 0.43944492)
  expect_true(all(dplyr::near(corpus_words_ngrams$tf_idf, tf_idf, tol = .Machine$double.eps^0.5)))
}
)
