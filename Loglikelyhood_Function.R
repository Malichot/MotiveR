## Stage - Dominique Legallois ##

## Loglikelyhood : ##

# Réf : https://github.com/zzzev/tidytlogl/blob/master/man/tidy_logl.Rd

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Paramètres :

# path : chemin du fichier motifs csv (sortie du script de regex)
# csv : nom du fichier motifs.
# tresh : treshold : minimum log-likelihood value (number) to include in results

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/" 
csv = "Corpus_motifs.csv" 


setwd(path)
corpus_spec <- fread(csv)

corpus_spec <- as_tibble(corpus_spec) %>%
  group_by(Oeuvre)

corpus <- corpus_spec %>%
  filter(Oeuvre == "Rigodon.cnr")
  
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

names(corpus_punct) <- c("motifs_1", "Oeuvre")

corpus_punct_2 <- corpus  %>%
  mutate(next_word = lead(motifs, 5),
         next_word2 = lead(motifs, 6),
         next_word3 = lead(motifs, 7),
         next_word4 = lead(motifs, 8), 
         next_word5 = lead(motifs,  9)) %>%
  #filter(!is.na(next_word), !is.na(next_word2), !is.na(next_word3), !is.na(next_word4), !is.na(next_word5)) %>%
  mutate(ngrammotif = paste(next_word, next_word2, next_word3, next_word4, next_word5))

corpus_punct_2 <- corpus_punct_2[,c("ngrammotif", "Oeuvre")]

names(corpus_punct_2) <- c("motifs_2", "Oeuvre")

corpus_bigram <- cbind(corpus_punct, corpus_punct_2) 

corpus_bigram <- corpus_bigram[,c("motifs_1", "motifs_2", "Oeuvre")]


# compute counts for word 1 & 2 independently
count_w1 <- corpus_bigram %>%
  count(motifs_1)

count_w2 <- corpus_bigram %>%
  count(motifs_2)

# compute counts for bi-grams
count_w12 <- corpus_bigram %>%
  count(motifs_1, motifs_2)

# get the original number of all bi-grams
N <- nrow(corpus_bigram)

# join this information and compute log-likelihood
LL_test <- count_w12 %>%
  left_join(count_w1, by = "motifs_1") %>%
  left_join(count_w2, by = "motifs_2") %>%
  rename(c_w1 = n.y, c_w2 = n, c_w12 = n.x) %>%
  mutate(
    p = c_w2 / N,
    p1 = c_w12 / c_w1,
    p2 = (c_w2 - c_w12) / (N - c_w1),
    LL = log((pbinom(c_w12, c_w1, p) * pbinom(c_w2 - c_w12, N - c_w1, p)) / (pbinom(c_w12, c_w1, p1) * pbinom(c_w2 - c_w12, N - c_w1, p)))
  )
head(LL_test)







Loglikelyhood <- function(path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/", 
                          csv = "Corpus_motifs.csv", 
                          tresh = 3){
  
  ## Librairies :
  require("tidyverse")
  require("tidyr")
  require("data.table")
  require("dplyr")
  
  ## Répertoire de travail :
  setwd(path)
  corpus_spec <- fread(csv)
  
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
  
#  df = corpus_spec_punct
  
  #group_key = df$Oeuvre
  #token_key = df$motifs
  
  ## Calcul du loglikelyhood :

  tidy_logl <- function(df, group_key, token_key, threshold = 1) {
    total_words_spoken <- nrow(df)
    q_group_key <- dplyr::enquo(group_key)
    per_group_totals <- df %>%
      dplyr::count(!!q_group_key) %>%
      dplyr::rename(per_group = n)
    
    q_token_key <- dplyr::enquo(token_key)
    per_token_totals <- df %>%
      dplyr::count(!!q_token_key) %>%
      dplyr::rename(per_token = n)
    
    df %>%
      dplyr::count(!!q_group_key, !!q_token_key) %>%
      dplyr::left_join(per_group_totals) %>%
      dplyr::left_join(per_token_totals) %>%
      dplyr::mutate(
        in_group_non_tokens = per_group - n,
        out_group_tokens = per_token - n,
        out_group_non_tokens = total_words_spoken - per_group - per_token
      ) %>%
      dplyr::rename( # see above linked PDF (p.2) for variable naming reference
        a = n,
        b = out_group_tokens,
        c = in_group_non_tokens,
        d = out_group_non_tokens
      ) %>%
      dplyr::select(-c(per_group, per_token)) %>%
      dplyr::mutate(
        e1 = (a + c) * ((a + b) / total_words_spoken),
        e2 = (b + d) * ((a + b) / total_words_spoken),
        ll = 2 * ((a * log(a / e1)) + (b * log(b / e2)))
      ) %>%
      dplyr::filter(!is.na(ll), ll > threshold) %>%
      dplyr::mutate(ll = dplyr::if_else(a < e1, ll * -1, ll)) %>%
      dplyr::select(-c(a, b, c, d)) %>%
      dplyr::arrange(desc(ll))
  }

  
  tidy_logl(df = corpus_spec, group_key = Oeuvre, token_key = motifs, threshold = 3)
  

}

test <- Loglikelyhood()


tmLogL <- function(df,group,group.of.interest,text,sparsity=0.80,threshold=3){ 
  # Load packages.
  require(tm)
  
  # First, collapse into large set of text.
  collapsed.text <- NULL
  group.names <- levels(as.factor(df[,group]))
  for(i in group.names){
    sub <- df[df[,group] %in% i, ]
    txt <- paste(sub[,text], collapse=" ")
    sub.txt <- data.frame(Group=i, Text=txt)
    collapsed.text <- rbind(collapsed.text, sub.txt)
  }
  
  # Second, create a corpus, then save as data frame.
  myReader <- readTabular(mapping=list(content="Text", id="Group"))
  corpus <- Corpus(DataframeSource(collapsed.text), readerControl=list(reader=myReader))
  corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
  tdm <- TermDocumentMatrix(corpus, control=list(tolower=F, removePunctuation=F, stopwords=F, removeNumbers=F))
  tdm <- removeSparseTerms(tdm, sparsity)
  tdm.df <- data.frame(as.matrix(tdm[1:dim(tdm)[1],]))
  tdm.df$word <- row.names(tdm.df)
  ln <- length(tdm.df)
  tdm.df <- tdm.df[,c(ln, 1:(ln-1))]
  
  # Third, calculate the LL.
  column.number <- which(colnames(tdm.df) %in% group.of.interest)
  tdm.df.2 <- tdm.df[tdm.df[,column.number] > 0, ]
  returned <- NULL
  for(w in seq_along(tdm.df.2[,1])){
    word <- tdm.df.2[w,1]
    a <- tdm.df.2[w, column.number] # word, corpus one
    b <- sum(tdm.df.2[w, -c(1, column.number)]) # word, corpus two
    c <- sum(tdm.df.2[-w, column.number]) # not word, corpus one
    d <- sum(tdm.df.2[-w, -c(1, column.number)]) # not word, corpus two
    N <- a + b + c + d
    e1 <- (a + c) * ((a + b)/N)
    e2 <- (b + d) * ((a + b)/N)
    chisq <- 2 * ((a * log(a / e1)) + (b * log(b / e2)))
    if(!is.nan(chisq) & chisq > threshold){
      if(a < e1) chisq <- chisq * - 1
      row <- data.frame(o1=a, o2=b, e1, e2, chisq, word=tdm.df.2[w,1], name=group.of.interest)
      returned <- rbind(returned, row)
    }
  }
  return(returned)
}

tmLogL(corpus_spec, group = corpus_spec$Oeuvre, group.of.interest = "Rigodon.cnr", text = mots)


  