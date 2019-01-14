
# http://rstudio.github.io/sparklyr/articles/guides-textmining.html
library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local", version="2.0.0")
getwd()

DonQuichote_path <- paste0(getwd(),"/DonQuichote.txt")
DonQuichote <-  spark_read_text(sc, "DonQuichote", DonQuichote_path) 

all_words <- DonQuichote %>%
  mutate(line = regexp_replace(line, "[_\"\'():;,.!?\\-]", " ")) 

all_words <- all_words %>%
  ft_tokenizer(input.col = "line",
               output.col = "word_list")

head(all_words, 4)

all_words <- all_words %>%
  ft_stop_words_remover(input.col = "word_list",
                        output.col = "wo_stop_words")


head(all_words, 4)

all_words <- all_words %>%
  mutate(word = explode(word_list)) %>%
  select(word) %>%
  filter(nchar(word) > 2)

head(all_words, 4)



head(all_words, 4)

all_words <- all_words %>%
  compute("all_words")


word_count <- all_words %>%
  group_by(word) %>%
  tally() %>%
  arrange(desc(n)) 

word_count

word_count %>%
  head(100) %>%
  collect() %>%
  with(wordcloud::wordcloud(
    word, 
    n,
    colors = c("#999999", "#E69F00", "#56B4E9","#56B4E9")))
