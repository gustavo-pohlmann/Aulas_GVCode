library(tidytext)
library(tidyr)
library(magrittr)
library(dplyr)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(wordcloud)

austen_bybook <- austen_books() %>% group_by(book) # agrupa os livros por título
head(austen_bybook)
austen_lines <- mutate(austen_bybook, linenumber = row_number()) # salva as linhas de origem
austen_chapters <- mutate(austen_lines, chapter = cumsum(str_detect(text, # separa por cada capítulo
                                                                    regex("^chapter [\\divxlc]", # com base no título
                                                                          ignore_case = TRUE))))

austen_ungrouped <- ungroup(austen_chapters) # desagrupa as estruturas para preparar os dados
tidy_books <- unnest_tokens(austen_ungrouped,word,text) # e coloca cada palavra como uma observação
head(tidy_books)

data(stop_words) # palavras como "and", "or", "the" são muito usadas mas não carregam em si nenhum significado próprio
tidy_books <- anti_join(tidy_books,stop_words) # portanto, vamos removê-las para manter a análise de frequência limpa

tidy_sort <- count(tidy_books, word, sort=TRUE) # coloca em ordem decrescente de frequência

filter(tidy_sort,n>540) -> tidy_filter # filtra apenas as palavras usadas mais de 540 vezes
mutate(tidy_filter,word=reorder(word,n)) -> tidy_mutate # reorganiza tudo

ggplot(tidy_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot # e faz o gráfico
print(tidy_ggplot)

wordcloud(tidy_sort$word,tidy_sort$n, # e um wordcloud com 50 palavras
          max.words = 50,
          rot.per = FALSE,colors= c("#973232", "#1E5B58", "#6A8D2F", "#287928"))

library(gutenbergr) # API do Projeto Gutenberg (https://www.gutenberg.org/)

eapoe <- gutenberg_works(author=="Poe, Edgar Allan")%>%gutenberg_download(meta_fields = "title")
tidy_poe <- eapoe %>%unnest_tokens(word,text)%>%anti_join(stop_words) # pega os livros do Edgar Allan Poe
poe_sort <- count(tidy_poe,word,sort=TRUE) # e prepara igual os da Jane Austen
poe_filter <- filter(poe_sort,n>320) # Feliz Halloween
mutate(poe_filter,word=reorder(word,n)) -> poe_mutate

ggplot(poe_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

wordcloud(poe_sort$word,poe_sort$n,
          max.words = 50,
          rot.per = FALSE,colors= c("#AB329C", "#195B5B", "#6D8D2F", "#287928"))

bronte <- gutenberg_download(c(1260,768,969,9182,767)) # e algumas obras das irmas Brontë
tidy_bronte <- bronte %>%unnest_tokens(word,text)%>%
  anti_join(stop_words)

# aqui junta os tibbles e remove metadados do projeto Gutenberg de acordo com o padrão deles
bind_rows(mutate(tidy_bronte, author="Brontë Sisters"), mutate(tidy_poe,author='Edgar Allan Poe'),mutate(tidy_books,author='Jane Austen')) -> tidy_bind
tidy_bind <- mutate(tidy_bind, word=str_extract(word,"[a-z']+"))

tidy_count <- count(tidy_bind,author,word) # conta as palavras usadas por cada autor
group_by(tidy_count,author)->tidy_group # reagrupa tudo

mutate(tidy_group,proportion = n/sum(n))->tidy_mutate # cria as proporções de uso (palavra/total de palavras do autor)
select(tidy_mutate,-n)->tidy_selected # e remove a frequência absoluta

tidy_spread <- spread(tidy_selected,author,proportion) # essas linhas preparam o tibble para irem sem problemas para o gráfico
tidy_gather<-gather(tidy_spread,author,proportion,'Brontë Sisters':'Edgar Allan Poe')

library(scales)

ggplot(tidy_gather, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "lightslategrey", high = "slateblue3") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# Vamos ver as correlações!
cor.test(data=tidy_gather[tidy_gather$author=="Brontë Sisters",], ~ proportion + `Jane Austen`)
cor.test(data=tidy_gather[tidy_gather$author=="Edgar Allan Poe",],~proportion+`Jane Austen`)

# Aula adaptada de: https://www.tidytextmining.com/tidytext.html