# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------

# para gerar as nuvens de palavra
# install.packages(c("wordcloud", "RColorBrewer", "wordcloud2", "tm"))

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

# para gerar os graficos
# install.packages(c("reshape2", "ggplot2"))

library(reshape2)
library(ggplot2)

# ---------------------
# Loading data
# ---------------------

data = read.csv(file = "./data/data2022_2.csv", sep = ";")
colnames(data)[8] = "Formato"

# ---------------------
# 1. quantidade de atendimentos (por disciplina)
# ---------------------

g1 = ggplot(data = data, mapping = aes(x = Disciplina, group = Disciplina, fill = Disciplina,
  colour = Disciplina))
g1 = g1 + geom_bar() + theme_bw()
g1 = g1 + theme(axis.text.x=element_blank()) + labs(x = NULL, y = "Quantidade")
ggsave(g1, file = "fig1_atendimentos_disciplina.pdf", width = 4.97, height = 2.63)

# ---------------------
# ---------------------

# 4. quais as formas de atendimento mais procuradas (pessoal, remoto?)
g2 = ggplot(data = data, mapping = aes(x = Formato, group = Formato, fill = Formato,
  colour = Formato))
g2 = g2 + geom_bar() + theme_bw()
g2 = g2 + theme(axis.text.x=element_blank()) + labs(x = NULL, y = "Quantidade")
ggsave(g2, file = "fig2_formato_atendimento.pdf", width = 4.51, height = 2.9)

# ---------------------
#  disciplina x formato
# ---------------------

g3 = ggplot(data = data, mapping = aes(x = Formato, group = Formato, fill = Formato,
  colour = Formato))
g3 = g3 + geom_bar() + theme_bw() + facet_wrap(~Disciplina, scales = "free")
g3 = g3 + theme(axis.text.x=element_blank()) + labs(x = NULL, y = "Quantidade")
ggsave(g3, file = "fig3_atendimentos_disciplina_formato.pdf", width = 6.25, height = 3.83)

# ---------------------
# ---------------------

# 5. Quais os pontos/tópicos mais procurados pelos alunos? (word cloud)
#     geral
#     por disciplina (?)

text = data$Assunto

docs = Corpus(VectorSource(text))
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("portuguese"))

dtm    = TermDocumentMatrix(docs)
matrix = as.matrix(dtm)
words  = sort(rowSums(matrix),decreasing=TRUE)
df     = data.frame(word = names(words),freq=words)

set.seed(42) # for reproducibility
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
  max.words=200, random.order=FALSE, rot.per=0.35,
  colors=brewer.pal(8, "Dark2"))

# ---------------------
# ---------------------

# 6 . Timeline de atedimentos (github), heatmap

time.data = as.data.frame(data)[,-1]
colnames(time.data)[1] = "Data"
pieces = strsplit(x = as.character(time.data$Data), split = "/")
dia   = unlist(lapply(pieces, function(temp){ return(temp[1])}))
mes   = unlist(lapply(pieces, function(temp){ return(temp[2])}))

time.df = cbind(time.data, dia, mes)
time.df$Freq = 0

freq.table = table(time.df$Data)
aux = lapply(1:length(freq.table), function(i) {
  date = names(freq.table)[i]
  values = unlist(strsplit(date, split = "/"))
  ret = c(values[1:2], freq.table[i])
  return(ret)
})

aux.df = as.data.frame(do.call("rbind", aux))
colnames(aux.df) = c("dia", "mes", "Freq")
aux.df$Freq = as.numeric(aux.df$Freq)


g5 = ggplot(data = aux.df, mapping = aes(x = dia, y = mes, fill = Freq))
g5 = g5 + geom_tile(colour = "black")
g5 = g5 + theme_bw() + labs(x = "Dia", y = "Mês")
g5 = g5 + scale_fill_gradient2(low = "white", high = "red")
ggsave(g5, file = "fig5_timeline_atendimentos.pdf", width = 6.54, height = 2.3)

# ---------------------
# ---------------------

# 2 Alunos diferentes que procuraram atendimento
# 3. quantidade de atendimentos em relação aos semestres anteriores?

# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------