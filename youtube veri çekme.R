#pakerler####
install.packages("tuber")#youtubeden veri cekimi icin
install.packages("tm")  # metinleri duzenlemek icin
install.packages("SnowballC") # metin koku icin
install.packages("wordcloud") # kelime bulutu uretici
install.packages("RColorBrewer") #renk paletleri
install.packages("syuzhet") # duyarlilik analizi
install.packages("ggplot2") # grafik cizmez icin

library("tuber")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

#vericekimi####
#tuber paketi ile
myclientid="***********************************************"
myclientsecret="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"#https://www.youtube.com/watch?v=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
yt_oauth(myclientid,myclientsecret,token="")
yorumlar=get_all_comments("2-jvlhc7c58")

#tekrar eden yorumlari temizleme
yorumlar<-yorumlar[!( duplicated(yorumlar$textDisplay)),]
#cekilen verilerin kayit edilmesi
dosya_yolu <- "yorumlar.csv"
write.table(yorumlar$textDisplay, file = dosya_yolu, sep = "|", quote = FALSE, row.names = FALSE)
veri<-read.table(file.choose(),header = F,sep="|")
#veriyi isleme####

kveri <- Corpus(VectorSource(veri))
#(tm paketini kullanarak)veri den gereksiz karekterlerin cikarilmasi islemi
toSpace <- content_transformer(function (x , pattern ) 
                              gsub(pattern, " ", x))
kveri <- tm_map(kveri, toSpace, "/")
kveri <- tm_map(kveri, toSpace, "@")
kveri <- tm_map(kveri, toSpace, "\\|")
#emoji temizleme 
kveri <- tm_map(yorum, content_transformer(function(x)
                            gsub("\\&\\#\\d+;", "", x)))

#metini kucuk harflere donusturme 
kveri <- tm_map(kveri, content_transformer(tolower))
#rakamlari cikartma
kveri <- tm_map(kveri, removeNumbers)
#ingilizce anlamsiz eklimerlin cikartilmasi 
kveri <- tm_map(kveri, removeWords, stopwords("english"))
#noktalama isaretlerinin kaldirilmasi
kveri <- tm_map(kveri, removePunctuation)
#fazla bosluklarin kaldirilmasi
kveri <- tm_map(kveri, stripWhitespace)
#metinlerin koklerine inme 
kveri <- tm_map(kveri, stemDocument)

#terim belge matrisi olusturma
TextDoc_dtm <- TermDocumentMatrix(kveri)
dtm_m <- as.matrix(TextDoc_dtm)
#Frekanslari azalan degere gore siralama
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)


#veri gorsellestirme####
#Encok tekrar eden 10 kelime 
head(dtm_d, 10)
#En cok kullanilan kelimerin sutun grafigi
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col ="lightgreen", main ="en cok tekrar eden 10 kelime",
        ylab = "kelime frekansi")

#word could paketi ile kelime bulutu olusturma
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 50,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


#syuzhet paketi ile duygu puanlama
#3 farkli method ile duygu analizi 
#1.syuzhet methodu 
syuzhet_vector <- get_sentiment(veri$V1, method="syuzhet")
#ilk bir kaci
head(syuzhet_vector)
# anailizn ozeti 
summary(syuzhet_vector)

#2.bing methodu 
bing_vector <- get_sentiment(veri$V1, method="bing")
head(bing_vector)
summary(bing_vector)
#3.affin methodu
afinn_vector <- get_sentiment(veri$V1, method="afinn")
head(afinn_vector)
summary(afinn_vector)
#ucunun ayni anda hangi kelimeye ne deger verdigini goruntuleme
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

#her satirin duygu analizi
d<-get_nrc_sentiment(veri$V1)#hocam bu kod3 ile 4 dk arasi calisyor 
#ilk 10 tanesinin goruntulenmesi
head (d,100)

#cubuk grafigine donusumu
#transpoze
td<-data.frame(t(d))
# rowSums ilevi,satir seklinde toplamiyi saglar
td_new <- data.frame(rowSums(td[2:5022]))
#donustume ve temizleme
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#cubuk grafigi
quickplot(sentiment, data=td_new2, weight=count, geom="bar", 
          fill=sentiment, xlab = "duygular(Sentiment)",
          ylab="frekans")+ggtitle("Duygu Grafigi")


