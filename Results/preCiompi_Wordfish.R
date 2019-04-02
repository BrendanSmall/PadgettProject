#install.packages("austin", repos = "http://R-Forge.R-project.org", 
#                 dependencies = "Depends", type = "source")
library(tm)
#library(NLP)
#library(austin)
#library(ggplot2)
library(plyr)
library(quanteda)
library(ggplot2)

setwd("C:\\Users\\Benja\\Box Sync\\_PHD\\_Florence")
data <- read.csv("data - input\\TblWordFish_FullDB.csv", stringsAsFactors = F) #"preCiompi_wordFrequenciesPerSpeech_AddData2.csv"
data <- subset(data, meet_type!="c")
#data <- subset(data, meet_type == "r")
#data <- subset(data, is.na(office.num.2))

#data <- read.csv("data - output\\preCiompi_wordFrequenciesPerSpeech_AddData2.csv", stringsAsFactors = F) 
#data <- subset(data, select = -c(ecclesia, guelf, papa, liga, bellum, pax, civis, unio, libertas, unitas,
#                             pecunia, clericus, praestantia, guerra, civitas, fiat, fiant, factum))
#data[is.na(data)] <- 0

#Chapter create
data$chapter.num <- NA
data$chapter.num[data$Chapter == 1376.1] <- 1
data$chapter.num[data$Chapter == 1376.2] <- 2
data$chapter.num[data$Chapter == 1377.1] <- 3
data$chapter.num[data$Chapter == 1377.2] <- 4
data$chapter.num[data$Chapter == 1378.1] <- 5

#create NA values for faction and office
data$faction[is.na(data$faction)] <- "NF1"
data$faction[data$faction == "Lib"] <- "Civ"
data$faction[data$faction == "P&L"] <- "P.G."
data$faction1[data$faction1 == 0] <- "NF2"
data$office.num.2 [is.na(data$office.num.2)] <- ""

# look at number of speeches by chapter and faction
ddply(data, .(faction, chapter.num), summarize, len = length(meet_type))
df <- ddply(data, .(faction, chapter.num, Speaker_id), summarize, len = length(meet_type))
ddply(df, .(faction, chapter.num), summarize, len = length(Speaker_id))


#########################
# (A) FACTIONS AS UNITS #
#########################
### (1) SEPARATE MODELS
#pdf("data - output\\wordfish_faction_units_by_chapter_separateModels.pdf")
lapply(1:5, function(x) {
  data2 <- subset(data, chapter.num == x)
  f1 <- ddply(data2, .(faction), summarize, text = paste(Speech_Lem_N, collapse = " "))
  colnames(f1) <- c("doc_id", "text")
  f2 <- ddply(data2, .(faction1), summarize, text = paste(Speech_Lem_N, collapse = " "))
  colnames(f2) <- c("doc_id", "text")
  f <- rbind(f1, f2)
  f$doc_id

  #turn speech into doc term matrix
  mycorpus <- corpus(f)
  mydtm <- dfm(mycorpus, remove_punct = T)
  mydtm <- dfm_trim(mydtm, min_termfreq = 3)
  
  mywf <- textmodel_wordfish(mydtm, dir = c(1,3)) 
  df <- cbind(mywf$docs, mywf$theta)
  df <- df[order(df[,2]),]
  #summary(mywf)
  textplot_scale1d(mywf) + ggtitle(paste("Chapter", x)) 
  ggsave(paste0("data - output\\wordfish_faction_units_by_chapter_separateModels_chapter", x, ".jpeg"))
  #textplot_scale1d(mywf, margin = "features", 
  #                 highlighted = c("pecunia", "papa", "guelf"))
  
  #df <- as.data.frame(cbind(mywf$features, mywf$beta))
  #df$V2 <- as.numeric(as.character(df$V2))
  #df <- df[order(df$V2),] 
  #write.csv(df, "data - output\\wordfish_words.csv", row.names = F)
})
#dev.off()

### (2) ONE MODEL
data2 <- data
f1 <- ddply(data2, .(faction, chapter.num, meet_type), summarize, text = paste(Speech_Lem_N, collapse = " "))
f1$facCh <- paste0(f1$faction, "_", f1$chapter.num, "_", f1$meet_type)
f1$len <- sapply(strsplit(f1$text, " "), length)
f1 <- subset(f1, len > 10)
a <- f1$faction
c <- f1$meet_type
f1 <- f1[,c("facCh", "text")]
f2 <- ddply(data2, .(faction1, chapter.num, meet_type), summarize, text = paste(Speech_Lem_N, collapse = " "))
f2$facCh <- paste0(f2$faction1, "_", f2$chapter.num, "_", f2$meet_type)
f2$len <- sapply(strsplit(f2$text, " "), length)
f2 <- subset(f2, len > 10)
b <- f2$faction1
d <- f2$meet_type
f2 <- f2[,c("facCh", "text")]
f <- rbind(f1, f2)
colnames(f) <- c("doc_id", "text")
ab <- c(a,b)
cd <- c(c,d)
ef <- paste0(ab,"_",cd)

#turn speech into doc term matrix
mycorpus <- corpus(f)
mydtm <- dfm(mycorpus, remove_punct = T)
mydtm <- dfm_trim(mydtm, min_termfreq = 10)

mywf <- textmodel_wordfish(mydtm, dir = c(5,15))
pdf("data - output\\wordfish_faction_units_by_chapter_and_meetingType_oneModel.pdf")
textplot_scale1d(mywf, groups = ef)
#ggsave("data - output\\wordfish_faction_units_by_chapter_oneModel.jpeg")
dev.off()


########################
# (B) SPEAKER AS UNITS #
########################
#pdf("data - output\\wordfish_speaker_units_by_chapter_PG_Civ.pdf")
lapply(1:5, function(x) {
  data2 <- subset(data, chapter.num == x)
  
  s <- ddply(data2, .(Speaker_id), summarize, text = paste(Speech_Lem_N, collapse = " "), 
             fac1 = paste(unique(faction)), fac2 = paste(unique(faction1)), len = sum(Speech_Len))
  s$len <- sapply(strsplit(s$text, " "), length)
  s <- subset(s, len > 2)
  s2 <- s[,1:2]
  colnames(s2) <- c("doc_id", "text")
  
  # get most prolific PG and Civ speakers
  p1 <- subset(s, fac1 == "P.G.")
  p1 <- p1[p1$len == max(p1$len),1]
  p1 <- match(p1,s2$doc_id)
  p2 <- subset(s, fac1 == "Civ")
  p2 <- p2[p2$len == max(p2$len),1]
  p2 <- match(p2,s2$doc_id)
  
  mycorpus_s <- corpus(s2)
  mydtm_s <- dfm(mycorpus_s, remove_punct = T)
  mydtm_s <- dfm_trim(mydtm_s, min_termfreq = 3)
  
  mywf_s <- textmodel_wordfish(mydtm_s, dir = c(p1,p2)) #  and
  #summary(mywf_s)
  #textplot_scale1d(mywf_s)
  textplot_scale1d(mywf_s, groups = s$fac1) + ggtitle(paste("Chapter", x))
  ggsave(paste0("data - output\\wordfish_speaker_units_by_chapter_PG_Civ_chapter", x, ".jpeg"), scale = 1.5)
  })
#dev.off()



#######################
# (C) SPEECH AS UNITS #
#######################
pdf("data - output\\wordfish_speech_units_by_chapter_PG_Civ.pdf")
lapply(1:5, function(x) {
  data2 <- subset(data, chapter.num == x)
  data2 <- subset(data2, AgreedID!="11636")
  s <- data2[,c(1,10,4,6)]
  names(s) <- c("AgreedID", "text", "fac1", "fac2")
  
  s$len <- sapply(strsplit(s$text, " "), length)
  s <- subset(s, len > 2)
  s2 <- s[,1:2]
  colnames(s2) <- c("doc_id", "text")
  
  print(nrow(s2))
  
  # get most prolific PG and Civ speakers
  p1 <- subset(s, fac1 == "P.G.")
  p1 <- p1[p1$len == max(p1$len),1]
  p1 <- match(p1,s2$doc_id)
  p2 <- subset(s, fac1 == "Civ")
  p2 <- p2[p2$len == max(p2$len),1]
  p2 <- match(p2,s2$doc_id)
  
  mycorpus_s <- corpus(s2)
  mydtm_s <- dfm(mycorpus_s, remove_punct = T)
  mydtm_s <- dfm_trim(mydtm_s, min_termfreq = 3)
  
  mywf_s <- textmodel_wordfish(mydtm_s, dir = c(p1,p2)) #  and
  summary(mywf_s)
  #textplot_scale1d(mywf_s)
  textplot_scale1d(mywf_s, groups = s$fac1) + ggtitle(paste("Chapter", x))
})
dev.off()



####################
# (D) EXPORE WORDS #
####################
topBottom <- sapply(1:5, function(x) {
    data2 <- subset(data, chapter.num == x)
    f1 <- ddply(data2, .(faction), summarize, text = paste(Speech_Lem_N, collapse = " "))
    colnames(f1) <- c("doc_id", "text")
    f2 <- ddply(data2, .(faction1), summarize, text = paste(Speech_Lem_N, collapse = " "))
    colnames(f2) <- c("doc_id", "text")
    f <- rbind(f1, f2)
    f$doc_id
    #turn speech into doc term matrix
    mycorpus <- corpus(f)
    mydtm <- dfm(mycorpus, remove_punct = T)
    mydtm <- dfm_trim(mydtm, min_termfreq = 3)
    # run wordfish
    mywf <- textmodel_wordfish(mydtm, dir = c(1,3)) 
    
    #a <- mywf$features[mywf$beta == max(mywf$beta)]
    #b<- mywf$features[mywf$beta == min(mywf$beta)]
    p90 <-quantile(mywf$beta, 0.9)
    p10 <-quantile(mywf$beta, 0.1)
    
    list(mywf$features[mywf$beta > p90], mywf$features[mywf$beta < p10])
    })
top10 <- unlist(c(topBottom[1], topBottom[3], topBottom[5], topBottom[7], topBottom[9]))
bottom10 <- unlist(c(topBottom[2], topBottom[4], topBottom[6], topBottom[8], topBottom[10]))

dftop10pct <- as.data.frame(table(top10))
dftop10pct <- dftop10pct[order(dftop10pct$Freq, decreasing = T),]
dftop10pct <- subset(dftop10pct, Freq > 1)
dftop10pct

dfbottom10pct <- as.data.frame(table(bottom10))
dfbottom10pct <- dfbottom10pct[order(dfbottom10pct$Freq, decreasing = T),]
dfbottom10pct <- subset(dfbottom10pct, Freq > 1)
dfbottom10pct




# ESTIAMTE INDIVIDUAL SPEECHES
data2 <- subset(data, Speech_Len > 20)
ind <- data2[,c("AgreedID", "Speech_Lem_N")]
colnames(ind) <- c("doc_id", "text")

c_ind <- corpus(ind)
mydtm_ind <- dfm(c_ind, remove_punct = T)

mywf_ind <- textmodel_wordfish(mydtm_ind, dir = c(17,11))

textplot_scale1d(mywf_ind, groups = ind$faction)


textplot_scale1d(tmod_wf, groups = docvars(mydtm_ind2, "faction1"))




#plot speech points
ggplot(mywf, aes(y = theta, x = )) + 
  geom_boxplot() +
  labs(x = "FactionXOfficeNum", y = "Ideology score") + guides(col = guide_legend(title = "Ideology in speech")) #geom_point() geom_boxplot()








#various factionXoffice definitions
data$fact.office <- NA

#full office breakdown
#data$fact.office <- paste(data$faction, data$office.num.2, sep="-")

#office vs no office
###
###HELP HERE - This doesnt correctly create the combination of Faction X Office
###
data$fact.office [nchar(data$office.num.2)> 0] <- paste(data$faction, 'O', sep="-")
data$fact.office [nchar(data$office.num.2)== 0] <- paste(data$faction, 'P', sep="-")

#add Captains of the Parte Guelf
#data$fact.office [data$office.num.2== 6] <- paste(data$faction, 'PG.C', sep="-")

#different subselection options
#& data$fact.office != "Lib-O" & data$fact.office != "NF-O" & data$fact.office != "P&L-O"
#data$chapter.num == 1 & data$meet_type == 'r'

#Create table with id and faction/facXoffice
ideo.df.1 <-data[which (data$chapter.num == 1 & data$meet_type == 'a' & data$faction != "P&L")
                 , c("AgreedID", "faction")]
colnames(ideo.df.1) <- c("doc_id", "FactionXOffice")

#create table with id and speech
text.1 <- data[which (data$chapter.num == 1 & data$meet_type == 'a' & data$faction != "P&L")
               , c("AgreedID", "Speech_Lem_basic")]
colnames(text.1) <- c("doc_id", "text")

#turn speech into doc term matrix
mycorpus <- Corpus(DataframeSource(text.1))
mydtm <- DocumentTermMatrix(mycorpus)

#run wordfish
mywf <- wordfish(as.wfm(mydtm), dir=c(17, 11), control = list(tol = .00003)) #ch1: 62, 226 Ch2: (234-349), (234-343)

#plot speech points
ggplot(mapping = aes(y = mywf$theta, x = ideo.df.1$FactionXOffice, color=ideo.df.1$FactionXOffice)) + geom_boxplot() +
  labs(x = "FactionXOfficeNum", y = "Ideology score") + guides(col = guide_legend(title = "Ideology in speech")) #geom_point() geom_boxplot()

#Threshold psi - frequency and beta - weights
#imptntwords.wf <- which(mywf$psi > -5)# & mywf$psi > 0)

#plot word scores by frequency and weight
ggplot(mapping = aes(x = mywf$beta[imptntwords.wf], y = mywf$psi[imptntwords.wf], label = mywf$words[imptntwords.wf])) + 
  geom_text(size = 3) + labs(x = "Beta (weights)", y = "Psi (Frequency)")
