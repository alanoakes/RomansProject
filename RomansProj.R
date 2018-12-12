#!/bin/bash
# Libraries Used ---------------------------------
library(reshape2)
library(ggplot2)

## CREATE DATASETS --------------------------------
# Create frequency table of occurences "sin" was
# found throughout KJV Bible using biblegateway.com
Books<-c("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy", "Joshua", "Judges", "Ruth", "1 Samuel", "2 Samuel", "1 Kings", "2 Kings", "1 Chronicles", "2 Chronicles", "Ezra", "Nehemiah", "Job", "Psalm", "Proverbs", "Ecclesiastes", "Song of Solomon", "Isaiah", "Jeremiah", "Lamentations", "Ezekiel", "Daniel", "Hosea", "Amos", "Micah", "Habakkuk", "Zephaniah", "Haggai", "Zechariah", "Matthew", "Mark", "Luke", "John", "Acts", "Romans", "1 Corinthians", "2 Corinthians", "Galatians", "Ephesians", "Philippians", "Colossians", "1 Thessalonians", "2 Thessalonians", "1 Timothy", "2 Timothy", "Titus", "Hebrews", "James", "1 Peter", "2 Peter", "1 John", "Jude", "Revelation")
Freq1<-c(14, 39, 87, 69, 18, 6, 7, 1, 26, 8, 26, 23, 15, 31, 11, 28, 23, 99, 22, 8, 1, 55, 37, 7, 38, 10, 11, 4, 7, 2, 4, 1, 2, 17, 14, 38, 23, 14, 50, 11, 7, 6, 7, 2, 8, 1, 1, 5, 1, 2, 29, 7, 7, 4, 16, 1, 5)
SinInBible<-data.frame(Books,Freq1)

# Create frequency table of top 10 single words
# found in Romans using https://wordcounter.io/
Words<-c("God", "unto", "shall", "law", "Christ", "ye", "thou", "sin", "Lord")
Freq2<-c(163,82,81,78,66,53,53,45,44)
RomWordCount<-data.frame(Words,Freq2)

# Create frequency table from searching "sin"
# in https://www.biblegateway.com, book of Romans
RomChapter<-1:16
Freq3<-c(0, 2, 5, 2, 8, 16, 11, 3, 0, 0, 1, 0, 0, 1, 1, 0)
SinInRomans<-data.frame(RomChapter,Freq3)

# section1: Sins
Sins<-c("law", "god", "sinned", "shall", "sin", "judged", "hath", "glory", "sins", "blessed")
Freq4<-c(6, 5, 3, 3, 3, 2, 2, 2, 2, 2)
Section1<-data.frame(Sins,Freq4)

# section2: Sin
Sin<-c("sin", "law", "unto", "god", "ye", "death", "shall", "righteousness", "christ", "dead")
Freq5<-c(41, 20, 15, 14, 14, 10, 7, 6, 6, 6)
Section2<-data.frame(Sin,Freq5)

# create frequency for word of sin and its variants
# Section1: sins (vs 1:1-5:8)
x1<-c("sinned", "sinned", "sinner", "sin", "sin", "sinned", "sins", "sins", "sin", "sinners")

# Section 2: sin (vs 5:9-8:39)
x2<-c("sin", "sin", "sinned", "sin", "sin", "sinned", "sinned", "sinners", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sins", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sin", "sinful", "sin", "sin", "sin", "sin", "sin", "sin", "sinful", "sin", "sin", "sin")
Section1a<-table(x1)
Section1b<-as.data.frame(Section1a)
Section2a<-table(x2)
Section2b<-as.data.frame(Section2a)
VarofSin<-merge(x=Section1b, y=Section2b, by.x="x1", by.y="x2", all = T)
VarofSin[is.na(VarofSin)] <- 0
colnames(VarofSin)<-c("SinVariants","Section1","Section2")


## HYPOTHESIS TESTING -----------------------------------------
## Watchman Nee states vs 1:1-5:11 contain more content 
## on `sins` whereas vs 5:12-8:39 contain more content on `sin`.
# what are top actual words in these two sections 
# text frequencies provided by wordcounter.io


# check stats --------------------------------
sum(SinInBible$Freq1) # sin ct'd in Bible
sum(RomWordCount$Freq2) # rep word ct in Romans
sum(SinInRomans$Freq3) # ct for sin similiar in Romans

# top ten books of Bible mentioning sin
SinInBible1<-SinInBible[order(-SinInBible$Freq1),]
toptenbooks<-head(SinInBible1, 10)
toptenbooks
plot(density(SinInBible$Freq1),
    main="Distribution per Book of KJV Bible Containing Word Sin",
    xlab="Frequency of Word Sin")

# Plots to scan sin frequency in Romans
boxplot(SinInRomans$Freq3,names="Word Sin Per Chapter")
SinInRomans
# Variants of sin used through both sections
VarofSin1<-melt(VarofSin)
ggplot(data=VarofSin1, aes(x=SinVariants, y=value, fill=variable)) +
geom_bar(stat="identity")

# word match test out of top ten words from each section
compSin<-merge(x=Section1, y=Section2, by.x="Sins", by.y="Sin", all = T)
colnames(compSin)<-c("FreqText", "Section1", "Section2")
compSin$FreqText<-as.character(compSin$FreqText)
compSin[!with(compSin,is.na(Section1)| is.na(Section2)),]
CompSin<-compSin[order(-compSin$FreqText),]
CompSin # compare lists

# this is a new line of text

