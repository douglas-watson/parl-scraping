rm(list=ls())

# working directory is "parl_scraping/biography_retrieval/biography_dates_analysis"

library(plyr)
library(jsonlite)
library(lubridate)
library(ggplot2)

wd <- getwd()
setwd("../bio_json/")

lf <- list.files()

# to do
# trier conseil national et conseil des états
# appondre intervalles quand consécutifs (par ex. Pierre Aguet)
# pouvoir gérer absence et retour du conseil national

df <- data.frame()

for (i in 1:length(lf)) {
	a <- fromJSON(lf[i])
	b <- a$councilMemberships
	if (length(b) != 0) {
		if (length(b$leavingDate) == 0) b$leavingDate <- NA
		df <- rbind.fill(df, data.frame(id = a$id, name = paste0(a$lastName, ", ", a$firstName, collapse=""), birthday = a$birthDate, entry = b$entryDate, leaving = b$leavingDate, canton = b$canton, conseil = b$council$name, stringsAsFactors = FALSE))
	}
}

setwd(wd)

date.extract <- function(a) {
	ymd(unlist(regmatches(a, gregexpr("^[0-9\\-]+", a))))
}

df$birthday <- date.extract(df$birthday)
df$entry <- date.extract(df$entry)
df$leaving[is.na(df$leaving)] <- "2015-11-30"
df$leaving <- date.extract(df$leaving)

df$difference <- df$leaving - df$entry

days_in_total <- aggregate(difference ~ id, data = df, sum)

days_in_total[order(days_in_total$difference),]

# Le plus longtemps (48 ans)
df[df$id == "2322",]

# Les plus courts (10 jours)
df[df$id == "2095",]
df[df$id == "2808",]
df[df$id == "2820",]
df[df$id == "2845",]

df[df$id == "2895",]

# D'après question de @munsterma sur Twitter : combien de temps les gens restent en fonction de leur âge ?
age_at_entry <- aggregate(entry - birthday ~ id, data = df, min)
age_at_end <- aggregate(leaving - birthday ~ id, data = df, max)
age_duree <- merge(merge(age_at_entry, age_at_end), days_in_total)
colnames(age_duree) <- c("id", "age", "fin", "difference")
age_duree$age <- as.numeric(age_duree$age)/365
age_duree$fin <- as.numeric(age_duree$fin)/365
age_duree$difference <- as.numeric(age_duree$difference)/365

g <- ggplot(age_duree, aes(x=difference,y=age)) + geom_point(size = 1) + ylab("Âge au moment de l'élection") + xlab("Durée du mandat") + ggtitle("Conseil national et Conseil des États") + ylim(20, 90)
png("age_vs_duree.png", width = 960, height = 480)
g
dev.off()

g <- ggplot(age_duree, aes(x=difference,y=fin)) + geom_point(size = 1) + ylab("Âge au moment du départ") + xlab("Durée du mandat") + ggtitle("Conseil national et Conseil des États") + ylim(20, 90)
png("age_vs_depart.png", width = 960, height = 480)
g
dev.off()

