##Analyses for Artisornis duets
require(plotrix)
require(ggplot2)
require(cowplot)
require(lme4)
require(lmerTest)
##Set working directory
setwd("/Users/jaymcentee/Dropbox/tailorbird etc/analyses/")


###########NEED TO REMOVE ACCIDENTAL MEASUREMENT TABLES THAT ARE TOO SHORT FROM NOTES IN SONGS################

####Idea for using order of notes is that we can use ADJACENT notes to capture variance. Perhaps use an additional covariate that is the number of the first of a note pair in the sequence. Or we could include first note as a category that is included as a random effect. This is probably the way to go.


##Read in data
data <- read.csv("Artisornis_duet_scoring_Jun82022.csv")
##571 rows
##table(data$Duet..Y.N.)
# 
# N   Y 
# 370 201 
##201 out of 571 initial rows are duets


data$Locality[which(data$Locality == "Within 100m of Njesi Camp, Njesi Forest, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"
data$Locality[which(data$Locality == "30-50m N Njesi Camp, Njesi Plateau, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"
data$Locality[which(data$Locality == "50m N Njesi Camp, Njesi Plateau, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"
data$Locality[which(data$Locality == "60-70m E of Njesi Camp, Njesi Plateau, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"
data$Locality[which(data$Locality == "60m ENE of Njesi Camp, Njesi Plateau, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"
data$Locality[which(data$Locality == "Njesi A, Njesi Plateau, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"
data$Locality[which(data$Locality == "Njesi A, Njesi Plateau, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"
data$Locality[which(data$Locality == "Njesi B, Njesi Plateau, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi Camp 2011"

data$Locality[which(data$Locality == "100-150m W of Njesi Camp, Njesi Plateau, Sanga District, Niassa Province, Mozambique")] <- "Njesi C"

data$Locality[which(data$Locality == "Njesi E, Njesi Forest, Malulo-Cal Village, Sanga District, Niassa Province, Mozambique")] <- "Njesi D"


####TO OUTPUT RECORDING LIST 
recordings <- rep(NA, length.out = length(data[,1]))
for (i in 1:length(data[,1])){
  recordings[i] <- paste(unlist(strsplit(data$Song[i], split = "_"))[1:5], collapse = "_")
}
recordings_list <- unique(recordings)
write.csv(recordings_list, file = "/Users/jaymcentee/Dropbox/Tailorbird duetting and territory defence/Manuscript/Summer 2023/recordings_list.csv")

##Calculations of number of solo songs versus duets for males and females of both species
table(data$Duet..Y.N.,data$Species)
# Artisornis metopias Artisornis moreaui
# N                  22                348
# Y                  98                103

solos <- subset(data, data$Duet..Y.N. == "N")
solos$male <- solos$Num.of.male.elements > 0
table(solos$male, solos$Species)
# Artisornis metopias Artisornis moreaui
# FALSE                   5                  1
# TRUE                   17                347

##Female song. What fraction of female vocalizations in recordings are solos?
##metopias 5/(5+98) = .0485 (103 female songs)
##moreaui 1/(1/103) = .0095 (104 female songs)
##Male song. What fraction of male vocalizations in recordings are solos?
##metopias 17/(17+98) = .1478 (115 male songs)
##moreaui 347/(347+103) = .7711  (450 male songs)

female_songs <- data.frame("solo" = c(5,1), "duet" = c(98,104), row.names = c("metopias", "moreaui"))
fisher.test(female_songs)

##SUBSET TO DUETS
data <- subset(data, data$Duet..Y.N. == "Y")
data <- data[complete.cases(data$Time.male.ends..s.),]
data <- data[complete.cases(data$Time.female.start..s.),]

##Check how many observations for each species
table(data$Species)
##98 metopias and 103 moreaui duets

data$Delta_male_female <- data$Time.female.start..s. - data$Time.male.start..s.
data$latency <- abs(data$Delta_male_female)
data$m_fraction_overlapped <- data$Num.of.male.elements.overlapped.by.female.elements/data$Num.of.male.elements
data$m_duration <- data$Time.male.ends..s. - data$Time.male.start..s.
data$f_duration <-data$Time.female.ends..s. - data$Time.female.start..s.

##Separate DUETTING data into two data frames, one for each species
moreaui <- subset(data, data$Species == "Artisornis moreaui")
metopias <- subset(data, data$Species == "Artisornis metopias")

##Look at some of the data
hist(moreaui$Num.of.male.elements)
##male number of elements is slightly bimodal and right-skewed
hist(moreaui$Num.of.female.elements)
##female number of elements is unimodal and right-skewed

##These next analyses are only relevant when analyzing duets only
##Create a new vector in each species' data frame that has the number of male minus number of female elements
# moreaui$male_minus_female_elements <- moreaui$Num.of.male.elements - moreaui$Num.of.female.elements
# metopias$male_minus_female_elements <- metopias$Num.of.male.elements - metopias$Num.of.female.elements

# moreaui_number_elements_test <- t.test(moreaui$Num.of.male.elements, moreaui$Num.of.female.elements, paired = TRUE)
# metopias_number_elements_test <- t.test(metopias$Num.of.male.elements, metopias$Num.of.female.elements, paired = TRUE)
##in A. moreaui, males sing more elements than females. In A. metopias, females sing more elements than males.

##What fraction of duets are initiated by males?
sum(moreaui$Delta_male_female > 0)/length(moreaui$Delta_male_female)
##95/103 In moreaui, males initiate ~92.2% of duets.

# sum(metopias$Delta_male_female > 0) #82
# length(metopias$Delta_male_female) #98
sum(metopias$Delta_male_female > 0)/length(metopias$Delta_male_female)
# ##82/98 = .837. In metopias, males initiate ~83.7% of duets.

##z-test for equality of proportions of male/female duet initiation
male_initiate <- c(92,80)
duet_total <- c(99, 95)
prop.test(male_initiate, duet_total)


##Calculate latency to response regardless of which sex initiates
median(moreaui$latency)
median(metopias$latency)

m_latency <- lmer(log(data$latency) ~ Species + (1|Locality), data = data)


##Calculate stats for number elements
mean(moreaui$Num.of.male.elements) ##[1] 6.563107
plotrix::std.error(moreaui$Num.of.male.elements) ##[1] 0.2439716
mean(moreaui$Num.of.female.elements) ##[1] 5.825243
plotrix::std.error(moreaui$Num.of.female.elements)##[1] 0.1593744

mean(metopias$Num.of.male.elements) ##9.27551
plotrix::std.error(metopias$Num.of.male.elements) ##1.228885
mean(metopias$Num.of.female.elements) ##10.94898
plotrix::std.error(metopias$Num.of.female.elements)##1.280333

m_elements.test <- lmer(log(data$Num.of.male.elements) ~ Species + (1|Locality), data = data)
anova(m_elements.test)
f_elements.test <- lmer(log(data$Num.of.female.elements) ~ Species + (1|Locality), data = data)



##Calculate mean proportion male notes overlapped by female notes and standard errors 	= √ [p (1-p) / n)]
mean(moreaui$m_fraction_overlapped) ##0.381413
(mean(moreaui$m_fraction_overlapped)*(1-mean(moreaui$m_fraction_overlapped))/length(moreaui$m_fraction_overlapped))^0.5##0.04786075
mean(metopias$m_fraction_overlapped) ##0.5482496
(mean(metopias$m_fraction_overlapped)*(1-mean(metopias$m_fraction_overlapped))/length(metopias$m_fraction_overlapped))^0.5##0.05027191

overlap.test <- lmer(asin(sqrt(data$m_fraction_overlapped)) ~ Species + (1|Locality), data = data)
anova(overlap.test)

##Calculate durations
mean(moreaui$m_duration, na.rm = TRUE)
plotrix::std.error(moreaui$m_duration, na.rm = TRUE)
mean(metopias$m_duration, na.rm = TRUE)
plotrix::std.error(metopias$m_duration, na.rm = TRUE)

mean(moreaui$f_duration, na.rm = TRUE)
plotrix::std.error(moreaui$f_duration, na.rm = TRUE)
mean(metopias$f_duration, na.rm = TRUE)
plotrix::std.error(metopias$f_duration, na.rm = TRUE)

mdur.test <- lmer(log(m_duration) ~ Species + (1|Locality), data = data)
fdur.test <- lmer(log(f_duration) ~ Species + (1|Locality), data = data)
anova(mdur.test)
anova(fdur.test)

# ##Make figures that show histograms of lag time
# png(filename = "/Users/jaymcentee/Dropbox/tailorbird etc/moreaui_lag_histogram.png", width = 7, height = 7, units = "in", res = 300)
# ggplot(moreaui, aes(x = Delta_male_female)) +
	# geom_histogram(binwidth = 0.3) +
	# labs(x = "Female lag time (s)", y = "Frequency")
# dev.off()

# png(filename = "/Users/jaymcentee/Dropbox/tailorbird etc/metopias_lag_histogram.png", width = 7, height = 7, units = "in", res = 300)
# ggplot(metopias, aes(x = Delta_male_female)) +
  # geom_histogram(binwidth = 0.3) +
  # labs(x = "Female lag time (s)", y = "Frequency")
# dev.off()

# ##We need to do a test of whether lag time is different between these two species. 
# ##Use Njesi samples only for these analyses?
# moreaui_male_starts <- moreaui[which(moreaui$Delta_male_female > 0),]
# mean(moreaui_male_starts$Delta_male_female)
# std.error(moreaui_male_starts$Delta_male_female)
# ##mean lag time for moreaui male-initiated duets is .94 s plus/minus 0.07 SE




####Analyses on spectrographic measurements of male versus female notes in moreaui and metopias, respectively
setwd("/Users/jaymcentee/Dropbox/tailorbird etc/measurements/moreaui_measurements/")
df <- read.delim(list.files()[1])
df <- subset(df, df$View == "Spectrogram 1")
df$name <- list.files()[1]
num_notes <- length(df[,1])
for (i in 1:num_notes){
  df$sex[i] <- strsplit(tail(unlist(strsplit(df$name[i], split = "_", fixed = TRUE)),n = 1), split = ".", fixed =TRUE)[[1]][1]
}
for (i in 1:length(df$name)){
  df$song[i] <- strsplit(df$name[i], split = ".Table", fixed = TRUE)[[1]][1]
  data_for_i <- data[which(data$Song == df$song[i]),]
  if(df$sex[i] == "f"){
  df$order_in_song[i] <- as.numeric(strsplit(data_for_i$Female.Measurement.order, split = ",", fixed = TRUE)[[1]][i])
  }else{
  df$order_in_song[i] <- as.numeric(strsplit(data_for_i$Male.measurement.order, split = ",", fixed = TRUE)[[1]][i])
  }
}

for(i in 2:length(list.files())){
  d <- read.delim(list.files()[i])
  d <- subset(d, d$View == "Spectrogram 1")
  d$name <- list.files()[i]
  num_notes <- length(d[,1])
  for (j in 1:num_notes){
  d$sex[j] <- strsplit(tail(unlist(strsplit(d$name[j], split = "_", fixed = TRUE)),n = 1), split = ".", fixed =TRUE)[[1]][1]
	}
  for (k in 1:length(d$name)){
  d$song[k] <- strsplit(d$name[k], split = ".Table", fixed = TRUE)[[1]][1]
  data_for_k <- data[which(data$Song == d$song[k]),]
  if(d$sex[k] == "f"){
  d$order_in_song[k] <- as.numeric(strsplit(data_for_k$Female.Measurement.order, split = ",", fixed = TRUE)[[1]][k])
  }else{
  d$order_in_song[k] <- as.numeric(strsplit(data_for_k$Male.measurement.order, split = ",", fixed = TRUE)[[1]][k])
  }
  df <- rbind(df, d)
}
}

##Above for loop currently throwing an error with i = 56. No row in data corresponding to song called "artisornis.moreaui_aug042011_njesi_jpm_c_0.05"

##2329 notes measured by SP for moreaui
df$sex <- rep(NA,length(df$name))
for (i in 1:length(df$sex)){
  df$sex[i] <- strsplit(tail(unlist(strsplit(df$name[i], split = "_", fixed = TRUE)),n = 1), split = ".", fixed =TRUE)[[1]][1]
}

df$locality <- rep(NA, length(df$name))
for (i in 1:length(df$song)){
  df$locality[i] <- strsplit(df$song[i], split = "_", fixed = TRUE)[[1]][3]
}

# df$order_in_song <- rep(NA, length(df$name))
# for(i in 1:length(df$name)){
	# sex <- df$sex[i]
	# data_for_i <- data[which(data$Song == df$song[i]),]
		
	# df$order_in_song[i] <- 
# }


cols_to_keep <- c("Begin.Time..s.","End.Time..s.","Low.Freq..Hz.", "High.Freq..Hz.", "Q1.Freq..Hz.", "Q3.Freq..Hz.", "Delta.Time..s.", "IQR.BW..Hz.", "Delta.Freq..Hz.", "Peak.Freq..Hz.", "Avg.Entropy..bits.", "sex", "locality", "song")
df <- df[,cols_to_keep]
colnames(df) <- c("begin_time", "end_time", "min_freq", "max_freq", "q1_freq", "q3_freq", "duration", "IQR_BW", "delta_freq", "peak_freq", "entropy", "sex", "locality", "song")

library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
pca_elements <- prcomp(df[,c(5:8, 10:11)], center = TRUE, scale = TRUE)

png(filename = "/Users/jaymcentee/Dropbox/tailorbird etc/PCA_Artisornis_moreaui_notes.png", width = 7, height = 7, units = "in", res = 300)
ggbiplot(pca_elements, groups = df$sex)
dev.off()

#pcs <- as.data.frame(pca_elements$x)

png(filename = "/Users/jaymcentee/Dropbox/tailorbird etc/PCA_Artisornis_moreaui_notes_by_locality.png", width = 7, height = 7, units = "in", res = 300)
ggbiplot(pca_elements, groups = df$locality)
dev.off()

#####################MALE SONG-LEVEL ANALYSES####################
df.m <- df[which(df$sex == "m"),] 
song_df.m <- data.frame(unique(df.m$song), row.names = unique(df.m$song))
song_df.m$min_freq <- tapply(df.m$min_freq, df.m$song, FUN = mean)
song_df.m$max_freq <- tapply(df.m$max_freq, df.m$song, FUN = mean)
song_df.m$q1_freq <- tapply(df.m$q1_freq, df.m$song, FUN = mean)
song_df.m$q3_freq <- tapply(df.m$q3_freq, df.m$song, FUN = mean)
song_df.m$duration <- tapply(df.m$duration, df.m$song, FUN = mean)
song_df.m$IQR_BW <- tapply(df.m$IQR_BW, df.m$song, FUN = mean)
song_df.m$delta_freq <- tapply(df.m$delta_freq, df.m$song, FUN = mean)
song_df.m$peak_freq <- tapply(df.m$peak_freq, df.m$song, FUN = mean)
song_df.m$entropy <- tapply(df.m$entropy, df.m$song, FUN = mean)

song_df.m$locality <- rep(NA, length(song_df.m$unique.df.m.song.))
for (i in 1:length(song_df.m$unique.df.m.song.)){
  song_df.m$locality[i] <- strsplit(as.character(song_df.m$unique.df.m.song.[i]), split = "_", fixed = TRUE)[[1]][3]
}

pca_male_songs <- prcomp(song_df.m[,c(4:7,9:10)], center = TRUE, scale = TRUE)
ggbiplot(pca_male_songs, groups = song_df.m$locality)

###################FEMALE SONG-LEVEL ANALYSES###################
##Need to find examples of songs where most notes were measured
##Make a column in the data frame called "data" that has the number of female notes measured

data$number_female_notes_measured <- rep(NA, length(data$Species))
for(i in 1:length(data$Song)){
  data$number_female_notes_measured[i] <- length(strsplit(as.character(data$Female.Measurement.order)[i], split = ",", fixed = TRUE)[[1]])
}

##First make a data frame of female notes only
df.f <- df[which(df$sex == "f"),]


##################BELOW TO BE FURTHER EDITED =###########################


data <- read.csv("~/Dropbox/tailorbird etc/analyses/Artisornis_duet_scoring.csv")

df$duet <- rep(NA, length(df$name))

for (i in 1:length(df$name)){
  df$duet[i] <- as.character(data$Duet..Y.N.[which(data$Song == df$song[i])])  
}


setwd("/Users/jaymcentee/Dropbox/tailorbird etc/measurements/metopias_male_measurements/")

df <- read.delim(list.files()[1])
df <- subset(df, df$View == "Spectrogram 1")
df$name <- list.files()[1]
for(i in 2:length(list.files())){
	d <- read.delim(list.files()[i])
	d <- subset(d, d$View == "Spectrogram 1")
	d$name <- list.files()[i]
	df <- rbind(df, d)
}
df$sex <- "male"

setwd("/Users/jaymcentee/Dropbox/tailorbird etc/measurements/metopias_female_measurements/")
for(i in 1:length(list.files())){
	d <- read.delim(list.files()[i])
	d <- subset(d, d$View == "Spectrogram 1")
	d$name <- list.files()[i]
	d$sex = "female"
	df <- rbind(df, d)
}

s <- c(seq(from = 4, to = 14), 19:20)
df <- df[,s]

colnames(df) <- c("begin_time", "end_time", "min_freq", "max_freq", "q1_freq", "q3_freq", "entropy", "delta_freq", "duration", "IQR_BW", "peak_freq", "name", "sex")


male_parts <- df[which(df$sex == "male"),]
female_parts <- df[which(df$sex == "female"),]

write.csv(male_parts, "male_metopias_measurements.csv")
write.csv(female_parts, "female_metopias_measurements.csv")


##Get rid of any measurements made on songs that were not bandpass filtered.
df <- df[which(df$max_freq < 11000),]
df <- df[which(df$entropy > 0),]

setwd("/Users/jaymcentee/Dropbox/tailorbird etc/measurements/")
pca_elements <- prcomp(df[,c(5:11)], center = TRUE, scale = TRUE)

write.csv(pca_elements[2], "loadings.csv")
write.csv(summary(pca_elements)[6], "component_importance.csv")

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)



png(filename = "/Users/jaymcentee/Dropbox/tailorbird etc/PCA_Artisornis_notes.png", width = 7, height = 7, units = "in", res = 300)
ggbiplot(pca_elements, groups = df$sex)
dev.off()
