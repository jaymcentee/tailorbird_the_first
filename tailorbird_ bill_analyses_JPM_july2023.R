##tailorbird bill measurements
##MUSEUM SPECIMENS ALONE
data <- read.csv("/Users/jaymcentee/Dropbox/Tailorbird duetting and territory defence/Manuscript/2024/Data/specimen_measurements_JPM.csv")

table(data$Sex)
data$Sex[which(data$Sex == "Female (not breeding)")] <- "Female"
data$Sex[which(data$Sex == "Female (young)")] <- "Female"
#Turn unknown sex into third sex category
data$Sex[which(data$Sex %in% c("?", "Female (?)"))] <- "U"
##Remove definitive juvenile
table(data$Sex)
data <- data[which(data$Sex %in% c("Female", "Male", "U")),]


data <- data[complete.cases(data$Bill.tip.to.nares, data$Bill.tip.to.skull, data$Bill.width..at.anterior.nares., data$Bill.depth..at.anterior.nares.),]

bills <- data.frame(data$Bill.tip.to.nares, data$Bill.tip.to.skull, data$Bill.width..at.anterior.nares., data$Bill.depth..at.anterior.nares.)

bill_PCA <- prcomp(bills, scale. = TRUE)
summary(bill_PCA)

all_bill_PCA_scores <- data.frame(bill_PCA$x)

#install_github("vqv/ggbiplot", force = TRUE)
library(ggbiplot)
ggbiplot(bill_PCA, obs.scale = 1, var.scale =1, groups = data$Species)

m1 <- glm(PC1 ~ data$Species + data$Sex, data = all_bill_PCA_scores)

metopias_data <- data[which(data$Species == "African Tailorbird"),]
metopias_bills <- data.frame(metopias_data$Bill.tip.to.nares, metopias_data$Bill.tip.to.skull, metopias_data$Bill.width..at.anterior.nares., metopias_data$Bill.depth..at.anterior.nares.)
metopias_bill_PCA <- prcomp(metopias_bills, scale. = TRUE)

ggbiplot(metopias_bill_PCA, obs.scale = 1, var.scale =1, groups = metopias_data$geographical_overlap)

metopias_bill_PCA_scores <- data.frame(metopias_bill_PCA$x)

meto_m1 <- glm(PC1 ~ metopias_data$geographical_overlap + metopias_data$Sex, data = metopias_bill_PCA_scores)

##mistnetting data
netted <- read.csv("/Users/jaymcentee/Dropbox/Tailorbird duetting and territory defence/Manuscript/Summer 2023/Data/Mist-netting.csv")

colnames(netted)[which(names(netted) == "Bill.Width..mm.")] <- "Bill.width..at.anterior.nares."
colnames(netted)[which(names(netted) == "Bill.Depth..mm.")] <- "Bill.depth..at.anterior.nares."
colnames(netted)[which(names(netted) == "Bill.Nares..mm.")] <- "Bill.tip.to.nares"
colnames(netted)[which(names(netted) == "Bill.Skull..mm.")] <- "Bill.tip.to.skull"
colnames(netted)[which(names(netted) == "Site")] <- "Location"

netted_tailorbirds <- netted[which(netted$Vernacular %in% c("Long-billed Tailorbird", "African Tailorbird")),]

#columns needed for analyses as named in "data": Individual.ID, Location, Scientific, geographical_overlap, Sex, Bill.tip.to.nares, Bill.tip.to.skull, Bill.width..at.anterior.nares.Bill.depth..at.anterior.nares.

#changing column names in netted_tailorbirds to correspond to "data"
#Scientific doesn't need change
#geographic_overlap must be created
netted_tailorbirds$geographical_overlap <- rep("y", length.out = length(netted_tailorbirds[,1]))
netted_tailorbirds$Individual.ID <- seq(1, length(netted_tailorbirds[,1]))

View(netted_tailorbirds)

netted_tailorbirds$source <- rep("field", length.out = length(netted_tailorbirds[,1]))

nt_for_bind <- data.frame(netted_tailorbirds$Individual.ID, netted_tailorbirds$Location, netted_tailorbirds$Scientific, netted_tailorbirds$geographical_overlap, netted_tailorbirds$Sex, netted_tailorbirds$Bill.width..at.anterior.nares., netted_tailorbirds$Bill.depth..at.anterior.nares., netted_tailorbirds$Bill.tip.to.nares, netted_tailorbirds$Bill.tip.to.skull, netted_tailorbirds$source)
colnames(nt_for_bind) <- c("Individual.ID", "Location", "Species", "geographical_overlap", "Sex", "Bill_width", "Bill_depth", "Bill_length_nares", "Bill_length_skull", "source")

data$source <- rep("museum", length.out = length(data[,1]))

data_for_bind <- data.frame(data$Individual.ID, data$Location, data$Scientific, data$geographical_overlap, data$Sex, data$Bill.width..at.anterior.nares., data$Bill.depth..at.anterior.nares., data$Bill.tip.to.nares, data$Bill.tip.to.skull, data$source)
colnames(data_for_bind) <- c("Individual.ID", "Location", "Species", "geographical_overlap", "Sex", "Bill_width", "Bill_depth", "Bill_length_nares", "Bill_length_skull", "source")

data_field_plus_mus <- rbind(nt_for_bind,data_for_bind)

data_field_plus_mus$Sex[which(data_field_plus_mus$Sex %in% c("?", "F?", "M?"))] <- "U"
data_field_plus_mus$Sex[which(data_field_plus_mus$Sex == "Male")] <- "M"
data_field_plus_mus$Sex[which(data_field_plus_mus$Sex == "Female")] <- "F"

data_field_plus_mus$groupings <- c(
"moreaui_Moz",
"moreaui_Moz",
"metopias_Moz",
"metopias_Moz",
"metopias_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"metopias_Moz",
"metopias_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_Moz",
"moreaui_EU",
"moreaui_EU",
"moreaui_EU",
"moreaui_EU",
"metopias_Moz",
"metopias_Moz",
"metopias_Moz",
"metopias_Moz",
"metopias_Moz",
"metopias_Moz",
"metopias_Moz",
"metopias_allo",
"metopias_allo",
"metopias_allo",
"metopias_allo",
"metopias_allo",
"metopias_allo",
"metopias_allo",
"metopias_allo",
"metopias_EU",
"metopias_EU",
"metopias_allo",
"metopias_allo"
)

field_plus_mus_bills <- data.frame(data_field_plus_mus$Bill_width, data_field_plus_mus$Bill_depth, data_field_plus_mus$Bill_length_nares, data_field_plus_mus$Bill_length_skull)
colnames(field_plus_mus_bills) <- c("Bill_width", "Bill_depth", "Bill_length_nares", "Bill_length_skull")

field_plus_mus_bills[] <- lapply(field_plus_mus_bills, as.numeric)

new_bill_PCA <- prcomp(field_plus_mus_bills, scale. = TRUE)
summary(new_bill_PCA)

new_bill_PCA_scores <- data.frame(new_bill_PCA$x)

new_bill_m1 <- glm(PC1 ~ data_field_plus_mus$Species + data_field_plus_mus$Sex + data_field_plus_mus$source, data = new_bill_PCA_scores)

ggbiplot(new_bill_PCA, obs.scale = 1, groups = data_field_plus_mus$groupings)


##Mozambique only analyses
data_field_plus_mus$Moz <- rep(NA, length.out = length(data_field_plus_mus[,1]))
for (i in 1:length(data_field_plus_mus[,1])){
  data_field_plus_mus$Moz[i] <- "Moz" %in% strsplit(data_field_plus_mus$groupings[i], split = "_")[[1]]
}

Moz_only_data <- data_field_plus_mus[which(data_field_plus_mus$Moz == TRUE),]
Moz_only_bills <- data.frame(Moz_only_data$Bill_width, Moz_only_data$Bill_depth, Moz_only_data$Bill_length_nares, Moz_only_data$Bill_length_skull)
colnames(Moz_only_bills) <- c("Bill_width", "Bill_depth", "Bill_length_nares", "Bill_length_skull")

Moz_only_bills[] <- lapply(Moz_only_bills, as.numeric)


Moz_bill_PCA <- prcomp(Moz_only_bills, scale. = TRUE)
summary(Moz_bill_PCA)

Moz_bill_PCA_scores <- data.frame(Moz_bill_PCA$x)

ggbiplot(Moz_bill_PCA, obs.scale = 1, groups = Moz_only_data$Species)

##metopias only analyses
meto_field_plus_mus <- data_field_plus_mus[which(data_field_plus_mus$Species == "Artisornis metopias"),]
