library(officer)
library(rstudioapi)
#library(readxl)


this_file = rstudioapi::getActiveDocumentContext()$path
path = box::file()
check_path = unlist(strsplit(this_file, split = "/"))
check_path = paste0(check_path[1:length(check_path)-1], collapse="/")
if (check_path != path){warning("There might be issues related to the path of files.", call. = TRUE, immediate. = FALSE, domain = NULL)}


setwd(file.path(path, "data"))
allfiles = dir()
print(allfiles)


data_df = read.csv2("results-survey983757_Test1.csv")
#data_df = read_xlsx("results-survey231835.xlsx")

#--------------
'
data_files = list.files(pattern="*.csv", full.names=F) #--> Namensliste erstellen
data_list = lapply(data_files, read.csv2, header=T, na.strings=c("", " ", "NA"), encoding="UTF-8")
data_df = data.frame()
for(i in 1:length(data_files)){
  temp_df = data_list[[i]]
  data_df = rbind(data_df, temp_df)
}
rm(i, data_files, temp_df)
write.csv2(data_df, "results.csv")
'
#--------------



#--> plots 
#------------------------------#



graftitle = names(data_df[10])

ergebnis = table(data_df[10])
#-------- plot -------
yAchse = c(0, max(ergebnis) + round(max(ergebnis*0.2), 0))
if(max(ergebnis) == 1 ){yAchse = c(0, 2)} #y-Achse ggf. auf 2 setzen
par(mar=c(10, 4, 4, 2) +.1) #Darstellungsbereich für Labels vergrößern
barplot(ergebnis, width = 0.9, main = strwrap(graftitle, width = 70), cex.main = 1, axes = T, ylim = yAchse, xpd = F, las = 2, cex.names = .8)
text(ergebnis, labels = c(as.character(ergebnis)), cex = 1, col = "blue", pos = 3) #Datenbeschriftung
#---------------------


ergebnis_2 = as.data.frame(table(data_df[10]))
#-------- plot -------
#require(ggplot2)

p1 = ggplot(ergebnis_2, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity", color = "blue", fill="grey",) + 
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
  labs(title = graftitle, y = "Häufigkeiten", x = "x-Werte") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.line.x = element_blank(),
    axis.title.x=element_blank(), #remove axis title
    axis.title.y=element_blank(), #remove axis title
    #axis.text.x=element_blank(),  #remove axis labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.ticks.x=element_blank()  #remove axis ticks
  )

plot(p1)
#---------------------



