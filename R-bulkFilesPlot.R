#library(officer)
library(rstudioapi)
library(readxl)
library(writexl)
library(ggplot2)

#---------------------------------------------------#
#-------------------- functions --------------------#

fun_locate_data_folder = function(){
  this_file = rstudioapi::getActiveDocumentContext()$path
  path = box::file()
  check_path = unlist(strsplit(this_file, split = "/"))
  check_path = paste0(check_path[1:length(check_path)-1], collapse="/")
  
  if (check_path != path){
    warning("There might be issues related to the path of files.", call. = TRUE, immediate. = FALSE, domain = NULL)
  }else{
    setwd(file.path(path, "data"))
    #allfiles = dir()
    #print(allfiles)
  }  
}

fun_gather_all_data = function(){
  data_files = list.files(pattern="*.xlsx", full.names=F)
  #data_list = lapply(data_files, read_xlsx)
  
  all_data = read_xlsx("_template.xlsx", col_names = T)
  used_files = NULL
  
  for(i in 1:length(data_files)){
    check_fname = substr(data_files[i], 1, 7)#--> read certain file names only
    if (check_fname == "results"){
      used_files = append(used_files, data_files[i])
      next_file = read_xlsx(data_files[i], col_names = T)
      colnames(next_file) = names(all_data)
      all_data = rbind(all_data, next_file) 
    }
  }
  #write_xlsx(all_data, "_result_file.xlsx")
  return(list(all_data, used_files))
}

fun_get_title = function(question){
  quest_title = names(plot_data[question])
  quest_title = (gsub("...", ": ", quest_title, fixed = T))
  quest_title = (gsub(".", " ", quest_title, fixed = T))
  quest_title = (gsub(": : : : :", ":", quest_title, fixed = T))
  return(quest_title)
}




#---------------- end of functions -----------------#
#---------------------------------------------------#

fun_locate_data_folder()
all_data = fun_gather_all_data()

used_files = unlist(all_data[2])
all_data = as.data.frame(all_data[1])
plot_data = all_data[, 8:length(all_data)]



plot_vars = (1:length(plot_data))
plot_vars = c(1:18)
#plot_vars = c(1:3)
doplot = T


#----------------------- ploting -------------------------#
if (doplot == T){
  for (i in plot_vars){
#----------- plots -----------#
no_quest = i
plot_this = plot_data[no_quest]
graftitle = fun_get_title(no_quest)


    if (i == 1){
      plot_this[,1] = as.numeric(substr(plot_this[,1], 1, 4))
      dummy_year = 2024 #--> 2do: use year of timestamp in questionaire
      plot_this[,1] = dummy_year - plot_this[,1]
      plot_this$age_group[plot_this[,1] < 20] = "jünger als 20"
      plot_this$age_group[plot_this[,1] >= 20 & plot_this[,1] < 30] = "20 bis 29"
      plot_this$age_group[plot_this[,1] >= 30 & plot_this[,1] < 40] = "30 bis 39"
      plot_this$age_group[plot_this[,1] >= 40 & plot_this[,1] < 50] = "40 bis 49"
      plot_this$age_group[plot_this[,1] >= 50 & plot_this[,1] < 60] = "50 bis 59"
      plot_this$age_group[plot_this[,1] >= 60] = "60 und älter"
      len_obs = na.omit(c(plot_this[,2]))
      graftitle = paste0("Altersgruppen der Teilnehmer/innen in Jahren zum Zeitpunkt der Befragung (N = ", length(len_obs), ")")
      plot_this = plot_this[2]
      ##------------------------------------------------------------ 2do: als function definieren
      ergebnis_2 = as.data.frame(table(plot_this))
      #-------- plot -------
      require(ggplot2)
      
      p1 = ggplot(ergebnis_2, aes(x=ergebnis_2[, 1], y=Freq)) + 
        geom_bar(stat = "identity", color = "blue", fill="grey",) + 
        geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
        labs(title = paste0(graftitle, "\n")) + 
        ylim(0, max(ergebnis_2$Freq)+round(max(ergebnis_2$Freq*0.25), 0)+1)+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          #axis.line.y = element_line(colour = "black"),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title.x=element_blank(), #remove axis title
          axis.title.y=element_blank(), #remove axis title
          axis.text.y=element_blank(),  #remove axis labels
          #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=.35),
          axis.ticks.x=element_blank(),  #remove axis ticks
          axis.ticks.y=element_blank()  #remove axis ticks
        )
      plot(p1)
      ##------------------------------------------------------------ 2do: als function definieren
    }

    else if (i == 2){
      insert = "../bulk_donut.R"
      if(file.exists(insert)){print(
        paste("inject ", insert))
        source(insert)
        plot(p_insert)
        rm(p_insert)
      }else {print(paste("file", insert, "not found for Column", i))}
    }

    else if (i == 16){
      insert = "../bulk_donut.R"
      if(file.exists(insert)){print(
        paste("inject ", insert))
        source(insert)
        plot(p_insert)
        rm(p_insert)
      }else {print(paste("file", insert, "not found for Column", i))}
    }

    else{
      
      '
      ergebnis = table(plot_this)
      #-------- plot -------
      yAchse = c(0, max(ergebnis) + round(max(ergebnis*0.2), 0))
      if(max(ergebnis) == 1 ){yAchse = c(0, 2)} #y-Achse ggf. auf 2 setzen
      par(mar=c(10, 4, 4, 2) +.1) #Darstellungsbereich für Labels vergrößern
      barplot(ergebnis, width = 0.9, main = strwrap(graftitle, width = 70), cex.main = 1, axes = T, ylim = yAchse, xpd = F, las = 2, cex.names = .8)
      text(ergebnis, labels = c(as.character(ergebnis)), cex = 1, col = "blue", pos = 3) #Datenbeschriftung
      #---------------------
      '
      
      ergebnis_2 = as.data.frame(table(plot_this))
      #-------- plot -------
      require(ggplot2)
      
      p1 = ggplot(ergebnis_2, aes(x=ergebnis_2[, 1], y=Freq)) + 
        geom_bar(stat = "identity", color = "blue", fill="grey",) + 
        geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
        labs(title = paste0(graftitle, "\n")) + 
        ylim(0, max(ergebnis_2$Freq)+round(max(ergebnis_2$Freq*0.25), 0)+1)+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          #axis.line.y = element_line(colour = "black"),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title.x=element_blank(), #remove axis title
          axis.title.y=element_blank(), #remove axis title
          axis.text.y=element_blank(),  #remove axis labels
          #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=.35),
          axis.ticks.x=element_blank(),  #remove axis ticks
          axis.ticks.y=element_blank()  #remove axis ticks
        )
      
      plot(p1)
      #---------------------
    }

}
}

