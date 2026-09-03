#library(officer)
library(rstudioapi)
library(readxl)
library(writexl)
library(gridExtra)
library(ggplot2)


this_file = rstudioapi::getActiveDocumentContext()$path
path = box::file()
check_path = unlist(strsplit(this_file, split = "/"))
check_path = paste0(check_path[1:length(check_path)-1], collapse="/")

if (check_path != path){
  warning("There might be issues related to the path...", call. = TRUE, immediate. = FALSE, domain = NULL)
}else{
  #setwd(file.path(path, "files"))
  setwd(file.path(path, "data"))
  #allfiles = dir()
  #print(allfiles)
}



# functions ---------------------------------------------------------------


fun_gather_all_data = function(){
  data_files = list.files(pattern="*.xlsx", full.names=F)
  #data_list = lapply(data_files, read_xlsx)
  
  names_all = c("Antwort ID", "Datum Abgeschickt", "Letzte Seite", "Start-Sprache", "Zufallsgeneratorstartwert", "Datum gestartet", "Datum letzte Aktivität", "In welchem Jahr sind Sie geboren?", "Angaben zur Geschlechtsidentität.", "Was ist Ihr höchster Bildungsabschluss?", "ildungsabschluss [Sonstiges]", "Das Angebot war interaktiv gestaltet.", "Der Anteil an Übungen / Interaktivem war angemessen.", "Der Anteil an Inputs / Vorträgen war angemessen.", "Die vermittelten Inhalte sind relevant für meine Arbeit.", "Das Thema ökologische Nachhaltigkeit wurde behandelt.", "Das Thema Gleichstellung der Geschlechter wurde behandelt.", "Die Inhalte waren verständlich aufbereitet.", "Der Aufbau des Angebotes war für mich nachvollziehbar.", "Ich habe Neues dazugelernt.", "Der zeitliche Umfang des Angebots war angemessen.", "Wurden digitale Tools / Hilfsmittel genutzt (z.B. von den Teilnehmenden oder von den Beratenden)?", "Ich habe mich bei der Nutzung der digitalen Tools / Hilfsmittel gut zurechtgefunden.", "Die digitalen Tools / Hilfsmittel wurden sinnvoll eingebunden.", "Würden Sie die Angebote des Zukunftszentrum weiterempfehlen?", "Warum würden Sie das Zukunftszentrum weiterempfehlen?", "Warum würden Sie das Zukunftszentrum nicht weiterempfehlen?", "Gab es Phasen mit selbständigem Lernen/Erarbeiten?", "Ich habe immer verstanden, was in den Selbstlernphasen zu tun war.", "Die Selbstlernphasen wurden sinnvoll eingesetzt.", "Meine Erwartungen an das Angebot wurden erfüllt.", "Was hat dazu geführt, dass Ihre Erwartungen erfüllt wurden?", "Was hat dazu geführt, dass Ihre Erwartungen nicht erfüllt wurden?", "Weitere Unterstützung gewünscht bei: Agiles Arbeiten", "Weitere Unterstützung gewünscht bei: Moderne Personalführung", "Weitere Unterstützung gewünscht bei: Wissensmanagement und digitales Lernen", "Weitere Unterstützung gewünscht bei: Mitbestimmung im Betrieb", "Weitere Unterstützung gewünscht bei: Gesundheit und Resilienz", "Weitere Unterstützung gewünscht bei: Künstliche Intelligenz", "Weitere Unterstützung gewünscht bei: Sichtbarkeit im öffentlichen Raum", "Weitere Unterstützung gewünscht bei: Sonstiges", "Arbeit und Alltag [Ich finde meine Arbeit abwechslungsreich.]", "Arbeit und Alltag [Ich arbeite im Team.]", "Arbeit und Alltag [Ich bekomme Anerkennung für meine Arbeit.]", "Arbeit und Alltag [Ich habe flexible Arbeitszeiten.]", "Arbeit und Alltag [Ich arbeite Vollzeit (35 Stunden oder mehr).]", "Arbeit und Alltag [Ich habe Betreuungspflichten (Kinder / pflegebedürftige Angehörige).]", "Arbeit und Alltag [Ich kann auch von Zuhause aus arbeiten.]", "Arbeit und Alltag [Ich bin in meiner Freizeit ehrenamtlich aktiv.]", "Arbeit und Alltag [Ich bin in meiner Freizeit politisch aktiv.]", "Arbeit und Alltag [Meine Muttersprache ist Deutsch.]", "Name des Unternehmens", "IQK / Beratung [Modul 1 - Digital-Agile Führung]", "IQK / Beratung [Modul 2 - Digital-Agile Kommunikation]", "IQK / Beratung [Modul 3 - Digitalisierung: Mitarbeitende einbinden]", "IQK / Beratung [Modul 4 - Lernkultur und Lerntools]", "IQK / Beratung [Modul 5 - Gesund, motiviert und arbeitsfähig]", "IQK / Beratung [Modul 6 - Sichtbarkeit im digitalen Raum]", "IQK / Beratung [Modul 7 - Einführung neuer Technologien]", "IQK / Beratung [Modul 8 - Datenkompetenz und Daten]", "IQK / Beratung [Modul 9 - KI-Wissen]", "IQK / Beratung [Vertiefte Beratung]", "Mitarbeitendenzahl", "Branche", "von Menschen mit Migrationshintergrund gegründet/geführt", "Von Menschen mit Migrationsgeschichte (1. Generation) gegründet/geführt", "Mehr als 50% der Belegschaft im Unternehmen hat einen Migrationshintergrund (ja/nein).", "Betriebsrat vorhanden? (ja/nein) Wenn ja, wie viele Betriebsratsmitglieder (sofern bekannt)?", "Zeitraum der Durchführung", "Handelt es sich um einen Ausbildungsbetrieb?")
  all_data = data.frame(matrix(ncol = length(names_all), nrow = 0))
  colnames(all_data) = names_all
  
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


all_data = fun_gather_all_data()

used_files = unlist(all_data[2])
all_data = as.data.frame(all_data[1])
plot_data = all_data[, 8:length(all_data)]


plot_vars = (1:length(plot_data))
#plot_vars = c(1:13)
doplot = T



# plots -------------------------------------------------------------------


if (doplot == T){
  
  #--> use PDF:
  pdf("LimesSurvey_alleErgebnisse.pdf", width = 12, height = 8, paper = "a4r")
  par(mfrow = c(4, 1), mar = c(2, 2, 2, 2))
  
  
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
    
    else if (i == 19){
      #ergebnis = as.data.frame(na.omit(plot_data[19]))
      ergebnis = as.data.frame(na.omit(plot_this)) #--> bad plot
      #ergebnis = as.data.frame(plot_this)
      grid.table(ergebnis)
    }
    
    else{
      
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
  
  #--> close PDF
  dev.off()
}

