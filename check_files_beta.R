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
  
  #--> 2do: read names from a file
  names_all = c("Antwort ID", "Datum Abgeschickt", "Letzte Seite", "Start-Sprache", "Zufallsgeneratorstartwert", "Datum gestartet", "Datum letzte Aktivität", "In welchem Jahr sind Sie geboren?", "Angaben zur Geschlechtsidentität.", "Was ist Ihr höchster Bildungsabschluss?", "Bildungsabschluss - Sonstiges", "Das Angebot war interaktiv gestaltet.", "Der Anteil an Übungen / Interaktivem war angemessen.", "Der Anteil an Inputs / Vorträgen war angemessen.", "Die vermittelten Inhalte sind relevant für meine Arbeit.", "Das Thema ökologische Nachhaltigkeit wurde behandelt.", "Das Thema Gleichstellung der Geschlechter wurde behandelt.", "Die Inhalte waren verständlich aufbereitet.", "Der Aufbau des Angebotes war für mich nachvollziehbar.", "Ich habe Neues dazugelernt.", "Der zeitliche Umfang des Angebots war angemessen.", "Wurden digitale Tools / Hilfsmittel genutzt (z.B. von den Teilnehmenden oder von den Beratenden)?", "Ich habe mich bei der Nutzung der digitalen Tools / Hilfsmittel gut zurechtgefunden.", "Die digitalen Tools / Hilfsmittel wurden sinnvoll eingebunden.", "Würden Sie die Angebote des Zukunftszentrum weiterempfehlen?", "Warum würden Sie das Zukunftszentrum weiterempfehlen?", "Warum würden Sie das Zukunftszentrum nicht weiterempfehlen?", "Gab es Phasen mit selbständigem Lernen/Erarbeiten?", "Ich habe immer verstanden, was in den Selbstlernphasen zu tun war.", "Die Selbstlernphasen wurden sinnvoll eingesetzt.", "Meine Erwartungen an das Angebot wurden erfüllt.", "Was hat dazu geführt, dass Ihre Erwartungen erfüllt wurden?", "Was hat dazu geführt, dass Ihre Erwartungen nicht erfüllt wurden?", "Weitere Unterstützung gewünscht bei: Agiles Arbeiten", "Weitere Unterstützung gewünscht bei: Moderne Personalführung", "Weitere Unterstützung gewünscht bei: Wissensmanagement und digitales Lernen", "Weitere Unterstützung gewünscht bei: Mitbestimmung im Betrieb", "Weitere Unterstützung gewünscht bei: Gesundheit und Resilienz", "Weitere Unterstützung gewünscht bei: Künstliche Intelligenz", "Weitere Unterstützung gewünscht bei: Sichtbarkeit im öffentlichen Raum", "Weitere Unterstützung gewünscht bei: Sonstiges", "Arbeit und Alltag:\nIch finde meine Arbeit abwechslungsreich.", "Arbeit und Alltag:\nIch arbeite im Team.", "Arbeit und Alltag:\nIch bekomme Anerkennung für meine Arbeit.", "Arbeit und Alltag:\nIch habe flexible Arbeitszeiten.", "Arbeit und Alltag:\nIch arbeite Vollzeit (35 Stunden oder mehr).", "Arbeit und Alltag:\nIch habe Betreuungspflichten (Kinder / pflegebedürftige Angehörige).", "Arbeit und Alltag:Ich kann auch von Zuhause aus arbeiten.", "Arbeit und Alltag:\nIch bin in meiner Freizeit ehrenamtlich aktiv.", "Arbeit und Alltag:\nIch bin in meiner Freizeit politisch aktiv.", "Arbeit und Alltag:\nMeine Muttersprache ist Deutsch.", "Name des Unternehmens", "IQK / Beratung [Modul 1 - Digital-Agile Führung]", "IQK / Beratung [Modul 2 - Digital-Agile Kommunikation]", "IQK / Beratung [Modul 3 - Digitalisierung: Mitarbeitende einbinden]", "IQK / Beratung [Modul 4 - Lernkultur und Lerntools]", "IQK / Beratung [Modul 5 - Gesund, motiviert und arbeitsfähig]", "IQK / Beratung [Modul 6 - Sichtbarkeit im digitalen Raum]", "IQK / Beratung [Modul 7 - Einführung neuer Technologien]", "IQK / Beratung [Modul 8 - Datenkompetenz und Daten]", "IQK / Beratung [Modul 9 - KI-Wissen]", "IQK / Beratung [Vertiefte Beratung]", "Mitarbeitendenzahl", "Branche", "von Menschen mit Migrationshintergrund gegründet/geführt", "Von Menschen mit Migrationsgeschichte (1. Generation) gegründet/geführt", "Mehr als 50% der Belegschaft im Unternehmen hat einen Migrationshintergrund (ja/nein).", "Betriebsrat vorhanden? (ja/nein) Wenn ja, wie viele Betriebsratsmitglieder (sofern bekannt)?", "Zeitraum der Durchführung", "Handelt es sich um einen Ausbildungsbetrieb?")
  all_data = data.frame(matrix(ncol = length(names_all), nrow = 0))
  colnames(all_data) = names_all
  
  used_files = NULL

  for(i in 1:length(data_files)){
    check_fname = substr(data_files[i], 1, 7)#--> read certain file names only
    if (check_fname == "results"){
      used_files = append(used_files, data_files[i])
      next_file = read_xlsx(data_files[i], col_names = T)
      colnames(next_file) = names(all_data)
      
      if (ncol(next_file) < 70){
        print(data_files[i])
        print(c("ncol: ", ncol(next_file)))
        print("-------------------")        
      }

      
      #all_data = rbind(all_data, next_file)
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



