

corr_data = plot_data
  

corr_data[corr_data == "Ja"] = 1
corr_data[corr_data == "Nein"] = 0
corr_data[corr_data == "Keine Angabe / Weiß nicht"] = NA


#19: Warum würden Sie das Zukunftszentrum weiterempfehlen?
#20: Warum würden Sie das Zukunftszentrum nicht weiterempfehlen?
#34: Haben sich anknüpfende oder neue Themen ergeben, bei denen Sie sich Unterstützung wünschen? [Sonstiges]
#


'

#corr_data = corr_data[-c(1:4)] |> na.omit()

#cor_18x16 = data.frame(plot_data[c(18, 16)]) |> na.omit()
#cor_18x16[cor_18x16 == "Ja"] = 1
#cor_18x16[cor_18x16 == "Nein"] = 0
#cor_18x16[cor_18x16 == "Keine Angabe / Weiß nicht"] = NA


cor_18x16 = data.frame(corr_data[c(18, 16)]) |> na.omit()

#--> https://universeofdatascience.com/how-to-convert-all-columns-of-data-frame-to-numeric-in-r/
cor_18x16 = as.data.frame(apply(cor_18x16, 2, function(x) as.numeric(x)))
str(cor_18x16)

cor(cor_18x16[1], cor_18x16[2], method = "pearson") |> round(2)


'

cor(x1, x2, method = "pearson") |> round(2)