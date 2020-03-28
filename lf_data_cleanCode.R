dat = read.csv("C:/Users/luiseduardo/OneDrive/Documentos/MScA/0. Side Projects/COVID-19/us_states_daily_measures_and_effects.csv", stringsAsFactors = FALSE)
dat[dat == "Y"] = 1
dat
write.csv(dat,"C:/Users/luiseduardo/OneDrive/Documentos/MScA/0. Side Projects/COVID-19/us_states_daily_measures_and_effects_fixed.csv")
