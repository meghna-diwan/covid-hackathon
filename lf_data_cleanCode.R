dat = read.csv("C:/Users/luiseduardo/OneDrive/Documentos/MScA/0. Side Projects/COVID-19/us_states_daily_measures_and_effects.csv")
d = dat[,5]
dat[,5] = NULL
dat[dat == 0] = NA  
dat
d[d == 999] = NA
d
dat["large_gathering_ban"] = d
dat
write.csv(dat,"C:/Users/luiseduardo/OneDrive/Documentos/MScA/0. Side Projects/COVID-19/us_states_daily_measures_and_effects_fixed.csv")
