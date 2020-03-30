library(tidyverse)
library(anytime)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)
`%nin%` = Negate(`%in%`)

# Resctructuce data to include all dates for all states, filling missing with 0 new cases and grouping cities into their respective states

dat = read.csv("C:/Users/luiseduardo/OneDrive/Documentos/MScA/0. Side Projects/COVID-19/covid-hackathon/us_covid19_w_measures_and_risk_pop.csv")
dat["X"] = NULL
dat["Date"] = lapply(dat["Date"],anytime)
dat["Last.Update"] = lapply(dat["Last.Update"],anytime)
dat = dat[!grepl("Una", dat$State),]

dat = dat %>% arrange(Province.State, Last.Update)

dat["rm"] = 0
err = 1
while (err > 0){
  err = 0
  for (i in c(1:nrow(dat))) {
    if (i == 1) {
      next
    } else if ((dat[i,"Last.Update"] == dat[i-1,"Last.Update"]) & (dat[i,"Province.State"] == dat[i-1,"Province.State"])){
      dat[i,"rm"] = 1
      print("caught")
      err = err + 1
    }
  }
  if (sum(dat$rm) > 0){
    dat = dat[!grepl(1, dat$rm),]
  }
}

dat = dat %>% group_by(Province.State, Date) %>% filter(Last.Update == max(Last.Update))

dat["Province.State"] = NULL
dat["Country.Region"] = NULL
dat["rm"] = NULL
dat[,c(8,9,10,11,12,13,14,15,16,17,18,19,20,21)] = lapply(dat[,c(8,9,10,11,12,13,14,15,16,17,18,19,20,21)], as.factor)
dat[,c(22,23,24,25,26,29)] = lapply(dat[,c(22,23,24,25,26,29)], as.numeric)

dt <- as.data.table(dat)
bys = names(dt)
bys = bys[bys %nin% c("Confirmed","Deaths","Recovered", "Latitude", "Longitude", "Last.Update")]
dt = dt[,.(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered)), by = bys]
dt = na.omit(dt, cols = "Deaths")
dat = as.data.frame(dt)

dat = complete(dat,State,Date)
nam = names(dat)
nam = nam[nam %nin% c("State", "Confirmed","Deaths","Recovered")]
dat = dat %>%
  group_by(State) %>%
  fill(nam, .direction = "down")

z_fac = as.factor("0")

dat <- dat %>%
  mutate(Free.Treatment = coalesce(Free.Treatment, z_fac),
         Early.RX.Refills = coalesce(Early.RX.Refills, z_fac),
         SEP = coalesce(SEP, z_fac),
         Waiver.1135 = coalesce(Waiver.1135, z_fac),
         Paid.Sick.Leave = coalesce(Paid.Sick.Leave, z_fac),
         stay_at_home = coalesce(stay_at_home, z_fac),
         non_essential_business_closure = coalesce(non_essential_business_closure, z_fac),
         large_gathering_ban = coalesce(large_gathering_ban, as.factor("999")),
         school_closure = coalesce(school_closure, z_fac),
         bar_restaurant_limits = coalesce(bar_restaurant_limits, z_fac),
         primary_election_postponed = coalesce(primary_election_postponed, z_fac),
         emergency_declaration = coalesce(emergency_declaration, z_fac),
         Confirmed = coalesce(Confirmed, 0),
         Deaths = coalesce(Deaths, 0),
         Recovered = coalesce(Recovered, 0))

dat = dat %>%
  group_by(State) %>%
  fill(c("risk_under_60","risk_over_60","adult_under_60","adult_over_60","hospital_beds","beds_per_thousand","total_chc","total_chc_delivery_sites","State_abb"),.direction = "downup")

dat[,"large_gathering_ban"] = as.numeric(as.character(dat$large_gathering_ban))

write.csv(dat,"C:/Users/luiseduardo/OneDrive/Documentos/MScA/0. Side Projects/COVID-19/covid-hackathon/us_covid19_w_measures_and_risk_pop_lfFix.csv")

states_daily = read.csv("C:/Users/luiseduardo/OneDrive/Documentos/MScA/0. Side Projects/COVID-19/covid-hackathon/us_covid19_w_measures_and_risk_pop_lfFix.csv")

# Create columns for future confirmed cases on each day

dt <- as.data.table(states_daily)
setkey(dt, State, Date)
dt[, new_confirmed := Confirmed]
dt[, new_confirmed_next_day := shift(new_confirmed, n = 1, type = "lead"), by = State]
dt[, new_confirmed_next_2days := shift(new_confirmed, n = 2, type = "lead"), by = State]
dt[, new_confirmed_next_3days := shift(new_confirmed, n = 3, type = "lead"), by = State]
dt[, new_confirmed_next_4days := shift(new_confirmed, n = 4, type = "lead"), by = State]
dt[, new_confirmed_next_7days := shift(new_confirmed, n = 7, type = "lead"), by = State]

states_daily = as.data.frame(dt)

st_daily = states_daily[states_daily["State"] == "Alabama",]

ggplot(data = st_daily, aes(Date, new_confirmed)) +
  geom_bar(stat="identity")

library(faraway)
library(MASS)
library(lmtest)
library(fitdistrplus)

# Create list of exponential fits for all states and measures, before and after, using exponential curve

states = unique(states_daily$State)
states = states[states != "Unassigned Location"]
states_results = list()
p = 1
for (j in states){
  st_daily = states_daily[states_daily["State"] == j,]
  effects_list = list()
  k = 1
  for (i in c(5:11,13:15)){
    if (sum(as.numeric(as.character(st_daily[,i]))) == 0){
      effects_list[[k]] = NA
      k = k+1
      next
    } else {
    before = st_daily[st_daily[,i] == 0,]
    if (sum(before$Confirmed) == 0){
      effects_list[[k]] = NA
      k = k + 1
      next
    } else {
    before_distr = fitdist(before$Confirmed,"exp")
    before_rate = before_distr$estimate
    before_aic = before_distr$aic
    before_test = ks.test(before$Confirmed,"pexp",rate=before_distr$estimate[1])
    after = st_daily[st_daily[,i] == 1,]
    tryCatch(
      expr = {
        after_distr = fitdist(after$Confirmed,"exp")
        after_rate = after_distr$estimate
        after_aic = after_distr$aic
        after_test = ks.test(after$Confirmed,"pexp",rate=after_distr$estimate[1])  
      },
      error = function(e){
        after_distr = "unable to calculate"
        after_rate = "unable to calculate"
        after_aic = "unable to calculate"
        after_test = "unable to calculate"
      }
    )
    effects_list[[k]] = list(before_distr = before_distr,
                           before_rate = before_rate,
                           before_aic = before_aic,
                           before_test = before_test,
                           after_distr = after_distr,
                           after_rate = after_rate,
                           after_aic = after_aic,
                           after_test = after_test)
    k = k + 1
    }
    }
  }
  states_results[[p]] = effects_list
  p = p + 1
  }

names(states_results) = states
for (i in names(states_results)){
  names(states_results[[i]]) = names(st_daily[,c(5:11,13:15)])
}
# Create list of exponential rates for good fits before and after (10% cutoff)

rates = list()
for (i in names(states_results)){
  for (j in names(states_results[[i]])){
    if (j %nin% names(rates)) {
      rates[[j]] = list(before = c(), after = c())
    }
    if (is.na(states_results[[i]][[j]])){
      next
    } else {
      if ((states_results[[i]][[j]]$before_test$p.value < 0.1) & (states_results[[i]][[j]]$after_test$p.value < 0.1)){
        rates[[j]]$before = append(rates[[j]]$before,states_results[[i]][[j]]$before_rate[[1]])
        rates[[j]]$after = append(rates[[j]]$after,states_results[[i]][[j]]$after_rate[[1]])
      }
    }
  }
}

# Same list but cutoff only applies to before (since we don't have much data for "after" casese yet)

rates_before_cutoff = list()
for (i in names(states_results)){
  for (j in names(states_results[[i]])){
    if (j %nin% names(rates_before_cutoff)) {
      rates_before_cutoff[[j]] = list(before = c(), after = c())
    }
    if (is.na(states_results[[i]][[j]])){
      next
    } else {
      if ((states_results[[i]][[j]]$before_test$p.value < 0.1) & (states_results[[i]][[j]]$after_test$p.value < 0.2)){
        rates_before_cutoff[[j]]$before = append(rates_before_cutoff[[j]]$before,states_results[[i]][[j]]$before_rate[[1]])
        rates_before_cutoff[[j]]$after = append(rates_before_cutoff[[j]]$after,states_results[[i]][[j]]$after_rate[[1]])
      }
    }
  }
}
