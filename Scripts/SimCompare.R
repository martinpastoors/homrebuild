#run comparisons

source(file.path(getwd(),"Scripts","setup.R"))

#const F, ICES AR, Double BP
runs2Compare <- paste0("WHOM_SS3_",c("OM2.2_MP5.0_1000_50","OM2.2_MP5.1_1000_50","OM2.2_MP5.2_1000_50"))
for (stat in c("Catch","SSB","Risk3","Risk1","IAV","IAVUpDown")){
  fCompare_runs(runs2Compare = runs2Compare, Res.dir = Res.dir, Plot.dir = Res.dir,
                PerfStat = stat,
                TargetFs = c(0.05,0.075,0.1,0.125,0.15,0.175),
                lStatPer = list('ST'=c(2021,2025),'MT'=c(2026,2030),'LT'=c(2031,2067)),
                Blim = OM2.2$refPts$Blim)}

#const F, ICES AR, Double BP, 50kt min TAC
runs2Compare <- paste0("WHOM_SS3_",c("OM2.2_MP5.01_1000_50","OM2.2_MP5.11_1000_50","OM2.2_MP5.21_1000_50"))
for (stat in c("Catch","SSB","Risk3","Risk1","IAV","IAVUpDown")){
  fCompare_runs(runs2Compare = runs2Compare, Res.dir = Res.dir, Plot.dir = Res.dir,
                PerfStat = stat,
                TargetFs = c(0.05,0.075,0.1,0.125,0.15,0.175),
                lStatPer = list('ST'=c(2021,2025),'MT'=c(2026,2030),'LT'=c(2031,2067)),
                Blim = OM2.2$refPts$Blim)}

#const F, ICES AR, Double BP, 20% IAV
runs2Compare <- paste0("WHOM_SS3_",c("OM2.2_MP5.02_1000_50","OM2.2_MP5.12_1000_50","OM2.2_MP5.22_1000_50"))
for (stat in c("Catch","SSB","Risk3","Risk1","IAV","IAVUpDown")){
  fCompare_runs(runs2Compare = runs2Compare, Res.dir = Res.dir, Plot.dir = Res.dir,
                PerfStat = stat,
                TargetFs = c(0.05,0.075,0.1,0.125,0.15,0.175),
                lStatPer = list('ST'=c(2021,2025),'MT'=c(2026,2030),'LT'=c(2031,2067)),
                Blim = OM2.2$refPts$Blim)}


#const F, ICES AR, Double BP, 20% IAV, above Btrigger only
runs2Compare <- paste0("WHOM_SS3_",c("OM2.2_MP5.03_1000_50","OM2.2_MP5.13_1000_50","OM2.2_MP5.23_1000_50"))
for (stat in c("Catch","SSB","Risk3","Risk1","IAV","IAVUpDown")){
  fCompare_runs(runs2Compare = runs2Compare, Res.dir = Res.dir, Plot.dir = Res.dir,
                PerfStat = stat,
                TargetFs = c(0.05,0.075,0.1,0.125,0.15,0.175),
                lStatPer = list('ST'=c(2021,2025),'MT'=c(2026,2030),'LT'=c(2031,2067)),
                Blim = OM2.2$refPts$Blim)}


