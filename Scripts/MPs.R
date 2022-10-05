#management procedure (MP) objects

#decision model + constraints
#estimation model

# F_targets     <- seq(0.0, 0.15, by=0.025)
F_targets     <- c(0.001, seq(0.025, 0.15, by=0.025))

Uncertainties <- list("cvF" = 0.22, "phiF" = 0.03, "cvSSB" = 0.36, "phiSSB" = 0.51)
Defaults <- list("cvF" = 0.24, "phiF" = 0.42, "cvSSB" = 0.24, "phiSSB" = 0.42)   # no defaults for SSB, assumed same as F for this
Perfect <- list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0)
Extreme <- list("cvF" = 0.7, "phiF" = 0.7, "cvSSB" = 0.7, "phiSSB" = 0.7)
Extreme.zeroCor <- list("cvF" = 0.7, "phiF" = 0, "cvSSB" = 0.7, "phiSSB" = 0)
  
#baseline, no harvest rule, no IAV control, no minimum TAC, no assessment/advice error
# MP1.0 <- list("code" = "MP1.0",
#               "desc" = "NoHCR_Test",
#               "xlab" = "Base",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

# MP1.01 <- list("code" = "MP1.01",
#               "desc" = "NoHCR_Test",
#               "xlab" = "Base",
#               "HCRName" = "None",
#               "F_target" = c(seq(0.06,0.16,by=0.02)),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))


#minimum TAC test
# MP1.1 <- list("code" = "MP1.1",
#               "desc" = "NoHCR_MinTACTest",
#               "xlab" = "80kt Min",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = 80000,
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

#maximum TAC test
# MP1.2 <- list("code" = "MP1.2",
#               "desc" = "NoHCR_MaxTACTest",
#               "xlab" = "150kt Max",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = 150000,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

#min/max TAC in combination
# MP1.3 <- list("code" = "MP1.3",
#               "desc" = "NoHCR_MinMaxTACTest",
#               "xlab" = "80/150",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = 80000,
#               "maxTAC" = 150000,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

#baseline, no harvest rule, no IAV control, no minimum TAC plus assessment/advice error
# MP1.4 <- list("code" = "MP1.4",
#               "desc" = "NoHCR_ObsErrTest",
#               "xlab" = "Inc Err",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = Uncertainties)

#baseline, no harvest rule, 20% TAC change limitation, no min/max TAC, no assessment/advice error
# MP1.5 <- list("code" = "MP1.5", 
#              "desc" = "NoHCR_20PctIAVTest", 
#              "xlab" = "0.2 IAV",
#              "HCRName" = "None",
#              "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#              "B_trigger" = NA,
#              "minTAC" = NA,
#              "maxTAC" = NA,
#              "TAC_IAV" = c(0.2,0.2),
#              "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

#baseline, no harvest rule, 30% TAC change limitation, no min/max TAC, no assessment/advice error
# MP1.6 <- list("code" = "MP1.6", 
#               "desc" = "NoHCR_30PctIAVTest", 
#               "xlab" = "0.3 IAV",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = c(0.3,0.3),
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

#baseline, no harvest rule, 10% TAC change limitation, no min/max TAC, no assessment/advice error
# MP1.7 <- list("code" = "MP1.7", 
#               "desc" = "NoHCR_10PctIAVTest", 
#               "xlab" = "0.1 IAV",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = c(0.1,0.1),
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

#baseline, no harvest rule, 10%/20% TAC change limitation, no min/max TAC, no assessment/advice error
# MP1.8 <- list("code" = "MP1.8", 
#               "desc" = "NoHCR_10_20PctIAVTest", 
#               "xlab" = "10_20 IAV",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = c(0.1,0.2),
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))
 
#baseline, no harvest rule, TAC change limitation - no limit on increase, 10% limit on decrease, no min/max TAC, no assessment/advice error
# MP1.9 <- list("code" = "MP1.9", 
#               "desc" = "NoHCR_100_10PctIAVTest", 
#               "xlab" = "100_10 IAV",
#               "HCRName" = "None",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = c(1,0.1),   #increase/decrease limits
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

# ==========================================================================================================================================

# #ICES HCR, no IAV control, no minimum TAC, no assessment/advice error
# MP2.0 <- list("code" = "MP2.0", 
#               "desc" = "ICESHCR", 
#               "HCRName" = "ICES",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = "MSYBtrigger",
#               "minTAC" = NA, 
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))
# 
# #num iterations investigation
# MP2.0_10000 <- list("code" = "MP2.0_10000", 
#                   "desc" = "ICESHCR", 
#                   "HCRName" = "ICES",
#                   "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#                   "B_trigger" = "MSYBtrigger",
#                   "minTAC" = NA, 
#                   "maxTAC" = NA,
#                   "TAC_IAV" = NA,
#                   "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

#ICES HCR, no IAV control, no minimum TAC, with assessment/advice error
# MP2.1 <- list("code" = "MP2.1",
#               "desc" = "ICESHCR",
#               "xlab" = "ICES AR",
#               "HCRName" = "ICES",
#               "F_target" = seq(0,0.2,0.025),
#               # "F_target" = c(0,0.05,0.074,0.1,0.115,0.2,0.3),
#               "B_trigger" = "MSYBtrigger",
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0.3, "phiF" = 0.3, "cvSSB" = 0.0, "phiSSB" = 0.0))

#ICES HCR, with IAV control, no minimum TAC, with assessment/advice error
# MP2.2 <- list("code" = "MP2.2",
#               "desc" = "ICESHCR with IAV",
#               "xlab" = "ICES AR with IAV",
#               "HCRName" = "ICES",
#               "F_target" = seq(0,0.2,0.025),
#               # "F_target" = c(0,0.05,0.074,0.1,0.115,0.2,0.3),
#               "B_trigger" = "MSYBtrigger",
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = c(0.20, 0.25),
#               "Obs" = list("cvF" = 0.3, "phiF" = 0.3, "cvSSB" = 0, "phiSSB" = 0))

 
# ==========================================================================================================================================

# #NFD rule No further decline
# MP3.0 <- list("code" = "MP3.0", 
#               "desc" = "NFD", 
#               "HCRName" = "NFD",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = "MSYBtrigger",
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))
# 

# #NFD rule No further decline + IAV constraint
# #issues here - investigate
# MP3.1 <- list("code" = "MP3.1", 
#               "desc" = "NFD_IAV", 
#               "HCRName" = "NFD",
#               "F_target" = c(0,0.05,0.074,0.1,0.108,0.2,0.3),
#               "B_trigger" = "MSYBtrigger",
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = 0.2,
#               "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))


# ===========================================================================
# Constant F, no IAV control, no minimum TAC, with assessment/advice error
# ===========================================================================

# MP5.00 <- list("code" = "MP5.00",
#               "desc" = "ConstF",
#               "xlab" = "Const F",
#               "HCRName" = "None",
#               "F_target" = F_targets,
#               "B_trigger" = NA,
#               "minTAC" = NA,
#               "maxTAC" = NA,
#               "TAC_IAV" = NA,
#               "Obs" = Uncertainties)

# MP5.00.perf <- list("code" = "MP5.00.perf",
#                "desc" = "ConstF",
#                "xlab" = "Const F",
#                "HCRName" = "None",
#                "F_target" = F_targets,
#                "B_trigger" = NA,
#                "minTAC" = NA,
#                "maxTAC" = NA,
#                "TAC_IAV" = NA,
#                "Obs" = Perfect)

# MP5.00.def <- list("code" = "MP5.00.def",
#                     "desc" = "ConstF",
#                     "xlab" = "Const F",
#                     "HCRName" = "None",
#                     "F_target" = F_targets,
#                     "B_trigger" = NA,
#                     "minTAC" = NA,
#                     "maxTAC" = NA,
#                     "TAC_IAV" = NA,
#                     "Obs" = Defaults)

#as 5.0 but with 50kt min TAC
# MP5.01 <- list("code" = "MP5.01",
#                "desc" = "ConstF minTAC",
#                "xlab" = "ConstF minTAC",
#                "HCRName" = "None",
#                "F_target" = F_targets,
#                "B_trigger" = NA,
#                "minTAC" = 50000,
#                "maxTAC" = NA,
#                "TAC_IAV" = NA,
#                "Obs" = Uncertainties)

#as 5.0 but with 20% IAV limit
# MP5.02 <- list("code" = "MP5.02",
#                "desc" = "ConstF IAV always",
#                "xlab" = "ConstF IAV always",
#                "HCRName" = "None",
#                "F_target" = F_targets,
#                "B_trigger" = NA,
#                "minTAC" = NA,
#                "maxTAC" = NA,
#                "TAC_IAV" = c(0.2,0.2),
#                "Obs" = Uncertainties)

#as 5.0 but with 20% IAV limit applied only above B_trigger
# MP5.03 <- list("code" = "MP5.03", "desc" = "ConstF IAV Btrig", "xlab" = "ConstF IAV Btrig", "HCRName" = "None",
#                "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                "TAC_IAV" = c(0.2,0.2), "Obs" = Uncertainties)

# MP5.03.perf <- list("code" = "MP5.03.perf", "desc" = "ConstF IAV Btrig", "xlab" = "ConstF IAV Btrig", "HCRName" = "None",
#                "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                "TAC_IAV" = c(0.2,0.2), "Obs" = Perfect)

# MP5.03.def <- list("code" = "MP5.03.def", "desc" = "ConstF IAV Btrig", "xlab" = "ConstF IAV Btrig", "HCRName" = "None",
#                "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                "TAC_IAV" = c(0.2,0.2), "Obs" = Defaults)

# MP5.03.ext <- list("code" = "MP5.03.ext", "desc" = "ConstF IAV Btrig", "xlab" = "ConstF IAV Btrig", "HCRName" = "None",
#                    "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                    "TAC_IAV" = c(0.2,0.2), "Obs" = Extreme)

# MP5.03.ext0 <- list("code" = "MP5.03.ext0", "desc" = "ConstF IAV Btrig", "xlab" = "ConstF IAV Btrig", "HCRName" = "None",
#                    "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                    "TAC_IAV" = c(0.2,0.2), "Obs" = Extreme.zeroCor)

# ===========================================================================
# ICES HCR, no IAV control, no minimum TAC, with assessment/advice error
# ===========================================================================

# MP5.10 <- list("code" = "MP5.10", "desc" = "ICESHCR", "xlab" = "ICES AR", "HCRName" = "ICES",
#               "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA,"maxTAC" = NA,
#               "TAC_IAV" = NA, "Obs" = Uncertainties)

# MP5.10.perf <- list("code" = "MP5.10.perf", "desc" = "ICESHCR", "xlab" = "ICES AR", "HCRName" = "ICES",
#                "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                "TAC_IAV" = NA, "Obs" = Perfect)

# MP5.10.def <- list("code" = "MP5.10.def", "desc" = "ICESHCR", "xlab" = "ICES AR", "HCRName" = "ICES",
#                     "F_target" = F_targets, "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                     "TAC_IAV" = NA, "Obs" = Defaults)

#as MP5.10 but min TAC=50kt
# MP5.11 <- list("code" = "MP5.11",
#                "desc" = "ICESHCR minTAC",
#                "xlab" = "ICES AR minTAC",
#                "HCRName" = "ICES",
#                "F_target" = F_targets,
#                "B_trigger" = "MSYBtrigger",
#                "minTAC" = 50000,
#                "maxTAC" = NA,
#                "TAC_IAV" = NA,
#                "Obs" = Uncertainties)

#as MP5.10 IAV of 20%
# MP5.12 <- list("code" = "MP5.12",
#                "desc" = "ICESHCR IAV always",
#                "xlab" = "ICES AR IAV always",
#                "HCRName" = "ICES",
#                "F_target" = F_targets,
#                "B_trigger" = "MSYBtrigger",
#                "minTAC" = NA,
#                "maxTAC" = NA,
#                "TAC_IAV" = c(0.2,0.2),
#                "Obs" = Uncertainties)

#as MP5.10 IAV of 20%
# MP5.13 <- list("code" = "MP5.13",
#                "desc" = "ICESHCR IAV above Btrigger",
#                "xlab" = "ICES AR IAV above Btrigger",
#                "HCRName" = "ICES",
#                "F_target" = F_targets,
#                "B_trigger" = "MSYBtrigger",
#                "minTAC" = NA,
#                "maxTAC" = NA,
#                "TAC_IAV" = c(0.2,0.2),
#                "Obs" = Uncertainties)

#as 5.2 but with min TAC=50kt
# MP5.13.def <- list("code" = "MP5.13.def",
#                    "desc" = "ICESHCR IAV above Btrigger with default uncertainties",
#                    "xlab" = "ICES AR IAV above Btrigger",
#                    "HCRName" = "ICES",
#                    "F_target" = F_targets,
#                    "B_trigger" = "MSYBtrigger",
#                    "minTAC" = NA,
#                    "maxTAC" = NA,
#                    "TAC_IAV" = c(0.2,0.2),
#                    "Obs" = Defaults)

#5.13 but zero assessment uncertainty
# MP5.13.perf <- list("code" = "MP5.13.perf",
#                     "desc" = "ICESHCR IAV above Btrigger with perfect assessment",
#                     "xlab" = "ICES AR IAV above Btrigger",
#                     "HCRName" = "ICES",
#                     "F_target" = F_targets,
#                     "B_trigger" = "MSYBtrigger",
#                     "minTAC" = NA,
#                     "maxTAC" = NA,
#                     "TAC_IAV" = c(0.2,0.2),
#                     "Obs" = Perfect)

#5.13 but extreme assessment uncertainty
# MP5.13.ext <- list("code" = "MP5.13.ext", "desc" = "ICESHCR IAV above Btrigger with extreme assessment",
#                     "xlab" = "ICES AR IAV above Btrigger", "HCRName" = "ICES", "F_target" = F_targets,
#                     "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                     "TAC_IAV" = c(0.2,0.2), "Obs" = Extreme)

# MP5.13.ext0 <- list("code" = "MP5.13.ext0", "desc" = "ICESHCR IAV above Btrigger with extreme assessment",
#                    "xlab" = "ICES AR IAV above Btrigger", "HCRName" = "ICES", "F_target" = F_targets,
#                    "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
#                    "TAC_IAV" = c(0.2,0.2), "Obs" = Extreme.zeroCor)

# ===========================================================================
# Double BP
# ===========================================================================

# MP5.20 <- list("code" = "MP5.20", "desc" = "Double BP HCR", "xlab" = "Double BP",
#               "HCRName" = "DoubleBP", "F_target" = F_targets, "B_trigger" = "MSYBtrigger",
#               "minTAC" = NA, "maxTAC" = NA, "TAC_IAV" = NA,
#               "Obs" = Uncertainties)

# MP5.20.perf <- list("code" = "MP5.20.perf", "desc" = "Double BP HCR", "xlab" = "Double BP",
#                "HCRName" = "DoubleBP", "F_target" = F_targets, "B_trigger" = "MSYBtrigger",
#                "minTAC" = NA, "maxTAC" = NA, "TAC_IAV" = NA,
#                "Obs" = Perfect)

# MP5.20.def <- list("code" = "MP5.20.def", "desc" = "Double BP HCR", "xlab" = "Double BP",
#                "HCRName" = "DoubleBP", "F_target" = F_targets, "B_trigger" = "MSYBtrigger",
#                "minTAC" = NA, "maxTAC" = NA, "TAC_IAV" = NA,
#                "Obs" = Defaults)

#as 5.20 but with min TAC=50kt
# MP5.21 <- list("code" = "MP5.21",
#                "desc" = "Double BP HCR",
#                "xlab" = "Double BP",
#                "HCRName" = "DoubleBP",
#                "F_target" = F_targets,
#                "B_trigger" = "MSYBtrigger",
#                "minTAC" = 50000,
#                "maxTAC" = NA,
#                "TAC_IAV" = NA,
#                "Obs" = Uncertainties)

#as 5.2 but with min TAC=50kt
# MP5.22 <- list("code" = "MP5.22",
#                "desc" = "Double BP HCR",
#                "xlab" = "Double BP",
#                "HCRName" = "DoubleBP",
#                "F_target" = F_targets,
#                "B_trigger" = "MSYBtrigger",
#                "minTAC" = NA,
#                "maxTAC" = NA,
#                "TAC_IAV" = c(0.2,0.2),
#                "Obs" = Uncertainties)

#as 5.2 but with min TAC=50kt
MP5.23 <- list("code" = "MP5.23",
               "desc" = "Double BP HCR",
               "xlab" = "Double BP IAVBtrig",
               "HCRName" = "DoubleBP",
               "F_target" = c(F_targets, 0.074),
               "B_trigger" = "MSYBtrigger",
               "minTAC" = NA,
               "maxTAC" = NA,
               "TAC_IAV" = c(0.2,0.2),
               "Obs" = Uncertainties)

#as 5.2 but with min TAC=50kt
MP5.23.def <- list("code" = "MP5.23.def", "desc" = "Double BP HCR with default uncertainties",
               "xlab" = "Double BP IAVBtrig", "HCRName" = "DoubleBP", "F_target" = F_targets,
               "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
               "TAC_IAV" = c(0.2,0.2), "Obs" = Defaults)

#5.23 but zero assessment uncertainty
MP5.23.perf <- list("code" = "MP5.23.perf", "desc" = "Double BP HCR with perfect assessment",
                   "xlab" = "Double BP IAVBtrig", "HCRName" = "DoubleBP", "F_target" = F_targets,
                   "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
                   "TAC_IAV" = c(0.2,0.2), "Obs" = Perfect)

#5.23 but extreme assessment uncertainty
MP5.23.ext <- list("code" = "MP5.23.ext", "desc" = "Double BP HCR with extreme assessment error",
                    "xlab" = "Double BP IAVBtrig", "HCRName" = "DoubleBP", "F_target" = F_targets,
                    "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
                    "TAC_IAV" = c(0.2,0.2), "Obs" = Extreme)

MP5.23.ext0 <- list("code" = "MP5.23.ext0", "desc" = "Double BP HCR with extreme assessment error",
                   "xlab" = "Double BP IAVBtrig", "HCRName" = "DoubleBP", "F_target" = F_targets,
                   "B_trigger" = "MSYBtrigger", "minTAC" = NA, "maxTAC" = NA,
                   "TAC_IAV" = c(0.2,0.2), "Obs" = Extreme.zeroCor)
# ===========================================================================
# Basic Test
# ===========================================================================

# MP99 <- list("code" = "MP99",
#              "desc" = "Test",
#              "xlab" = "Test",
#              "HCRName" = "None",
#              "F_target" = c(0.075),
#              "B_trigger" = NA,
#              "minTAC" = NA,
#              "maxTAC" = NA,
#              "TAC_IAV" = NA,
#              "Obs" = list("cvF" = 0, "phiF" = 0, "cvSSB" = 0, "phiSSB" = 0))

# MP98 <- list("code" = "MP98",
#              "desc" = "ICESHCR",
#              "xlab" = "ICES AR",
#              "HCRName" = "ICES",
#              "F_target" = c(0.1),
#              "B_trigger" = "MSYBtrigger",
#              "minTAC" = NA,
#              "maxTAC" = NA,
#              "TAC_IAV" = c(0.2,0.2),
#              "Obs" = Uncertainties)