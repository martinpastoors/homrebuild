#operating models

#operating model
#implementation model
#biology and fishery
#initial numbers
#SRR object, autocorrelation flag, min/max residuals (extreme trim)
#Bio vector years (number and stochastic flag)
#Sel vector years (number and stochastic flag)
#observation model (NA here)

#WGWIDE2022 SS Update assessment, IBPWHM reference points, stochastic bio and selection
OM2.3 <- list("code" = "OM2.3",
              "desc" = "WGWIDE22",
              "IM" = NA,
              "SRR" = "SRR.WG22.SegReg_Blim.exterm", 
              "RecAR" = TRUE, 
              "maxRecRes" = c(3,-3),
              "BioYrs" = c(2012,2021), 
              "BioConst" = FALSE, 
              "SelYrs" = c(2012,2021), 
              "SelConst" = FALSE,
              "Obs" = NA,
              refPts = list("Fpa" = 0.074, 
                            "Flim" = 0.103, 
                            "Fmsy" = 0.074, 
                            "Bpa" = 1168272,
                            "Blim" = 834480, 
                            "MSYBtrigger" = 1168272, 
                            "Bloss" = 761613),
              "pBlim" = 0.05)

#WGWIDE2020 SAM assessment, IBPWHM method for reference points, stochastic bio and selection
OM2.5 <- list("code" = "OM2.5",
              "desc" = "WGWIDE22_sam",
              "IM" = NA,
              "SRR" = "SRR.WG22.SegReg_Blim.exterm", 
              "RecAR" = TRUE, 
              "maxRecRes" = c(3,-3),
              "BioYrs" = c(2012,2021), 
              "BioConst" = FALSE, 
              "SelYrs" = c(2012,2021), 
              "SelConst" = FALSE,
              "Obs" = NA,
              refPts = list("Fpa" = 0.115, 
                            "Flim" = 0.161, 
                            "Fmsy" = 0.115, 
                            "Bpa" = 856540,
                            "Blim" = 611814, 
                            "MSYBtrigger" = 856540, 
                            "Bloss" = 604476),
              "pBlim" = 0.05)

