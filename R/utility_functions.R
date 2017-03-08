getBestPredictions <- function(trobj){
  bt <- trobj$bestTune
  trobj$pred %>%
    filter(ncomp == bt$ncomp, keepX == bt$keepX, keepY == bt$keepY) %>%
    separate(Resample, c("Fold", "Rep"))
}
