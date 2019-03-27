cardValues <- 
function (cards, worthlessCommons = T, verbose = F) 
{
  commons <- subset(cards, cards$R == "C" & cards$foil == F)
  uncommons <- subset(cards, cards$R == "U" & cards$foil == 
                        F)
  rares <- subset(cards, cards$R == "R" & cards$foil == F)
  mythics <- subset(cards, cards$R == "M" & cards$foil == F)
  if (worthlessCommons == T) {
    commonsP <- 0
  }
  else {
    commonsP <- lapply(Map(rtriangle, 1000, commons$Low, 
                           commons$Med, commons$Low), mean) %>% unlist()
  }
  uncommonsP <- lapply(Map(rtriangle, 1000, uncommons$Low, 
                           uncommons$Med, uncommons$Low), mean) %>% unlist()
  if (nrow(rares) == 0) {
    raresP <- 0
  }
  else {
    raresP <- lapply(Map(rtriangle, 1000, rares$Low, rares$Med, 
                         rares$Med), mean) %>% unlist()
  }
  if (nrow(mythics) == 0) {
    mythicsP <- 0
  }
  else {
    mythicsP <- lapply(Map(rtriangle, 1000, mythics$Low, 
                           mythics$Med, mythics$Med), mean) %>% unlist()
  }
  commonsF <- subset(cards, cards$R == "C" & cards$foil == 
                       T)
  uncommonsF <- subset(cards, cards$R == "U" & cards$foil == 
                         T)
  raresF <- subset(cards, cards$R == "R" & cards$foil == T)
  mythicsF <- subset(cards, cards$R == "M" & cards$foil == 
                       T)
  if (nrow(commonsF) == 0) {
    commonsFP <- 0
  }
  else {
    commonsFP <- lapply(Map(rtriangle, 1000, commonsF$Low, 
                            commonsF$Med, commonsF$Low), mean) %>% unlist()
  }
  if (nrow(uncommonsF) == 0) {
    uncommonsFP <- 0
  }
  else {
    uncommonsFP <- lapply(Map(rtriangle, 1000, uncommonsF$Low, 
                              uncommonsF$Med, uncommonsF$Low), mean) %>% unlist()
  }
  if (nrow(raresF) == 0) {
    raresFP <- 0
  }
  else {
    raresFP <- lapply(Map(rtriangle, 1000, raresF$Low, raresF$Med, 
                          raresF$Low), mean) %>% unlist()
  }
  if (nrow(mythicsF) == 0) {
    mythicsFP <- 0
  }
  else {
    mythicsFP <- lapply(Map(rtriangle, 1000, mythicsF$Med, 
                            mythicsF$High, mythicsF$High), mean) %>% unlist()
  }
  if (verbose == F) {
    res <- sum(commonsP, uncommonsP, raresP, mythicsP, commonsFP, 
               uncommonsFP, raresFP, mythicsFP)
  }
  else {
    resC <- data.frame(commons, TCGdistPrice = commonsP)
    resU <- data.frame(uncommons, TCGdistPrice = uncommonsP)
    if (nrow(rares) > 0) {
      resP <- data.frame(rares, TCGdistPrice = raresP)
    }
    else {
      resP <- NULL
    }
    if (nrow(mythics) > 0) {
      resM <- data.frame(mythics, TCGdistPrice = mythicsP)
    }
    else {
      resM <- NULL
    }
    if (nrow(commonsF) > 0) {
      resFC <- data.frame(commonsF, TCGdistPrice = commonsFP)
    }
    else {
      resFC <- NULL
    }
    if (nrow(uncommonsF) > 0) {
      resFU <- data.frame(uncommonsF, TCGdistPrice = uncommonsFP)
    }
    else {
      resFU <- NULL
    }
    if (nrow(raresF) > 0) {
      resFR <- data.frame(raresF, TCGdistPrice = raresFP)
    }
    else {
      resFR <- NULL
    }
    if (nrow(mythicsF) > 0) {
      resFM <- data.frame(mythicsF, TCGdistPrice = mythicsFP)
    }
    else {
      resFM <- NULL
    }
    res <- rbind(resC, resU, resP, resM, resFC, resFU, resFR, 
                 resFM)
  }
  return(res)
}