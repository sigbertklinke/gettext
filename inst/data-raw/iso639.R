iso639 <- read.csv("iso639-full.csv", stringsAsFactors = FALSE)
iso639 <- data.frame(code=trimws(c(iso639$alpha3.b, iso639$alpha3.t, iso639$alpha2)),
                     english=trimws(rep(iso639$English, 3)),
                     french=trimws(rep(iso639$French, 3)),
                     stringsAsFactors = FALSE)
iso639 <- iso639[iso639$code!="",]
iso639 <- iso639[!duplicated(iso639),]
pos    <- which(iso639$code=="qaa-qtz")
local  <- expand.grid('q', letters[1:20], letters)
local  <- paste0(local[,1], local[,2], local[,3])
iso639 <- rbind(iso639, data.frame(code=local, 
                                   english=rep(iso639[pos,2], length(local)), 
                                   french=rep(iso639[pos,3], length(local)),
                                   stringsAsFactors = FALSE)) 
iso639 <- iso639[-pos,]
iso639$english <- iconv(iso639$english, to="UTF-8")
iso639$french <- iconv(iso639$french, to="UTF-8")
save(iso639, file='../../data/iso639.rda', version=2)