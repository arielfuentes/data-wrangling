C09junpos <- read.table("TRXconPOS.txt", header = F, fill = T, stringsAsFactors = F, sep = "\t")
C09junpos[, 22:23] <- lapply(C09junpos[, 22:23], function(x) as.numeric(gsub(",", ".", x)))
