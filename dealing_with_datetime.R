C09junpos <- read.table("TRXconPOS.txt", header = F, fill = T, stringsAsFactors = F, sep = "\t")
C09junpos[, 22:23] <- lapply(C09junpos[, 22:23], function(x) as.numeric(gsub(",", ".", x)))
#Convert to POSIXlt
C09junpos$V3 <- strptime(C09junpos$V3, "%Y-%m-%d %H:%M:%S")
#Extract Dates
C09junpos$V26 <- as.Date(C09junpos$V3)
#Extract time as character
C09junpos$V27 <- strftime(C09junpos$V3, format = "%H:%M:%S")
