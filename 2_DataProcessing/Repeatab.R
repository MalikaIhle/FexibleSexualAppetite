#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	Check repeatab measurement carapace width
#	 Start : 23/06/2017
#	 last modif : 23/06/2017
#	 commit: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

library(tidyr)

tbl <- read.table("repeatab.txt", header=TRUE)
spreadtable <- spread(tbl, key=label,value = length)
subtable <- spreadtable[is.na(spreadtable$a) | is.na(spreadtable$b),]
subtable <- data.frame(ID=subtable$ID, a=subtable$a[!is.na(subtable$a)],b=subtable$b[!is.na(subtable$b)],c=subtable$c[!is.na(subtable$c)])

# same picture, two measurements
cor.test(spreadtable$b, spreadtable$c) # 0.92

# two pictures, same day
cor.test(spreadtable$a, spreadtable$b) # 0.87
cor.test(spreadtable$a, spreadtable$c) # 0.82

# two pictures, different day
cor.test(subtable$a,subtable$b) # 0.84
cor.test(subtable$a,subtable$c) # 0.94



