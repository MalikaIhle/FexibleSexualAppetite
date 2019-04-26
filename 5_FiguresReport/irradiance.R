#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Plot irradiance light bulb used during training and testing
#  these measurements were taken by Calum in May 2017, using IT 2.5ms, and putting the probe directly under the light (90 degree angle)
#	 Start : 2019 04 03
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table <- read.table("1_RawData/IrradianceLamps.txt", header=TRUE)
head(table)
table <- table[table$nm <700,]

library(ggplot2)
library(here)


setEPS()
pdf(paste(here(), "5_FiguresReport/IrradianceLamps.pdf", sep="/"), height=3, width=6.8)

ggplot(data=table, aes(x=nm, y=intensity, colour = lamp)) +
  scale_y_continuous(name = "Intensity") +
  scale_x_continuous(name = "Wavelength (nm)") +
  scale_color_manual(values = c("training"="orange","testing" ="darkorange"), 
                     labels = c("SoLux MR16 3500K 50W used during training" ,
                                "SoLux PAR38 3500K 90W used during testing")) +
                       
  geom_line() +
 theme_classic() +
  theme (panel.border = element_rect(colour = "black", fill=NA), # ad square box around graph 
         axis.title.x=element_text(size=10),
         axis.title.y=element_text(size=10),
         legend.position=c(0.25,0.85),
         legend.title = element_text(size=rel(0.8)),
         legend.text = element_text(size=rel(0.7)),
         legend.key.size = unit(0.8, 'lines'))+
guides(colour = guide_legend(title = "Light bulbs")) 


dev.off()
