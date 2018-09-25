# Custom-line-and-bar-plots-on-R


##
#                        Plots RMSD 
##prepared by arnold
#Create csv files containing 2nd column of rmsd.xvg : Bash: cat 70x_366_run3_rmsd.xvg | awk '{print $2}' > rmsd_70x366_3.csv
#
# 
rm(list=ls())


setwd('/home/path/to/current/working/directory/')

#**Import data
options(stringsAsFactors=FALSE)

###70x_data####
x_apo_1 = read.table('/home/path/to/data/directory/rmsd_70xapo3.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE) 
x_apo_2 = read.table('/home/path/to/data/directory/rmsd_70xapo4.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE) 
x_366_1 = read.table('/home/path/to/data/directory/rmsd_70x366_2.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE)
x_366_2 = read.table('/home/path/to/data/directory/rmsd_70x366_3.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE)


#**frame imported data
x_run1_frame = data.frame(x_apo_1 = x_apo_1$V1, 
                        x_366_1 = x_366_1$V1)

x_run2_frame = data.frame(x_apo_2 = x_apo_2$V1, 
                          x_366_2 = x_366_2$V1)



############################################################

#RMSD PLOTS
png(filename ="landscape_rmsd_graph.png",width = 27.5, height = 16.19, units = 'cm', res=300)
#png(filename ="portrait_rmsd_graph.png",width = 17.78, height = 9.05, units = 'cm', res=300)


op = par(mfrow=c(4,3), mgp = c(0.4, 0.35, 0), oma = c(3,3,2.0,2.0) + 0.1 ) 

#mfrow=c(4,3) > divide worksheet: 4 rows, 3 columns (12 graphs)
#mgp = adjust space between tick labels and axis #Not effective with multiple plots
#oma = create room/rows of text to add at (bottom  margin,left  margin, top  margin, right  margin)


#****** 366_70x

par(family="Times")     #change font: others calibri, etc
par(mar = c(0.8, 1.2, 0.0, 0.5) + 0.2)       #Add room for rotated labels 6=bottom axis, 4=left axis, 3=top, 1=right axis

at_tick <- seq(1,(length(x_run1_frame$x_apo_1) + 1),10000)   # create positions for tick marks, one more than number of bars
at_minr <- seq(5000,(length(x_run1_frame$x_apo_1) + 1),10000)   #vector to which minor ticks should be added

plot(x_run1_frame$x_apo_1,
     type = "l",
     #main="Gbind per-residue contribution",
     ylab = "",
     xlab = "",
     #cex.lab=1.5, #size of x and y labels
     #axisnames = FALSE,
     #space = 0,                                           #No spaces in between bars
     axes = FALSE,                                        #get rid of axes
     ylim=c(0,3),                                       #y limits
     xaxs = "i",yaxs = "i",                            #both axes should start at zero
     col = "black",
     lwd=2.0)                                       #line width



#add other lines to graph
lines(x_run2_frame$x_apo_2, type = "l", col = "green4", lwd=2.0, xaxs = "i",yaxs = "i") #type = line graph
lines(x_run1_frame$x_366_1, type = "l", col = " blue", lwd=2.0, xaxs = "i",yaxs = "i") 
lines(x_run2_frame$x_366_2, type = "l", col = "magenta", lwd=2.0, xaxs = "i",yaxs = "i") 


  
#Insert y label closer to axis
title(ylab="RMSD (nm)", line=2.5, cex.lab=1.5,font.lab=2) #line = spacing between axis and text/axis label; cex.lab - size of text; font.lab = 2 (use bold)
title(xlab = "Time (ns)", line=1.5, cex.lab=1.5, font.lab=2)  


#Add custom axes to figure
axis(side=2, at=seq(0.0,3.0, by=0.5),lwd=2.5, pos = 0.0, las=2, tck = 0.03, font = 2, cex.axis=1.2) #side 2(left y-axis), 
                                                                                                    #at= sequence of number labels from 0-3, tick breaks every 0.5
                                                                                                    #lwd= axis line width 
                                                                                                    #pos=0. position y axis at x=0
                                                                                                    #las=2 labels are perpendicular,
                                                                                                    #cex.axis = size of numeric tick labels
                                                                                                    #tck =  length of ticks
rug(x = seq(0.25,2.75, by = 0.5), tck = 0.015,lwd = 2.5,  side = 2)         #minor ticks
axis(side=1, at = at_tick - 1, lwd=2.5,labels = FALSE, tck = 0.03, font = 2)       #side 1(bottom x-axis) add x-axis with offset positions(at_tick), with ticks, but without labels
rug(x = at_minr, tck = 0.015,lwd = 2.5,  side = 1)                 #minor ticks


#axis(side=4, at=seq(0.0,3.0, by=0.5),lwd=2.5, las=2,labels = FALSE, tck = 0.03, font = 2)    #side 4 (right y-axis)
#rug(x = seq(0.25,2.75, by = 0.5), tck = 0.015,lwd = 2.5,  side = 4)                          #minor ticks
#axis(side=3, at = at_tick - 1, lwd=2.5,labels = FALSE, tck = 0.03, font = 2)                 #side 3 (top x-axis), #tck = ticks inside and length = 0.035
#rug(x = at_minr, tck = 0.015,lwd = 2.5,  side = 3)                                           #minor ticks



#Paste x_axis labels
text(at_tick,
     par("usr")[3]-0.2,             #3 = specifies on which x-axis to place labels: on lower axis at ylim, 0.70= space between tickmarks and labels
     #srt = 90,                       #rotate 90 degrees
     adj= 0.5,                         #Position labels, inside=-1
     xpd = TRUE,                       #Outside graph
     labels = paste(0+ seq(0,100,20)), #paste numerical labels increamenting by 20
     cex=1.2,                        #size of labels
     font = 2,                       #make labels bold
     lwd=5.0)

#Insert legend
lgnd <-legend(-53500,3.05, # places a legend at -53500 (x) and 3.05 (y),
              c( "X-apo_Run1", "X-apo_Run2", "X-complex1", "X-complex2", "X-complex3"),# legend labels
              cex=0.95, #font size
              y.intersp=0.75, #horizontal space between text
              x.intersp=0.4,
              ncol=5,    #Number of columns in your legend
              lty=c(1,1), # gives the legend appropriate symbols (lines)
              lwd=c(4,4),col=c("black", " blue","blue", "green4", "magenta3"),# gives the legend lines their width and the correct color 
              xpd=NA, #outside box
              text.font = 2,#bold
              text.width = 15500, #moves the columns closer together
              bty = "o",  #add the box around the legend : "n" for no box
              box.lwd= 1.5) #box line width


#using legend function, Insert title / label your graph
lgnd <-legend(15000,4.06,
              c( "PfHsp70-x"),
              cex=2.1, #font size
              xpd=NA, #outside box
              text.font = 2,#bold
              bty = "n")  #do not add the box around the legend


#To label graph on the right side using ligand name:
#mtext("Ligand", side=4, line=1.35,cex = 1.3, xpd = NA, font = 2)

dev.off()







#****************************************************************************************


##
###                        Plots RMSF
##prepared by arnold
##Create csv files containing 2nd column of rmsf.xvg : Bash: cat 70x_366_run3_rmsf.xvg | awk '{print $2}' > rmsf_70x366_3.csv


rm(list=ls())

setwd('/home/path/to/current/working/directory/')


options(stringsAsFactors=FALSE)
###PfHsp70-x####
x_apo_1 = read.table('/home/path/to/data/directory/rmsf_70xapo3.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE) 
x_apo_2 = read.table('/home/path/to/data/directory/rmsf_70xapo4.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE) 
x_366_1 = read.table('/home/path/to/data/directory/rmsf_70x366_2.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE)
x_366_2 = read.table('/home/path/to/data/directory/rmsf_70x366_3.csv',sep=',', header = FALSE, quote="'", stringsAsFactors=FALSE, fill=TRUE)


#**frame imported data
x_run1_frame = data.frame(x_apo_1 = x_apo_1$V1, 
                          x_366_1 = x_366_1$V1)
                        

#Duplicates#
x_run2_frame = data.frame(x_apo_2 = x_apo_2$V1, 
                          x_366_2 = x_366_2$V1)

############################################################


#rmsf PLOTS
png(filename ="landscape_rmsf_graph.png",width = 27.5, height = 16.19, units = 'cm', res=300)
#png(filename ="portrait_rmsf_graph.png",width = 17.78, height = 9.05, units = 'cm', res=300)

op = par(mfrow=c(4,3), mgp = c(0.4, 0.35, 0), oma = c(3,3,2.0,2.0) + 0.1 ) 


#****** 366_x

par(family="Times")     #change font
par(mar = c(0.8, 1.2, 0.0, 0.5) + 0.2)       #Add room for rotated labels 6=bottom axis, 4=left axis, 3=top, 1=right axis
at_tick <- seq(1,(length(x_run1_frame$x_apo_1) + 1),50)   # create positions for tick marks, one more than number of bars
at_minr <- seq(25,(length(x_run1_frame$x_apo_1) + 1),50)   #vector to which minor ticks should be added


plot(x_run1_frame$x_apo_1,
     type = "l",
     #main="Gbind per-residue contribution",
     ylab = "",
     xlab = "",
     #cex.lab=1.5, #size of x and y labels
     #axisnames = FALSE,
     #space = 0,                                           #No spaces in between bars
     axes = FALSE,                                        #get rid of axes
     ylim=c(0,3),                                     #y limits
     xaxs = "i",yaxs = "i",                               #both axes should start at zero
     #xlim = c(1,600+max(e_frame$Contribution_energy)),
     col = "black",
     lwd=2.0)   

lines(x_run2_frame$x_apo_2, type = "l", col = "green4", lwd=2.0, xaxs = "i",yaxs = "i") 
lines(x_run1_frame$x_366_1, type = "l", col = " blue", lwd=2.0, xaxs = "i",yaxs = "i") 
lines(x_run2_frame$x_366_2, type = "l", col = "magenta", lwd=2.0, xaxs = "i",yaxs = "i") 


#Add axes to figure
axis(side=2, at=seq(0.0,3.0, by=0.5),lwd=2.2, pos = 0.0, las=2, tck = 0.03, font = 2, cex.axis=1.2) 
rug(x = seq(0.25,2.75, by = 0.5), tck = 0.015,lwd=2.2,  side = 2) 
axis(side=1, at = at_tick - 1, lwd=2.2,labels = FALSE, tck = 0.03, font = 2)      
rug(x = at_minr, tck = 0.015,lwd=2.2,  side = 1)

# axis(side=4, at=seq(0.0,3.0, by=0.5),lwd=2.2, las=2,labels = FALSE, tck = 0.03, font = 2) 
# rug(x = seq(0.25,2.75, by = 0.5), tck = 0.015,lwd = 2.5,  side = 4)
# axis(side=3, at = at_tick - 1, lwd=2.5,labels = FALSE, tck = 0.03, font = 2) 
# rug(x = at_minr, tck = 0.015,lwd = 2.5,  side = 3)


#Paste labels 
text(at_tick, 
     par("usr")[3]-0.2,         
     #srt = 90,                 
     adj= 0.5,                 
     xpd = TRUE,
     labels = paste(0 + seq(0, (length(x_run1_frame$x_apo_1)), 50)), 
     cex=1.2,                     
     font = 2,                     
     lwd=5.0)                       


    
    #Add line partitions and text to indicate domain names
    abline(v=378, col = "black", lty = 5,  lwd=2.2) 
    text(195,1.5,labels ="NBD",pos=3, cex=1.3, font = 2 )   
    
    abline(v=392, col = "black", lty = 5,  lwd=2.2) 
    text(385,1.7,labels ="Linker",pos=3, cex=1.3, font = 2 )
    
    abline(v=505, col = "black", lty = 5,  lwd=2.2) 
    text(450,1.35,labels ="B-SBD",pos=3, cex=1.3, font = 2 )
    
    #abline(v=510, col = "gray60", lty = 5,  lwd=1.8) 
    text(565,1.5,labels ="A-LID",pos=3, cex=1.3, font = 2 )

lgnd <-legend(-550,3.1, # places a legend at the appropriate place c(“Health”,”Defense”),
              c("X-apo_Run1", "X-apo_Run2", "X-complex1", "X-complex2", "X-complex3"),# puts text in the legend
              cex=1.0, #font size
              y.intersp=0.75, #horizontal space between text
              x.intersp=0.4,
              ncol=5,
              lty=c(1,1), # gives the legend appropriate symbols (lines)
              lwd=c(4,4),col=c("black","red","blue", "green4", "magenta3"),# gives the legend lines their width and the correct color and width
              xpd=NA, #outside box
              text.font = 2,#bold
              text.width = 160, #moves the columns closer together
              bty = "o",  #add the box around the legend]
              box.lwd= 1.5) #box line width

#To label graph on the right side using ligand name:
#mtext("Ligand", side=4, line=1.35,cex = 1.3, xpd = NA, font = 2)

dev.off()





#***********************************************************************************

##Per residue energy contribution Barplots
#prepared by Arnold

rm(list=ls())
#install.packages("gplots")
library(gplots)


#getting the directory all set up
setwd('/home/path/to/current/working/directory/')
#open energyMapIn.dat, save as csv sep by ','. Open .csv add column labels Residue and Contribution_energy
#Retrieve collumn 1 of residues conjoined to their respective numbering from contrib_energy.dat
#Paste on Residues collumn to replace the bare numbers
# enery_mapin looks:      Residue  Contribution_energy
#                         ALA1     0.7893
#                         PRO2     -5.7753



#importing data
options(stringsAsFactors=FALSE)
h_comp1 = read.table('/home/path/to/data/directory/energyMapIn.csv',sep=',', header = TRUE, quote="'", stringsAsFactors=FALSE, fill=TRUE) 

#creating the data frame
h_bfe = data.frame(Residue=h_comp1$Residues, 
                   h_comp1 = h_comp1$Contribution_energy,
                   h_comp2 = h_comp2$Contribution_energy,
                   h_endocomp1 = h_endocomp1$Contribution_energy,
                   h_endocomp2 = h_endocomp2$Contribution_energy) 

#Plotting
png(filename ="png_fbenergy_combi_Hs_.png",width = 17.78, height = 9.0, units = 'cm', res=300)
#tiff(filename ="tiff_fbenergy_combi_Hs_.tiff",width = 17.78, height = 9.0, units = 'cm', res=300)

op = par(mfrow=c(2,2), mgp = c(0.2, 0.2, 0), oma = c(1.1,1.7,0.2,0) + 0.1 ) #2x2 plotting area

par(mar = c(0.8, 0.45, 0.7, 0.3) + 0.2)       #Add room for rotated labels 6=bottom axis, 4=left axis, 3=top, 1=right axis
at_tick <- seq(1,(length(h_bfe$h_comp1) + 1),50)   # create positions for tick marks, one more than number of bars
at_minr <- seq(25,(length(h_bfe$h_comp1) + 1),50)   #vector to which minor ticks should be added

par(family="Times")     #change font
barplot2(h_bfe$h_comp1,
         #main="Gbind per-residue contribution", #Exclude header 
         ylab = "",
         xlab = "",
         #cex.lab=1.6, #size of x and y labels
         axisnames = FALSE,
         space = 0,                                           #No spaces in between bars
         axes = FALSE,                                        #get rid of axes
         ylim=c(-12,12),                                      #y limits
         xaxs = "i",yaxs = "i",                              #both axes should start at zero
         #width =  c(1,3),                                    #Bar width, works well with xlim
         #xlim = c(1,600+max(e_frame$Contribution_energy)),
         col = "black")                                   #bars with different colors
                 
        #Insert y label closer to axis
        #title(ylab="Total binding energy (kJ/mol)", line=2.5, cex.lab=1.5, font.lab=2)
        #title(xlab = "Residues", line=2.5, cex.lab=1.5, font.lab=2)  


         #Add axes to figure
         axis(side=2, at=seq(-12,12, by=4),lwd=1.8, pos = 0.0, las=2, tck = 0.03, font=2, cex.axis=0.8) #customise y-axis, limits -20, 20, tick breaks every 5, pos=1. position y axis at x=0, las=2 labels are perpendicular
         rug(x = seq(-10,10, by = 4), tck = 0.015,lwd = 1.8,  side = 2) #minor ticks
         axis(side=1, at = at_tick - 1, lwd=1.8,labels = FALSE, tck = 0.03)       #add x-axis with offset positions, with ticks, but without labels, lwd=bold.
         rug(x = at_minr-1, tck = 0.015,lwd = 1.8,  side = 1)
         
         
         axis(side=4, at=seq(-12,12, by=4),lwd=1.8, las=2,labels = FALSE, tck = 0.03) #customise y-axis, limits -20, 20, tick breaks every 5, pos=1. position y axis at x=0, las=2 labels are perpendicular
         rug(x = seq(-10,10, by = 4), tck = 0.015,lwd = 1.8,  side = 4)
         axis(side=3, at = at_tick - 1, lwd=1.8,labels = FALSE, tck = 0.03) #tck = ticks inside and length = 0.035
         rug(x = at_minr - 1, tck = 0.015,lwd = 1.8,  side = 3)
         
         #box(lwd=2.0)                                                 #lwd increase font size of box frame
        
         #Paste labels 
         text(at_tick, 
             par("usr")[3]-1.20,             #3 = specifies on which x-axis to place labels: on lower axis at ylim, 0.70= space between tickmarks and labels
             #srt = 90,                       #rotate 90 degrees
             adj= 0.5,                         #Position labels, inside=-1
             xpd = TRUE,
             labels = paste(0 + seq(0, (length(h_bfe$h_comp1)), 50)), #paste numerical labels
             cex=0.8,                        #size of labels
             lwd=5.0, font = 2)                        #make labels bold
         
         #insert labels to bar peaks
         text(h_bfe$h_comp1, labels = ifelse(h_bfe$h_comp1 > 5, h_bfe$Residue, NA),cex=0.7,col='black',adj = c(0,-0.3), lwd=1.0, font = 2)# font = 2 bold #not sure of this but it works. 5=cutoff
         text(h_bfe$h_comp1, labels = ifelse(h_bfe$h_comp1 < -5, h_bfe$Residue, NA),cex=0.7, col='black',adj = c(0,1.0), lwd=1.0,   font = 2) #cex = textsize, adj=indent label abit
         
         #Overlay rectangular shades on regions showing substantial contributions
         rect(165,par("usr")[3],166,par("usr")[4],col= rgb(0,0,1,alpha=0.3),border=NA) #bottom, top, left, right
         rect(377,par("usr")[3],399,par("usr")[4],col= rgb(0,0,1,alpha=0.3),border=NA) #bottom, top, left, right
         rect(408,par("usr")[3],417,par("usr")[4],col= rgb(0,0,1,alpha=0.3),border=NA) #bottom, top, left, right
         rect(438,par("usr")[3],455,par("usr")[4],col= rgb(0,0,1,alpha=0.3),border=NA) #bottom, top, left, right 
         rect(474,par("usr")[3],481,par("usr")[4],col= rgb(0,0,1,alpha=0.3),border=NA) #bottom, top, left, right
         rect(498,par("usr")[3],529,par("usr")[4],col= rgb(0,0,1,alpha=0.3),border=NA) #bottom, top, left, right

         #Insert legend
         legend(30,16.3, # places a legend at the appropriate place c(“Health”,”Defense”), 
                c("Hsp72-SANC00132 run1"),# puts text in the legend
                cex=0.9, #font size
                y.intersp=1.1, #horizontal space between text
                lty=c(1), # gives the legend appropriate symbols (lines)
                lwd=c(4),col=c("black"),# gives the legend lines their width and the correct color and width
                xpd=TRUE, #outside box
                text.font = 2,
                bty = "n" ) #remove the box around

         ###number labels
         lgnd <-legend(-75,18.1, # places a legend at the appropriate place c(“Health”,”Defense”), 
                       c( "(i)"),# puts text in the legend
                       horiz = FALSE,
                       cex=1.4, #font size
                       #y.intersp=0.75, #horizontal space between text
                       #x.intersp=0.4,
                       #ncol=5,
                       #lty=c(1,1), # gives the legend appropriate symbols (lines)
                       #lwd=c(4,4),col=c("black", "red","blue", "green4", "magenta3"),# gives the legend lines their width and the correct color and width
                       xpd=NA, #outside box
                       text.font = 2,#bold
                       #text.width = 15500, #moves the columns closer together
                       bty = "n")  #add the box around the legend 
dev.off()
