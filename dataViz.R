#Data visualizations code
library(seewave)
library(tuneR)
library(ggplot2)
library(tidyverse)

voc <- readWave("C:\\Users\\Jawor\\Desktop\\repos\\creative-data-visualization\\VCH_ex_June17_MZ10.wav")

seewave::duration(voc)

seewave::spectro(voc, 
                 f = voc@samp.rate, 
                 main = "Basic Spectrogram", 
                 flim = c(0, 0.5), 
                 tlim = c(5.3, 6))

voc2 <- readWave("C:\\Users\\Jawor\\Desktop\\repos\\creative-data-visualization\\VTR_ex_June17_MZ24.wav")
seewave::duration(voc2)
spectro(voc2, 
        f = voc@samp.rate, 
        main = "Basic Spectrogram", 
        flim = c(0, .85), 
        tlim = c(9, 12.5))
