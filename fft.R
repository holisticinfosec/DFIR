# Fast Frugal Tree example for DFIR Redefined: Deeper Functionality for Investigators with R
# FFTRees created by Nathaniel D. Phillips
# http://nathanieldphillips.com/2016/08/making-fast-good-decisions-with-the-fftrees-r-package/

library("FFTrees")
cvss <- read.csv("c:/coding/r/FFTrees/CVSS.csv")
cvss.fft <- FFTrees(formula = critical ~., data = cvss)

plot(cvss.fft, what = "cues")

plot(cvss.fft, 
     main = "CVSS FFT", 
     decision.names = c("Non-Critical", "Critical"))

plot(cvss.fft, 
     main = "CVSS FFT", 
     decision.names = c("Non-Critical", "Critical"),
     tree = 1)