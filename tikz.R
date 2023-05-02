#Load the tikzDevice package, if you dont have it, install with:
library(tikzDevice)
# install.packages("tikzDevice", repos="http://R-Forge.R-project.org")
require(tikzDevice)

# The following wwill create normal.tex in the working
# directory the first time this is run it may take a long time because the
# process of calulating string widths for proper placement is
# computationally intensive, the results will get cached for the current R
# session or will get permenantly cached if you set
# options( tikzMetricsDictionary='/path/to/dictionary' ) which will be
# created if it does not exist.  Also if the flag standAlone is not set to
# TRUE then a file is created which can be included with \include{}
tikz('normal.tex', standAlone = TRUE, width=5, height=5)

# Normal distribution curve
x <- seq(-4.5,4.5,length.out=100)
y <- dnorm(x)

# Integration points
xi <- seq(-2,2,length.out=30)
yi <- dnorm(xi)

# plot the curve
plot(x,y,type='l',col='blue',ylab='$p(x)$',xlab='$x$')
# plot the panels
lines(xi,yi,type='s')
lines(range(xi),c(0,0))
lines(xi,yi,type='h')

#Add some equations as labels
title(main="$p(x)=\\frac{1}{\\sqrt{2\\pi}}e^{-\\frac{x^2}{2}}$")
int <- integrate(dnorm,min(xi),max(xi),subdivisions=length(xi))
text(2.8, 0.3, paste("\\small$\\displaystyle\\int_{", min(xi),
                     "}^{", max(xi), "}p(x)dx\\approx", round(int[['value']],3),
                     '$', sep=''))

#Close the device
dev.off()

# Compile the tex file
tools::texi2dvi('normal.tex',pdf=T)

# optionally view it:
# system(paste(getOption('pdfviewer'),'normal.pdf'))