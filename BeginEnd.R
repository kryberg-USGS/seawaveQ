# startup
setwd("~/rPackages/seawaveq")
load("~/rPackages/seawaveq/SW.RData")
#load("~/rPackages/seawaveq/.RData")
loadhistory("~/rPackages/seawaveq/SW.Rhistory")
library(waterData)
library(seawaveQ)
data(swData)

setwd("~/rPackages/seawaveq/vignette")
Sweave("vignette.Rnw")
Stangle("vignette.Rnw")

# exit
savehistory("~/rPackages/seawaveq/SW.Rhistory")
save.image("~/rPackages/seawaveq/SW.RData")
quit('no')


# save(pe08d2i1, pe10d2i1, pe25d2i1, pe26d2i1, file="exampleData.RData")
# save(pe08d2i1, pe10d2i1, file="exampleData.RData")
save(IllRivValleyCty, qwMoRivOmaha, cqwMoRivOmaha, examplecavdat, 
     examplecavmat, examplecdatsub, exampleqwcols,
     examplestpars, examplecentmp, exampleclog, exampletndlin, 
     exampletndlinpr, exampletseas, exampletseaspr, exampletyr, 
     exampletyrpr, file="swData.RData")

Sweave("vignette.Rnw")

library(roxygen2)

system("rm -r seawaveQ")
system("rm -r seawaveQ.Rcheck")
system("rm seawaveQ_1.0.0.tar.gz")
clear_caches()
package.skeleton(name="seawaveQ",force=TRUE,
                 code_files=c("rosBoxPlot.R", "combineData.R", 
                              "seawaveq-package.R", "compwaveconv.R", 
                              "fitswavecav.R", "seawaveQPlots.R", 
                              "prepData.R", "fitMod.R", "cenScatPlot.R"))
roxygenize("seawaveQ",roxygen.dir="seawaveQ",copy.package=FALSE)
system("cp DESCRIPTION ./seawaveQ/.")
system("cp NEWS ./seawaveQ/.")
system("cp LICENSE ./seawaveQ")
system("cp *.Rd ./seawaveQ/man/.")
system("mkdir ./seawaveQ/data")
system("mkdir ./seawaveQ/man/figures")
system("mkdir ./seawaveQ/vignettes")
#system("mkdir ./seawaveQ/inst/doc")
#system("cp vignetteStub.Rnw ./seawaveQ/inst/doc/.")
#system("mv ./seawaveQ/inst/doc/vignetteStub.Rnw ./seawaveQ/inst/doc/vignette.Rnw")
system("cp ./vignette/vignette.Rnw ./seawaveQ/vignettes/.")
system("cp *.png ./seawaveQ/man/figures/.")
system("cp *.pdf ./seawaveQ/man/figures/.")
system("cp swData.RData ./seawaveQ/data")
system("rm ./seawaveQ/Read-and-delete-me")
system("R CMD build seawaveQ")
system("R CMD check --as-cran seawaveQ_1.0.0.tar.gz")

remove.packages("seawaveQ")
install.packages("seawaveQ_1.0.0.tar.gz",repos=NULL,type="source")
library(seawaveQ)
