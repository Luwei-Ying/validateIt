* This is a forked version of `validateIt`. This package allows to conduct additional validation tasks such as Label intrusion tasks (LI2 or LI4).


## validateIt: An R Package for Topic and Label Validation

Authors: [Luwei Ying](http://luweiying.org), [Jacob Montgomery](https://pages.wustl.edu/montgomery) and [Brandon Stewart](http://brandonstewart.org)

Please email all comments/questions to luwei.ying [AT] wustl.edu

### Installation Instructions
The package is currently not available on CRAN. You can install the most recent development version using the devtools package. First you have to install devtools using the following code.  Note that you only have to do this once

```  
if(!require(devtools)) install.packages("devtools")
```  

Then you'll need to install the dependency, "pyMTurkR", using the function `install_github`. If you need to interact with Mturk, be sure to follow the detailed instructions here: github.com/cloudyr/pyMTurkR
```
devtools::install_github("cloudyr/pyMTurkR")
```

Now you can install our package

```
devtools::install_github("Luwei-Ying/validateIt", dependencies=TRUE)
```

Note that this will install all the packages suggested and required to run our package.  It may take a few minutes the first time, but this only needs to be done on the first use.  In the future you can update to the most recent development version using the same code. 
