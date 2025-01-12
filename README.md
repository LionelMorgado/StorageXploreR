# StorageXploreR
## StorageXploreR: a package for data storage profiling and exploration in R.

Over time, organizations can accumulate large amounts of data following poor storage standards. For example, it is common to find in legacy data, multiple copies of the exact same file spread all over the storage, the same data stored in different file formats, or files in formats which are suboptimal for storage. This can arise adverse situations, such as a lack of storage space or add unnecessary storage costs.

StorageXploreR provides a set of functions to scan data storage units and quickly create a utilization profile to facilitate downstream optimization and standardization. Using file attributes such as the name, extension, format and size, it enables the detection of multi-copy and name-related files as candidates for potential removal or format optimization.

This vignette shows some common ways in which the functions in this package can be used. It is however not exhaustive and will not show every argument of every function. You can view the documentation of a function by adding a ? in front of it (e.g. ?plot_donut). This allows to get a more detailed description of the functions and all their arguments. Additional examples of how the functions in this package can be used are also available.
![StorageXploreR_plots](https://github.com/user-attachments/assets/08431116-05ae-42e3-a75c-c3a41e58fc28)



# Installation

The package can be installed from github.
```{r install_package, eval=FALSE}
install_github("LionelMorgado/StorageXploreR")
```

The package also needs to be loaded every time `R` is restarted.
```{r Load package, message=FALSE}
library(StorageXploreR)
```
