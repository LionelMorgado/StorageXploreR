# StorageXploreR
## StorageXploreR: a package for data storage profiling and exploration in R.

Over time, organizations can accumulate large amounts of data following poor storage standards. For example, it is common to find in legacy data, multiple copies of the exact same file spread all over the storage, the same data stored in different file formats, or files in formats which are suboptimal for storage. This can rise adverse situations, such as a lack of storage space or add unnecessary storage costs.

StorageXploreR provides a set of functions to scan data storage units and quickly create a utilization profile to facilitate downstream optimization and standardization. Using file attributes such as the name, extension, format and size, it enables the detection of multi-copy and name-related files as candidates for potential removal or format optimization.

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
