# StorageXploreR
## StorageXploreR: a package for data storage profiling and exploration in R.

Over time, organizations can accumulate large amounts of data following poor storage standards.
For example, when multiple copies of the same data are kept or suboptimal file formats are used,
challenging situations such as a lack of storage space or additional storage costs can arise. 

StorageXploreR provides a set of functions to scan data storage units and quickly create a utilization
profile to facilitate downstream optimization and standardization. Using file attributes such as the 
name, extension, format and size, it enables the detection of multi-copy and name-related files as 
candidates for potential removal or format optimization.
![StorageXploreR_plots](https://github.com/user-attachments/assets/e737bed5-00b8-462b-883c-66794b5aca83)


# Installation

The package can be installed from github.
```{r install_package, eval=FALSE}
install_github("LionelMorgado/StorageXploreR")
```

The package also needs to be loaded every time `R` is restarted.
```{r Load package, message=FALSE}
library(StorageXploreR)
```
