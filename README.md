# NIDAP Functions

This package contains all NIDAP-specific functions built for Palantir Foundry Code Workbook and manages their dependencies in a controlled way. It ensures that any previously loaded packages are unloaded, then loads only the libraries required for the current function. This prevents conflicts from package priority settings and maintains a clean, consistent environment for running analyses.

##Functions include:

**API utilities for Palantir Foundry** — enabling file access from S3 buckets and pulling data directly into an existing compute node.

**Environment management utilities** — unloading previously loaded packages and loading only those required for the current function, thereby resetting and standardizing the package order.

**Post-it note navigation tools** — breadcrumb-style GUI elements that guide users through specific steps of their workflow, helping to maintain orientation and context.

###Installation
You can install the package directly from the GitHub source tarball:

install.packages(
  "https://github.com/NIDAP-Community/nidapFunctions/raw/main/nidapFunctions_0.7.8.tar.gz",
  repos = NULL,
  type = "source"
)

###Usage: 
library(nidapFunctions)
