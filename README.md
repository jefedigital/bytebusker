# bytebusker

Some helper functions for repetitive tasks in R, leaning on the tidyverse wherever possible.

-   **bb_boxmatrix** - takes a dataframe and returns a matrix of boxplots histograms of the numeric columns using ggplot2.

-   **bb_cleancols** - takes a dataframe and returns it with cleaned-up column labels.

-   **bb_corrplot** - takes a dataframe and makes a nice correlation plot of the numeric columns using corrr.

-   **bb_histmatrix** - takes a dataframe and returns a matrix of histograms of the numeric columns using ggplot2.

-   **bb_screeplot** - takes a prcomp object and returns a simple scree plot (bar or line) using ggplot.

-   **bb_table** - takes a dataframe and makes a nice table using kable.

-   **bb_vifplot** - takes an lm object and makes a Variance Inflation Factor (VIF) plot using car::vif()
