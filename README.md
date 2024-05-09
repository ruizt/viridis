# repository for c. viridis thermal ecology

Citation:

H. A. Moniz, J. H. Buck, H. L. Crowell, S. M. Goetz, T. D. Ruiz, S. M. Boback, E. N. Taylor (2024). Reproductive state impacts the thermal ecology of female rattlesnakes at a high-elevation site.

Contributors:

T. D. Ruiz, H.A. Moniz.

Dependencies:

─ Session info ─────────────────────────────────────────────────
 setting  value
 version  R version 4.4.0 (2024-04-24)
 os       macOS Sonoma 14.4.1
 system   x86_64, darwin20
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       America/Los_Angeles
 date     2024-05-09
 rstudio  2024.04.0+735 Chocolate Cosmos (desktop)
 pandoc   NA

─ Packages ─────────────────────────────────────────────────────
 package      * version   date (UTC) lib source
 backports      1.4.1     2021-12-13 [1] CRAN (R 4.4.0)
 bitops         1.0-7     2021-04-24 [1] CRAN (R 4.4.0)
 broom          1.0.5     2023-06-09 [1] CRAN (R 4.4.0)
 cli            3.6.2     2023-12-11 [1] CRAN (R 4.4.0)
 clipr          0.8.0     2022-02-22 [1] CRAN (R 4.4.0)
 cluster        2.1.6     2023-12-01 [1] CRAN (R 4.4.0)
 colorspace     2.1-0     2023-01-23 [1] CRAN (R 4.4.0)
 deSolve      * 1.40      2023-11-27 [1] CRAN (R 4.4.0)
 dplyr        * 1.1.4     2023-11-17 [1] CRAN (R 4.4.0)
 emmeans      * 1.10.1    2024-04-06 [1] CRAN (R 4.4.0)
 estimability   1.5       2024-02-20 [1] CRAN (R 4.4.0)
 fansi          1.0.6     2023-12-08 [1] CRAN (R 4.4.0)
 farver         2.1.1     2022-07-06 [1] CRAN (R 4.4.0)
 fda          * 6.1.8     2024-03-09 [1] CRAN (R 4.4.0)
 fds          * 1.8       2018-10-31 [1] CRAN (R 4.4.0)
 forcats      * 1.0.0     2023-01-29 [1] CRAN (R 4.4.0)
 generics       0.1.3     2022-07-05 [1] CRAN (R 4.4.0)
 ggplot2      * 3.5.1     2024-04-23 [1] CRAN (R 4.4.0)
 glue           1.7.0     2024-01-09 [1] CRAN (R 4.4.0)
 gridExtra    * 2.3       2017-09-09 [1] CRAN (R 4.4.0)
 gtable         0.3.5     2024-04-22 [1] CRAN (R 4.4.0)
 hdrcde         3.4       2021-01-18 [1] CRAN (R 4.4.0)
 hms            1.1.3     2023-03-21 [1] CRAN (R 4.4.0)
 KernSmooth     2.23-22   2023-07-10 [1] CRAN (R 4.4.0)
 ks             1.14.2    2024-01-15 [1] CRAN (R 4.4.0)
 labeling       0.4.3     2023-08-29 [1] CRAN (R 4.4.0)
 lattice        0.22-6    2024-03-20 [1] CRAN (R 4.4.0)
 lifecycle      1.0.4     2023-11-07 [1] CRAN (R 4.4.0)
 lubridate    * 1.9.3     2023-09-27 [1] CRAN (R 4.4.0)
 magrittr       2.0.3     2022-03-30 [1] CRAN (R 4.4.0)
 MASS         * 7.3-60.2  2024-04-24 [1] local
 Matrix         1.7-0     2024-03-22 [1] CRAN (R 4.4.0)
 mclust         6.1.1     2024-04-29 [1] CRAN (R 4.4.0)
 modelr       * 0.1.11    2023-03-22 [1] CRAN (R 4.4.0)
 munsell        0.5.1     2024-04-01 [1] CRAN (R 4.4.0)
 mvtnorm        1.2-4     2023-11-27 [1] CRAN (R 4.4.0)
 nlme         * 3.1-164   2023-11-27 [1] CRAN (R 4.4.0)
 patchwork    * 1.2.0     2024-01-08 [1] CRAN (R 4.4.0)
 pcaPP        * 2.0-4     2023-12-07 [1] CRAN (R 4.4.0)
 pillar         1.9.0     2023-03-22 [1] CRAN (R 4.4.0)
 pkgconfig      2.0.3     2019-09-22 [1] CRAN (R 4.4.0)
 pkgload        1.3.4     2024-01-16 [1] CRAN (R 4.4.0)
 pracma         2.4.4     2023-11-10 [1] CRAN (R 4.4.0)
 purrr        * 1.0.2     2023-08-10 [1] CRAN (R 4.4.0)
 R6             2.5.1     2021-08-19 [1] CRAN (R 4.4.0)
 ragg           1.3.0     2024-03-13 [1] CRAN (R 4.4.0)
 rainbow      * 3.8       2024-01-23 [1] CRAN (R 4.4.0)
 RCurl        * 1.98-1.14 2024-01-09 [1] CRAN (R 4.4.0)
 readr        * 2.1.5     2024-01-10 [1] CRAN (R 4.4.0)
 rlang          1.1.3     2024-01-10 [1] CRAN (R 4.4.0)
 rstudioapi     0.16.0    2024-03-24 [1] CRAN (R 4.4.0)
 scales         1.3.0     2023-11-28 [1] CRAN (R 4.4.0)
 sessioninfo    1.2.2     2021-12-06 [1] CRAN (R 4.4.0)
 stringi        1.8.3     2023-12-11 [1] CRAN (R 4.4.0)
 stringr      * 1.5.1     2023-11-14 [1] CRAN (R 4.4.0)
 systemfonts    1.0.6     2024-03-07 [1] CRAN (R 4.4.0)
 textshaping    0.3.7     2023-10-09 [1] CRAN (R 4.4.0)
 tibble       * 3.2.1     2023-03-20 [1] CRAN (R 4.4.0)
 tidyr        * 1.3.1     2024-01-24 [1] CRAN (R 4.4.0)
 tidyselect     1.2.1     2024-03-11 [1] CRAN (R 4.4.0)
 tidyverse    * 2.0.0     2023-02-22 [1] CRAN (R 4.4.0)
 timechange     0.3.0     2024-01-18 [1] CRAN (R 4.4.0)
 tzdb           0.4.0     2023-05-12 [1] CRAN (R 4.4.0)
 utf8           1.2.4     2023-10-22 [1] CRAN (R 4.4.0)
 vctrs          0.6.5     2023-12-01 [1] CRAN (R 4.4.0)
 withr          3.0.0     2024-01-16 [1] CRAN (R 4.4.0)
 xtable         1.8-4     2019-04-21 [1] CRAN (R 4.4.0)

 [1] /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library

────────────────────────────────────────────────────────────────