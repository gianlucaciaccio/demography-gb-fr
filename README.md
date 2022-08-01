# demography-gb-fr
This repo contains the source code of my project carried out during my academic experience at the University of Bologna.

## Goal
The goal of this project is to make a comparison on the evolution of mortality and life expectancy in Great Britain and France, verifying whether the geographical proximity has produced a different demographic development in the two different countries. The relevance of this study consists in highlighting the possible links between historical events, such as wars and epidemics, the different climates, the different welfare systems and the evolution of the population structure of the two countries.

## Data and Tools
The data for the analysis was obtained from the [Human Mortality Database](https://www.mortality.org/), developed by the Department of Demography of the [University of California, Berkeley (USA)](https://www.berkeley.edu/), and freely available online, which provides information on mortality - statistics relating to the distribution by age of deaths and population, rates and mortality tables - of 38 countries from the 19th century to the present, provided by the statistical centers of the various countries. For this project, in order to make a correct comparison of the two countries, the period 1922-2018 was considered, as they are the only data available for Great Britain (for France the data start from 1816).

The French data were made available by [INSEE](https://www.insee.fr/) (Institut National de la Statistique et des Études Économiques), while those relating to Great Britain by the [Office for National Statistics](https://www.ons.gov.uk/) and the [General Register Office for Scotland](https://www.genguide.co.uk/). The analysis was conducted in [R](https://cran.r-project.org/) using the `demography` package.

## Summary of the analysis
The analysis is structured in two steps:

- Exploratory data analysis:

  - Log death rates by age and time of the two countries
  - Life expectancy at birth total and by sex
  - Mortality tables and $e_x$ curves
  - Survival curves ( $l_x$ ), [rectangularization](https://journals.library.ualberta.ca/csp/index.php/csp/article/download/15700/12505) and death curves ( $d_x$ ) by age, sex and years

- Forecasting of global and gender mortality rates with [Lee-Carter model](http://pagesperso.univ-brest.fr/~ailliot/doc_cours/M1EURIA/regression/leecarter.pdf) (from 2019 to 2050)
