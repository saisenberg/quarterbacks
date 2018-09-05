
## Assigning Success Likelihoods to Recently Drafted Quarterbacks

This project attempts to predict quarterbacks' NFL success - both for recently drafted quarterbacks, and for those whose career outcomes we already know. We first develop an objective definition of "success", and use this definition to classify quarterbacks as either "successful" or "unsuccessful." Then, using the players' college statistics and physical attributes, we use random forest modeling to predict the quarterbacks' respective chances at success. Ultimately, we end up with a series of success likelihood projection distributions for a number of quarterbacks drafted within the last two years, and analyze these results to determine the recent draftees most likely to succeed.  

A full description of the project can be found at [**saisenberg.com**](https://saisenberg.com/projects/quarterbacks.html).

### Getting started

#### Prerequisite software

* Python (suggested install through [Anaconda](https://www.anaconda.com/download/))

* [R](https://www.r-project.org/)

* [RStudio](https://www.rstudio.com/products/rstudio/download/)

#### Prerequisite libraries

* Python:
    - imblearn (```!pip install imblearn```)
    - matplotlib, numpy, os, pandas, seaborn, sklearn, warnings (```all installed with Anaconda```)


* RStudio:

```
lib <- c('data.table', 'dplyr', 'ggplot2', 'gtools', 'htmltab', 'jsonlite', 'rvest', 'stringr')
install_packages(lib)
```

### Instructions for use

#### 1. Run */r/cfb_stats.R*

This program collects, cleans, and aggregates college football quarterback data from *[sports-reference.com](https://sports-reference.com/cfb/)*.  

The output of */r/cfb_stats.R* can also be found at */data/cfb_stats.csv*.

#### 2. Run */r/nfl_stats.R*

This program collects, cleans, and aggregates NFL quarterback data from *[pro-football-reference.com](https://pro-football-reference.com)*, and classifies quarterbacks as "successes" or "non-successes." Additionally, the program merges these classifications with the previously-scraped college football quarterback data.

The output of */r/nfl_stats.R* can also be found at */data/prev_drafted_qb.csv* and */data/recent_drafted_qb.csv*.

#### 3. Run the code contained in */python/nfl_qbs.ipynb*

This code attempts predict quarterback success using the previously-scraped college football quarterback data. After first iterating through the random forest algorithm twice, the code repeats the process ten thousand times, each time generating different success likelihood predictions for recently drafted quarterbacks. Results for each quarterback are then aggregated and analyzed.

The output of */python/nfl_qbs.ipynb* has been included within the iPython Notebook file.


### Author

* **Sam Isenberg** - [saisenberg.com](https://saisenberg.com) | [github.com/saisenberg](https://github.com/saisenberg)


### License

This project is licensed under the MIT License - see the *LICENSE.md* file for details.

### Acknowledgments

* [pro-football-reference.com](https://www.pro-football-reference.com)
* [sports-reference.com](https://sports-reference.com/cfb/)
* [Zack Thoutt](https://www.kaggle.com/zynicide/nfl-football-player-stats)

