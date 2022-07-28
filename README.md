# Hybrid Touch/Tangible Spatial Selection in Augmented Reality -- GRSI

## General
We document and allow, in this repository, to reproduce the statistical analysis we performed for the paper "Mickael Sereno, Stéphane Gosset, Lonni Besançon, Tobias Isenberg. Hybrid Touch/Tangible Spatial Selection in Augmented Reality. Computer Graphics Forum, Wiley, 2022, 41 (3), pp.403--415. DOI: [10.1111/cgf.14550](https://doi.org/10.1111/cgf.14550)." The paper can be found at https://hal.inria.fr/hal-03699232/document as open-access. The related OSF repositories are at https://osf.io/pwauq/ (First Study) and at https://osf.io/rvpuc/ (Second Study). 

## Bibtex
```
@article{Sereno:2022:HTT,
  author      = {Mickael Sereno and Stéphane Gosset and Lonni Besan{\c{c}}on and Tobias Isenberg},
  title       = {Hybrid Touch/Tangible Spatial Selection in Augmented Reality},
  journal     = {Computer Graphics Forum},
  year        = {2022},
  volume      = {41},
  number      = {3},
  month       = jun,
  pages       = {403--415},
  doi         = {10.1111/cgf.14550},
  doi_url     = {https://doi.org/10.1111/cgf.14550},
  oa_hal_url  = {https://hal.inria.fr/hal-03699232},
  osf_url     = {https://osf.io/qu634/},
  osf_url2    = {https://osf.io/pwauq},
  osf_url3    = {https://osf.io/rvpuc},
  url         = {https://tobias.isenberg.cc/VideosAndDemos/Sereno2022HTT},
  github_url  = {https://github.com/MickaelSERENO/SciVis_Server/tree/CHI2020},
  pdf         = {https://tobias.isenberg.cc/personal/papers/Sereno_2022_HTT.pdf},
  video       = {https://youtu.be/JrBdTcuqHts},
}
```

## Random Seed Issue
Note that due to the non-use of a constant random seed, the generated images can vary slightly for every data analysis relying on bootstrapping (which relies on random sampling). The generated images, however, are stable and we took into consideration the "dance of statistical analysis"; https://www.aviz.fr/badstats#sec3 in our paper.

The values we provide in the tables Tables 2-13 (in the appendix) are the values we got for a single run of the scripts that generated the images we reported in the original paper.

## Requirements

* Rstudio (for Windows and OSX users. Linux users can also install it if they prefer to do everything with a graphical user interface (GUI)). https://www.rstudio.com/
* R (if you do not install Rstudio which should normally install a R environment).
* R packages:
    * ```dplyr```
    * ```ggplot2```
    * ```reshape2```
    * ```data.table```
    * ```pracma```

Note that those packages contain requirements that we also use. The R package manager will install automatically those requirements.

Example for Arch Linux:
```
sudo pacman -Syu r

R
> install.packages("dplyr")
> install.packages("ggplot2")
> install.packages("reshape2")
> install.packages("data.table")
> install.packages("pracma")
> Ctrl+D # to exit
```

Note that Rstudio can detect and install automatically all the required packages by opening, via Rstudio, the scripts ```FirstStudy/main.R``` and ```SecondStudy/main.R```. Rstudio is not necessary to execute the data analysis, only Rscript is (installed by default with your R environment, at least on the Arch Linux distribution).

## Build

* For Linux users, via command line:
    * Make sure that you have "Rscript" installed in your system and that it is accessible via your PATH environment variable. It should normally come with the installation of the R environment in, e.g., ```/usr/bin/Rscript```.
    * Run the bash script "./produce.sh". Note that you may have to change the default working directory to the root of this repository for this sh script to work:
    ```
    cd <repository>
    ./produce.sh
    ```
* For OSX and Windows users (command lines):
    * Run, via Rscript, the two scripts ```FirstStudy/main.R``` and ```SecondStudy/main.R```. Note that you may have to change the default working directory to, respectively, ```FirstStudy/``` and ```SecondStudy/```.
    ```
    cd FirstStudy
    Rscript main.R
    cd ../SecondStudy
    Rscript main.R
    ```
* For all users (GUI version):
    * Run separately the two scripts ```FirstStudy/main.R``` and ```SecondStudy/main.R``` via Rstudio. Note that you may have to change the default working directory to, respectively, ```FirstStudy/``` and ```SecondStudy/```.

We tested all the scripts for R version 4.2 on Arch Linux 5.18.12-arch1-1.

## Output

All the PDFs are generated in:

* ```FirstStudy/ROutput``` (For the first study related images)
* ```SecondStudy/ROutput``` (For the second study related images)

The generated images will correspond to the Figures 6-20 and Figures 22-23 we reported in the paper.
Generated images in the form of ```PW\*.pdf``` correspond to the pair-wise comparisons of the reported metrics in the paper. For instance, ```PWmcc.pdf``` corresponds to "pair-wise comparisons of the MCC score".

## Additional notes

In this repository, we already extracted from the users' logs the CSV files (```FirstStudy/data/log/data.csv``` and ```SecondStudy/data/log/data.csv```) we feed to the R scripts. Those CSV files correspond to the data we report per triplet of (user, trial, technique). The users' low level logs are available in the respective OSF repositories (The .tar.gz files in the directories ```Scripts/Python/logs``` for the first study related logs and ```Results/``` for the second study related logs) for analysts that want to perform additional data analysis and data mining on what the users did throughout the two studies.

We also manipulated Figures 19 (a) and 19 (b) (corresponding PDFs: ```SecondStudy/external.pdf```, ```SecondStudy/PWexternal.pdf```) to make them as space efficient as possible with a vector image editor (e.g., Inkscape). Those changes are cosmetic and we did not manipulate in any way the data. We did this to save space in the PDF due to the limited number of pages.
