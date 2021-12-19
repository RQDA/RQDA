# RQDA ![R-CMD-check](https://github.com/RQDA/RQDA/workflows/R-CMD-check/badge.svg)

_In December 2021 the orphaned RGtk2 package was archived on CRAN and this is the end for RQDA. It was fun while it lasted, but it's time to move on._

[RQDA](http://rqda.r-forge.r-project.org/) is an R package for computer-aided qualitative data analysis

## Installation of Devel Version

Assuming you have a working version of the current release (and all its dependencies).

```R
#install.packages("devtools") ## install it only if you haven't done it yet
devtools::install_github("RQDA/RQDA", INSTALL_opts = "--no-multiarch")
```


## Installation for Linux users from source package

```{R}
# packages you need to run and build RQDA
pkgs <- c("RSQLite", "gWidgets2RGtk2", "DBI",
          "stringi", "RGtk2", "igraph", "gWidgets2", "devtools")
install.packages(pkgs)

devtools::install_github("RQDA/RQDA")
```


## Installation for Windows users from source package

```{R}
# packages you need to run and build RQDA
pkgs <- c("RSQLite", "gWidgets2RGtk2", "DBI",
          "stringi", "RGtk2", "igraph", "gWidgets2", "devtools")
install.packages(pkgs)

# run this once and click OK when asked to install Gtk2
library(RGtk2)

# RGtk2 installs only i686 or x86_64 files therefore no-mutliarch is required
devtools::install_github("RQDA/RQDA", INSTALL_opts = "--no-multiarch")
```


## Installation for macOS from source package (untested)

1. Install Xocde from the App store, launch Xcode and follow the instruction to install all the components. In addition, open a Terminal and run the following command to install the command line tools: 
   ```
   $ sudo xcode-select --install
   ```

2. Go to https://www.xquartz.org/, download and install XQuartz-2.8.1.dmg.

3. Go to https://www.macports.org/install.php, download and install macport (Install MacPorts for your version of OS X, e.g Sierra). If you had a working MacPorts and updated the OS, they you need to migrate a MacPorts installation by following these [instructions](https://trac.macports.org/wiki/Migration). 

4. Open a Terminal and run the following commands:
   ```
   $ sudo port install pkgconfig
   $ sudo port install gtk2
   ```

5. Download and install the binary version of R.

6. If the above step is successful, launch terminal to invoke R and install RQDA from within R:
```terminal
$ R
```
```R
> devtools::install_github("RQDA/RQDA")
```

7. If all steps are successful, then we can launch RQDA by the following R command:
```R
> library(RQDA) 
```


## How to change the font sizes of the RQDA interface
1. There is a font setting interface in the Settings Tab.

2. This can be achived by modifying ~/.gtkrc-2.0 (create on if not exists), for instance:

```
style "user-font" {
    font_name = "Lucida Grande 14"
}
widget_class "*" style "user-font"

gtk-font-name="Lucida Grande 14"
gtk-enable-mnemonics = 0
```
