# Lightweight versioned data

The rationale for lightweight versioned data is laid out in [this paper](https://github.com/richfitz/data_versioning).  But the simple goal is to provide versioned data to the world in a way that allows coded access to all versions, new and old.  Moreover, the idea of this project is to provide the tools to do this for free, and without too onerous on-going financial or time commitments, while maintaining curation of the underlying data.  

This set of instructions relies on a basic knowledge of git and github.  If you're a bit rusty on this see [here for a general introduction](http://environmentalcomputing.net/version-control/).  This tutorial frequently uses tools for setting up R packages.  For a excellent and general introduction to the topic see [Hadley Wickham's website/book](http://r-pkgs.had.co.nz/).  

### Setting up a new lightweight versioned dataset

#### Setting up the software


1. Fork and clone this `versioned_data_template` repository
2. Rename your repository (in Github this is currently accessed via the settings button) to reflect your dataset; this name should be the same as the R packages that will distribute your data, so something short, precise, and memorable is preferred.
3. Add your data to the repository folder, preferably as a `.csv` file. (If you're data is too complex for a csv file, this is still possible, see below.  The data may be--but does not have to be--pushed to the cloud repository.)
4. Install the R library `devtools` if you don't have it already
5. Install a package called `datastorr` which manages the interface between your computer and github behind the scenes.  For more on datastorr functionality see [this repo](https://github.com/richfitz/datastorr.example).  To install, in R run: 
```r
devtools::install_github("ropenscilabs/datastorr")
```
#### Renaming things for your specific dataset

1. In the `dataset_access.R` file, rename the main function called `dataset_access_function` to something specific to your dataset 
2. Also in the `dataset_access.R` file, find the `dataset_info` function and change 1) the name of the repository to your repository name 2) the name of the file to reflect the name of the file that contains your data.  
3. *Option for non-csv data structures:* If your dataset is too complicated to fit into a `csv` file, you will have to write an input function that loads your data into R.  Write this input function, include it in the `dataset_access.R` and replace `read_csv` with a call to your input function so that your dataset reads nicely into R in a way that's convenient for your users.  
4. Modify the description and licence files to reflect the title, authors, and date for your package (see [here for more details](http://r-pkgs.had.co.nz/description.html))
5. The template is set up with a MIT licence, but this may be changed for different situations.  

### Documentation for your users (we have set up this template using [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) for ease of use):

1.  Include a description of the dataset in the documentation section of the function formerly known as `dataset_access_function` which you renamed above.  This will show up as the R help file for users once they download and install your package.
2. It is also good practice to add an example of that same function to show users how to use the important features of your dataset. 
3.  Include the meta-data for your dataset.  We recommend adding a zip file to the release with the appropriate meta-data.  In this way the meta-data is both versioned and included in the DOI.  

### Building your documentation and loading your package

1. In R, with the appropriate working directory set, call `devtools::document()` 
2. Install your package locally call `devtools::load_all()`

### Upload to the cloud

1. Commit and push your changes to github
2. On your local machine run `<your_package_name>:::dataset_release("<description>")`  Where `<description>` is a description of what changed in the package.  This should push version 0.0.1 to a github release.  

### Testing your package

1. Test that everything works by calling the function formerly known as `dataset_access_function`.  The data should download from github and load nicely into R. 

### Setting up Digital Object Identifier (DOI) assignment

The specifics of this depend on which DOI minter you use.  We have used both zenodo and figshare.  Each source has their own short tutorials for setting this up.  The Zenodo/Github tutorial is [here](https://guides.github.com/activities/citable-code/).  All of the points made in the tutorial apply equally to code and to data.  

**That's it.  You now have a package that is set up for distributing stable versioned data to the world.**

## Managing interactions with users of your database

We recommend suggesting that users flag issues using the "issue tracker" functionality of Github.  This will allow specific questions to be asked, discussed, and resolved.  *Note: if you find an issue with this tutorial, please raise an issue on this repository!*  In some cases these queries may lead to improvements of the underlying dataset, in that case, it makes sense to release a new version of the database.  

## Maintaining a versioned dataset 

When your dataset improves via error fixes or data addition, and you're happy with the changes, there are a few simple steps to bump the dataset into the future.    

1. Update the `DESCRIPTION` file to **increase** the version number.   [Semantic versioning](http://semver.org/) is one way to manage these changes.
2.  Update your data and meta-data files with all your improvements
3.  Commit data and code changes and `DESCRIPTION` and push to GitHub
4.  With R in the package directory, run
```r
<your_package_name>:::dataset_release("<description>")
```
where `"<description>"` is a brief description of the improvements to the dataset.

Happy data versioning!
