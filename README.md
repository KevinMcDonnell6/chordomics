[![Build Status](https://travis-ci.org/KevinMcDonnell6/chordomics.svg?branch=master)](https://travis-ci.org/KevinMcDonnell6/chordomics)
# Chordomics
[![Screenshot](https://github.com/KevinMcDonnell6/chordomics/blob/master/Walkthroughs/Screenshots/Screenshot.png)](https://github.com/KevinMcDonnell6/chordomics)

Chordomics is a tool to visualize and interpret linked data, such as from metagenomics or metaproteomics where both taxonomic and functional data is obtained.

Published in Oxford Bioinformatics:  https://doi.org/10.1093/bioinformatics/btz711

# Demo Version
The demonstration version of Chordomics is available at https://kmcd.shinyapps.io/chordomics/. Here, a toy dataset is provided to allow you to see the explorative capabilities Chordomics provides before you need to install it!

# Installation
To install from the commandline, download and run the install script:

```{bash}
curl -o run_chordomics.R  https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/run_chordomics.R && Rscript run_chordomics.R
```

To install from an R session, one must first have the `devtools` package installed as follows:
```r
install.packages("devtools")
```

Then to download the `chordomics` package use the following command:

```r
devtools::install_github('KevinMcDonnell6/chordomics')
```


# Running Chordomics

## Using Chordomics
Launch the app!

```r
chordomics::launchApp()
```

Once the app is running, follow the steps to preprocess, load, and view your data!

For detailed step-by-step walkthroughs look into the "Walkthroughs" folder above.

## Preparing your metaproteomics data
The input data must be `.csv` format, with column names. The output from programs like MPA^[<https://github.com/compomics/meta-proteome-analyzer>] is ideal (see "MPAdata" Walkthrough); It will look for columns "Proteins", containing one or more Uniprot accessions.

## Preparing your metagenomics data
You will need a run ID from from a public repository on MG-RAST^[<http://www.mg-rast.org/>]. 
Alternatively you can manually download the required files and upload them to Chordomics. For more information look at the "MGRASTdata" and "MGRASTDataProcessing" files in the Walkthroughs folder above.

## Data Processing
The App can handle both MG-RAST data as well as MetaProteomeAnalyzer (MPA) files.

For metagenomic or metatranscriptomic data first upload your samples to MG-RAST. Then enter your MG-RAST ID or upload the correct files to the app. Chordomics will merge the taxonomy and function files as well as adding the descriptions for the COG IDs that MG-RAST has annotated the data with. The processed file can then be saved.

For metaproteomics data upload your data to the MPA and then export the Meta-proteins file. Then upload this file to the app. Chordomics will use the UniProt API to add COG IDs to each of the meta-proteins. It will then add the descriptions for each ID. The processed file can then be saved.

## Visualising the data
The Chord Plot tab is where the user is able to view the data they have loaded into the App. 
* Clicking Load Example Data shows already processed data for the user to experiment with.
* The datasets can be viewed together (default) or individually by selecting the name of the dataset on the left panel.
* Selecting a taxonomic rank from the panel changes the rank shown on the plot.
* Selecting a taxonomic group on the chord diagram (e.g. "Bacteria" for the example data) selects only that taxon. Changing the rank now allows the user to view the subtaxa of their selection.
* Similarly the functions can have a hierarchical structure. The example data is labelled with functional categories ("COG_Category") and their COG ("COG_Name"). This can be applied to other annoatations such as KEGG, if given the appropriate headings by the user.

# What it is doing
## with metaproteomics data
Given a metaproteomics csv file, Chordomics gets functional data from UniProt, which can then be saved to your computer.  This makes it easier to re-run analyses. Certain checks are performed to make sure all the required fields have data, and the cleaned data with the COG annotations is returned to be downloaded.

## with metagenomic/metatransciptomic data
Given an MG-RAST ID (usually starting with "mgm"), the taxonomy and function annotations are downloaded. Be warned -- this can take a long time. Alternatively these files can be uploaded manually to Chordomics.  For now, please only use datasets from assembled metagenomes, rather than just reads.  The datases are combined -- retaining only the sequences for which both functional and taxonomic annotations are available. The COGs are assigned, NCBI taxids are linked, and the data is returned to be downloaded.



# Troubleshooting
## Chordomics input data
Chordomics currently requires the input to have at least one taxonomic column ("Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species"), and one or more of the following: "COG_Category"	"COG_Name".  This consistency allows us to handle the hierarchical nature of both the functional and taxonomic data.  If you wish to display different types of data, we incorporated  SVG downloading via Crowbar^[<http://nytimes.github.io/svg-crowbar/>].


## Metaproteomic utility input data
The fields in the csv file should be quoted, as the lists of Uniprot accessions are also comma-separated in the output from the MPA.  So, ensure the files are quoted, commas are used as the separator, and commas are also used as the within-field seprator.  The following headers are required:
```
"Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species", "Proteins"
```
"Proteins" should contain one or more UniProt accessions, separated by commas; any extra headers are ignored.


## Metagenomic/metatranscriptomic utility input data
If your selected MG-RAST id is running slowly, it is likely due to the time it takes to download the data files.  Sadly, MG-RAST does not provide any given file with both taxonomic and functional information, so we have to download both and merge them.  Try with a small dataset first, such as "mgm4762935.3".

# Cite us!!
If you like Chordomics and use it in a publication, please cite us!
```
@article{mcdonnell2019chordomics,
  title={Chordomics: a visualisation tool for linking function to phylogeny in microbiomes},
  author={McDonnell, Kevin and Waters, Nicholas and Howley, Enda and Abram, Florence},
  journal={Bioinformatics},
  year={2019}
}
```
