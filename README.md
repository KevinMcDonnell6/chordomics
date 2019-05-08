# chordomics

Chordomics is a tool to visualize and interpret linked data, such as from metagenomics or metaproteomics where both taxanomic and functional data is obtained.

# Installation
Chordomics relies of the `chorddiag` R package, and can be installed from an R session as follows:
``` r
install.packages("devtools")
devtools::install_github("KevinMcDonnell6/chorddiag")
```

Then download the `chordomics` package using the following command:
```r
devtools::install_github('KevinMcDonnell6/chordomics')
```


# Running Chordomics
## Preparing your metaproteomics data
The input data must be `.csv` format, with column names. The output from programs like MPA^[<https://github.com/compomics/meta-proteome-analyzer>] is ideal; It will look for columns "Proteins", containing one or more Uniprot accessions.

## Preparing your metagenomics data
You will need a run ID from MG-RAST^[<http://www.mg-rast.org/>]. 

## Using the Chordomics viewer
Next, launch the app!

```r
chordomics::launchApp()
```

Once the app is running, follow the steps to preprocess, load, and view your data!


## Visualising the data
The Plot tab is where the user is able to view the data they have loaded into the App. 
Clicking Load Example Data shows already processed data for the user to experiment with.
The datasets can be viewed together (default) or individually by selecting the name of the dataset on the left panel.
Selecting a taxonomic rank from the panel changes the rank shown on the plot.
Selecting a taxonomic group on the chord diagram selects only that taxon. Changing the rank now allows the user to view the subtaxa of their selection.
Similarly the functions can have a hierarchical structure. The example data is labelled with functional categories ("group.function") and their COG ("predicted.function"). This can be applied to other annoatations such as KEGG, applied by the user.

# What it is doing
## with metaproteomics data
Given a metaproteomics csv file, Chordomics gets functional data from UniProt, which is then saved to a `chordomics` folder in your home directory.  This makes it easier to re-run analyses.  Next, a parsimony approach is used to filter our what data is being dispayed for a given peptide. Certain check are performed to make sure all the required fields have data, and the cleaned data with the COG annotations is returned to be downloaded.

## with metagenomic/metatransciptomic data
Given an MG-RAST ID (usually starting with "mgm"), the taxonomy and function annotations are downloaded.  Be warned -- this can take a long time.  For now, please only use datasets from assembled metagenomes, rather than just reads.  The datases are combined -- retaining only the sequences for which both functional and taxanomic annotations are available. The COGs are assigned, NCBI taxids are linked, and the data is returned to be downloaded.



# Troubleshooting
## Metaproteomic input data
The fields in the csv file should be quoted, as the lists of Uniprot accessions are also comma-separated in the output from the MPA.  So, ensure the files are quoted, commas are used as the separator, and commas are also used as the within-field seprator.  The following headers are required:
```
"Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species", "Proteins"
```
Any extra headers are ignored.


## Metagenomic input data
If your selected MG-RAST id is running slowly, it is likely due to the time it takes to download the data files.  Sadly, MG-RAST does not provide any given file with both taxonomic and functional information, so we have to download both and merge them.  
