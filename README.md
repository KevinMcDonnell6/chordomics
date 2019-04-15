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
devtools::install_github('KevinMcDonnell6/chordomicsTest6')
```


# Running Chordomics
## Preparing your metaproteomics data
The input data must be `.csv` format, with column names. The output from programs like MPA^[<https://github.com/compomics/meta-proteome-analyzer>] is iteal; It will look for columns "Proteins", containing one or more Uniprot Accessions.
## Preparing your metagenomics data


## Using the Chordomics viewer
Next, launch the app!

```r
chordomicsTest6::launchApp()
```

Once the app is running, follow the steps to preprocess, load, and view your data!

# What it is doing
## with metaproteomics data
Given a metaproteomics csv file, Chordomics gets functional data from UniProt, which is then saved to a `.chordomics` folder in your home directory.  This makes it easier to re-run analyses.  Next, a parsimonious approach is used to filter our what data is being dispayed for a given peptide. Certain check are performed to make sure all the required fields have data, and the
Complete
Add extra rows if needed
Add labels to COG ids
Saving file
Done
## with metagenomic/metatransciptomic data



# Troubleshooting
## Metaproteomicc data input data
The fields in the csv file should be quoted, as the lists of Uniprot accessions are also comma-separated in the output from the MPA.  So, ensure the files are quoted, commas are used as the separator, and commas are also used as the within-field seprator.
