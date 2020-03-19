#!/usr/bin/env python3
#-*- coding: utf-8 -*-

import argparse
import sys
import os
import requests

from ete3 import NCBITaxa
ncbi = NCBITaxa()

cognames = os.path.expanduser(os.path.join("~", "chordomics", "cognames2003-2014.tab"))
cogcats = os.path.expanduser(os.path.join("~", "chordomics", "fun2003-2014.tab"))
urlpre = "ftp://ftp.ncbi.nih.gov/pub/COG/COG2014/data/"

if not os.path.exists(cognames):
    print("Downloading COG Names from NCBI")
    os.system("curl --url %s --output %s" % (
        urlpre + "cognames2003-2014.tab",
        cognames )
    )

if not os.path.exists(cogcats):
    print("Downloading COG Categories from NCBI")
    os.system("curl --url %s --output %s" % (
        urlpre + "fun2003-2014.tab",
        cogcats)
    )

cogcats_dict = {}
cognames_dict = {}
with open(cogcats, "r") as catin:
    header = True
    for line in catin:
        if header:
            header =False
            continue
        cat, catdesc = line.strip().split("\t")
        cogcats_dict[cat] = catdesc

with open(cognames, "r", encoding = "ISO-8859-1") as cogin:
    """ note from README ftp://ftp.ncbi.nih.gov/pub/COG/COG2014/data
    Functional classes (categories) are described in the file
    fun2003-2014.tab (see 2.3). Some COGs belong to more than one
    functional class; in these cases the class listed first is considered
    to be primary.
    """
    header = True
    for line in cogin:
        if header:
            header =False
            continue
        cog, cat, desc = line.strip().split("\t")
        primary_cat = list(cat)[0]
        cognames_dict[cog]={"desc":desc,
                            "catdesc":cogcats_dict[primary_cat]}


def get_args():  # pragma: no cover
    parser = argparse.ArgumentParser(
        description="Join MG-RAST datasets for use with chordomics",
        add_help=False)
    # contigs not needed if just re-analyzing the results
    parser.add_argument(
        "--ontology", action="store",
        help="Ontology file ")
    parser.add_argument(
        "--organism", action="store",
        help="organism file ")
    # parser.add_argument(
    #     "--nseqs", action="store",
    #     type=int,
    #     help="number of total seqs, used to calculate overall percentages; " +
    #     "required if API is responding slowly for getting sequence count ",
    #     required=False)
    # parser.add_argument("--input_data", action="store",
    #                     help="input_data file ", required=False)
    parser.add_argument("--mgm_id", action="store",
                        help="MG-RAST ID ", required=False)
    parser.add_argument("-o", "--output", action="store",
                        help="destination dir", required=True)
    args = parser.parse_args()
    return args


def read_from_url(url, nrows=10000):
    link = "http://www.somesite.com/details.pl?urn=2344"
    f = urllib.urlopen(link)
    myfile = f.read()


def make_short_name(longname):
    return(" ".join(longname.replace("]", "").replace("[", "").split(" ")[0:2]))


def make_org_dict(path):
    org_dict = {}
    header = True
    with open(path, "r") as inf:
        for line in skip_last(inf):
            if header:
                header = False
                continue
            if len(line.strip().split("\t")) != 4:
                continue
            thisid, m5nr, sequence, annotations = line.strip().split("\t")
            for annotation in annotations.split(";"):
                annotation = make_short_name(annotation)
                thisid = thisid.replace("|RefSeq", "")
                if annotation not in org_dict.keys():
                    org_dict[annotation] = [thisid]
                else:
                    org_dict[annotation].append(thisid)
    return(org_dict)


def make_ont_dict(path):
    """ return a dictionary of read: cog1;cog2 pairs
    """
    ont_dict = {}
    header = True
    with open(path, "r") as inf:
        for line in skip_last(inf):
            if header:
                header = False
                continue
            if len(line.strip().split("\t")) != 4:
                continue
            thisid, m5nr, sequence, annotations = line.strip().split("\t")
            thisid = thisid.replace("|COG", "")
            ont_dict[thisid] = [x.replace("a", "").replace("ccession=[", "").split("],")[0]  for x in annotations.split("];a")]
    return(ont_dict)


def tally_org_ont(org, ont):
    n_nomatch = 0
    org_ont_dict = {thisorg: {"onts": [], "counts":[]} for thisorg in org.keys()}
    for i, (thisorg, id_list) in enumerate(org.items()):
        for id in id_list:
            try:
                for thisont in ont[id]:
                    if thisont in org_ont_dict[thisorg]["onts"]:
                        org_ont_dict[thisorg]["counts"][org_ont_dict[thisorg]["onts"].index(thisont)] += 1
                    else:
                        org_ont_dict[thisorg]["counts"].append(1)
                        org_ont_dict[thisorg]["onts"].append(thisont)
            except KeyError:
                n_nomatch += 1
    print(str(n_nomatch) + " lacked matches between the ontology and organism sets")
    return(org_ont_dict)


def get_seq_count(thisid):
    r = requests.get("https://api-ui.mg-rast.org/metagenome/" + thisid + "?verbosity=stats&detail=sequence_stats")
    return(r.json()["sequence_count_raw"])


def skip_last(iterator):
    # Taken from SO, we need to avoid the last line which marks the completion of download
    # https://stackoverflow.com/questions/16846460
    prev = next(iterator)
    for item in iterator:
        yield prev
        prev = item


def make_taxa_dict(shortname):
    taxid = ncbi.get_name_translator([shortname])
    if taxid == {}:
        return()
    this_lineage = {"Superkingdom": None,
 	            "Kingdom":None,
 	            "Phylum": None,
 	            "Class":None,
 	            "Order": None,
 	            "Family": None,
 	            "Genus": None,
 	            "Species": None }
    this_lin_ids =  ncbi.get_lineage(taxid[shortname][0])
    this_lin =  ncbi.get_rank(this_lin_ids)
    this_names = ncbi.get_taxid_translator(this_lin_ids)
    for rank in this_lineage.keys():
        thisrank = rank.lower()
        try:
            this_lineage[rank] = {"id": [k for k, v in this_lin.items() if v == thisrank][0]}
        except IndexError:
            pass
    for rank, id_dict in this_lineage.items():
        if id_dict is None:
            continue
        try:
            this_lineage[rank]['name'] = [v for k, v in this_names.items() if k == id_dict["id"]][0]
        except IndexError:
            pass
    return(this_lineage)


def get_name(d, level):
    try:
        name = d['lineage'][level]['name']
    except TypeError:
        name =  "NA"
    if name == "NA" and level == "Kingdom":
        name = get_name(d, "Superkingdom")
    return name


def main():
    #print("setting up NCBI Taxa database; this takes a while the first time")
    args = get_args()
    # if args.nseqs is None and False:
    #     print("Getting number of seqs")
    #     args.nseqs = get_seq_count(args.mgm_id)
    print("Tallying ontology file")
    ont_dict = make_ont_dict(args.ontology)
    print("Processed %i ontology hits" % len(ont_dict))
    print("Tallying organism file")
    org_dict = make_org_dict(args.organism)
    print("Processed %i organism hits" % len(org_dict))
    print("combining the datasets")
    combined = tally_org_ont(org=org_dict, ont=ont_dict)
    print("Assigning taxonomy")

    for name in combined.keys():
        combined[name]["lineage"] = make_taxa_dict(name)
    print("Writing out joined results")
    missing_cogs = []
    counter = 0
    with open(args.output, "w") as outf:
        outf.write('"X","Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species","COG_Name","COG_Category"\n')
        for org, onts_counts_lineage in combined.items():
            for i, ont in enumerate(onts_counts_lineage["onts"]):
                # if True:
                #     print(cognames_dict[ont])
                #     continue
                for count in onts_counts_lineage["counts"]:
                    try:
                        outf.write(
                            '"{counter}","{Skingdom}","{Kingdom}","{Phylum}","{Class}","{Order}","{Family}","{Genus}","{Species}","{COG_Name}","{COG_Category}"\n'.format(
                                counter=counter,
                                Skingdom=get_name(onts_counts_lineage, "Superkingdom"),
                                Kingdom=get_name(onts_counts_lineage, "Kingdom"),
                                Phylum=get_name(onts_counts_lineage, "Phylum"),
                                Class=get_name(onts_counts_lineage, "Class"),
                                Order=get_name(onts_counts_lineage, "Order"),
                                Family=get_name(onts_counts_lineage, "Family"),
                                Genus=get_name(onts_counts_lineage, "Genus"),
                                Species=get_name(onts_counts_lineage, "Species"),
                                COG_Name=cognames_dict[ont]["desc"],
                                COG_Category=cognames_dict[ont]["catdesc"]
                            )
                        )
                        counter = counter + 1
                    except KeyError:
                        if ont not in cognames_dict.keys():
                            missing_cogs.append(ont)
                            outf.write(
                                '"{counter}","{Skingdom}","{Kingdom}","{Phylum}","{Class}","{Order}","{Family}","{Genus}","{Species}","{COG_Name}","{COG_Category}"\n'.format(
                                    counter=counter,
                                    Skingdom=get_name(onts_counts_lineage, "Superkingdom"),
                                    Kingdom=get_name(onts_counts_lineage, "Kingdom"),
                                    Phylum=get_name(onts_counts_lineage, "Phylum"),
                                    Class=get_name(onts_counts_lineage, "Class"),
                                    Order=get_name(onts_counts_lineage, "Order"),
                                    Family=get_name(onts_counts_lineage, "Family"),
                                    Genus=get_name(onts_counts_lineage, "Genus"),
                                    Species=get_name(onts_counts_lineage, "Species"),
                                    COG_Name=ont,
                                    COG_Category="UNKNOWN"
                                )
                            )
                            counter = counter + 1
                        else:
                            print("Error with the following ontology -- organism: {} -- {}".format(ont, org))
    print("The following COGs were not found; \n" +
          "see the old database found at ftp://ftp.ncbi.nih.gov/pub/COG/COG/")
    print(set(missing_cogs))
    print("Done!")



if __name__ == "__main__":
    main()


def test_make_org_dict():
    test_dict = {
        'Natronomonas pharaonis': ['mgm4762935.3|contig_121_1069145_length_20328_multi_3_in_1_out_0_12718_14127_-|RefSeq'],
        'Lactobacillus salivarius': ['mgm4762935.3|contig_51_826956_length_45026_multi_4_in_0_out_0_9855_10283_-|RefSeq'],
        'Lactobacillus salivarius': ['mgm4762935.3|contig_51_826956_length_45026_multi_4_in_0_out_0_9855_10283_-|RefSeq'],
        'Ferroglobus placidus': ['mgm4762935.3|contig_81_2449664_length_20179_multi_2_in_0_out_0_4654_6346_+|RefSeq'],
        'Clostridium botulinum': ['mgm4762935.3|contig_121_1103569_length_8205_multi_2_in_2_out_2_4061_4312_+|RefSeq'],
        'Aciduliprofundum boonei': ['mgm4762935.3|contig_111_1628766_length_37341_multi_2_in_0_out_0_11642_15072_-|RefSeq'],
        'Clostridium thermocellum': ['mgm4762935.3|contig_71_7197930_length_16664_multi_2_in_0_out_0_9360_11931_+|RefSeq'],
        'Clostridium thermocellum': ['mgm4762935.3|contig_71_7197930_length_16664_multi_2_in_0_out_0_9360_11931_+|RefSeq'],
        'Clostridium thermocellum': ['mgm4762935.3|contig_71_7197930_length_16664_multi_2_in_0_out_0_9360_11931_+|RefSeq'],
        'Ruminococcus albus': ['mgm4762935.3|contig_101_47814_length_32684_multi_2_in_0_out_0_3909_5352_-|RefSeq'],
        'Methanosphaera stadtmanae': ['mgm4762935.3|contig_121_282635_length_6565_multi_2_in_0_out_2_2839_4642_-|RefSeq'],
        'Ammonifex degensii': ['mgm4762935.3|contig_111_1628766_length_37341_multi_2_in_0_out_0_27395_28315_+|RefSeq'],
        'Methanosaeta thermophila': ['mgm4762935.3|contig_111_1628766_length_37341_multi_2_in_0_out_0_27395_28315_+|RefSeq'],
        'Paenibacillus sp. JDR-2]': ['mgm4762935.3|contig_121_394099_length_3154_multi_2_in_2_out_0_1_1139_-|RefSeq'],
        'Methanothermobacter thermautotrophicus': ['mgm4762935.3|contig_121_570908_length_16058_multi_2_in_2_out_2_10587_14405_-|RefSeq'],
        'Meiothermus ruber': ['mgm4762935.3|contig_121_457105_length_4459_multi_2_in_0_out_1_2831_4299_-|RefSeq']
    }
    org_dict = make_org_dict("./tests/sample/organism")
    for i  in test_dict.keys():
        assert test_dict[i] == org_dict[i], "mismatched org dict"


def test_get_seq_count():
    seq_count = get_seq_count("mgm4762935.3")
    print(seq_count)
    assert seq_count  == 109, "problem getting seqeunce count"
