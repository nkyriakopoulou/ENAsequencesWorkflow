# Analysis workflow for the sequences stored in ENA

This repository contains the set of scripts to analyze the European 
Nucleotide Archive (ENA) ‘Nucleotide sequences’ dataset. The dataset 
was downloaded from the ENA portal API (https://www.ebi.ac.uk/ena/portal/api/) by using curl and Git CMD. 

This study aimed to explore the extent to which sequences are 
associated with source material identifiers during their 
submisssion to ENA and quanitfy those that are linked to 
a Darwin Core (DwC) - structured idnetifier. The DwC - like 
structures (institutionCode:collectionCode:catalogNumber) 
effectively link sequences to a physical specimen stored 
in a Natural Science Collection (NSC). Three recently 
published projects/studies that have analysed sequences 
obtained from NSC specimens were also examined in order 
to evaluate how researchers report source material and 
if they conform to the International Nucleotide Sequence 
Database Collaboration (INSDC) guidelines.

This repository is entirely made of R scripts. 
The scripts require R version 4.0.3 and above.
