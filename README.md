## Data repository for the doctoral thesis of Albin Blaschka
"With teeth and claws: Managing and restoring ecosystem services of an alpine cultural landscape"
(Original german title: "Mit Zähnen und Klauen: Erhalt und Wiederherstellung von Ökosystemleistungen einer alpinen
Kulturlandschaft")

With this thesis, based on a national project with the title **"Innovative High-Pasture Management with Sheep for
a Sustainable Land-Use of an Alpine Cultural Landscape"**, it was possible to develop a sustainable land use concept
for an alpine, Central European cultural landscape, fulfilling requirements of agricultural practice, touristic needs
and societal challenges.

The full thesis (in german) is available via Zenodo (https://zenodo.org/record/18296). An extended abstract both in german and
[english] (https://github.com/albinblaschka/teeth-and-claws/wiki/With-teeth-and-claws...-English-abstract) is available on the [wiki]
(https://github.com/albinblaschka/teeth-and-claws/wiki/).

Here on GitHub the original data and accompanying R-Scripts are available.


### The scripts

#### Analysis of cover in different functional groups
- Script: [analysis_functionalgroups.R](https://github.com/albinblaschka/teeth-and-claws/blob/master/analysis_functionalgroups.R);
- Data: [data_functionalgroups_development.csv](https://github.com/albinblaschka/teeth-and-claws/blob/master/data_functionalgroups_development.csv)
- Data format: data_functionalgroups_development.csvt

To study the changes in vegetation and restoration of pastures already infested with shrubs by targeted pasturing, a trial
following a factorial design was set up, with four replicates. The development of three functional groups (dwarf shrubs,
herbs and open soil) is analysed with a GLM, for details see the comments in the script.

**Columns:**

```
variant: Factor, 4 levels "A","B","C","D"
      A: Null variant, no impact
      B: First mown, followed by targeted pasturing
      C: Only targeted pasturing
      D: Extensive pasturing, browsing
year: int - denotes the duration of the trial (4 years); variant D was started one year later as the other variants (3 years)
replicate: int - four replicates were used for all variants
functionaltype: Factor, 3 levels - "open soil","dwarf shrubs", "herbs"
frequency: cover of the functional types, measured with a quadrat (1m², divided by 10cm by 10cm): Dependent variable
```
