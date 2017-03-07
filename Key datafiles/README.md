I attempted to store my final patent datasets here, but realized they were too big for github. Currently files are archived in my personal Google Drive. A future application will pull data from the PatentsView API to generate patent density graphs on demand, making the archived data moot.

Datafiles that need archiving or replacing are

filename | description | R | STATA | tab
--- | --- | --- | :---: | :---: | :---:
bib_plus_mcfcls | patent bibliographic data merged with class/subclasses | 1 | 1 | 1
bib_plus_primary_class2 | patent bibliographic data merged with top-level (e.g. p) class only | 1 | 1 | 1
mcf_cumsum | counts and cumulative sum (since 1975) of all patents by class/subclass | 1 | 1 | 1
mcf_cumsum_1990 | counts and cumulative sum (since 1990) of all patents by class/subclass | 1 | 1 | 1
pmcf_cumsum	counts | and cumulative sum (since 1975) of all patents by top-level class | 1 | 1 | 1
pmcf_cumsum_1990 | counts and cumulative sum (since 1990) of all patents by  top-level class | 1 | 1 | 1
Pats_useme | TechMatch patents merged with bibliographic data and top-level class descriptor, includes cleaned laboratory names | 1 |  | 1
Patents by Lab | original raw list of TechMatch data |  |  | 1
Pats_subclass_labeled | like Pats_useme but with one entry for every patent number + class/subclass | 1 |  | 1
