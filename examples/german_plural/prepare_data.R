##
## Prepare data set of German noun forms (extracted from TIGER Treebank)
##

GerNouns <- read.delim("tiger_nouns.txt.gz", header=FALSE, quote="", col.names=c("word", "number"), colClasses=c("character", "factor"), fileEncoding="utf8")

## extract some plausible features with more or less strong associations to plural
GerNouns <- transform(GerNouns,
	suff_e = grepl("e$", word, perl=TRUE),
	suff_n = grepl("n$", word, perl=TRUE),
	suff_s = grepl("s$", word, perl=TRUE),
	umlaut = grepl("[ÄÖÜäöü]", word, perl=TRUE),
	double_cons = grepl("([bcdfgklmnprqstwxz])\\1", word, ignore.case=TRUE, perl=TRUE)
)

## are there correlations between features and number?
with(GerNouns, prop.table(table(number, suff_e), margin=2))
with(GerNouns, prop.table(table(number, suff_n), margin=2))
with(GerNouns, prop.table(table(number, suff_s), margin=2))
with(GerNouns, prop.table(table(number, umlaut), margin=2))
with(GerNouns, prop.table(table(number, double_cons), margin=2))

## save data frame with features in .rda format
save(GerNouns, file="GerNouns.rda", compress="xz")
