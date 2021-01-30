#HOW TO FIND PAPERS ON SCIENTIFIC JOURNALS################

# extract from scientific journals

# libraries suggested to make the packages working----------------
library(dplyr)
library(httr)
library(utils)
library(XML)
library(devtools)
library(knitr)
library(rmarkdown)
library(roxygen2)
library(testthat)
library(knitr)


#install.packages("aRxiv")
#install.packages("jstor")
#install.packages("RISmed")
#install.packages("easyPubMed")
#install.packages("wikifacts")

library(aRxiv)
library(jstor)
library(RISmed)
library(pubmed.mineR)
library(easyPubMed)
library(wikifacts)

ls("package:aRxiv")
ls("package:jstor")
ls("package:RISmed")
ls("package:easyPubMed")
ls("package:wikifacts")

list.files(system.file(package = "aRxiv"), recursive = T, full.names = T)

#SEARCH#######################################################
# search for more packages libraries needed:---------------------
library(tidyverse)  # for data manipulation
library(dlstats)    # for package download stats
library(pkgsearch)  # for searching packages


medPkg <-  pkg_search(query="medicine",size=200) #87 packages
head(medPkg)
## narrowed down to orphaned packages and packages with a score less than 190.

medPkgShort <- medPkg %>% #55 packages
  filter(maintainer_name != "ORPHANED", score > 190) %>%
  select(score, package, downloads_last_month) %>%
  arrange(desc(downloads_last_month))

length(medPkgShort$package)
head(medPkgShort)

shortList <- medPkgShort$package

downloads <- cran_stats(shortList)

head(downloads)
names(downloads)

ggplot(downloads, aes(end, downloads, group=package, color=package)) +
  geom_line() + geom_point(aes(shape=package)) +
  scale_y_continuous(trans = 'log2')

# my_shortList of selected packages

my_shortList<-c("aRxiv","jstor","RISmed","easyPubMed","wikifacts")
my_downloads <- cran_stats(my_shortList)

head(my_downloads)
names(my_downloads)

ggplot(my_downloads, aes(end, downloads, group=package, color=package)) +
  geom_line() + geom_point(aes(shape=package)) +
  scale_y_continuous(trans = 'log2')


#SELECT###########################################################

# wikifacts search-----------------

# to be able to use "wiki_define()" function it needs to install the package from github
# devtools::install_github("keithmcnulty/wikifacts")

animals<- data.frame(
  name=c("kangaroo",
         "kookaburra",
         "wombat",
         "tasmanian devil",
         "quokka")
)

# get the literature from the web
knitr::kable(
  animals%>%
    dplyr::mutate(definition = wiki_define(name,sentence=1))
)

# arXiv search-------------------------
url
utils::browseURL(url)


search_epi<-arxiv_search("ti:injuries")
arxiv_open(search_epi,limit=2)
# https://arxiv.org/abs/nlin/0010012v1  (see if it is good for fire)
# https://www.semanticscholar.org/paper/A-simple-model-for-the-spatial-spread-and-control-K%C3%A4ll%C3%A9n-Arcuri/e46c1f276aea9f04e0c7ba6f4d30c42069f423e3 (see if it is good for rabies)



data(arxiv_cats)

arxiv_count(query = "cat:stat.AP")


z <- arxiv_search('au:"Peter Hall" AND ti:deconvolution')
arxiv_open(z)


y <- arxiv_search('ti:injuries',limit = 4)
arxiv_open(y)


?arxiv_search

# jstor search--------------------

# Articles
# jst_get_article()
# jst_get_references()
# jst_get_footnotes()

# Books
# jst_get_book()
# jst_get_chapters()

# Both
# jst_get_authors()


# jst_get_article("example.xml")%>%
#  tidyr::gather(columns,rows)

# example found for extracting data from "facebook"
jst_import_zip(
  zip_archive = "facebook.zip",
  import_spec = jst_define_import(
    article = c(jst_get_article,
                jst_get_footnotes,
                jst_get_references),
    ngram2 = jst_get_ngram
  ),
  out_file = "out_file"
)


