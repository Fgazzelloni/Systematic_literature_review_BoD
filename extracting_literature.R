##################################

#attempt to extraction for literature###############

library(knitr)
library(dplyr)
library(wikifacts)
library(devtools)


#devtools::install_github("keithmcnulty/wikifacts")

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


###############################################
library(jstor)

library(RISmed)

library(pubmed.mineR)

##############################################

#Atricles
jst_get_article()
jst_get_references()
jst_get_footnotes()
#Books
jst_get_book()
jst_get_chapters()
#Both
jst_get_authors()


jst_get_article("example.xml")%>%
  tidyr::gather(columns,rows)

```{r}
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
```



















