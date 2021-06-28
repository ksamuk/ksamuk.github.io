library("tidyverse")
library("RefManageR")
library("fuzzyjoin")
library("lubridate")
#remotes::install_github("ROpenSci/bibtex")
#install.packages("fuzzyjoin")
#install.packages("RefManageR")
#install.packages("bibtex")

# bibtex citations
pub_bib <- ReadBib("data/lab_bibliography.bib", check = FALSE)
BibOptions(check.entries = FALSE, sorting ="ydnt", style = "citation", bib.style = "authoryear", cite.style = 'alphabetic')
pub_bib <- sort(pub_bib)

bib_indexes <- pub_bib %>% as.data.frame() %>% select(title)
bib_indexes$id <- 1:nrow(bib_indexes)
bib_indexes$title <- bib_indexes$title %>% stringr::str_squish()

# apa format citations
pub_apa <- read_lines("data/lab_bibliography.txt", skip_empty_rows = TRUE)
pub_apa <- data.frame(apa = pub_apa) %>%
  mutate(title = gsub(".*\\([0-9]{4}\\). ", "", apa) %>% gsub("\\..*", "", .))

# abstract metadata
pub_csv <- read_csv("data/lab_bibliography.csv") %>%
  filter(`Item type`!="Ph.D. Thesis") %>%
  select(Title, Abstract, `Date published`) %>%
  rename(title=Title, pub_date = `Date published`) %>%
  mutate(pub_date = ifelse(nchar(pub_date) == 7, paste0(pub_date, "-01"), pub_date)) %>%
  arrange(desc(pub_date)) %>%
  tibble

# links to files/urls/code/data
meta_df <- read_csv("data/lab_bibliography_links.csv")
 
pub_df <- pub_csv %>%
  stringdist_left_join(pub_apa, ignore_case = TRUE, max_dist = 4) %>%
  rename(title = title.x ) %>% 
  select(-title.y) %>%
  stringdist_left_join(bib_indexes, ignore_case = TRUE, max_dist = 4) %>%
  rename(title = title.x ) %>% 
  select(-title.y) %>%
  stringdist_left_join(meta_df, ignore_case = TRUE, max_dist = 4) %>%
  rename(title = title.x ) %>% 
  select(-title.y)
  
  
#@title
#@author_year@
#@journal_pages@
#@citation_text_apa
#@citation_text_bibtex
#@abstract
#@pdf_link@
#@html_link@
#@data_links@
#@code_links@

build_publication_card <- function(row_id){
  
  pub_df_sub <- pub_df_year[row_id,]
  row_id <- pub_df_sub$id
  
  apa <- pub_df_sub$apa
  bibtex <- toBibtex(pub_bib[pub_df_sub$id]) %>% as.character %>% paste(collapse ="\n")
  
  title <- pub_df_sub$title
  author_year <- gsub("\\).*", "", apa) %>% paste0(").")
  pub_year <- gsub("[^0-9]", "", author_year)
  journal_pages <- gsub(".*\\([0-9]{4}\\). ", "", apa) %>% gsub(".*[a-zA-z]+\\. ", "", .)
  
  abstract <- pub_df_sub$Abstract
  
  data_links <- "TBD_link_to_data"
  code_links <- "TBD_link_to_code"
  
  pub_card <- read_lines("components/publication_card.html")
  
  # title
  title_row <- Map(grepl,"@title@",pub_card) %>% unlist %>% which %>% as.numeric
  pub_card[title_row] <- gsub("@title@", title, pub_card[title_row])
  
  # author_year
  author_year_row <- Map(grepl,"@author_year@",pub_card) %>% unlist %>% which %>% as.numeric
  pub_card[author_year_row] <- gsub("@author_year@", author_year, pub_card[author_year_row])
  
  # journal pages
  journal_pages_row <- Map(grepl,"@journal_pages@",pub_card) %>% unlist %>% which %>% as.numeric
  pub_card[journal_pages_row] <- gsub("@journal_pages@", journal_pages, pub_card[journal_pages_row])
  
  # citation text apa
  pub_card <- lapply(pub_card, function(x) gsub("@citation_text_apa@", apa, x))
  
  # citation text bibtex
  pub_card <- lapply(pub_card, function(x) gsub("@citation_text_bibtex@", bibtex, x))
  
  # abstract
  abstract_row <- Map(grepl,"@abstract@", pub_card) %>% unlist %>% which %>% as.numeric
  pub_card[abstract_row] <- gsub("@abstract@", abstract, pub_card[abstract_row])
  
  # pdf link
  if(grepl(".pdf", pub_df_sub$pdf)){
    pdf_link <- paste0("files/papers/", pub_df_sub$pdf)
  } else{
    pdf_link <-pub_df_sub$url
  }
  
  pub_card <- lapply(pub_card, function(x) gsub("@pdf_link@", pdf_link, x))
  
  # html link
  pub_card <- lapply(pub_card, function(x) gsub("@html_link@", pub_df_sub$url, x))
  
  # data links
  # TBD 
  
  # code links
  # TBD
  
  # uniquify nav tabs
  pub_card <- lapply(pub_card, function(x) gsub("href=\"#info", paste0("href=\"#info", row_id), x))
  pub_card <- lapply(pub_card, function(x) gsub("href=\"#abstract", paste0("href=\"#abstract", row_id), x))
  pub_card <- lapply(pub_card, function(x) gsub("href=\"#data", paste0("href=\"#data", row_id), x))
  
  pub_card <- lapply(pub_card, function(x) gsub("id=\"info", paste0("id=\"info", row_id), x))
  pub_card <- lapply(pub_card, function(x) gsub("id=\"abstract", paste0("id=\"abstract", row_id), x))
  pub_card <- lapply(pub_card, function(x) gsub("id=\"data", paste0("id=\"data", row_id), x))
  
  # uniquify citation buttons
  pub_card <- lapply(pub_card, function(x) gsub("id=\"pubbutton_apa", paste0("id=\"pubbutton_apa", row_id), x))
  pub_card <- lapply(pub_card, function(x) gsub("id=\"pubbutton_bib", paste0("id=\"pubbutton_bib", row_id), x))
  pub_card <- lapply(pub_card, function(x) gsub("@pub_year@", paste0(pub_year), x))
  
  
  write_lines(pub_card, file = "../publications.html", append = TRUE)
  
}

file.remove("../publications.html")
pub_head <- read_lines("components/publication_head.html")
write_lines(pub_head, file = "../publications.html", append = TRUE)

pub_df$year <- gsub("-.*", "", pub_df$pub_date)
years <- unique(pub_df$year)
for (i in years){
  
  pub_df_year <- pub_df %>%
    filter(year == i)
  
  #year_header <- paste0("<h2 class=\"fh5co-uppercase-heading-sm text-center\">",i,"</h2>")
  #write_lines(year_header, file = "../publications.html", append = TRUE)
  
  pub_cards <- lapply(1:nrow(pub_df_year), build_publication_card)
  
}

pub_tail <- read_lines("components/publication_tail.html")
write_lines(pub_tail, file = "../publications.html", append = TRUE)

file.copy("output/publications.html", "../publications.html", overwrite = TRUE)
  
  

