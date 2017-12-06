#### ---- STL Microbrew Scrape ---- ####
# Matt Milunski
# Scrape Civil Life, Schlafly, 4 Hands, Urban Chestnut

# packages ----
library(rvest)
library(data.table)

# scrape data from schlafly ----
## function to get beer names
beer_names <- function(url_link, css) {
    require(rvest)
    ## read the html page into R and extract text
    beer_names <- read_html(url_link) %>% 
        html_node(css = css) %>% 
        html_text(trim = TRUE)
    ## remove punctuation, numbers, and the IBU/ABV units
    beer_names <- gsub("[[:punct:]]|[[:digit:]]|*IBU*|*ABV*|*Available*|*year*|*round*|*exclusively at select Schnucks Markets*", 
                       " ", beer_names)
    ## remove month names
    beer_names <- gsub("January|February|March|April|May|June|July|August|September|October|November|December",
                       "", beer_names)
    ## split string along 2 or more spaces
    name_split <- strsplit(beer_names, "[ ]{2, }")
    ## convert to data.frame
    name_df <- data.frame(name_split, stringsAsFactors = FALSE)
    ## provide name to data.frame and return result
    names(name_df) <- "beer"
    name_df
}

## beer chart scraper
beer_chart <- function(url_link, abv_ibu_css, details_css) {
    require(rvest)
    ## read the abv_ibu page into R and extract text
    abv_ibu <- read_html(url_link) %>% 
        html_node(css = abv_ibu_css) %>% 
        html_text(trim = TRUE)
    ## remove garbage
    chart <- gsub("ABV|IBU|\\:|\\%|[ ]{2, }", "", abv_ibu)
    ## split string and create dataframe
    chart <- strsplit(chart, "\\|") %>% 
        data.frame(., stringsAsFactors = FALSE) %>% 
        t()
    row.names(chart) <- NULL
    colnames(chart) <- c("abv", "ibu")
    # get details_css
    details <- read_html(url_link) %>% 
        html_node(css = details_css) %>% 
        html_text(trim = TRUE)
    details <- gsub("", "", details)
    details
}

## scrape the names of schlafly beer for each series
schlafly_yr_rnd <- beer_names("http://schlafly.com/beers/#/year-round", 
                              '#tabs > div.chart.active > ul')
schlafly_core_ssn <- beer_names("http://schlafly.com/beers/#/core-seasonal",
                                '#tabs > div:nth-child(3) > ul')
schlafly_spec_rel <- beer_names("http://schlafly.com/beers/#/special-release", 
                                '#tabs > div:nth-child(4) > ul')
schlafly_can_sess <- beer_names("http://schlafly.com/beers/#/can-sessions", 
                                '#tabs > div:nth-child(5) > ul')
schlafly_wood_age <- beer_names("http://schlafly.com/beers/#/wood-aged", 
                                '#tabs > div:nth-child(6) > ul')
schlafly_btl_con <- beer_names("http://schlafly.com/beers/#/bottle-conditioned",
                               '#tabs > div:nth-child(7) > ul')
schlafly_limited <- beer_names("http://schlafly.com/beers/#/limited-edition", 
                               '#tabs > div:nth-child(8) > ul')
schlafly_draft <- beer_names("http://schlafly.com/beers/#/draft-only", 
                             '#tabs > div:nth-child(9) > ul')

## bind data.frames
schlafly_list <- list(schlafly_yr_rnd, schlafly_core_ssn, schlafly_spec_rel,
                      schlafly_can_sess, schlafly_wood_age, schlafly_btl_con,
                      schlafly_limited, schlafly_draft)
schlafly_rbind <- do.call(rbind, schlafly_list)
schlafly_rbind$beer <- gsub("N lack IPA", "Black IPA", schlafly_rbind$beer)

## create URLs
schlafly_rbind$url <- paste0("http://schlafly.com/beers/styles/",
                             tolower(gsub("\\s", "-", schlafly_rbind$beer)),
                             "/")







