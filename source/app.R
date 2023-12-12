library(jsonlite)
library(tibble)
library(shiny)

quran <- fromJSON("https://api.quran.com/api/v3/chapters/")

chapters <- tibble(indices = quran$chapters$chapter_number, chapters = quran$chapters$name_simple)
chapters <- chapters[order(chapters$chapters),]

binary_search <- function(dataset, keyword, indices) {
    # Sort the data if indices are included
    dataset <- data.frame("data" = dataset)
    if (!missing(indices)) {
        dataset <- data.frame(
            "data" = dataset,
            "indices" = indices
            )
    }
    
    for (i in 1:length(dataset$data)) {
        dataset$data[i] <- gsub(" ", "", gsub("'", "", gsub("-", "", tolower(dataset$data[i]))))
    }
    
    dataset <- dataset[order(dataset$data),]
    
    # Search
    first <- 1
    last <- length(dataset$data)
    position <- -1
    middle <- 0
    found <- FALSE
    
    while ((!found) && (first <= last)) {
        middle <- round(abs((first + last) / 2))
        
        set <- dataset$data[middle]
        key <- gsub(" ", "", gsub("'", "", gsub("-", "", tolower(keyword))))
        
        print(set)
        print(key)
        print(middle)
        
        if (set == key) {
            found <- TRUE
            position <- middle
        } else if (set > key) {
            last <- middle - 1
        } else {
            first <- middle + 1
        }
    }
    
    if (!missing(indices) && position != -1) {
        return (dataset$indices[position])
    } else {
        first <- 1
        last <- length(dataset$data)
        middle <- 0
        found <- FALSE
        
        while ((!found) && (first <= last)) {
            middle <- round(abs((first + last) / 2))
            
            set <- dataset$data[middle]
            key <- gsub(" ", "", gsub("'", "", gsub("-", "", tolower(keyword))))
            
            if (match_string(key, set)) {
                found <- TRUE
                position <- middle
            } else if (match_string(set, key)) {
                found <- TRUE
                position <- middle
            } else if (set > key) {
                last <- middle - 1
            } else {
                first <- middle + 1
            }
        }
        return (dataset$indices[position])
    }
}

match_string <- function(haystack, needle) {
    # Convert string to vector (not necessary but makes things easier)
    haystack <- strsplit(haystack, "")[[1]]
    needle <- strsplit(needle, "")[[1]]
    
    i <- 1
    j <- 1
    match <- FALSE
    
    while ((match == FALSE) && (i < length(haystack))) {
        if (needle[j] == haystack[i]) {
            j <- j + 1
            if (j == length(needle)) {
                match <- TRUE
            }
        } else {
            j <- 1
        }
        i <- i + 1
    }
    
    return (match)
}

capitalize <- function(string) {
    return (paste(sep = "",
        toupper(substr(string, 1, 1)),
        substr(string, 2, nchar(string))))
}

make_ordinal <- function(number) {
    suffixes <- c("st", "nd", "rd")
    
    number <- as.character(number)
    len <- nchar(number)
    last_digit <- substr(number, len, len)
    suffix <- "th"
    
    before_last_digit <- "0"
    if (len > 1) {before_last_digit = substr(number, len - 1, len - 1)}
    if ((last_digit %in% c("1", "2", "3")) && (before_last_digit != "1")) {suffix <- suffixes[as.integer(last_digit)]}
    
    print(suffix)
    
    return (paste(number, suffix, sep = ""))
}

###
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    div(style = "max-width: 1000px; margin: 0 auto; text-align: justify;",
        div(style = "font-size: 2em",
            titlePanel(
                div("Chapters in The Noble Quran", class = "title"),
                windowTitle = "Chapters in The Noble Quran"
                ),
            div(
                span(style = "h4", "by Dimas Atha Putra"),
                span(style = "font-size: .5em", "230605110052")
                )
            ),
        
        hr(),
        
        sidebarLayout(
            sidebarPanel(
                textInput("keyword", "What chapter?")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                div(style="max-width: 1000px; margin: 0 auto; text-align: center;",
                    uiOutput("found")
                )
            )
        ),
        uiOutput("info")
    ),
)

server <- function(input, output) {
    output$found <- renderUI({
        keyword <- "Maryam"
        if (input$keyword != "") {
            keyword <- input$keyword
        }
        
        pos <- binary_search(chapters$chapters, keyword, chapters$indices)
        
        if (pos != -1) {
            chapter_name <- quran$chapters$name_complex[pos]
            chapter_number <- quran$chapters$chapter_number[pos]
            chapter_verses <- quran$chapters$verses_count[pos]
            chapter_revelated <- quran$chapters$revelation_order[pos]
            chapter_town <- quran$chapters$revelation_place[pos]
            
            div(style = 'font-family: "Century"',
                h2(paste(chapter_name, ", the ", make_ordinal(chapter_number), " chapter in The Noble Quran.", sep = "")),
                p(paste("Consists of ", chapter_verses, " verses, it is the ", make_ordinal(chapter_revelated), " revelation in the town of ", capitalize(chapter_town), sep = ""))
            )
        } else {
            div(style = 'font-family: "Century"',
                h3("Chapter not found."),
                p("Try guessing for the correct transliteration of the chapter. It should be \"Al-Fatihah\" instead of \"AlFatihah\"")
            )
        }
    })
    
    output$info <- renderUI({
        keyword <- "Maryam"
        if (input$keyword != "") {
            keyword <- input$keyword
        }
        
        pos <- binary_search(chapters$chapters, keyword, chapters$indices)
        
        if (pos != -1) {
            chapter_info <- fromJSON(paste("https://api.quran.com/api/v3/chapters/", pos, "/info", sep = ""))
            div(
                div(style = 'font-family: "Amiri"; font-size: 10vw; text-align: center', quran$chapters$name_arabic[pos]),
                HTML(gsub("\\\\", "", chapter_info$chapter_info$text))
            )
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
