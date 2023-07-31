#run all the necessary libraries
library(readxl)
library(data.table)
library(RSelenium)
library(netstat)
library(tidyverse)
library(openxlsx)
library(xlsx)


#create new tables
setwd("C:/Users/meder/OneDrive/Desktop/Thesis docs/inputs/")
whole_table <-read_excel("genes raw data.xlsx")
whole_table_2D <- whole_table[, c("Gene","2D_4w_MRTX")]
whole_table_3D <- whole_table[, c("Gene","3D_4w_MRTX")]

names(whole_table_2D)[which(names(whole_table_2D) == "2D_4w_MRTX")] <- "MRTX_2D_4w"
names(whole_table_3D)[which(names(whole_table_3D) == "3D_4w_MRTX")] <- "MRTX_3D_4w"

#sort the data and select only the first 100 genes with the highest beta-score
top_100_2D <- whole_table_2D[order(whole_table_2D$MRTX_2D_4w, decreasing = TRUE)[1:200], ]
top_100_3D <- whole_table_3D[order(whole_table_3D$MRTX_3D_4w, decreasing = TRUE)[1:200], ]

#Get the common genes in top_100_2D and top_100_3D
common_genes <- intersect(top_100_2D$Gene, top_100_3D$Gene)

# Create a new data frame that contains the common genes
common_genes <- data.frame(Gene = common_genes)

# Merge the common_genes data frame with the top_100_2D data frame to get MRTX_2D_4w values
common_genes <- merge(common_genes, top_100_2D, by = "Gene")

# Merge the common_genes data frame with the top_100_3D data frame to get MRTX_3D_4w values
common_genes <- merge(common_genes, top_100_3D, by = "Gene")
write.xlsx(common_genes, "common_genes.xlsx")

############SCRAPING PART#####################

#start the engine
rs_driver_object <- rsDriver(
  browser = "firefox",
  chromever = NULL,
  verbose = F,
  port = free_port(),
)

#start the client
remote_driver <- rs_driver_object$client
remote_driver$open()
#start the scraping
getGeneTable <-function(remote_driver, gene_code, path, filename){
  
  link <- paste0("https://www.genecards.org/")
  remote_driver$navigate(link)
  remote_driver$setTimeout(type = "page load", milliseconds = 5000)
  Sys.sleep(3.5)
  
  # gene input 
  
  gene <- tryCatch(
    {
      suppressMessages({
        remote_driver$findElement(using = "xpath", value = '/html/body/div[2]/div[2]/main/div/div[6]/div/div[2]/div[2]/div[1]/div/div/input')
      })
    },
    error = function(e) {
      NULL
    }
  )
  
  if (is.null(gene)) {
    Sys.sleep(1.5)
    gene <- tryCatch(
      {
        remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div[2]/main/div/div[6]/div/div[2]/div[2]/div[1]/div/div/input")
      },
      error = function(e) {
        NULL
      }
    )
  }
    if(!is.null(gene)){
      gene$sendKeysToElement(list(gene_code))
    }else{
      message("Can't find search element")
    }
    
    

  remote_driver$setTimeout(type = "page load", milliseconds = 5000)
  Sys.sleep(1.5)
  
  # search button
  
  
  btn <- tryCatch(
    {
      suppressMessages({
        remote_driver$findElement(using = "xpath", value = '/html/body/div[1]/div[2]/main/div/div[6]/div/div[2]/div[2]/div[1]/div/div/span/button')
      })
    },
    error = function(e) {
      NULL
    }
  )
  
  if (is.null(btn)) {
    Sys.sleep(1.5)
    btn <- tryCatch(
      {
        remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div[2]/main/div/div[6]/div/div[2]/div[2]/div[1]/div/div/span/button")
      },
      error = function(e) {
        NULL
      }
    )
  }
  
  if(!is.null(btn)){
    btn$clickElement()
  }else{
    message("Can't find button element")
  }
    
 
  
  message("Your gene has been selected...")
  Sys.sleep(3.5)
  
  #html
  pgSource <- read_html(remote_driver$getPageSource()[[1]],encoding = "UTF-8")
  
  smr1 <- html_text(html_node(pgSource,xpath = "/html/body/div[1]/div[2]/div/div/main/div[2]/div/div/section[2]/div[1]/ul/li/p"))
  smr1 <- gsub('\n','',smr1)
  smr1 <- gsub(';','',smr1)
  smr2 <- html_text(html_node(pgSource,xpath = "/html/body/div[1]/div[2]/div/div/main/div[2]/div/div/section[2]/div[2]/p"))
  smr2 <- gsub('\n','',smr2)
  smr2 <- gsub(';','',smr2)
  
  tmp <- data.table(Gene=gene_code,Entrez=smr1,GeneCards=smr2) 
  
  if(!is.null(tmp)){
    
    message("I'm writing your file..")
    
    filename <- paste0(path, filename, ".csv")
    
    if (!file.exists(filename)) {
      con <- file(description = filename, "w")
      write.table(tmp, con, row.names = FALSE, append = FALSE, sep = ";", col.names = TRUE, quote = FALSE)
    } else {
      con <- file(description = filename, "a")
      write.table(tmp, con, row.names = FALSE, append = TRUE, sep = ";", col.names = FALSE, quote = FALSE)
    }
  }
  close(con)
}

grid<-setDT(copy(common_genes))
x <-0

final<- lapply(1:nrow(grid),function(i) {
  
  x <<- x +1
  message("Gene", x,"...Name: ", grid[i,Gene])
  getGeneTable(remote_driver = remote_driver,
               gene_code = grid[i,Gene],
               filename = "genes info",
               path = "C:/Users/meder/OneDrive/Desktop/Thesis docs/inputs/"
               )
  Sys.sleep(1.5)
  
}
)

remote_driver$close()
