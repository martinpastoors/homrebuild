##############################################
# utility functions
##############################################

#' Progress function
#'
#' Non exported function, plots the prgress of an iterative procedure using "[=>   ]", "[==> ]", etc.
#'
#' @param p Value
#' @return NULL
loader <- function(p)
{
  if (p==0) cat("0%                       50%                     100%\n")
 str <- paste0(rep(c("\r[", "=", ">", " ", "]"), c(1, floor(p*50), 1, 50 - floor(p*50), 1)), collapse = "")
 cat(str)
 utils::flush.console()
 if (floor(p) == 1) cat("\n")
}

fGetValsScan <- function(Nums,RPs){
  
  #Nums - character vector with values or the name of refererence points to be substituted
  #RPs - named vector of reference points 

  if (length(Nums)==1){
    if (is.na(Nums)){
      return(NA)
    }
  }

  #extract the numeric ones
  ret <- as.numeric(Nums[!is.na(suppressWarnings(as.numeric(Nums)))])
  
  #now the specified reference points
  if (sum(is.na(suppressWarnings(as.numeric(Nums))))>0) {
    ret <- c(ret,as.numeric(RPs[Nums[is.na(suppressWarnings(as.numeric(Nums)))]]))
  }
  
  #add a warning if there are unmatched RPs
  
  sort(ret)
  
}


# Load RData function
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# lowcase function
lowcase <- function(df) {
  names(df) <- tolower(names(df)) %>% gsub("\\?|\\s+|\\.+|_+|\\(|\\)","",.) 
  df
}

# Theme publication (for ggplot)
theme_publication <- function(base_size=14, base_family="Helvetica") {
  # library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title       = element_text(face = "bold",size = rel(0.8), hjust = 0.0),
            plot.margin      = unit(c(10,5,5,5),"mm"),
            plot.background  = element_rect(colour = NA),
            text             = element_text(),
            axis.title       = element_text(face = "bold",size = rel(1)),
            axis.title.y     = element_text(angle=90,vjust =2),
            axis.title.x     = element_text(vjust = -0.2),
            axis.text        = element_text(), 
            axis.line        = element_line(colour="black"),
            axis.ticks       = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            panel.border     = element_rect(colour="black" , size=0.1),
            panel.background = element_rect(colour = NA),
            strip.background = element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text       = element_text(face="bold"),
            legend.key       = element_rect(colour = NA),
            legend.position  = "bottom",
            legend.direction = "horizontal",
            legend.key.size  = unit(0.2, "cm"),
            legend.spacing   = unit(0, "cm"),  # updated from legend.margin which is deprecated
            legend.title     = element_text(face="italic", size=rel(0.8))
    ))
}

