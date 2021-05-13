
#' draw a table
#'
#' @param df data frame
#' @param widths numeric vector, width of column from df
#' @param align character, text-alignment code, one-character-per-column
#' @param main character, title on top of table
#' @param Rcssclass character, style class
#'
plot.table <- function(df, widths=rep(1, ncol(df)), align=rep("c", ncol(df)),
                      main="",
                      Rcssclass=c()) {
  
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("table",Rcssclass))
  
  xlim <- c(0, sum(widths))
  ylim <- c(-nrow(df), 1)
  align <- unlist(strsplit(align, ""))
  align.adj <- c("l"=0, "c"=0.5, "r"=1)
  
  # draw main body, the one column at a time
  parplot(xlim, ylim, type="n")
  xleft <- 0
  for (i in 1:ncol(df)) {
    iadj <- align.adj[align[i]]
    if (align[i]=="r") {
      xleft <- xleft + widths[i]
    } else if (align[i]=="c") {
      xleft <- xleft + widths[i]/2
    } 

    # draw the header and then the column values
    icolname <- colnames(df)[i]
    text(xleft, 0.5, gsub("_|\\.", " ", icolname),
         adj=c(iadj, 0.5),
         Rcssclass="header")
    text(rep(xleft, nrow(df)), 0.5-seq(1, nrow(df)),
         gsub("_", " ", df[[i]]),
         adj=c(iadj, 0.5), Rcssclass=c("cell", icolname))

    if (align[i]=="l") {
      xleft <- xleft + widths[i]
    } else if (align[i]=="c") {
      xleft <- xleft + widths[i]/2
    }
  }

  mtext(side=3, main, Rcssclass="main")
}

