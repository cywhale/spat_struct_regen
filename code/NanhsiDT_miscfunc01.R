# misc functions used in my Nanhsi-related papers
############ Parsing scientific names utilities
############ modified from word_pre_bracket, delete xxx; so that the scientific name can be more accurate
############ change 'v' to 'var'
############ sympplify_tw : take first two words
############ 20160623 add : handle (cf. : confer speceies) and merged into odbapi.R
#' Parse scientific names to simplified form
#'
#' param x A character vector of scentific names
#' param trim.cf_sp A boolean value to trim "cf." in scientific names which means confer species (default) or not
#' param pattern.var_sp A character string indicates abbreviation of variety of species, like "var."(default).
#' param trim.var_sp A boolean value to trim "var." and following words (usually means variety) in scientific names (see pattern.var_sp) or not (default).
#' param trim.auther_year_in_bracket A boolean value to trim strings in bracket (default, usually means authors and year) or not.
#' param trim.dummy_num A boolean value to trim leading or trailing non-alphabet symbols/numbers (default) or not.
#' param simplify_two A boolean value to trim scientific names to (maximum) two words only or not (default).
#' param trim.spp_abbrev A boolean value to trim trailing "spp."/"sp." or not (default).
#' return A charcter vector of simplified scientific names
#' examples
#' sciname_simplify(c("Aricidea sp. A", "Armandia cf. leptocirrus","Thalenessa spinosa asitica","Ablennes hians (Valenciennes; 1843)"), simplify_two=TRUE, trim.spp_abbrev=TRUE)
#' > [1] "Aricidea" "Armandia leptocirrus" "Thalenessa spinosa" "Ablennes hians"
#' rdname sciname_simplify
#  export
sciname_simplify <- function (x, trim.cf_sp = TRUE,
                              pattern.var_sp = "var.", trim.var_sp = FALSE,
                              trim.auther_year_in_bracket = TRUE,
                              trim.dummy_num = TRUE,
                              simplify_two = FALSE,
                              trim.spp_abbrev= FALSE) {
  if (length(x)==0) return(c())
  if (all(is.na(x))) return(rep(NA_character_, length(x)))

  trimx <- gsub("^\\s+|\\s+$", "", gsub("\\s{1,}", " ", as.character(x)))

  if (trim.cf_sp) {
    trimx <- gsub("\\b(cf\\. )","",trimx)
  }

  trimx <- gsub("\\sv{1}(?:\\.{0,}|ar{0,1}\\.{0,})\\s", paste0(" ",pattern.var_sp," "), trimx)

  wlx <- nchar(trimx)+1
  if (trim.auther_year_in_bracket) {
    wl  <- regexpr("\\(",paste0(trimx,"("))
    wl1 <- regexpr("(?:\\s|\\spage\\s)[a-zA-Z0-9]+(?:;|\\))",trimx)  ### ex: "Phalacroma rotundatum Lachmann) Kofoid And Michener ]- "
    wl1[wl1<0] <- nchar(trimx[which(wl1<0)])+1
    wl2 <- regexpr("\\s\\)",paste0(trimx," )"))
    wl3 <- regexpr(paste0("\\s",pattern.var_sp, "(?:\\s{1}[a-zA-Z]+\\s{0,1})"),trimx)   ## ex: "Ceratium vultur var sumatranum Steeman Nielsen; 1934)"
    wl3[wl3>0] <- wl3[wl3>0]+attributes(wl3)$match.length[wl3>0]
    wl3[wl3<0] <- nchar(trimx[which(wl3<0)])+1
    wlx <- pmin(wl,wl1,wl2,wl3)
  }
  if (trim.var_sp) {
    wl4 <- regexpr(paste0("\\s",pattern.var_sp, "(?:\\s{1}[a-zA-Z]+\\s{0,1})"),trimx)
    wl4[wl4<0] <- nchar(trimx[which(wl4<0)])+1
    wlx <- pmin(wlx,wl4)
  }

  trimx <- gsub("\\s+$", "", substr(trimx,1,wlx-1))

  if (trim.dummy_num) {
    #trimx<- gsub("^[^a-zA-Z]+|[^a-zA-Z]+$", "", trimx)
    trimx <- gsub("^[^a-zA-Z]+|(?!\\.)[^a-zA-Z]+$","", trimx, perl=TRUE)
  } else {
    #trimx<- gsub("^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$", "", trimx)
    trimx <- gsub("^[^a-zA-Z0-9]+|(?!\\.)[^a-zA-Z0-9]+$", "", trimx, perl=TRUE)
  }

  # Tolower second word
  #wlx <- nchar(trimx)+1
  wl1 <- attributes(regexpr("^[a-zA-Z0-9\\.]+",trimx))$match.length
  wl2 <- regexpr("\\s\\b[a-zA-Z0-9\\-\\.]+(?:\\s|$)",trimx, perl=T)
  wl2[wl2>0] <- wl2[wl2>0]+attributes(wl2)$match.length[wl2>0]-1
  #wl2[wl2<0] <- nchar(trimx[which(wl2<0)])+1
  wlx <- pmax(wl1,wl2)

  trimx <- paste0(substr(trimx,1,wl1),tolower(substr(trimx,wl1+1,wlx)),substr(trimx,wlx+1,nchar(trimx)))

  if (simplify_two) {
    if (trim.dummy_num) {
      #trimx<- gsub("^[^a-zA-Z]+|[^a-zA-Z]+$", "", trimx)
      trimx <- gsub("^[^a-zA-Z]+|(?!\\.)[^a-zA-Z]+$","", substr(trimx,1,wlx), perl=TRUE)
    } else {
      trimx <- substr(trimx,1,wlx)
    }
  }

  if (trim.spp_abbrev) {
    return(gsub("\\s{1,}", " ",gsub("\\s+$|\\.+$", "", gsub("\\b(sp\\.)|\\b(spp\\.)","",trimx))))
  } else {
    return(gsub("\\s+$", "", gsub("\\b((sp\\.)+$)|\\b((spp\\.)+$)|((\\w{0,})\\.+$)","\\2\\4\\6", trimx, perl=TRUE)))
  }
}

## plot functions
## require packages:
require(ggplot2)
require(grid)
require(gridExtra)
require(gtable)

## align widths of ggplot objects
align_widths <- function (p1,p2) {
  gA <- ggplotGrob(p1)
  gB <- ggplotGrob(p2)
  maxh = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  gA$widths[2:5] <- as.list(maxh)
  gB$widths[2:5] <- as.list(maxh)
}

## Shared legend, title, axes in multi-plots of ggplot2
## modified codes from https://goo.gl/O5nW4y provided by user5029763, in stackoverflow forum
ggplot_shared_axes <- function(..., elements = c('legend', 'title', 'yaxis', 'xaxis'), byrow=FALSE, plot=FALSE,
                               specify_ylab_bycol= c(), #NA_character_,
                               adj_xlab_pos = FALSE, adj_ylab_pos = FALSE
) {
  #require(gtable)s
  plots <- list(...)

  if (!'yaxis' %in% elements & length(specify_ylab_bycol)>0) {

    plotx <- lapply(plots, function(x, elements = elements){
      if('legend' %in% elements) x <- x + theme(legend.position="none")
      if('title' %in% elements) x <- x + theme(plot.title = element_blank())
      if('xaxis' %in% elements) x <- x + theme(axis.title.x = element_blank())
      #if('yaxis' %in% elements) x <- x + theme(axis.title.y = element_blank())
      x
    }, elements = elements)

    #debug
    #print("Start handle plotx")
    #print(length(plotx))

    for (i in 1:length(plotx)) {
      assign(paste0("g",i), ggplotGrob(plotx[[i]]))
      gx <- get(paste0("g",i))
      yax <- which(gx$layout$name=="ylab-l")
      pp <- c(subset(gx$layout, grepl("panel", gx$layout$name), se = t:r))

      gx[["grobs"]][[yax]]$children[[1]]$label <- specify_ylab_bycol[1]
      ny <- gx$grobs[[yax]]
      cols <- sort(unique(pp$l))

      if (length(cols)>=2) {
        for (j in 1:(length(cols)-1)) {
          # define y-axis labels
          ny$children[[1]]$label <- specify_ylab_bycol[j+1]

          ### control b= max(pp$b) or min(pp$b), you can up/down the label

          if (j==1) {
            assign(paste0("gx",i), gtable_add_cols(gx,gx$widths[gx$layout[yax, ]$l],pos = pp$l[j]))
          } else {
            assign(paste0("gx",i), gtable_add_cols(get(paste0("gx",i)),
                                                   gx$widths[gx$layout[yax, ]$l],pos = pp$l[j]))
          }

          if (adj_ylab_pos) {
            assign(paste0("gx",i), gtable_add_grob(get(paste0("gx",i)),
                                                   textGrob(ny$children[[1]]$label, gp=ny$children[[1]]$gp,#gpar(fontsize=12))
                                                            vjust=ny$children[[1]]$vjust,
                                                            hjust=ny$children[[1]]$hjust, rot=90),
                                                   #ny,
                                                   t = min(pp$t), l = pp$l[j]+1,
                                                   b = min(pp$b), r = pp$l[j]+1,
                                                   clip = "off", name = paste0("ylab-",j+1)))
          } else {
            assign(paste0("gx",i), gtable_add_grob(get(paste0("gx",i)),
                                                   #textGrob(ny$children[[1]]$label, gp=ny$children[[1]]$gp,#gpar(fontsize=12))
                                                   #        vjust=ny$children[[1]]$vjust,
                                                   #        hjust=ny$children[[1]]$hjust, rot=90),
                                                   ny,
                                                   t = min(pp$t), l = pp$l[j]+1,
                                                   b = min(pp$b), r = pp$l[j]+1,
                                                   clip = "off", name = paste0("ylab-",j+1)))
          }

          ### renew pp
          pp <- c(subset(get(paste0("gx",i))$layout, grepl("panel", get(paste0("gx",i))$layout$name), se = t:r))
        }
      }
      #debug
      #print("get one plotx")
      #print(i)
      #grid.arrange(get(paste0("gx",i)))
    }
  }

  #debug
  #print("Start handle plots")
  #print(i)
  g <- ggplotGrob(plots[[1]])$grobs

  if('legend' %in% elements) {
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lwidth <- sum(legend$width)
  }
  if('title' %in% elements) {
    title <- g[[grep("plot.title", sapply(g, function(x) x$name))]]
    theight <- sum(title$height)
  }
  if('xaxis' %in% elements) {
    #xx <- ggplot_build(plots[[1]])
    #g1 <- ggplot_gtable(xx)
    #xaxis <- g1$grobs[grepl("xlab", g1$layout$name)][[1]]
    xaxis <- g[[grep("axis.title.x", sapply(g, function(x) x$name))]]
    xheight <- sum(xaxis$height)
  }
  if('yaxis' %in% elements) {
    yaxis <- g[[grep("axis.title.y", sapply(g, function(x) x$name))]]
    ywidth <- sum(yaxis$width)
  }

  plots <- lapply(plots, function(x, elements = elements){
    if('legend' %in% elements) x <- x + theme(legend.position="none")
    if('title' %in% elements) x <- x + theme(plot.title = element_blank())
    if('xaxis' %in% elements) x <- x + theme(axis.title.x = element_blank())
    if('yaxis' %in% elements) x <- x + theme(axis.title.y = element_blank())
    x
  }, elements = elements)


  if (!'yaxis' %in% elements & length(specify_ylab_bycol)>0) {
    #print("Start arrangeGrob plotx")
    #print(length(plots))

    if (byrow) {
      #plots <- do.call(arrangeGrob, c(lapply(paste0("gx",1:length(plots)),get), nrow = 1, padding=0))
      plots <- do.call(arrangeGrob, c(sapply(paste0("gx",1:length(plots)),get, envir=sys.frame(sys.parent(0)), simplify=FALSE), nrow = 1, padding=0))
    } else {
      #plots <- do.call(arrangeGrob, c(lapply(paste0("gx",1:length(plots)),get), ncol = 1, padding=0))
      plots <- do.call(arrangeGrob, c(sapply(paste0("gx",1:length(plots)),get, envir=sys.frame(sys.parent(0)), simplify=FALSE), ncol = 1, padding=0))
    }
  } else {
    #print("Start arrangeGrob plotx")
    #print(length(plots))

    if (byrow) {
      plots <- do.call(arrangeGrob, c(plots, nrow = 1))
    } else {
      plots <- do.call(arrangeGrob, c(plots, ncol = 1))
    }
  }


  if('legend' %in% elements)
    plots <- arrangeGrob(plots, legend, nrow = 1, widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth))
  if('yaxis' %in% elements)
    if (adj_ylab_pos) {
      plots <- arrangeGrob(#yaxis,
        textGrob(yaxis$children[[1]]$label, gp=yaxis$children[[1]]$gp,#gpar(fontsize=12))
                 vjust=yaxis$children[[1]]$vjust,
                 hjust=yaxis$children[[1]]$hjust, rot=90),
        plots, nrow = 1, widths = grid::unit.c(ywidth, unit(1, "npc") - ywidth))
    } else {
      plots <- arrangeGrob(yaxis,
                           plots, nrow = 1, widths = grid::unit.c(ywidth, unit(1, "npc") - ywidth))
    }
  if('title' %in% elements)
    plots <- arrangeGrob(title, plots, ncol = 1, heights = grid::unit.c(theight, unit(1, "npc") - theight))
  if('xaxis' %in% elements)
    if (adj_xlab_pos) {
      plots <- arrangeGrob(plots, #xaxis,
                           textGrob(xaxis$children[[1]]$label, gp=xaxis$children[[1]]$gp,#gpar(fontsize=12))
                                    vjust=xaxis$children[[1]]$vjust,
                                    hjust=xaxis$children[[1]]$hjust),
                           ncol = 1, heights = grid::unit.c(unit(1, "npc") - xheight, xheight))
    } else {
      plots <- arrangeGrob(plots, xaxis,
                           ncol = 1, heights = grid::unit.c(unit(1, "npc") - xheight, xheight))
    }
  if (plot) {
    grid.arrange(plots)
  } else {
    plots
  }
}
