nicecurve <- function (expr, from = NULL, to = NULL, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = xname, ylab = NULL, log = NULL, 
          xlim = NULL, grid = FALSE, ylim = NULL, main = NULL,
          xaxis = NULL, yaxis = NULL, ...) 
{
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  } else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
          all.vars(sexpr))) 
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
                    xname), domain = NA)
    expr <- sexpr
  }
  if (dev.cur() == 1L && !identical(add, FALSE)) {
    warning("'add' will be ignored as there is no existing plot")
    add <- FALSE
  }
  addF <- identical(add, FALSE)
  if (is.null(ylab)) 
    ylab <- deparse(expr)
  if (is.null(from) || is.null(to)) {
    xl <- if (!is.null(xlim)) 
      xlim
    else if (!addF) {
      pu <- par("usr")[1L:2L]
      if (par("xaxs") == "r") 
        pu <- extendrange(pu, f = -1/27)
      if (par("xlog")) 
        10^pu
      else pu
    }
    else c(-5, 5)
    if (is.null(from)) 
      from <- xl[1L]
    if (is.null(to)) 
      to <- xl[2L]
  }
  lg <- if (length(log)) 
    log
  else if (!addF && par("xlog")) 
    "x"
  else ""
  if (length(lg) == 0) 
    lg <- ""
  if (grepl("x", lg, fixed = TRUE)) {
    if (from <= 0 || to <= 0) 
      stop("'from' and 'to' must be > 0 with log=\"x\"")
    x <- exp(seq.int(log(from), log(to), length.out = n))
  } else x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())
  if (length(y) != length(x)) 
    stop("'expr' did not evaluate to an object of length 'n'")
  yl <- if(is.null(ylim))
    ylim = c(min(floor(min(y)*1.05),-floor(min(y)),-.1*range(y),-2), ceiling(max(y)*1.05))
       else ylim
  if (is.null(main))
    main = expr
  if (isTRUE(add)) 
    lines(x = x, y = y, type = type, ...)
  else plot(x = x, y = y, type = type,  
            xlim = xlim, log = lg, bty='n', xaxt='n', yaxt='n', 
            ylab="", xlab="", col="steelblue", lwd=2, ylim=yl, main=main, ...)
  
  
  
  stepx <- par()$xaxp
  stepy <- par()$yaxp
  
  
  gridx <- seq(stepx[1L], stepx[2L], stepx[3L]/2)
  gridy <- seq(stepy[1L], stepy[2L], stepy[3L])
  
  xran <- stepx[2L] - stepx[1L]
  yran <- stepy[2L] - stepy[1L]
  
  if (grid)
    abline(v=gridx, h=gridy, lty=3, col="gray")
  
  xunit <- par("pin")[1L]/(xran)
  yunit <- par("pin")[2L]/(yran)
  
  if (is.null(xaxis)){
    gridx <- gridx[gridx!=0]
  } else{
    gridx <- xaxis[xaxis!=0]
  }
  if (is.null(yaxis)){
    gridy <- gridy[gridy!=0]
  } else{
    gridy <- yaxis[yaxis!=0]
  }
  
  xtip <- c(mean(c(par()$usr[1L],stepx[1L])),mean(c(par()$usr[2L],stepx[2L])))
  ytip <- c(mean(c(par()$usr[3L],stepy[1L])),mean(c(par()$usr[4L],stepy[2L])))
  
  lines(xtip, c(0,0), lwd=2)
  lines(c(0,0), ytip, lwd=2)
  
  points(xtip[2L],0, pch=-9658)
  points(0,ytip[2L], pch=17, cex=1.2)
  
  text(xtip[2L],0, expression(x), pos=3)
  text(0,ytip[2L], expression(y), pos=4)
  
  xtw <- 0.05/xunit
  ytw <- 0.05/yunit
  
  for (gy in gridy){
    lines(c(-xtw,xtw), rep(gy,2), lwd=2)
    text(0, gy, gy, pos=2, cex=0.8)      
  }
  for (gx in gridx){
    lines(rep(gx,2), c(-ytw,ytw), lwd=2)
    text(gx, 0, gx, pos=1, cex=0.8)
  }
  
  
  
  
  
  invisible(list(x = x, y = y))
}