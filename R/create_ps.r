

examples.create.ps = function() {
  library(restorepoint)
  library(stringtools)
  setwd("D:/libraries/RTutor2/RTutor2/vignettes/problemsets")
  sol.file = "Example2_sol.Rmd"
  create.ps(sol.file = sol.file)
  
  setwd("D:/libraries/RTutor2/examples")
  ps.name = "Example"; sol.file = paste0(ps.name,"_sol.Rmd")
  libs = NULL # character vector of all packages you load in the problem set
  #name.rmd.chunks(sol.file) # set auto chunk names in this file
  create.ps(sol.file=sol.file, ps.name=ps.name, user.name=NULL,libs=libs)
  show.shiny.ps(ps.name, load.sav=FALSE,  sample.solution=TRUE, is.solved=FALSE, catch.errors=TRUE, launch.browser=TRUE)

}


#' Generate a problem set from a solution file
#' 
#' Generates  _struc.r file, .rps file, empty problem set .r and .rmd files
#' and a sample solution .rmd file (overwrites existing files)
#' @export
create.ps = function(sol.file, ps.name=NULL, user.name= "ENTER A USER NAME HERE", sol.user.name="Jane Doe", dir = getwd(), header="", footer="", libs=NULL, stop.when.finished=FALSE, extra.code.file = NULL, var.txt.file = NULL, rps.has.sol=TRUE) {
  restore.point("create.ps")
  setwd(dir)
  txt = readLines(sol.file)
  txt =  name.rmd.chunks(txt=txt)
  te = parse.sol.rmd(txt=txt)
  if (!is.null(ps.name))
    te$ps.name = ps.name
  write.sample.solution(te=te, header=header,footer=footer, user.name=sol.user.name, ps.dir=dir)
  task.txt = write.empty.ps(te=te,  header=header,footer=footer, user.name=user.name, ps.dir=dir)
  rps = te.to.rps(te=te)
  
  # Store information about empty problem set in order to easily export
  # an html problem set into it
  task.txt = sep.lines(task.txt)
  rps$empty.rmd.txt = task.txt
  rps$empty.rmd.chunk.lines = get.chunk.lines(task.txt)
  rps$empty.rmd.user.name.line = which(str.starts.with(task.txt,"user.name = '"))[1]
  rps$empty.rmd.ps.dir.line = which(str.starts.with(task.txt,"ps.dir =  '"))[1]
  rps$empty.rmd.ps.file.line = which(str.starts.with(task.txt,"ps.file = '"))[1]
  
  
  rps$shiny.dt = make.shiny.dt(rps=rps, txt=task.txt)
  source.rps.extra.code(extra.code.file, rps)
  if (!is.null(var.txt.file)) {
    rps$var.dt = read.var.txt(var.txt.file)
  } else {
    rps$var.dt = NULL
  }
  rps$libs = libs
  rps$has.sol = rps.has.sol
  if (!rps.has.sol) {
    rps$cdt$sol.txt = rep("",NROW(rps$cdt))
  }
  
  save.rps(rps)
  remove.ups(ps.name=rps$ps.name)
  if (stop.when.finished) {
    stop.without.error("The problem set files have been succefully created.")
  }
}

source.rps.extra.code = function(extra.code.file, rps) {
  restore.point("source.rps.extra.code")
  # Source extra.code
  if (!is.null(extra.code.file)) {
    rps$extra.code.env = new.env()
    source(extra.code.file, local = rps$extra.code.env)
  } else {
    rps$extra.code.env = NULL
  }
}

examples.parse.sol.rmd = function() {
  library(restorepoint)
  library(stringtools)
  setwd("D:/libraries/RTutor2/RTutor2/vignettes/problemsets")
  sol.file = "Example2_sol.Rmd"
}

save.rps = function(rps,file = paste0(rps$ps.name,".rps")) {
  save(rps,file=file)
}


load.rps = function(ps.name=NULL,file = paste0(ps.name,".rps")) {
  load(file=file)
  rps
}


parse.sol.rmd = function(sol.file=NULL, txt=NULL) {
  te = get.empty.te()  
  if (is.null(txt))
    txt = readLines(sol.file)

  row = 0
  while (row<length(txt)) {
    row = row+1
    te <- parse.sol.line(row=row,txt=txt,te=te)
  }
  te
}


write.sample.solution = function(file = paste0(ps.name,"_sample_solution.Rmd"), sol.txt=te$sol.txt,ps.name=te$ps.name, te,...) {
  restore.point("write.sample.solution")
  sol.txt = include.ps.extra.lines(sol.txt, ps.file=file, ps.name=ps.name,te=te,...)
  writeLines(sol.txt, file)
}

write.empty.ps = function(file = paste0(te$ps.name,".Rmd"), task.txt=te$task.txt,ps.name=te$ps.name, te,...) {
  task.txt = include.ps.extra.lines(task.txt, ps.file=file, ps.name=ps.name,te=te,...)
  writeLines(task.txt, file)
  invisible(task.txt)
}

te.to.rps = function(te) {
  restore.point("te.to.rps")
  rps = new.env()
  
  copy.into.envir(source=te, dest=rps,
    names=c("ps.name","infos","awards")
  )
  ex.ind = 1
  
  ex.ind = 3
  
  # Create a data frame with chunk metadata
  li = lapply(seq_along(te$ex), function(ex.ind) {
    ex = te$ex[[ex.ind]]
    if (length(ex$chunks)==0) 
      return(NULL)
    
    add.chunk = sapply(ex$chunks, function(ck) isTRUE(ck$add))
    num.e = sapply(ex$chunks, function(ck) length(ck$e.li))
    str = sapply(ex$chunks, function(ck) str.trim(paste0(ck$test.txt,collapse="")))
    has.test = nchar(str)>0
    
    chunk.opt = lapply(ex$chunks, function(ck) ck$chunk.opt)

    test.expr = lapply(ex$chunks, function(ck) {
      lapply(ck$test.txt, parse.text)
    })
    hint.expr = lapply(ex$chunks, function(ck) {
      lapply(ck$hint.txt, parse.text)
    })
    sol.txt =  sapply(ex$chunks, function(chunk) paste0(chunk$sol.txt, collapse="\n"))
    
    task.txt =  sapply(ex$chunks, function(chunk) paste0(chunk$task.txt, collapse="\n"))
    part =  lapply(ex$chunks, function(chunk) chunk$part)
    e.li = lapply(ex$chunks, function(ck) {
      ck$e.li
    })
    e.source.li = lapply(ex$chunks, function(ck) {
      ck$e.source.li
    })
    
    
    dt = data.table(ex.ind = ex.ind, ex.name = names(te$ex)[ex.ind], chunk.ps.ind=0, chunk.ex.ind = seq_along(ex$chunks), chunk.name = names(ex$chunks), chunk.opt=chunk.opt, part=part, num.e = num.e, has.test = has.test, e.li = e.li, e.source.li = e.source.li, test.expr=test.expr, hint.expr=hint.expr, task.txt = task.txt, sol.txt=sol.txt) 
    # Remove chunks without expressions
    dt = dt[add.chunk,]
    if (NROW(dt)>0) 
      dt$chunk.ex.ind = 1:NROW(dt) 
    dt
  })
  cdt = do.call(rbind,li)
  cdt$chunk.ps.ind = 1:NROW(cdt)
  # Add has.passed for each test
  cdt$e.tests.passed = lapply(cdt$test.expr, function(test.expr.li) {
    lapply(test.expr.li, function(et) {
      rep(FALSE, length(et))
    })
  })

  # Awards
  adt = rbindlist(te$awards)
  if (length(adt)==0)
    adt = data.table(chunk.name=character(0), award.name=character(0))
  
  cdt = left_join(cdt, dplyr::select(adt, chunk.name, award.name),by="chunk.name")
  rps$cdt = cdt
  rps$awards = te$awards
  
  
  # Add has.passed for each test
  
  li = lapply(cdt$chunk.ps.ind, function(ci) {
    restore.point("dhfjdgjghbh")
    ck = cdt[ci,]
    exi = ck$ex.ind
    li = lapply(seq_along(ck$test.expr[[1]]), function(ei) {
      restore.point("dhfjdgjghbh nfhdbfhb")
      et = ck$test.expr[[1]][[ei]]
      data.table(ex.ind=exi, chunk.ps.ind=ci, e.ind=ei, test.e.ind = seq_along(et), test.ps.ind=0, test.passed=FALSE)
    })
    rbindlist(li)
  })
  tdt = rbindlist(li)
  tdt$test.ps.ind = 1:NROW(tdt)
  rps$tdt=tdt
  
  # Just store exercise names
  num.chunks = sapply(seq_along(te$ex), function(ex.ind) sum(cdt$ex.ind==ex.ind))
  import.var = lapply(te$ex, function(act.ex) act.ex$import.var)
  
  rps$edt = data.table(ex.ind = seq_along(te$ex),ex.name = names(te$ex), num.chunks=num.chunks, import.var = import.var) 
  
 
  
  rps
}


parse.sol.line = function(row,txt, te) {
  restore.point("parse.sol.line")    
  str = txt[row]

  te$row = row  
  chunk.starts = str.starts.with(str,"```{r")
  chunk.ends = str.starts.with(str,"```") & ! chunk.starts
  block.starts = str.starts.with(str,"#<")
  block.ends = str.starts.with(str,"#>")
   
  change = chunk.starts | chunk.ends | block.starts | block.ends
 
  if (!change) {    
    parse.no.change.line(row,str,txt,te)    
  } else if (chunk.starts) {
    parse.chunk.starts(row,str,txt,te)    
  } else if (chunk.ends) {
    if (!is.true(te$in.chunk | te$block.type %in% te$markdown.blocks)) {
      display("in row ", row, " there was a line ``` but no code chunk was opened. Interpret it as a verbatim chunk.")
      parse.no.change.line(row,str,txt,te)    
    } else {
      parse.chunk.ends(row,str,txt,te)    
    }
  } else if (block.starts) {    
    parse.block.starts(row,str,txt,te)    
  } else if (block.ends) {
    parse.block.ends(row,str,txt,te)    
  }
  te
}

parse.no.change.line = function(row,str,txt, te) {
  restore.point("parse.no.change.line")    
  # Normal Markdown text without being in a block
  if (!te$in.chunk & !te$in.block) {
    te$task.txt = c(te$task.txt, str)
    te$sol.txt = c(te$sol.txt, str)
    if (str.starts.with(str,"# Problemset ")) {
      te$ps.name = str.trim(str.right.of(str, "# Problemset "))
    } else if (str.starts.with(str,"## Exercise ")) {
      parse.exercise.starts(row, str, txt, te)
    } else {
      part.rows = which(grepl("#'[ ]?([a-z]|[ivx]*)\\)",str))
      if (length(part.rows)>0)
        te$part = str.right.of(str.left.of(str,")"),"#' ")
    }
    
  # Normal line of code without beeing in a block
  # Treat as a "chunk" block
  } else if (te$in.chunk & !te$in.block) {
    te$block.txt = c(te$block.txt, str)    
  
  # Within a block 
  } else if (te$in.block) {
    te$block.txt = c(te$block.txt, str)
  }
}

parse.exercise.starts = function(row,str,txt, te) {
  restore.point("te.exercise.starts")    

  te$prev.ex.name=te$ex.name
  te$ex.name = str.trim(str.between(str, "# Exercise ","-- "))
  te$part = ""
  ex = get.empty.ex()
  ex$ex.name = te$ex.name
  te$act.ex = ex
  te$ex[[ex$ex.name]] = ex
}

parse.chunk.starts = function(row,str,txt, te) {
  restore.point("parse.chunk.starts")    
  if (te$block.type %in% te$markdown.blocks) {
    te$block.txt = c(te$block.txt, str)
  } else if (te$in.block | te$in.chunk) {
    stop(paste0("in row ", row, " you start a chunk without having closed the chunk before."), call.=FALSE)      
  } else {
    opt = chunk.opt.string.to.list(str, with.name=TRUE)
    chunk.name = opt[[1]]  
    te$chunk.head = str
    te$chunk.opt = opt[-1]
    te$chunk.name = gsub('"','', chunk.name, fixed=TRUE)
    te$in.chunk = TRUE
    te$chunk.str = paste0(" in chunk ", te$chunk.name)
    te$block.type = "chunk"
    te$block.start = row+1
    
    ck = get.empty.chunk()
    ck$chunk.name = te$chunk.name
    ck$ex.name = te$act.ex$ex.name
    ck$chunk.opt = te$chunk.opt

    ck$part = te$part
    te$act.ex$chunks[[te$chunk.name]] = ck
    te$act.chunk = ck
  }
}

parse.chunk.ends = function(row,str,txt, te) {
  restore.point("parse.chunk.ends")    
    
  if (te$block.type %in% te$markdown.blocks) {
    te$block.txt = c(te$block.txt, str)
  } else if (te$in.block) {
    stop(paste0(te$chunk.str, " ending in row ", row, " you forgot to close your ", block.type," block with #>"), call.=FALSE)      
  } else {
    te$block.end = row-1
    add.te.block(te)
    add.te.chunk(te,te$act.chunk)
    
    te$prev.chunk.name = te$chunk.name
    te$in.chunk = FALSE
    te$chunk.name = ""
    te$chunk.str = ""
    te$chunk.code = NULL
    te$block.txt = NULL
  }
}

parse.block.starts = function(row,str,txt, te) {
  restore.point("parse.block.starts")  
  #if (row==25) stop()
  if (te$in.block) {
    stop(paste0(te$chunk.str," in row ", row, " you start a new block without having closed the previous ", te$block.type, " block."), call.=FALSE)      
  }
  
  # Add the virtual code block 
  if (te$in.chunk & nchar(paste0(str.trim(te$block.txt),collapse=""))>0) {
    te$block.end = row-1
    add.te.block(te)
  }
  te$block.txt = NULL
  te$block.start.row = row
  te$block.head = str
  te$block.type = str.trim(str.between(str,"#< "," "))
  te$in.block = TRUE
  
  if (!te$block.type %in% te$blocks) {
    stop(paste0(te$chunk.str," in row ", row, " you open a block of unknown type:\n  '", te$block.type,"'\nI only know the block types:\n  ", paste0(te$blocks, collapse=", "),"."), call.=FALSE)
  }
  if (te$in.chunk & !te$block.type %in% te$code.blocks) {
    stop(paste0(te$chunk.str, " in row ", row, " you open a '", block.type,"' block. But '", te$block.type, "' blocks can only be opened in your markdown text outside of code chunks."), call.=FALSE)
  }
  if (!te$in.chunk & !te$block.type %in% te$markdown.blocks) {
    stop(paste0(" in row ", row, " you open a '", te$block.type,"' block outside a code chunk. But '", te$block.type, "' blocks can only be opened inside code chunks."), call.=FALSE)
  }
}

parse.block.ends = function(row,str,txt, te) {
  restore.point("parse.block.ends") 
  if (!te$in.block) {
    stop(paste0(te$chunk.str, " in row ", row, " you close a block with #>, but have no open block."), call.=FALSE)      
  } else {
    add.te.block(te)
    te$in.block = FALSE
    te$block.txt = NULL
    
    if (te$in.chunk) {
      te$block.type = "chunk"
      te$block.start = row+1
    } else {
      te$block.type = ""        
    }
  }
}


add.te.chunk = function(te,ck) {
  restore.point("add.te.chunk")
  if (length(ck$e.li)>0 | isTRUE(ck$has.task)) {
    te$task.txt = c(te$task.txt, te$chunk.head, ck$task.txt,"```")
    te$sol.txt = c(te$sol.txt, te$chunk.head, ck$sol.txt,"```")
    ck$add = TRUE
  }
  
}

add.te.block = function(te) {
  restore.point("add.struc.block")
  type = te$block.type
  args = str.trim(str.right.of(te$block.head, te$block.type))
  ck = te$act.chunk
  
  btxt = te$block.txt
  
  # Check if code in block can be parsed
  if (type %in% te$code.blocks) {
    expr = tryCatch(parse.text(btxt),
    error = function(e) {
        str = paste0(" when parsing your code",te$chunk.str," between rows ", te$block.start, " and ", te$block.end, ":\n ", str.right.of(paste0(as.character(e), collapse="\n"),":") )
      stop(str, call.=FALSE)      
    })
  }
  if (type %in% c("task","task_notest")) {
    ck$has.task = TRUE    
  }

  # Add test code
  if (type %in% c("chunk","task","task_notest","notest")) {
    add.te.code(te,ck)
  } else if (type == "extra_test") {
    ck$test.txt = c(ck$test.txt,btxt)
  } else if (type == "test") {
    test.txt = paste0(btxt, collapse="\n")
    ck$test.txt[length(ck$test.txt)] <- test.txt
    # Remove default hint for manual tests
    ck$hint.txt[length(ck$hint.txt)] <- ""
  } else if (type == "test_arg") {    
    test.txt = test.code.for.e(te$last.e, extra.arg = paste0(btxt,collapse=", "))  
    ck$test.txt[length(ck$test.txt)] <- test.txt
  } else if (type == "test_hint_arg") {
    #browser()
    extra.arg = paste0(btxt,collapse=",")
    test.txt = test.code.for.e(te$last.e, extra.arg = extra.arg)  
    ck$test.txt[length(ck$test.txt)] <- test.txt
     
    hint.txt = hint.code.for.e(te$last.e, extra.arg = extra.arg)  
    ck$hint.txt[length(ck$hint.txt)] <- hint.txt

  } else if (type == "test_calls") {
     test.txt = test.code.for.e(te$last.e, extra.arg = paste0(btxt,collapse=", "))  
     ck$test.txt[length(ck$test.txt)] <- test.txt
  } else if (type == "compute") {
    var = args
    hint.txt = hint.code.for.compute(btxt,var=var)
    ck$hint.txt[length(ck$hint.txt)] <- hint.txt
    test.txt = test.code.for.compute(btxt,var=var)
    ck$test.txt[length(ck$test.txt)] <- test.txt
    
    te$sol.txt = c(te$sol.txt, te$block.txt)
  } else if (type == "hint") {
    #browser()
    ck$hint.txt[length(ck$hint.txt)] <- btxt
  } else if (type == "add_to_hint") {
    hint.txt = hint.code.for.e(te$last.e,extra.code = btxt)  
    ck$hint.txt[length(ck$hint.txt)] <- hint.txt
  } else if (type == "settings") {
    add.te.settings(te)
  } else if (type == "info") {
    add.te.info(te)
  } else if (type == "award") {
    add.te.award(te)
  } else if (type == "ignore") {
  } else {
    str = paste0(chunk.str, " there is an unknown block head: ", te$block.head)
    stop(str, call.=FALSE)
  }
  te$code.txt = NULL
  te$block.head = NULL
}

add.te.code = function(te,ck) {
  restore.point("add.te.code")
  
  #if (te$block.type=="chunk")
    #stop("")
  task = te$block.type == "task" | te$block.type == "task_notest"
  notest = te$block.type == "notest" | te$block.type == "task_notest"

  ck$sol.txt = c(ck$sol.txt, te$block.txt)
  if (task) {
    ck$task.txt = c(ck$task.txt, te$block.txt)
  }

  if (!notest) {
    code.txt = str.trim(te$block.txt)
    code.txt = code.txt[nchar(code.txt)>0]    

    ret = tryCatch(parse.text.with.source(te$block.txt),
      error = function(e) {
        e.str = paste0(as.character(e), collapse="\n")
        str = paste0(" when parsing your code",te$chunk.str," between rows ", te$block.start, "-", te$block.end, ":\n ", str.right.of(e.str,":"))
        stop(str, call.=FALSE)
    })
    e.li = ret$expr
    e.source.li = ret$source
    
    if (length(e.li)>0) {
      test.txt = sapply(seq_along(e.li), function(i) test.code.for.e( e.li[[i]]))   
      hint.txt = sapply(seq_along(e.li), function(i) hint.code.for.e( e.li[[i]]))   
      
      te$counter = te$counter+length(e.li)
      ck$test.txt = c(ck$test.txt,test.txt)
      ck$hint.txt = c(ck$hint.txt,hint.txt)
      ck$e.li = c(ck$e.li, e.li)
      ck$e.source.li  = c(ck$e.source.li, e.source.li)
      te$last.e = e.li[[length(e.li)]]
      enter.code.str =  "\n# enter your code here ...\n"
      enter.code.str =  ""
      if (!task & 
        !identical(te$task.txt[length(te$task.txt)], enter.code.str)) {
        ck$task.txt = c(ck$task.txt, enter.code.str)
      }
    # Empty code.txt
    } else {
      te$last.e = NULL    
    }
  }  
} 

add.te.settings = function(te) {
  
  restore.point("add.te.settings")
  txt = te$block.txt
  env = new.env()
  eval(parse(text=txt,srcfile=NULL), envir=env)
  import.var = as.list(env$import.var)
  if (length(import.var)>0) {
    if (is.null(names(import.var)))
      names(import.var) = rep("", length(import.var)) 
    names(import.var)[names(import.var) == ""] = te$prev.ex.name
  }
  te$act.ex$import.var = import.var
}


add.te.info = function(te) {
  restore.point("add.te.info")
  #stop()
  require(knitr)
  require(markdown)
  str = te$block.head
  info.name = str.between(str, '"','"')
  
  txt = te$block.txt
  #txt = c(paste0("**",info.name,":** "), txt)
  ktxt = knit(text=txt)
  html= markdownToHTML(text=ktxt, fragment.only=TRUE)

  if (FALSE) {
    htmlFile <- tempfile(fileext=".html")
    writeLines(html,htmlFile)
    rstudio::viewer(htmlFile)  
  }
  info = list(info.name=info.name,type="html", html=html, rmd=txt)
  str = paste0('info("', info.name,'") # Run this line (Strg-Enter) to show info')
  te$task.txt = c(te$task.txt,str)
  te$infos[[info.name]] = info
}


add.te.award = function(te) {
  require(knitr)
  require(markdown)
  
  str = te$block.head
  name = str.between(str, '"','"')

  txt = te$block.txt
  txt = c(paste0("### Award: ",name,"\n"), txt)
  ktxt = knit(text=txt)
  html= markdownToHTML(text=ktxt, fragment.only=TRUE)
  if (FALSE) {
    htmlFile <- tempfile(fileext=".html")
    writeLines(html,htmlFile)
    rstudio::viewer(htmlFile)  
  }
  award = list(award.name=name, chunk.name=te$prev.chunk.name, html=paste0(html,collapse="\n"), txt=paste0(te$block.txt, collapse="\n"))
  te$awards[[name]] = award
}


examples.test.code.for.e = function() {
  f = function(e) {
    e = substitute(e)
    test.code.for.e(e)
  }
  
  f(fun <- function(x) {x*x})
}

test.code.for.e = function(e, extra.arg="") {
  restore.point("test.code.for.e")
  if (is.null(e))
    return("")
  
  extra.arg = ifelse(extra.arg=="","",paste0(",",extra.arg))  
  if (is.assignment(e)) {
    var = deparse1(e[[2]],collapse="\n")
    rhs = deparse1(e[[3]],collapse="\n")
    call.name = name.of.call(e[[3]])   
    if (call.name == "function") {
      code=paste0("check.function(", var, "<-",rhs,extra.arg,")")    
    } else {
      code = paste0("check.assign(", var, "<- ",rhs,extra.arg,")")
    }
  } else {
    estr = deparse1(e)
    code = paste0("check.call(", estr,extra.arg,")")
  }
  code  
}

hint.code.for.e = function(e, extra.code = NULL, extra.arg = NULL) {
  restore.point("hint.code.for.e")
  if (is.null(e))
    return("")
  if (!is.null(extra.arg))
    extra.arg =  paste0(",", extra.arg)
  
  if (!is.null(extra.code)) {
    extra.code = paste0("\n  ",paste0(extra.code,collapse="\n  "))
  }
  estr = deparse1(e)
  if (is.assignment(e)) {
    var = deparse1(e[[2]])
    rhs = deparse1(e[[3]])
    call.name = name.of.call(e[[3]])
    
    if (call.name == "function") {
      rhs = deparse1(e[[3]], collapse="\n")
      code = paste0("hint.for.function(",var ,"<-",rhs, extra.arg,")",
                    extra.code)
    } else {
      code = paste0("hint.for.assign(",var ,"<-",rhs,extra.arg,")",
                    extra.code)
    }
  } else {
    code = paste0("hint.for.call(",estr,extra.arg,")", extra.code)
  }
  code  
}

test.code.for.compute = function(code, var, extra.arg="") {
  restore.point("test.code.for.compute")
  code.txt = paste0("{\n", paste0(code, collapse="\n"),"\n",var,"\n}")
  test.txt = paste0("check.variable('", var,"',",code.txt,extra.arg,")")
  test.txt
}

hint.code.for.compute = function(code, var, extra.code = NULL) {
  restore.point("hint.code.for.compute")
  ec = parse.expr.and.comments(code, comment.start="## ") 
  comments = lapply(ec$comments, function(str) gsub('"',"'",str, fixed=TRUE))
  comment.code = paste0("list(",paste0('"',comments,'"', collapse=", "),")")
  
  code = paste0(code, collapse="\n")
  com = paste0("hint.for.compute({\n",code,"\n},",comment.code,", var= '",var,"'",
               extra.code,"\n})")
  com  
}

get.empty.ex = function() {
  ex = new.env()
  ex$chunks = list()
  ex
}
get.empty.chunk = function() {
  ck = new.env()
  ck$test.txt = NULL
  ck$hint.txt = NULL
  ck$task.txt = NULL
  ck$sol.txt = NULL
  ck$expr = NULL
  ck
}

get.empty.te = function() {
  te = new.env()
  te$block.type = ""
  te$in.block = FALSE
  te$in.chunk = FALSE
  te$block.head = NULL
  
  te$task.txt = NULL
  te$sol.txt = NULL
  te$code.txt = NULL
  
  te$part = NULL
  te$last.e = NULL
  te$counter = 0

  te$markdown.blocks = c("info","award","ignore")
  te$code.blocks = c("test","test_arg","test_hint_arg","extra_test","test_calls",
                  "hint","add_to_hint",
                  "task","task_notest","notest",
                  "compute","settings")
  te$blocks = c(te$markdown.blocks, te$code.blocks)
  te$act.chunk = NULL
  te$act.ex = NULL
  te$ps.name = NULL
  te$ex = te$infos = te$awards = list()
  te
}

include.ps.extra.lines = function(txt, ps.file, ps.name=te$ps.name,te=NULL,...) {
  chunk.row = which(str.starts.with(txt,"# Problemset"))[1]
  if (is.na(chunk.row))
    chunk.row = 1
  str = ps.rtutor.chunk(ps.name=ps.name, ps.file=ps.file,...)
  txt[chunk.row] = paste0(txt[chunk.row],"\n\n",paste0(str,collapse="\n"))
  txt
  
}

ps.rtutor.chunk = function(ps.name,ps.dir = "C:/problemsets/", ps.file = paste0(ps.name,".Rmd"), header="", user.name="ENTER A USER NAME HERE",...) {

  str = paste0("  
```{r include=FALSE}
",header,"
ps.dir =  '",ps.dir,"' # set to the folder in which this file is stored
ps.file = '",ps.file,"' # set to the name of this file
user.name = '",user.name,"' # set to your user name

library(RTutor2)
check.problem.set('",ps.name,"', ps.dir, ps.file, user.name=user.name, reset=FALSE)

# To check your solution in RStudio save (Ctrl-S) and then run all chunks (Ctrl-Alt-R)
```
Name: `r user.name`
") 
  str
}


#' Generate default header text for a Rmd file
#' @export
install.header.txt = function() {
"
# Remove comments below if you need to install packages
# install.packages('devtools');install.packages('whisker');install.packages('stringr')
# install.packages('RJSONIO');
# library(devtools)
# install_github(repo = 'restorepoint', username = 'skranz')
# install_github(repo = 'RTutor', username = 'skranz')    
"  
}

#' Generate default footer text for a Rmd file
#' @export
zip.submit.footer.txt = function(ps.name) {
paste0("
#'
#'## Sumbitting your solution
#'
#' Submit your solution as a zip file with name
#'`solution_",ps.name,"_by_username.zip`
#' that contains the files
#' `",ps.name,".rmd, ", ps.name,".log, username_",ps.name,".ups`
#' (replace `username` by your user name)
#' 
#' If you have installed RTools (http://cran.r-project.org/bin/windows/Rtools/) and updated your Windows PATH variable you can also try calling
#' `zip.solution()` 
#' to generate the zip file automatically.
")
}


#' Set default names for the chunks of problem set rmd files 
name.rmd.chunks = function(rmd.file=NULL, txt=readLines(rmd.file), only.empty.chunks=TRUE, keep.options=TRUE) {
  restore.point("name.rmd.chunks")
  ex.name = ""
  part.name = ""
  in.code = FALSE
  i = 2
  counter = 1

  str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"
  for (i in 1:length(txt)) {
    str = txt[i]
    
    
    if (str.starts.with(str, "```{r")) {
      if ((!only.empty.chunks) | str.trim(str) == "```{r }" | str.trim(str) == "```{r}") {
        counter.str = ifelse(counter==1,"", paste0(" ",counter))
        if (has.substr(str,",")) {
          rhs.str = paste0(",",chunk.opt.list.to.string(chunk.opt.string.to.list(str)))      
        } else {
          rhs.str = ""
        }
        txt[i] = paste0('```{r "',ex.name,' ',part.name, counter.str,'"', rhs.str,"}")
      }
      counter = counter+1
    } else if (str.starts.with(str,"## Exercise ")) {
      ex.name = str.right.of(str,"## Exercise ")
      ex.name = gsub("#","", ex.name, fixed=TRUE)
      ex.name = str.left.of(ex.name," --", not.found="all")
      counter = 1
      part.name = ""
    } else if (!is.na(temp.part <- str_extract(str,"^([a-z]|[ivx]*)\\)")[1]  )) {
      part.name = temp.part
      counter = 1
    }
  }
  if (!is.null(rmd.file))
    writeLines(txt, rmd.file)
  invisible(txt)
}


get.chunk.lines = function(txt) {
  restore.point("get.chunk.lines")
  chunk.start = str.starts.with(txt,"```{")
  chunk.end = which(str.starts.with(txt,"```") & !chunk.start)
  chunk.start = which(chunk.start)
  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)
  
  header = txt[chunk.start]
  chunk.name = sapply(header,USE.NAMES=FALSE, function(str) chunk.opt.string.to.list(str, with.name=TRUE)[[1]])
  
  quick.df(chunk.name=chunk.name, start.line=chunk.start, end.line=chunk.end)
}

make.shiny.dt = function(rps, rmd.file, txt = readLines(rmd.file)) {
  restore.point("make.shiny.dt")
  library(stringtools)
  library(markdown)
  txt = sep.lines(merge.lines(txt))
  
  chunk.start = str.starts.with(txt,"```{")
  chunk.end = which(str.starts.with(txt,"```") & !chunk.start)
  chunk.start = which(chunk.start)

  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)
  
  chunk.end.plus1 = chunk.end+1
  ex.start = which(str.starts.with(txt,"## Exercise "))
  info.start = which((str.starts.with(txt,"info(")))
  cont.start = which((str.starts.with(txt,"#. continue")))
  
  
  
  df.chunk = data.frame(start=chunk.start, type="chunk", type.ind=seq_along(chunk.start))
  df.info = data.frame(start=info.start, type=rep("info", length(info.start)), type.ind=seq_along(info.start))
  df.cont = data.frame(start=cont.start, type=rep("continue", length(cont.start)), type.ind=seq_along(cont.start))
  df.task = data.frame(start=sort(c(1,ex.start,chunk.end+1, info.start+1, cont.start+1)), type="task")


  
  df.task$type.ind = 1:NROW(df.task)
  
  
  df = rbind(df.chunk,df.info,df.cont, df.task)
  df = df[!duplicated(df$start),]
  df = arrange(df, start)
  df$end = c(df$start[-1]-1, length(txt))
  df
  n = NROW(df)

  df.ex = data.frame(start=c(1,ex.start), ex.ind = c(0,seq_along(ex.start)))
  if (df.ex$start[2]==1)
    df.ex = df.ex[-1,]
  #df.ex$end = c(df.ex$start[-1]-1, length(txt))
  df.ex
  
  df$ex.ind = df.ex$ex.ind[findInterval(df$start, df.ex$start)]

  # views
  views = sort(c(cont.start, ex.start))
  df$view.ind = findInterval(df$start, views)
  df
  
  
  dt = data.table(fragment.ind = 1:n,ex.ind=df$ex.ind, view.ind=df$view.ind, type=df$type, type.ind=df$type.ind, chunk.name="",chunk.ind=0,info.name="", html=vector("list", n), code="")
  keep.row = rep(TRUE, NROW(dt))
  
  i = 5
  for (i in 1:n) {
    if (dt$type[i]=="chunk") {
      header = txt[df$start[i]]
      opt = chunk.opt.string.to.list(header, with.name=TRUE)
      chunk.name = opt[[1]]
      chunk.ind = which(rps$cdt$chunk.name == chunk.name)[1]
      if (is.na(chunk.ind)){
        keep.row[i] = FALSE
        next
      }
      dt$chunk.name[i] = chunk.name
      dt$chunk.ind[i] = chunk.ind
      code = txt[(df$start[i]+1):(df$end[i]-1)]
      dt$code[[i]] = paste0(code, collapse="\n")
      #shiny.dt$html[[i]] = editChunkUI(chunk.name=chunk.name,code=code)
    } else if (dt$type[i]=="task") {
      code = txt[df$start[i]:df$end[i]]
      #if (any(str.starts.with(code, "a)"))) {
      #restore.point("jkhskjfhdkjfkjdn")
      #  stop()
      #}
      if (nchar(paste0(code, collapse="\n"))==0)  {
        keep.row[i] = FALSE
      } else {
        #dt$html[[i]] = withMathJax(HTML(markdownToHTML(text=code, fragment.only=TRUE)))
        dt$html[[i]] = HTML(markdownToHTML(text=code, fragment.only=TRUE))
      }
    } else if (dt$type[i]=="info") {
      header = txt[df$start[i]]
      info.name = str.between(header,'"','"')
      #html = withMathJax(HTML(rps$infos[[info.name]]$html))
      html = HTML(rps$infos[[info.name]]$html)
      
      collapseId = paste0("collapse_info_",i)
      collapsePanelId = paste0("collapse_panel_info_",i) 
      dt$html[[i]] = bsCollapse(open = NULL, id = collapseId,
        bsCollapsePanel(paste0("Info: ",info.name),id=collapsePanelId, html )
      )

    } else if (dt$type[i]=="continue") {
      
    }
  }
  
  
  dt = dt[keep.row,]
  dt
}

remove.verbatim.end.chunks = function(chunk.start, chunk.end) {
  restore.point("remove.verbatim.end.chunks")
  df = data.frame(ind =c(0, seq_along(chunk.start), seq_along(chunk.end)),
                  row=c(0, chunk.start,chunk.end),
                  type=c("f",
                         rep("s",length(chunk.start)),
                         rep("e",length(chunk.end))
                       )
                  )
  df = arrange(df,row)
  df$del =  df$type == "e" & !is.true(lag(df$type) == "s")
  
  keep.ind = df$ind[df$type=="e" & !df$del]
  chunk.end[keep.ind]
}


chunk.opt.string.to.list = function(str, with.name=FALSE) {
  restore.point("chunk.opt.string.to.list")
  #str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"

  tokens = str.split(str,",")
  str = str.between(str,"{r","}")
  code = paste0("list(",str,")")
  li = eval(parse(text=code,srcfile=NULL))
  
  if (!with.name) {
    li = li[-1]
  }
  li
}


