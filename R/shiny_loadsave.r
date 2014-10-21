
load.save.ui = function(ps=get.ps()) {
  restore.point("load.save.ui")
  
  pattern = paste0(".*\\Q_",ps$name,".sav\\E")
  
  files = list.files(pattern=pattern)
  
  # if default sav file does not exist, create the sav file
  if (!ps$sav.file %in% files) {
    save.sav(ps=ps)  
    files = list.files(pattern=pattern)
  }
  
  file.end = paste0("_",ps$name,".sav")
  fluidRow(
    fluidRow(
      bsActionButton("loadBtn","Load"),
      selectizeInput('loadFileInput',"",choices=files,multiple=FALSE, width="80%", selected=ps$sav.file),
      bsAlert("loadSaveAlert")
    ),
    fluidRow(
      bsActionButton("saveAsBtn","Save as"),
      textInput('saveFileInput',"",value=ps$sav.file),
      helpText(paste0('(file name must end with "',file.end,'")')),
      bsAlert("saveAsAlert")
    )
  )
}

load.save.observer = function(server.env = parent.frame()) {
  expr = substitute(env=list(session = server.env$session), expr= {
    observe({
      if (has.counter.increased("loadBtn",input$loadBtn)) {
        ps = get.ps()
        file = isolate(input$loadFileInput)
        
        
        ok = load.and.set.sav(file, ps=ps)
        if (ok) {
          update.all.chunks()
          createAlert(session,inputId = "loadSaveAlert",
            title = "Successfully loaded.",
            message="",
            type = "success", append=FALSE
          )

        } else {
          createAlert(session,inputId = "loadSaveAlert", 
            title = "Could not load saved solution.",
            message = ps$failure.message,
            type = "warning", append=FALSE
          )          
        }
      }
    }) 
   observe({
      if (has.counter.increased("saveAsBtn",input$saveAsBtn)) {
        
        ps = get.ps()
        file = isolate(input$saveFileInput)
        restore.point("saveAs")

        file.end = paste0("_",ps$name,".sav")
        if (!str.ends.with(file, file.end)) {
          createAlert(session,inputId = "saveAsAlert", 
            title = "Invalid file name",
            message = paste0('Your file name must end with "', file.end,'".'),
            type = "warning", append=FALSE
          )          
        } else {
          ps$sav.file = file
          save.sav(ps=ps)
          
          pattern = paste0(".*\\Q_",ps$name,".sav\\E")
          files = list.files(pattern=pattern)
          updateSelectizeInput(session,'loadFileInput',choices=files,selected=ps$sav.file)
          
          createAlert(session,inputId = "saveAsAlert",
            title = paste0("Saved as ", ps$sav.file),
            message="",
            type = "success", append=FALSE
          )
        }
      }
    }) 
       
  })
  eval(expr,server.env)
}


save.sav = function(file=ps$sav.file, user.name=get.user()$name,ps=get.ps(), copy.into.global=TRUE) {
  restore.point("save.sav")
  sav = list(
    ps.name = ps$name,
    user.name = user.name,
    stud.code = ps$cdt$stud.code,
    mode = ps$cdt$mode,
    is.solved = ps$cdt$is.solved
  )
  save(sav, file=file)
  
  copy.into.env(source=ps$stud.env, dest=globalenv())
}

# load a sav file and set the current problem set to it
load.and.set.sav = function(file=ps$sav.file, ps=get.ps()) {
  restore.point("load.and.set.sav")
  sav = load.sav(file, ps)
  # need to check whether the problem set is identical
  
  res = compare.sav.with.ps(sav=sav, ps=ps)
  if (!res$ok) {
    ps$failure.message = res$msg
    return(FALSE)
  }
  
  ps$sav.file = file
  ps$cdt$mode = sav$mode
  ps$cdt$stud.code = sav$stud.code
  ps$cdt$is.solved = sav$is.solved  
  rerun.solved.chunks(ps)
  
  return(TRUE)
}

load.sav = function(file =ps$sav.file, ps=get.ps()) {
  restore.point("load.sav")

  if (is.null(file))
    return(NULL)
  if (!file.exists(file))
    return(NULL)
  load(file)
  return(sav)
}

compare.sav.with.ps = function(sav, ps) {
  
  if (ps$name != sav$ps.name) {
    msg = paste0("Your stored solution is from problem set ", sav$ps.name, " but you are currently working on problem set ", ps$name,".")
    return(list(ok=FALSE,msg=msg))
  }
  return(list(ok=TRUE, msg=NULL))
}


update.chunk = function(chunk.ind, ps = get.ps()) {
  restore.point("update.chunk")
  id = paste0("r.chunk_",chunk.ind,".ui.mode")  
  ps[[id]]$counter=isolate(ps[[id]]$counter+1)
  ps$cdt$has.ui.renderer[chunk.ind] = TRUE
}

update.all.chunks = function(ps=get.ps()) {
  restore.point("update.all.chunks")
  for (chunk.ind in ps$cdt$chunk.ps.ind) {
    update.chunk(chunk.ind, ps)  
  }
} 
