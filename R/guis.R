require(seao) || stop("seao package is absent")
require(tcltk) || stop("tcltk support, necessary for gui, is absent")

.tmp.ea <- ".tmp.ea"
env.gui.ea <- new.env(FALSE,NULL)
assign(.tmp.ea, list(), env.gui.ea)



seao.guistart <- function() {

    assign(.tmp.ea, list(), env.gui.ea)

    tcl.done <- tclVar(0)
    
    tkwin <- tktoplevel()
    tkbind(tkwin,
           "<Destroy>", function()tclvalue(tcl.done) <- 2)
    tkwm.title(tkwin,
               "Simple evolutionary algorithms")


    tkpack(tkwin.par <- tkframe(tkwin),
           tkwin.buts <- tkframe(tkwin),
           pady="10",
           side="top")
    
    tkpack(tkwin.left <- tkframe(tkwin.par,
                                 relief="groove", borderwidth="2"),
           tkwin.right <- tkframe(tkwin.par,
                                  relief="groove", borderwidth="2"),
           padx="10",
           side="left")

    
    tkpack(tkwin.topframe <- tkframe(tkwin.left,
                                     relief="ridge", borderwidth="3"),
           tkbutton(tkwin.left,
                    text="Fill out fitnesses",
                    relief="ridge", borderwidth="3",
                    command=showvalues.gui),
           tkbutton(tkwin.left,
                    text="Next generation",
                    relief="ridge", borderwidth="3",
                    command=nextgeneration.gui),
           tkbutton(tkwin.left,
                    text="Save or export data",
                    relief="ridge", borderwidth="3",
                    command=save.ea.gui),
           pady="10")

    
    tkgrid(tkbutton(tkwin.topframe,
                    text="Define genomestructure",
                    relief="groove", borderwidth="1",
                    command=genomestruc.gui),
           row="0", column="0")


    tkgrid(tkbutton(tkwin.topframe,
                    text="Make random generation",
                    relief="groove", borderwidth="1",
                    command=newgen.gui),
           row="1", column="0")
    
    tkgrid(tklabel(tkwin.topframe,
                   text="OR"),
           row="0", column="2")
    
    tkgrid(tkbutton(tkwin.topframe,
                    text="Load data",
                    relief="groove", borderwidth="1",
                    command=function() {load(tclvalue(tkgetOpenFile(filetypes="{{RData} {.RData .Rda}}")));
                                        assign(.tmp.ea, ea, env.gui.ea)}),
           row="0", column="3")
    



    tkpack(tkbutton(tkwin.right,
                    text="Fitness evolution plot",
                    relief="ridge", borderwidth="3",
                    command=plotfitness.gui),
           tkbutton(tkwin.right,
                    text="Allel evolution plot",
                    relief="ridge", borderwidth="3",
                    command=plotgene.gui),
           pady="10")
    

    tkpack(tkbutton (tkwin.buts,
                     text="OK",
                     command=function()tclvalue(tcl.done) <- 1),
           tkbutton (tkwin.buts,
                     text="Cancel",
                     command=function()tclvalue(tcl.done) <- 2),
           side="left")

    
    
    tkwait.variable(tcl.done)

    tkdestroy(tkwin)

}



showvalues.gui <- function(ea=NULL, gen=NULL) {

    if (!(length(ea$generations) && length(ea$genes)))
        ea <- get(x=.tmp.ea, envir=env.gui.ea)
    
    if (!is.numeric(gen))
        i.gen <- length(ea$generations)
    else
        i.gen <- gen
    
    tcl.done <- tclVar(0)
    
    for (i in 1:length(ea$generations[[i.gen]]$fit))
        assign(paste("tcl.test", i, sep=""), tclVar(ea$generations[[i.gen]]$fit[i]))
    
    
   
    tkwin <- tktoplevel()
    tkbind(tkwin,
           "<Destroy>",
           function()tclvalue(tcl.done) <- 2)
    tkwm.title(tkwin,
               "Fill out fitnesses")
    
    tkpack(tkwin.vals <- tkframe(tkwin),
           expand="yes", fill="both",
           side="top",
           pady="5")
    tkpack(tkwin.buts <- tkframe(tkwin),
           side="top",
           pady="5")
    
    
    
    tkpack(tkbutton (tkwin.buts,
                     text="OK",
                     command=function()tclvalue(tcl.done) <- 1),
           tkbutton (tkwin.buts,
                     text="Cancel",
                     command=function()tclvalue(tcl.done) <- 2),
           side="left")
    
    

    tkpack(tkwin.valsup <- tkframe(tkwin.vals),
           expand="yes", fill="both",
           side="top", 
           pady="0", padx="0")
    tkpack(tkwin.vals.sx <- tkscrollbar(tkwin.vals, orient="horiz",
                                        command=function(...) tkxview(tkwin.vals.text, ...)),
           fill="x", side="bottom")

    tkpack(tkwin.vals.text <- tktext(tkwin.valsup,
                                     yscrollcommand=function(...) tkset(tkwin.vals.sy, ...),
                                     xscrollcommand=function(...) tkset(tkwin.vals.sx, ...),
                                     wrap="none",
                                     setgrid="true", borderwidth="0",
                                     width=min(100, length(ea$genes)*10),
                                     height=min(40, length(ea$generations))),
           expand="yes",
           fill="both", side="left", 
           pady="0", padx="0")
    tkpack(tkwin.vals.sy <- tkscrollbar(tkwin.valsup, orient="vert",
                                        command=function(...) tkyview(tkwin.vals.text, ...)),
           fill="y", side="right", 
           pady="0", padx="0")
    
    
    for (i in 1:length(ea$generations[[i.gen]]$fit)) {
        tkinsert(tkwin.vals.text, "end", paste(i, ". ", sep=""))
        tkwindow.create(tkwin.vals.text, "end", window=tkentry(tkwin.vals.text,
                                                textvariable=eval(parse(text=paste("tcl.test", i, sep=""))), width="6"))
        for (j in 1:length(ea$genes))
            tkinsert(tkwin.vals.text, "end", paste("\t", ea$generations[[i.gen]]$allel[i,j]))
        tkinsert(tkwin.vals.text, "end", "\n")
    }

    
    tkwait.variable(tcl.done)
    
    if (tclvalue(tcl.done)=="1") {
        for (i in 1:length(ea$generations[[i.gen]]$fit))
            ea$generations[[i.gen]]$fit[i] <- eval(parse(text=tclvalue(eval(parse(text=paste("tcl.test", i, sep=""))))))
    }
    
    tkdestroy(tkwin)
    
    assign(.tmp.ea, ea, envir=env.gui.ea)
    
    return(ea)
    
    
}




newgen.gui <- function(ea=NULL) {

    if (!length(ea$genes))
        ea <- get(.tmp.ea, envir=env.gui.ea)

    tcl.nind <- tclVar(25)
    tcl.method <- tclVar("random")
    tcl.done<-tclVar(0)

    tkwin <- tktoplevel()
    tkbind(tkwin,
           "<Destroy>",
           function()tclvalue(tcl.done) <- 2)
    tkwm.title(tkwin,
               "Initial generation")
    

    
    tkpack(tkwin.newgen <- tkframe(tkwin),
           tkwin.buts <- tkframe(tkwin),
           side="top",
           pady="5")


    tkpack(tkwin.nind <- tkframe(tkwin.newgen),
           tkwin.method <- tkframe(tkwin.newgen),
           side="top",
           pady="5")

    
    tkpack(tkscale(tkwin.nind, from=1, to=500,
                   showvalue=T, variable=tcl.nind,
                   resolution=1, orient="horiz"),
           tklabel(tkwin.nind, text="Number of individuals"),
           side="top")

    tkpack(tklabel(tkwin.method, text="Method"),
           tkradiobutton(tkwin.method, text="Random", value="random", variable=tcl.method),
           tkradiobutton(tkwin.method, text="Maximum gene diversity", value="maxgenediv", variable=tcl.method),
           anchor="w",
           side="top")
    
    tkpack(tkbutton (tkwin.buts,
                     text="OK",
                     command=function()tclvalue(tcl.done) <- 1),
           tkbutton (tkwin.buts,
                     text="Cancel",
                     command=function()tclvalue(tcl.done) <- 2),
           tkbutton (tkwin.buts,
                     text="Help",
                     command=function()help(initgen, pager=tkpager)),
           side="left")
    

    
    tkwait.variable(tcl.done)
    

    if (tclvalue(tcl.done)=="1") {
        i.ind <- eval(parse(text=tclvalue(tcl.nind)))
        str.method <- tclvalue(tcl.method)
        ea <- newgen(ea, n.ind=i.ind, method=str.method)
    }

    tkdestroy(tkwin)

    assign(.tmp.ea, ea, envir=env.gui.ea)
    
    return(ea)
    
}


save.ea.gui <- function(ea=NULL) {

    if (!(length(ea$generations) && length(ea$genes)))
        ea <- get(.tmp.ea, envir=env.gui.ea)

    save(ea, file=tclvalue(tkgetSaveFile(filetypes="{{RData} {.RData .Rda}}")))
}



genomestruc.gui<-function() {
                                        # Make genomestructure

    updatevalues<-function () {
        tclvalue(tcl.genesel) <- tclvalue(tkcurselection(ttwin.framegenes.list.l))
        i.geneval <- eval(parse(text=tclvalue(tcl.genesel)))
        tkconfigure(ttwin.framedetails.labelparameter,
                    text=tclvalue(tkget(ttwin.framegenes.list.l, i.geneval, i.geneval)))

        tkconfigure(ttwin.framedetails.entrymin,
                    textvariable=tclVar(ttd.gene.min[i.geneval + 1]))
        
        tkconfigure(ttwin.framedetails.entrymax,
                    textvariable=tclVar(ttd.gene.max[i.geneval + 1]))
        
        tkconfigure(ttwin.framedetails.entrystep,
                    textvariable=tclVar(ttd.gene.step[i.geneval + 1]))
        
        tkconfigure(ttwin.framedetails.entrynames,
                    textvariable=tclVar(ttvs.gene.names[i.geneval + 1]))
    }
    
    changevalues<-function() {
        i.geneval <- eval(parse(text=tclvalue(tcl.genesel)))
        assign("ttd.gene.min",
               c(ttd.gene.min[0:i.geneval],
                 eval(parse(text=tclvalue(tkget(ttwin.framedetails.entrymin)))),
                 ttd.gene.min[(i.geneval+2):length(ttd.gene.min)]),
               inherits=T)
        assign("ttd.gene.max",
               c(ttd.gene.max[0:i.geneval],
                 eval(parse(text=tclvalue(tkget(ttwin.framedetails.entrymax)))),
                 ttd.gene.max[(i.geneval+2):length(ttd.gene.max)]),
               inherits=T)
        assign("ttd.gene.step",
               c(ttd.gene.step[0:i.geneval],
                 eval(parse(text=tclvalue(tkget(ttwin.framedetails.entrystep)))),
                 ttd.gene.step[(i.geneval+2):length(ttd.gene.step)]),
               inherits=T)
        assign("ttvs.gene.names",
               c(ttvs.gene.names[0:i.geneval],
                 tclvalue(tkget(ttwin.framedetails.entrynames)),
                 ttvs.gene.names[(i.geneval+2):length(ttvs.gene.names)]),
               inherits=T)
        tkfocus(ttwin.framegenes.list.l)
                                        #        updatevalues()
    }
    
    

    removegene<-function() {
        i.geneval <- eval(parse(text=tclvalue(tcl.genesel)))
        assign("ttd.gene.min",
               c(ttd.gene.min[0:i.geneval],
                 ttd.gene.min[(i.geneval+2):length(ttd.gene.min)]),
               inherits=T)
        assign("ttd.gene.max",
               c(ttd.gene.max[0:i.geneval],
                 ttd.gene.max[(i.geneval+2):length(ttd.gene.max)]),
               inherits=T)
        assign("ttd.gene.step",
               c(ttd.gene.step[0:i.geneval],
                 ttd.gene.step[(i.geneval+2):length(ttd.gene.step)]),
               inherits=T)
        assign("ttvs.gene.names",
               c(ttvs.gene.names[0:i.geneval],
                 ttvs.gene.names[(i.geneval+2):length(ttvs.gene.names)]),
               inherits=T)
        tkdelete(ttwin.framegenes.list.l, i.geneval, i.geneval)
    }

    
    addgene<-function() {
        
        ttb.done<-tclVar(0)
        tts.gene<-tclVar("<Parametername>")
        ttwin.addgene <- tktoplevel()
        tkwm.title(ttwin.addgene,
                   "Parameter name")
        
        tkpack(tklabel(ttwin.addgene,
                       text="Give parameter name:"),
               tkentry(ttwin.addgene,
                       textvariable=tts.gene),
               tkbutton(ttwin.addgene,
                        text="OK",
                        command=function()tclvalue(ttb.done) <- 1),
               side="top")
        tkbind(ttwin.addgene,
               "<Destroy>",
               function()tclvalue(ttb.done) <- 2)
        
                                        #        updatevalues()
        tkwait.variable(ttb.done)
        if(tclvalue(ttb.done) == "1") {
            tts.gene <- tclvalue(tts.gene)
            tkinsert(ttwin.framegenes.list.l,
                     "end",
                     tts.gene)
        }
        
        tkdestroy(ttwin.addgene)
    }
    
    
    ttd.gene.min<-NA
    ttd.gene.max<-NA
    ttd.gene.step<-NA
    ttvs.gene.names<-NA
    tcl.genesel <- NULL
    ttwin.ttb.done<-tclVar(0)

    ttwin <- tktoplevel()
    tkbind(ttwin,
           "<Destroy>",
           function()tclvalue(ttwin.ttb.done) <- 2)
    tkwm.title(ttwin,
               "Parameter set structure")
    

    tkpack(ttwin.frames <- tkframe(ttwin),
           ttwin.buts <- tkframe(ttwin),
           side="top",
           pady="5")
    

    tkpack(ttwin.butok <- tkbutton (ttwin.buts,
                                    text="OK",
                                    command=function()tclvalue(ttwin.ttb.done) <- 1),
           ttwin.butcancel <- tkbutton (ttwin.buts,
                                        text="Cancel",
                                        command=function()tclvalue(ttwin.ttb.done) <- 2),
           side="left")
    
    
    tkpack(ttwin.framegenes <- tkframe(ttwin.frames,
                                       borderwidth="1",
                                       relief="groove"),
           ttwin.framedetails <- tkframe(ttwin.frames,
                                         borderwidth="1",
                                         relief="groove"),
           side="left",
           pady="2",
           padx="5")
    
    
    tkpack(ttwin.framegenes.buts <- tkframe(ttwin.framegenes),
           ttwin.framegenes.list <- tkframe(ttwin.framegenes),
           side="left")
    

    tkpack(tkbutton(ttwin.framegenes.buts,
                    text="Add",
                    command=addgene),
#           tkbutton(ttwin.framegenes.buts,
#                    text="Clear all",
#                    command=""),
           tkbutton(ttwin.framegenes.buts,
                    text="Remove",
                    command=removegene),
           side="top")

    
    
    tkgrid(ttwin.framegenes.list.l <- tklistbox(ttwin.framegenes.list,
                                                height=8,
                                                yscrollcommand=function(...) tkset(ttwin.framegenes.list.sy, ...),
                                                xscrollcommand=function(...) tkset(ttwin.framegenes.list.sx, ...)),
           row="0", column="0",
           rowspan="1", columnspan="1",
           sticky= "news")
    tkgrid(ttwin.framegenes.list.sy <- tkscrollbar(ttwin.framegenes.list,
                                                   orient="vert",
                                                   command=function(...) tkyview(ttwin.framegenes.list.l, ...)),
           row="0", column="1",
           rowspan="1", columnspan="1",
           sticky= "news")
    tkgrid(ttwin.framegenes.list.sx <- tkscrollbar(ttwin.framegenes.list,
                                                   orient="horiz",
                                                   command=function(...) tkxview(ttwin.framegenes.list.l, ...)),
           row="1", column="0",
           rowspan="1", columnspan="1",
           sticky= "news")
    tkbind(ttwin.framegenes.list.l, "<ButtonRelease-1>", updatevalues)
    tkbind(ttwin.framegenes.list.l, "<Return>", updatevalues)
    
    
    tkpack(ttwin.framedetails.f <- tkframe(ttwin.framedetails),
           ttwin.framedetails.b <- tkbutton(ttwin.framedetails,
                                            text="Change",
                                            command=changevalues),
           side="top")


    tkgrid(ttwin.framedetails.labelparameter <- tklabel(ttwin.framedetails.f,
                                                        text="<Parameter>",
                                                        relief="flat"),
           columnspan=2)
    
    tkgrid(tklabel(ttwin.framedetails.f,
                   text="Minimum",
                   relief="flat"),
           ttwin.framedetails.entrymin <- tkentry(ttwin.framedetails.f,
                                                  textvariable=tclVar(NA),
                                                  relief="sunken"))

    tkgrid(tklabel(ttwin.framedetails.f,
                   text="Maximum",
                   relief="flat"),
           ttwin.framedetails.entrymax <- tkentry(ttwin.framedetails.f,
                                                  textvariable=tclVar(NA),
                                                  relief="sunken"))

    tkgrid(tklabel(ttwin.framedetails.f,
                   text="Step",
                   relief="flat"),
           ttwin.framedetails.entrystep <- tkentry(ttwin.framedetails.f,
                                                   textvariable=tclVar(NA),
                                                   relief="sunken"))

    tkgrid(tklabel(ttwin.framedetails.f,
                   text="Names",
                   relief="flat"),
           ttwin.framedetails.entrynames <- tkentry(ttwin.framedetails.f,
                                                    textvariable=tclVar(NA),
                                                    relief="sunken"))


    tkwait.variable(ttwin.ttb.done)
    

    list.g<-NULL

    if (tclvalue(ttwin.ttb.done)=="1") {
        i.genes<-eval(parse(text=tclvalue(tksize(ttwin.framegenes.list.l))))
        list.g$genes <- list()
        for (i in 1:i.genes) {
            list.g$genes[[i]]<-list()
            s.name<-eval(tclvalue(tkget(ttwin.framegenes.list.l, i-1, i-1)))
            if ((substr(s.name, 1, 1)=='{') &&
                (substr(s.name, nchar(s.name), nchar(s.name))=='}'))
                s.name<-substr(s.name, 2, nchar(s.name) - 1)

            list.g$genes[[i]]$name <- s.name
            d.max<-ttd.gene.min[i] + round((ttd.gene.max[i]-ttd.gene.min[i])/ttd.gene.step[i])*ttd.gene.step[i]
            i.allels<-(d.max - ttd.gene.min[i])/ttd.gene.step[i] + 1
            list.g$genes[[i]]$min <- ttd.gene.min[i]
            list.g$genes[[i]]$max <- d.max
            list.g$genes[[i]]$n.allels <- i.allels
            
            vstr.names <- strsplit(ttvs.gene.names[i], ";")[[1]]
            if ( length(vstr.names) != (d.max-ttd.gene.min[i])/ttd.gene.step[i] + 1 )
                vstr.names<-NULL
            list.g$genes[[i]]$names <- vstr.names
        }
    }
    tkdestroy(ttwin)

    assign(.tmp.ea, list.g, envir=env.gui.ea)
    
    return(list.g)
}






nextgeneration.gui <- function(ea=NULL) {

    if (!(length(ea$generations) && length(ea$genes)))
        ea <- get(.tmp.ea, envir=env.gui.ea)

    tcl.done <- tclVar(0)
    tcl.nind <- tclVar(length(ea$generations[[length(ea$generations)]]$fit))
    tcl.pars <- tclVar(ceiling(sqrt(length(ea$generations))))
    tcl.sel.base <- tclVar("fit")
    tcl.sel.resc <- tclVar(0)
    tcl.corate <- tclVar(90)
    tcl.mut.base <- tclVar("unif")
    tcl.mut.spread <- tclVar(1)
    tcl.mut.rate <- tclVar(15)

    tkwin <- tktoplevel()
    tkbind(tkwin,
           "<Destroy>", function()tclvalue(tcl.done) <- 2)
    tkwm.title(tkwin,
               "Evolutionary algorithm parameters")
    

    tkpack(tkwin.main <- tkframe(tkwin),
           tkwin.buts <- tkframe(tkwin),
           side="top")

    tkpack(tkbutton(tkwin.buts,
                    text="OK",
                    command=function()tclvalue(tcl.done) <- 1),
           tkbutton(tkwin.buts,
                    text="Cancel",
                    command=function()tclvalue(tcl.done) <- 2),
           side="left",
           padx=10)

                  
    tkpack(tkwin.frmind <- tkframe(tkwin.main, relief="groove", borderwidth="2"),
           tkwin.frmsel <- tkframe(tkwin.main, relief="groove", borderwidth="2"),
           tkwin.frmco <- tkframe(tkwin.main, relief="groove", borderwidth="2"),
           tkwin.frmmut <- tkframe(tkwin.main, relief="groove", borderwidth="2"),
           side="left", padx="5")
    
    tkpack(tkscale(tkwin.frmind, from=1, to=500,
                   showvalue=T, variable=tcl.nind,
                   resolution=1, orient="horiz"),
           tklabel(tkwin.frmind, text="Number of individuals"),
           tkscale(tkwin.frmind, from=1, to=length(ea$generations),
                   showvalue=T, variable=tcl.pars,
                   resolution=1, orient="horiz"),
           tklabel(tkwin.frmind, text="Number of previous generations to use"),
           side="top")

    tkpack(tklabel(tkwin.frmsel, text="Selection parameters"),
           tkwin.frmsel1 <- tkframe(tkwin.frmsel),
           tkwin.frmsel2 <- tkframe(tkwin.frmsel),
           side="top",
           pady="5")
           
    tkpack(tklabel(tkwin.frmsel1, text="Base"),
           tkradiobutton(tkwin.frmsel1, text="Fitness", value="fit", variable=tcl.sel.base),
           tkradiobutton(tkwin.frmsel1, text="Ranking", value="rank", variable=tcl.sel.base),
           anchor="w",
           side="top")
    tkpack(tkscale(tkwin.frmsel2, from=0, to=5,
                   showvalue=T, variable=tcl.sel.resc,
                   resolution=0.05, orient="horiz"),
           tklabel(tkwin.frmsel2, text="rescale"),
           side="top")

    tkpack(tklabel(tkwin.frmco, text="Crossover parameters"),
           tkwin.frmco1 <- tkframe(tkwin.frmco),
           side="top",
           pady="5")
    tkpack(tkscale(tkwin.frmco1, from=0, to=100,
                   showvalue=T, variable=tcl.corate,
                   resolution=1, orient="horiz"),
           tklabel(tkwin.frmco1, text="rate"),
           side="top")

    tkpack(tklabel(tkwin.frmmut, text="Mutation parameters"),
           tkwin.frmmut1 <- tkframe(tkwin.frmmut),
           tkwin.frmmut2 <- tkframe(tkwin.frmmut),
           tkwin.frmmut3 <- tkframe(tkwin.frmmut),
           side="top",
           pady="5")
    tkpack(tklabel(tkwin.frmmut1, text="Base (distribution)"),
           tkradiobutton(tkwin.frmmut1, text="Uniform", value="unif", variable=tcl.mut.base),
           tkradiobutton(tkwin.frmmut1, text="Normal", value="norm", variable=tcl.mut.base),
           anchor="w",
           side="top")
    tkpack(tkscale(tkwin.frmmut2, from=0, to=1,
                   showvalue=T, variable=tcl.mut.spread,
                   resolution=0.01, orient="horiz"),
           tklabel(tkwin.frmmut2, text="spread"),
           side="top")
    tkpack(tkscale(tkwin.frmmut3, from=0, to=100,
                   showvalue=T, variable=tcl.mut.rate,
                   resolution=1, orient="horiz"),
           tklabel(tkwin.frmmut3, text="rate"),
           side="top")
    
    
    tkwait.variable(tcl.done)

    
    if(tclvalue(tcl.done) == "1") {
        i.nind <- eval(parse(text=tclvalue(tcl.nind)))
        i.pars <- eval(parse(text=tclvalue(tcl.pars)))
        str.sel.base <- eval(tclvalue(tcl.sel.base))
        i.sel.resc <- eval(parse(text=tclvalue(tcl.sel.resc)))
        i.corate <- eval(parse(text=tclvalue(tcl.corate)))
        str.mut.base <- eval(tclvalue(tcl.mut.base))
        i.mut.spread <- eval(parse(text=tclvalue(tcl.mut.spread)))
        i.mut.rate <- eval(parse(text=tclvalue(tcl.mut.rate)))
        local.ea <- nextgen(ea,
                            n.ind=i.nind,
                            gen.parent=c((length(ea$generations) - i.pars + 1): length(ea$generations)),
                            selection=list(base=str.sel.base, rescale=i.sel.resc),
                            corate=i.corate,
                            mutation=list(base=str.mut.base, spread=i.mut.spread, rate=i.mut.rate))
        
        assign(x=.tmp.ea, value=local.ea, envir=env.gui.ea)
    } else {
        local.ea <- NULL
    }
    
    tkdestroy(tkwin)

    return(local.ea)

}




plotfitness.gui<-function(ea=NULL) {

    if (!(length(ea$genes) && length(ea$generations)))
        ea <- get(.tmp.ea, envir=env.gui.ea)


    tcl.show<- tclVar("average")
    tcl.gens<- tclVar(length(ea$generations))
    tcl.xlabel<- tclVar("Batch")
    tcl.ylabel<- tclVar("Fitness")
    tcl.title<- tclVar("Fitness evolution")
    tcl.name<- tclVar("<name>")
    tcl.filetype <- tclVar("png")
    b.gens.sav<- 1 # in case replot.maybe is called too early

    
    replot <- function(...) {
        str.show <- tclvalue(tcl.show)
        i.gens <- eval(parse(text=tclvalue(tcl.gens)))
        str.xlabel <- tclvalue(tcl.xlabel)
        str.ylabel <- tclvalue(tcl.ylabel)
        str.title <- tclvalue(tcl.title)
        eval(substitute(plotevolution.fit(struc.ea=ea,
                                          gens=c((length(ea$generations) - i.gens + 1):length(ea$generations)),
                                          show=str.show,
                                          main=str.title,
                                          xlab=str.xlabel,
                                          ylab=str.ylabel)))
    }
    
    saveplot <- function(...) {
        str.type <- tclvalue(tcl.filetype)
        str.name <- tclvalue(tcl.name)
        if (str.name != "<name>") {
            switch(str.type,
                   png = {png(filename=paste(str.name, ".", str.type, sep=""))
                          replot(...)
                          dev.off()},
                   ps = {postscript(filename=paste(str.name, ".", str.type, sep=""))
                         replot(...)
                         dev.off()},
                   jpg = {jpg(filename=paste(str.name, ".", str.type, sep=""))
                          replot(...)
                          dev.off()}
                   )
        }
    }
    

    replot.maybe <- function(...)
    {
        if (as.numeric(tclvalue(tcl.gens)) != b.gens.sav)
            replot()
    }
    
    
    base <- tktoplevel()
    tkwm.title(base, "Plotevolution")
    
    spec.frm <- tkframe(base,borderwidth=2)
    left.frm <- tkframe(spec.frm)
    middle.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)

    
    # left frames:
    frame1 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame1, text="Show"))
    for ( i in c("box", "average", "all",
                 "max", "min", "stdev") ) {
        tmp <- tkradiobutton(frame1, command=replot,
                             text=i, value=i, variable=tcl.show)
        tkpack(tmp, anchor="w")
    }
    
    # middle frames:
    frame2 <- tkframe(middle.frm, relief="groove", borderwidth=2)
    tkgrid(tklabel(frame2, text="Generations"),
           tkscale(frame2, command=replot.maybe, from=2, to=length(ea$generation),
                   showvalue=T, variable=tcl.gens,
                   resolution=1, orient="horiz"))
    tkgrid(tklabel(frame2, text="Main Title"),
           tkentry(frame2, textvariable=tcl.title))
    tkgrid(tklabel(frame2, text="x label"),
           tkentry(frame2, textvariable=tcl.xlabel))
    tkgrid(tklabel(frame2, text="y label"),
           tkentry(frame2, textvariable=tcl.ylabel))
    tkgrid(tkbutton(frame2, text="Apply", command=replot), columnspan=2)
    

    # right frame
    frame3 <- tkframe(right.frm, relief="groove", borderwidth=2)
    tkgrid(tklabel(frame3, text="Name"),
           tkentry(frame3, textvariable=tcl.name))
    for (i in c("png", "ps", "jpg")) {
        tmp <- tkradiobutton(frame3,
                             text=i,
                             value=i,
                             variable=tcl.filetype)
        tkgrid(tmp, columnspan=2)
    }
    tkgrid(tkbutton(frame3, text="Save", command=saveplot),
           columnspan=2)


    
    tkpack(frame1,
           fill="x")
    tkpack(frame2,
           fill="x")
    tkpack(frame3,
           fill="x")
    tkpack(left.frm, middle.frm, right.frm,
           side="left",
           anchor="n")
    
    ## `Bottom frame' (on base):
    q.but <- tkbutton(base,text="Quit",
                      command=function()tkdestroy(base))
    
    tkpack(spec.frm, q.but)
    
    replot()
}

 

plotgene.gui<-function(ea=NULL) {

    if (!(length(ea$genes) && length(ea$generations)))
        ea <- get(.tmp.ea, envir=env.gui.ea)


    tcl.gene<-tclVar(1)
    tcl.gens<- tclVar(length(ea$generations))
    tcl.xlabel<- tclVar("Value")
    tcl.ylabel<- tclVar("Frequency")
    tcl.title<- tclVar("Generation")

    tcl.name<- tclVar("<name>")
    tcl.filetype <- tclVar("png")
    b.gens.sav<- 1 # in case replot.maybe is called too early
    
    replot <- function(...) {
        i.gene <- eval(parse(text=tclvalue(tcl.gene)))
        i.gens <- eval(parse(text=tclvalue(tcl.gens)))
        str.xlabel <- tclvalue(tcl.xlabel)
        str.ylabel <- tclvalue(tcl.ylabel)
        str.title <- tclvalue(tcl.title)
        eval(substitute(plotevolution.gene(ea,
                                           gene=i.gene,
                                           gens=c(1:i.gens),
                                           main=str.title,
                                           xlab=str.xlabel,
                                           ylab=str.ylabel)))
    }
    
    saveplot <- function(...) {
        str.type <- tclvalue(tcl.filetype)
        str.name <- tclvalue(tcl.name)
        if (str.name != "<name>") {
            switch(str.type,
                   png = {png(filename=paste(str.name, ".", str.type, sep=""))
                          replot(...)
                          dev.off()}
                   )
        }
    }
    

    replot.maybe <- function(...)
    {
        if (as.numeric(tclvalue(tcl.gens)) != b.gens.sav)
            replot()
    }
    
    
    base <- tktoplevel()
    tkwm.title(base, "Plot evolution")
    
    spec.frm <- tkframe(base,borderwidth=2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)

    
    # left frames:
    frame1 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkgrid(tklabel(frame1, text="Gene"),
           tkscale(frame1, command=replot.maybe, from=1, to=length(ea$genes),
                   showvalue=T, variable=tcl.gene,
                   resolution=1, orient="horiz"))
    tkgrid(tklabel(frame1, text="Generations"),
           tkscale(frame1, command=replot.maybe, from=2, to=length(ea$generation),
                   showvalue=T, variable=tcl.gens,
                   resolution=1, orient="horiz"))
    tkgrid(tklabel(frame1, text="Main Title"),
           tkentry(frame1, textvariable=tcl.title))
    tkgrid(tklabel(frame1, text="x label"),
           tkentry(frame1, textvariable=tcl.xlabel))
    tkgrid(tklabel(frame1, text="y label"),
           tkentry(frame1, textvariable=tcl.ylabel))
    tkgrid(tkbutton(frame1, text="Apply", command=replot), columnspan=2)
    

    # right frame
    frame3 <- tkframe(right.frm, relief="groove", borderwidth=2)
    tkgrid(tklabel(frame3, text="Name"),
           tkentry(frame3, textvariable=tcl.name))
    for (i in c("png", "ps", "jpg")) {
        tmp <- tkradiobutton(frame3,
                             text=i,
                             value=i,
                             variable=tcl.filetype)
        tkgrid(tmp, columnspan=2)
    }
    tkgrid(tkbutton(frame3, text="Save", command=saveplot),
           columnspan=2)


    
    tkpack(frame1,
           fill="x")
    tkpack(frame3,
           fill="x")
    tkpack(left.frm, right.frm,
           side="left",
           anchor="n")
    
    ## `Bottom frame' (on base):
    q.but <- tkbutton(base,text="Quit",
                      command=function()tkdestroy(base))
    
    tkpack(spec.frm, q.but)
    replot()
}
