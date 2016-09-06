* heatmap v. 0.3, updated 08/09/2016
* CONTACT: Tom Cui (Tom.Cui@chicagobooth.edu)

* TODO: Does this function convert times or not?
* For now, to work have to convert data into machine-readable time first.

program define heatmap
    syntax varlist(ts min=3) [if] [pweight fweight aweight iweight], id(varname) ///
        save(string asis) [Nquantiles(integer 10) tsort(string) TPERiod(string) ///
        CONTROLs(varlist numeric ts fv) Absorb(varname) GRPFunc(string) noAddmean ///
        probs(numlist) polbreak(string asis) splitx(string) splity(string) ///
        customf(numlist) count OUTliers ISFactor ZTItle(string) KEEPagg]

    version 12.1

    local cutlast = 0
    foreach cut of local customf {
        if `cut' < `cutlast' {
            di as error "Percentiles in argument customf must be in ascending order."
            exit
        }
        local cutlast = `cut'
    }
    if !inlist("`tperiod'", "", "year", "NULL", "yearmon", "quarter") {
        di as error "Time interval type not implemented."
        exit
    }
    if "$linkdir" == "" {
        di "Warning: Assuming Rscript is in PATH. Otherwise, set the directory" ///
           " where R was installed as the global linkdir (see manual for details)."
        local Rscript_call Rscript
    }
    else {
        local Rscript_call $linkdir/Rscript
    }
    if "`polbreak'" != "" | "`tsort'" != "" {
        if "`tperiod'" == "yearmon" {
            di `"Warning: Because time interval in data recognized as "yearmon","' ///
            `" ensure values in polbreak, tsort are of the form "Jan 2010", "Jul 2012"..."'
        }
        else if "`tperiod'" == "quarter" {
            di `"Warning: Because time interval in data recognized as "quarter","' ///
            `" ensure values in polbreak, tsort are of the form "2010Q1", "2012Q3"..."'
        }
    }

    * If aggregation happened earlier, skip directly to heatmap generation
    capture confirm file heatmap_out.csv
    if "`keepagg'" == "" | _rc != 0 {

    preserve
    * Keep FE columns if considering residuals
    * DOESN'T WORK WITH CONTROLS WITH OPERATORS
    * keep `varlist' `exp' `controls' `id' `absorb'
    if "`if'" != "" keep `if'
    tokenize `varlist'
    local yvar `1'
    local xvar `2'
    local ivar `3'

    if (`"`controls'`absorb'"' != "") quietly {
    heatmap_projection `yvar' `xvar' [`weight' `exp'] `if',  ///
        control(`controls') absorb(`absorb')
    }
    * Regrouping the index variable if continuous
    heatmap_aggregate `yvar' `xvar' `ivar' [`weight' `exp'] `if', id(`id') ///
        nquantiles(`nquantiles') tsort(`tsort') probs(`probs') grpfunc(`grpfunc') `isfactor'
    * Rename columns, convert time to string
    drop if missing(quantile)
    rename (`yvar' `ivar') (z index)
    time_conv, tperiod(`tperiod')

    }
    else {
        di "Note: CSV exists, therefore aggregation process skipped."
    }
    * Export each cell value to a CSV, then run the R script/ggplot
    * that actually makes the map.
    if regexm("`save'", ".*/") == 1 local outdir = regexs(0)
    local fname "`outdir'heatmap_out.csv"
    capture confirm file `fname'
    if "`keepagg'" == "" | _rc != 0 export delimited `fname', replace

    gen_args `outdir'heatmap_args, fname(`fname') save(`save') tperiod(`tperiod') ///
        `count' `outliers' `isfactor' splitx(`splitx') splity(`splity') ztitle(`ztitle') ///
        polbreak(`polbreak') customf(`customf')
    gen_link // Generate the heatmap_link script if file does not exist

    !"`Rscript_call'" heatmap_link.R --args "`outdir'heatmap_args.csv"
    if "`keepagg'" != "" exit
    erase "`outdir'heatmap_args.csv"
    erase `fname'

end

program define heatmap_projection /* %< */
    syntax varlist(ts max=2) [if] [pweight fweight aweight iweight/], ///
        [CONTROLs(varlist numeric ts fv) Absorb(varname) noAddmean]
        
    if "`exp'" != "" local wgt [`weight'=`exp']
    * Run projection on controls for y, x

        if `"`absorb'"'!="" local absorb "absorb(`absorb')"
        foreach var in `1' `2' {
                tempvar residvar
                _regress ``var'' `controls' `wgt', `absorb'
                _predict `residvar' if e(sample), residuals
                if ("`addmean'"!="noaddmean") {
                        summarize ``var'' `wgt' `if', meanonly
                        replace `residvar'=`residvar'+r(mean)
                }
                local `var' `residvar'
        }
end
* %>

program define heatmap_aggregate /* %< */
    syntax varlist(ts max=3) [if] [pweight fweight aweight iweight/], id(varname) ///
        nquantiles(integer) [tsort(string) probs(numlist) GRPFunc(string) ISFactor]

    local tsort = real("`tsort'") // Conversion to avoid need for default arg
    if "`exp'" != "" local wgt [`weight'=`exp']
    if "`grpfunc'" == "" local grpfunc mean
    tempfile orig quant
    save `orig', replace

    * Building a table with unique id index and the instrument/group
    * Also allows sorting for time-varying variables
    capture confirm tsort
    if _rc == 0 keep if `3' == `tsort'
    duplicates drop `id', force
    * Build the instrument quantiles, or retain as groups.
    capture confirm probs
    if _rc == 0 {
        foreach num of local probs {
            local pscale = 100*`num'
            local probs_scale `probs_scale' `pscale'
        }
        _pctile `2' `wgt', p(`probs_scale')
        xtile quantile = `2', cutpoints(r(r#))
    }
    else if "`isfactor'" == ""{
        xtile quantile = `2' `wgt', nquantiles(`nquantiles')
    }
    else {
        gen quantile = `2'
    }
    * Merge and aggregate
    save `quant', replace
    use `orig', clear
    merge n:1 `id' using `quant', nogen
    collapse (`grpfunc') `1', by(`3' quantile) // Summary funcs only
end
* %>

program define time_conv /* %< */
    syntax, [tperiod(string)]

    if "`tperiod'" != "NULL" {
        if "`tperiod'" == "" replace index = dofy(index)
        else if "`tperiod'" == "yearmon" replace index = dofm(index)
        else if "`tperiod'" == "quarter" replace index = dofq(index)
        format index %td
    }
end
* %>

program define gen_link /* %< */

    * This function writes the R script needed to link the Stata and R commands
    * onto disk, if the file does not exist in the current directory
    capture confirm file heatmap_link.R
    if _rc != 0 {
       if "`c(os)'" == "Windows" {
       !echo library(heatmapEco) > heatmap_link.R

       !echo out = read.csv(commandArgs(T)[2], header=F, stringsAsFactors=F, na.strings='') >> heatmap_link.R
       !echo args = lapply(out\$V2, identity) >> heatmap_link.R
       !echo args = setNames(args, lapply(out\$V1, identity))[!is.na(args)] >> heatmap_link.R
       !echo p = args[which(names(args) == 'pol.break')] >> heatmap_link.R
       !echo args[which(names(args) == 'pol.break')] = list(eval(parse(text=p))) >> heatmap_link.R
       !echo p = args[which(names(args) == 'custom.f')] >> heatmap_link.R
       !echo args[which(names(args) == 'custom.f')] = list(as.numeric(eval(parse(text=p)))) >> heatmap_link.R

       !echo if (!is.null(args\$t.per)) ( >> heatmap_link.R
       !echo       if (args\$t.per == 'NULL') args\$t.per = NULL >> heatmap_link.R
       !echo ) >> heatmap_link.R

       !echo print('Evaluation of function arguments:') >> heatmap_link.R
       !echo print(args) >> heatmap_link.R
       *!echo print(p) >> heatmap_link.R

       !echo Sys.sleep(0.6) >> heatmap_link.R
       !echo b = do.call('heatmapEco', args) >> heatmap_link.R
       }
       * Unix echo calls can be safely put in single quotes, but now
       * the single quotes in Windows ver. must be double quotes
       else {
       !echo '#!/usr/local/bin' > heatmap_link.R
       !echo 'library(heatmapEco)' >> heatmap_link.R

       !echo 'out = read.csv(commandArgs(T)[2], header=F, stringsAsFactors=F, na.strings="")' >> heatmap_link.R
       !echo 'args = lapply(out\$V2, identity)' >> heatmap_link.R
       !echo 'args = setNames(args, lapply(out\$V1, identity))[!is.na(args)]' >> heatmap_link.R
       !echo 'p = args[which(names(args) == "pol.break")]' >> heatmap_link.R
       !echo 'args[which(names(args) == "pol.break")] = list(eval(parse(text=p)))' >> heatmap_link.R
       !echo 'p = args[which(names(args) == "custom.f")]' >> heatmap_link.R
       !echo 'args[which(names(args) == "custom.f")] = list(as.numeric(eval(parse(text=p))))' >> heatmap_link.R

       !echo 'if (!is.null(args\$t.per)) (' >> heatmap_link.R
       !echo '      if (args\$t.per == "NULL") args\$t.per = NULL' >> heatmap_link.R
       !echo ')' >> heatmap_link.R

       !echo 'print("Evaluation of function arguments:")' >> heatmap_link.R
       !echo 'print(args)' >> heatmap_link.R

       !echo 'Sys.sleep(0.6)' >> heatmap_link.R
       !echo 'b = do.call("heatmapEco", args)' >> heatmap_link.R
       }
    }

end
* %>

program define gen_args /* %< */
    syntax anything(name=output), fname(string asis) save(string asis) [tperiod(string) count ///
        outliers isfactor splitx(string) splity(string) ztitle(string) ///
        polbreak(string asis) customf(string)]

    * This function writes the arguments to the R heatmap call "safely,"
    * i.e. within Stata. R link script then reads the R script needed to link the Stata and R commands
    * onto disk, if the file does not exist in the current directory

    clear

    if "`tperiod'" != "NULL" local tfmt %d%b%Y
    if "`count'" != "" local count TRUE
    if "`outliers'" != "" local out_bool TRUE
    if "`isfactor'" != "" local isfactor TRUE

    local argparse fname save tfmt tperiod count out_bool isfactor ///
        splitx splity ztitle

    mata: X = ("xq", "fname", "save", "t.fmt", "t.per", "count", ///
         "outliers", "factor.ax", "split.x", "split.y", "zlab", ///
        "pol.break", "custom.f")' // Last row are vector-valued args
    getmata argnames=X, force
    gen argv = ""

    local i = 1
    replace argv = "TRUE" in `i'
    foreach arg of local argparse {
        local ++i
        replace argv = "``arg''" in `i'
    }
    * String vectors
    if "`polbreak'" != "" {
    replace argv = "c('" + subinstr("`polbreak'", ", ", "','",.) + "')" in -2
    }
    * Numeric vectors
    if "`customf'" != "" {
    replace argv = "c(" + subinstr("`customf'", " ", ",",.) + ")" in -1
    }
    display "Verify policy markers correct: " argv[12] // currently -2 == 12
    assert `i' + 2 == _N

    export delimited using "`output'", novar replace 

end /* %> */
