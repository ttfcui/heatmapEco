library(heatmapEco)
out <- read.csv(commandArgs(T)[2], header=F, stringsAsFactors=F,
                na.strings="")
args <- lapply(out$V2, identity)
args <- setNames(args, lapply(out$V1, identity))[!is.na(args)]
p <- args[which(names(args) == "pol.break")]
args[which(names(args) == "pol.break")] <- list(eval(parse(text=p)))
p <- args[which(names(args) == "custom.f")]
args[which(names(args) == "custom.f")] <- list(as.numeric(eval(parse(text=p))))
if (!is.null(args$t.per)) (
      if (args$t.per == "NULL") args$t.per <- NULL
)
print("Evaluation of function arguments:")
print(args)

Sys.sleep(0.6)
b <- do.call("heatmapEco", args)
