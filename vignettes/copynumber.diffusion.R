# sub-script of CrossmapDecomposition.Rmd
# run crossmap diffusion info on some genes


if (!assignc("expression.diffusion.examples")) {  
  expression.diffusion.examples =
    configsrun.diffusion(expression.configs, features=gene.diffusion.text)
  expression.diffusion.examples = merge(
    expression.diffusion.examples,
    expression.configs[, c("config.label", "diffusion.label",
                           "diffusion.strength.positions",
                           "diffusion.strength.interactions")],
    by="config.label")
  setcolorder(expression.diffusion.examples,
              c("config.label", "diffusion.label"))
  savec(expression.diffusion.examples)
}

