## ADB

## Compile Rmarkdown report

# compile_rmd function ----------------------------------------------------
compile_rmd_word <- function(file) {
  rmarkdown::render(input =       here::here('rmds', paste(file, '.rmd', sep = '')),
                    output_file = here::here('reports', paste(file, '.docx', sep = '') )
  )
}
