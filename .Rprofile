if (interactive())
  try(fortunes::fortune(), silent = TRUE)

  # Caminho para o arquivo .Rprofile padrão no diretório home
  temp_home_rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
  # Caminho para o arquivo .Rprofile no diretório do projeto
  temp_project_rprofile <- file.path(getwd(), ".Rprofile")
  # Verifica se o arquivo .Rprofile existe no diretório do projeto
  if (!file.exists(temp_project_rprofile)) {
    invisible(file.copy(temp_home_rprofile, temp_project_rprofile))
    message("O arquivo .Rprofile carregado de ~/.Rprofile")
  } else {
    message("O arquivo .Rprofile já existente")
  }
  options(prompt = "R> ", #The R prompt, from the boring > to the exciting R>
          digits = 4, #The number of digits displayed
          show.signif.stars = FALSE, #Removing the stars after significant  p-values.
          continue = "   " #Removing the + in multi-line functions.
  )
  
  rm_temp<-function(pattern="^temp_"){
    temp_objects <- grep(pattern, ls(envir = .GlobalEnv), value = TRUE)
    if (length(temp_objects) > 0) {
      rm(list = temp_objects, envir = .GlobalEnv)}
    invisible(NULL)
  }
  rm_temp()
