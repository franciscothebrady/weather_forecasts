RunProcess = function(executable, arguments)
{
  command = paste0("\"", executable,  "\"", arguments)
  
  print (command)
  
  exitCode = system(
    command,
    intern = FALSE,
    ignore.stdout = FALSE,
    ignore.stderr = FALSE,
    wait = TRUE,
    input = NULL,
    show.output.on.console = TRUE,
    invisible = FALSE
  )
  
  if (exitCode != 0)
  {
    stop("Process returned error")
    
  }
  return (exitCode)
}

ExecutableFileName7Zip <- function()
{
  executableName <- "C:\\Program Files (x86)\\7-Zip\\7z.exe"

  # "C:\\PROGRA~2\\7-Zip\\7z.exe"
  # "C:\\Program Files (x86)\\7-Zip\\7z.exe"
  # "C:\\Users\\Franc\\7-Zip\\7z.exe"
  # "C:\Program Files\7-Zip\7z.exe"
  if (file.exists(executableName))
  {
    return (executableName)
  }
  
  stop("failed to find 7zip")
}

Decompress7Zip <- function(zipFileName, outputDirectory, delete)
{
  executableName <- ExecutableFileName7Zip()
  
  arguments <- paste(sep = "",
                     " e ",
                     "\"",
                     zipFileName,
                     "\" ",
                     "\"-o",
                     outputDirectory,
                     "\" ",
                     "\"-y",
                     "")
  
  print(arguments)
  
  RunProcess(executableName, arguments)
  
  if (delete)
  {
    unlink(zipFileName)
    
  }
}
