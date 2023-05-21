#' Synchronize Folders
#'
#' This function synchronizes files between a source folder and a destination folder.
#'
#' @return None
#' @export

pullShane <- function() {
    
    sourceFolder <- r"(\\pnl\projects\MSSHARE\Shane_Kelly_MSS\powershel_test_dest)"
    destinationFolder <- r"(C:\Users\kell343\OneDrive - PNNL\Documents\dev\test_source)"
    
    # Check if source folder is accessible
    if (!dir.exists(sourceFolder)) {
        stop("Source folder is not accessible!")
    }
    
    # Check if destination folder is accessible
    if (!dir.exists(destinationFolder)) {
        stop("Destination folder is not accessible!")
    }
    
    syncFiles <- function(sourcePath, destinationPath) {
        file <- basename(sourcePath)
        destination <- file.path(destinationPath, sub(sourceFolder, "", file))
        
        tryCatch({
            if (file.exists(sourcePath)) {
                file.copy(sourcePath, destination, overwrite = TRUE)
                cat(paste(file, "- Synced\n"), file = "stdout", append = TRUE)
            } else if (file.exists(destination)) {
                file.remove(destination)
                cat(paste(file, "- Deleted\n"), file = "stdout", append = TRUE)
            }
        }, error = function(e) {
            cat(paste(file, "- Sync Failed\n"), file = "stdout", append = TRUE)
        })
    }
    
    syncFilesInFolder <- function(folderPath) {
        files <- list.files(path = folderPath, recursive = TRUE, full.names = TRUE)
        
        for (file in files) {
            syncFiles(file, destinationFolder)
        }
    }
    
    syncFilesInFolder(sourceFolder)
    
    cat("Folder synchronization completed.\n", file = "stdout")
}
