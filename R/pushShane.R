#' Synchronize Folders
#'
#' This function synchronizes files and directory structure between a source folder and a destination folder.
#' If a file is modified or deleted in the source folder, it will be updated or deleted in the destination folder.
#' The directory structure in the source folder will be replicated in the destination folder.
#'
# @param sourceFolder The path to the source folder.
# @param destinationFolder The path to the destination folder.
#' @return None
#' @export

pushShane <- function() {
    
    sourceFolder <- r"(C:\Users\kell343\OneDrive - PNNL\Documents\dev\test_source)"
    destinationFolder <- r"(\\pnl\projects\MSSHARE\Shane_Kelly_MSS\powershel_test_dest)"
    
    
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
        destination <- file.path(destinationPath, sub(sourceFolder, "", sourcePath))
        
        if (file.exists(sourcePath)) {
            dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
            file.copy(sourcePath, destination, overwrite = TRUE)
            cat(paste(file, "- Synced\n"), file = "stdout", append = TRUE)
        } else {
            if (file.exists(destination)) {
                file.remove(destination)
                cat(paste(file, "- Deleted\n"), file = "stdout", append = TRUE)
            }
        }
    }
    
    syncFilesInFolder <- function(folderPath) {
        files <- list.files(path = folderPath, recursive = TRUE, full.names = TRUE)
        
        for (file in files) {
            syncFiles(file, destinationFolder)
        }
    }
    
    sourceFolder <- normalizePath(sourceFolder)
    destinationFolder <- normalizePath(destinationFolder)
    
    syncFilesInFolder(sourceFolder)
    
    cat("Folder synchronization completed.\n", file = "stdout")
}
