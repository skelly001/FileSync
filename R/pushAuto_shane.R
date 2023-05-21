#' Synchronize Two Folders
#'
#' This function synchronizes two folders by mirroring the files and directories
#' from the source folder to the destination folder.
#'
# @param sourceFolder Path to the source folder.
# @param destinationFolder Path to the destination folder.
#' @import tools
#'
#' @examples
#' synchronizeFolders("C:/Path/to/Source/Folder", "//RemoteComputer/Shared/Folder")
#'
pushAuto_shane <- function() {
    
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
        destination <- file.path(destinationPath, file)
        
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
    
    syncWatcher <- tools::watchlist(
        sourceFolder,
        handler = function(...) {
            sourcePath <- file.path(sourceFolder, basename(.))
            syncFiles(sourcePath, destinationFolder)
        }
    )
    
    cat("Folder synchronization started. Press Esc to stop.\n", file = "stdout")
    
    while (!tools::checkEscape()) {
        Sys.sleep(1)
    }
    
    cat("\nSynchronization interrupted.\n", file = "stdout")
}
