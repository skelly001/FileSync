

pushShane <- function() {
    
    sourceFolder <- "C:/Users/kell343/OneDrive - PNNL/Documents/dev/test_source"
    destinationFolder <- "//pnl/projects/MSSHARE/Shane_Kelly_MSS/powershel_test_dest"
    
    # Check if source folder is accessible
    if (!dir.exists(sourceFolder)) {
        stop("Source folder is not accessible!")
    }
    
    # Check if destination folder is accessible
    if (!dir.exists(destinationFolder)) {
        stop("Destination folder is not accessible!")
    }
    
    syncFiles <- function(file, destinationFolder) {
        fileSourceEnding <- sub(normalizePath(sourceFolder, winslash = "/"), 
                                "", 
                                normalizePath(file, winslash = "/"))
        destination <- file.path(destinationFolder, fileSourceEnding)
        
        tryCatch({
            if (!file.exists(destination)) {
                if (!file.exists(dirname(destination))) {
                dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
            }
                file.copy(file, dirname(destination))
                cat(paste(file, "- Copied\n"), file = "stdout", append = TRUE)
            } else {
                # if (file.exists(destination)) {
                #     if (file.info(destination)$ctime != file.info(destination)$mtime) {
                #         file.copy(file, dirname(destination))
                #     }
                # }
                    # cat(paste(file, "- Overwritten\n"), file = "stdout", append = TRUE)
            }
        }, error = function(e) {
            cat(paste(file, "- Sync Failed\n"), file = "stdout", append = TRUE)
        })
    }
    
    syncFilesInFolder <- function(folderPath) {
        files             <- list.files(path = folderPath, recursive = TRUE, full.names = TRUE)
        files             <- normalizePath(files, winslash = "/")
        sourceFolder      <- normalizePath(sourceFolder, winslash = "/")
        destinationFolder <- normalizePath(destinationFolder, winslash = "/")
        
        for (file in files) {
            syncFiles(file, destinationFolder)
        }
    }
    
    syncFilesInFolder(sourceFolder)
    
    cat("Folder synchronization completed.\n", file = "stdout")
}

#How know when to overwrite a file?
#For sync, how know when to copy/delete?
#How know which to rename?
