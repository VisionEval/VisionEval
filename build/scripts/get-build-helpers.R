# Helper functions for Building / Configuring

if ( ! suppressWarnings(require(git2r,quietly=TRUE)) ) {
  install.packages("git2r", lib=dev.lib, dependencies=NA, type=.Platform$pkgType )
}

# Helper function to get name of current branch on repopath
localBranch <- function(repopath) {
  localbr <- git2r::branches(repopath,flags="local")
  hd <- which(sapply(localbr,FUN=git2r::is_head,simplify=TRUE))
  return( localbr[[hd]]$name )
}

# Helper function to get roots and branches
checkBranchOnRoots <- function(roots,branches) {
#  rtb <- names(branches)
  for ( rt in roots ) {
    # It's okay if there is not branch (i.e. not github)
    # but it's an error if there is a local branch name and it doesn't match
    if ( length(branches) > 0 ) {
      br <- branches[rt]
      if ( length(br)==1 && nzchar(br) ) {
        repopath <- get(rt)
  #       cat("Examining branch for",rt,"which should be",paste0("'",br,"'"),"\n")
        if ( dir.exists(repopath) && git2r::in_repository(repopath) ) {
          # Find the currently checked out branch by looking at HEAD for local branches
          # cat("Need branch",paste0("<",br,">"),"on repository",repopath,"\n")
          hd <- localBranch(repopath)
          # cat("Have branch",paste0("<",hd,">"),"on repository",repopath,"\n")
          cat("hd:",hd,"\n")
          cat("br:",br,"\n")
          if ( hd != br) {
            cat(paste("Root",rt,"wants branch",paste0("<",br,">"),"but has",paste0("<",hd,">")),"\n")
            return(FALSE)
          }
        } else {
          end.message <- if ( ! dir.exists(repopath) ) {
            "does not exist"
          } else if ( ! git2r::in_repository(repopath) ) {
            "is not a Git repository"
          } else {
            "is bad for an unknown reason"
          }
          cat("Branch",paste0("'",br,"'"),"specified, but",repopath,end.message,".\n")
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
}

# Helper function for other scripts, to verify situational awareness

checkVEEnvironment <- function() {
  # Check for situational awareness, and report if we are lost
  # Returns 0 or 1
  if ( ! exists("ve.installer") || is.na(ve.installer) || ! file.exists(ve.installer) ) {
    message("Missing ve.installer; run build-config.R\n")
    return(FALSE)
  } else if ( ! exists("ve.repository") || is.na(ve.repository) ) {
    message("Missing ve.repository definition; run build-config.R\n")
    return(FALSE)
  } else if ( ! exists("ve.dependencies") || is.na(ve.dependencies) ) {
    message("Missing ve.dependencies definition; run build-config.R\n")
    return(FALSE)
  } else if ( ! exists("ve.runtime") || is.na(ve.runtime) ) {
    message("Missing ve.runtime definition; run build-config.R\n")
    return(FALSE)
  } else if ( ! exists("ve.pkgs") || is.na(ve.pkgs) ) {
    message("Missing ve.pkgs definition; run build-config.R\n")
    return(FALSE)
  } else if ( ! exists("ve.lib") || is.na(ve.lib) ) {
    message("Missing ve.lib definition; run build-config.R\n")
    return(FALSE)
  } else if ( ! exists("ve.roots") || ! exists("ve.branches") || ! checkBranchOnRoots(ve.roots,ve.branches) ) {
    message("Missing roots, or incorrect branches\n")
    return(FALSE)
  }
  return(TRUE)
}

# The following two helpers extract modules from built packages
# Used in the scripts to detect whether a module has been built yet.
modulePath <- function( module, path ) {
  # determine which module in a vector of names is present in the
  # path
  #
  # Args:
  #   module: a character vector of module names to look for
  #   path: a file system path to look for the modules
  #
  # Returns:
  #   A character vector of the file system names (include version
  #   number strings) for the corresponding packages in module,
  #   if any
  mods <- dir(path)
  # result <- mods[grep(paste("^", basename(module), "_", sep=""), mods)]
  matching <- paste("^", basename(module), "_", sep="")
  test<-sapply(matching,FUN=function(x){ grep(x,mods) },simplify=TRUE,USE.NAMES=FALSE)
  if ( class(test)=="list" ) test <- integer(0) # weirdness of sapply(simplify=TRUE) when empty
  result <- mods[test]
}

moduleExists <- function( module, path ) {
  # determine if modulePath found any 'modules' in 'path'
  #
  # Args:
  #   module: a character vector of module names to look for
  #   path: a file system path to look for the modules
  #
  # Returns:
  #   TRUE if any matching modules were found in path, else FALSE
  #
  # Let us genuflect briefly toward a coding standard that calls for
  # a dozen lines of documentation for a one line "alias"
  found <- modulePath(module,path)
  found.test <- length(found)>0
}

# Helper function to compare package path (source) to a built target (modification date)
newerThan <- function( srcpath, tgtpath, quiet=TRUE ) {
  # Compare modification time for a set of files to a target file
  #
  # Args:
  #   srcpath - a single folder containing a bunch of files that might be newer, or a vector of files
  #   tgtpath - one (or a vector) of files that may be older, or may not exist
  #   quiet - if TRUE, then print a message about what is being tested
  #
  # Value: TRUE if the most recently modified source file is newer
  #        than the oldest target file
  if (!quiet) cat("Comparing",srcpath,"to",paste(tgtpath,collapse="\n"),"\n")
  if ( any(is.null(srcpath)) || any(is.na(srcpath)) || any(nchar(srcpath))==0 || ! file.exists(srcpath) ) return(TRUE)
  if ( any(is.null(tgtpath)) || any(is.na(tgtpath)) || any(nchar(tgtpath))==0 || ! file.exists(tgtpath) ) return(TRUE)
  if ( dir.exists(srcpath) ) srcpath <- file.path(srcpath,dir(srcpath,recursive=TRUE,all.files=FALSE))
  if ( dir.exists(tgtpath) ) tgtpath <- file.path(tgtpath,dir(tgtpath,recursive=TRUE,all.files=FALSE))
  if ( length(tgtpath) < 1 ) return(TRUE)
  source.time <- file.mtime(srcpath)
  target.time <- file.mtime(tgtpath)
  source.newest <- order(source.time,decreasing=TRUE)
  target.newest <- order(target.time,decreasing=TRUE)
  if (!quiet) cat("Source path:",srcpath[source.newest[1]],strftime(source.time[source.newest[1]],"%d/%m/%y %H:%M:%S"),"\n")
  if (!quiet) cat("Target:",tgtpath[target.newest[1]],strftime(target.time[target.newest[1]],"%d/%m/%y %H:%M:%S"),"\n")
  newer <- source.time[source.newest[1]] > target.time[target.newest[1]]
  if (!quiet) cat("Newer:",newer,"\n")
  newer
}
