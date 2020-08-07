# exporter.R (defines ve.export tool)

This file documents the `ve.export()` tool for VisionEval, which allows users to export results of model scenario runs into a set of `.csv` files. The source code is located in `tools/exporter.R`.

- [Manual](#Manual) for `ve.export()`
- [Structure](#Code) of the `exporter.R` source code file

## <a id="Manual"></a>Manual for `ve.export()`

`ve.export` provides a simple function to locate and dump a VisionEval Datastore into `.csv` files. `ve.export` is contained in the `tools/exporter.R` file in the standard VisionEval distribution.  By default, VisionEval loads `ve.export` when the user runs `VisionEval.bat` (or sources `VisionEval.R`).  Loading `ve.export` makes use of the [R `import` package](https://cran.r-project.org/web/packages/import/index.html). The import code looks like this (`VisionEval.R` also ensures that the `import` package itself is available):

```
  import::here(ve.export,.from="tools/exporter.R")
```

Once `ve.export` is loaded, you can run it with suitable arguments (including none at all).

Running `ve.export()` with no arguments will work fine. Defaults will be supplied, and if the function is run interactively, it will provide a dialog to find a directory in which `ModelState.Rda` exists.

If you run `ve.export()` from a script, it will export based on `ModelState.Rda` in the current directory (the default), or if you provide `modelStateFile` it will (only) look in whatever directory is implied in that argument.  If there is no `ModelState.Rda` in the given directory, it will exit with an error.

The `includeTables` and `excludeTables` arguments help you limit what to export.  By default, all tables except `Model` (which is not an output table in any case, but rather a list of the modules that were used) and `Vehicle` are exported. `Vehicle` could be exported but in VERPAT, it is broken - two sets of outputs are presented for the same table, but they have an incompatible number of rows. That's a bug in VERPAT or one of the modules used by it.  If there are tables you don't need, you can exclude them (or alternatively, provide an explicit list of tables to include).

Any table not listed in `includeTables` will be excluded.

Any table listed in both `includeTables` and `excludeTables` will be excluded. If `includeTables` does not list anything, all tables are included (and then any named in `excludeTables` are excluded).

If you set the `quiet` parameter to `TRUE` it will only issue errors; otherwise you'll get various debugging progress messages.

By default, the `outputFolder` will be created (relative to the folder in the `modelStateFile` parameter), but if `outputFolder` already exists, an error will be reported and the function will terminate. If `overwrite` is `TRUE`, the `outputFolder` will be *forcefully* and *recursively* deleted.  __*NOTE:*__ Obviously, be very careful about how you set `outputFolder` if you choose to `overwrite`!

### Argument Reference

The following arguments (with the indicated default values) are available and can be adjusted as necessary.

  - `modelStateFile="ModelState.Rda"` :
  
      Path to ModelState.rda, or of a folder containing ModelState.rda,
      or to another file in a folder containing ModelState.rda  
      __*NOTE:*__ This argument is used for its *path*, not the filename.  So the default is the same as if you provided `""` or `"."` (the current directory). The file that will be sought will always literally be called `ModelState.Rda` (which is what the VisionEval framework always calls it). 
  
  - `outputFolder="output"`
  
      Path to folder in which to put the `.csv` files.  
      The path can be an absolute path, or a path relative to wherever `ModelState.Rda` was found.  The output path will be created recursively, so the intermediate path elements do not already need to exist.  
      __*NOTE*__: The path will be deleted *recursively* if overwrite is `TRUE`. Caution is recommended.
  
  - `includeTables=character(0)`

      Character vector of table names to include from the Datastore.  
      `character(0)` or `""` will include all tables (unless excluded, see `excludeTables` for details on tables that are automatically excluded)
  
  - `excludeTables=c("Vehicle")`
  
      Character vector of table names to exclude from the Datastore.  
      `character(0)` or `""` will exclude no tables.  
      Table `Model` will always be excluded, unless it is specified in `includeTables`  
      Table `Vehicle` from VERPAT includes an incompatible numbers of rows (that's a bug in VERPAT and its modules that needs to be addressed somehow)
  
  - `overwrite=TRUE`
  
      if `TRUE`, blow away the `outputFolder` if it already exists.   
      If `outputFolder` exists and `overwrite` is `FALSE` then an error will be reported and nothing else will happen.
  
  - `quiet=FALSE`
  
      If `FALSE`, dish up some progress messages and extra debugging data  
      If `TRUE`, only report errors
    
### Output Reference

`ve.export()` is run for its side-effects.  It will read the Datastore implied by the location of `ModelState.Rda` and create a set of `.csv` files (one per Table, after the include and exclude lists are processed) in the `outputFolder`.  The `.csv` files will have one column for each of the data vectors (`.Rda` files) associated with the corresponding Table. 

### Function Return Value

`ve.export()` returns the full path to the directory whose `ModelState.Rda` file was used to identify the Datastore.

## <a id="Code"></a>Structure of the `exporter.R` source code file

The `ve.export()` function is defined in `exporter.R`.  It simply executes a series of helper functions that perform the following operations:

  1. `getModelStateFile()` locates `ModelState.Rda`, possibly with an interactive dialog.
  2. `indexModelState()` uses the `Datastore` element of `ModelState_ls` to find the non-input Datastore elements (i.e. those that were generated by the model rather than provided in the `inputs` folder for the scenario).
  3. `outputData()` extracts the data in the Datastore using the index elements from `indexModelState()`. Following the logic of the former `export_to_csv.R` script, base year data from tables other than `Azone`,`Bzone` and `Marea` will be skipped. A desirable enhancement of `ve.export()` will be to specify which years should be written for which tables (might want to err in the direction of 'everything', since filtering after the fact is quite feasible).
  4. `exportData()` filters out the tables of interest and writes the results to the `.csv` files (one per logical Table, in the `outputFolder`).