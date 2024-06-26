## Re-submission

```         
Package was archived on CRAN
   
   CRAN repository db overrides:
     X-CRAN-Comment: Archived on 2024-05-27 as requires archived package
       'geodata'.
```

This is a re submission with minor changes as mentioned in news.md. The package was earlier archived on 05/28/24 due to archival of one of its dependency. Since, `geodata` is back on cran, resubmitting the package for the consideration.

The review comments are dated as feedback received and the submission addressing them.

## First submission

R cmd check is generating the following note -

```         
Maintainer: ‘Krishna Keshav <kkeshav@ufl.edu>’
  
  New submission
  
  Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1029/2007gb002947
      From: inst/doc/analysis.html
      Status: 403
      Message: Forbidden
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.1029/2007gb002947
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
```

We found a relevant discussion in a CRAN's mailing list that it is due to issue in underlying server and thus proceeding with submission.

Eliminated usage of `<<-`. However, we still use this assignment operator with `memoise` package in `.onLoad()` of `ccri_helper.R` which is the recommended way by. Based on this [thread](https://github.com/r-lib/memoise/issues/76) , `memoise` doesn't modify R environment.

We have used `dontrun` to prevent some time consuming functions to run during build process. These functions might run slow since they are required to download files (up to 37mb each).

## Feedback - 10/19, Submit - 10/19

```         
Please add () behind all function names in the description texts
(DESCRIPTION file). e.g: --> parameters.yaml()
```

*parameters.yaml* is not a function but config file and outcome of `get_parameters()` function.

```         
Please unwrap the examples if they are executable in < 5 sec, or replace
\dontrun{} with \donttest{} or explain why \dontrun{} is indeed
necessary for these examples.
```

I wrapped all the functions that requires downloading of files with `\dontrun` so that it doesn't slow down the build process. I might be wrong here, but what I have understood is `\donttest` is not triggered in R cmd check. I have now replaced all the occurrences with `\donttest` .

## Feedback - 10/22, Submit - 10/30

Our package was conditionally accepted. The build failed for some environments. See the report [here](https://nam10.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcran.r-project.org%2Fweb%2Fchecks%2Fcheck_results_geohabnet.html&data=05%7C01%7Ckkeshav%40ufl.edu%7C0e9750fb07c84cc2d05208dbd2e51116%7C0d4da0f84a314d76ace60a62331e1b84%7C0%7C0%7C638335653506437699%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000%7C%7C%7C&sdata=EbkniC3U%2FFsPiw%2Bpjg4VxBnbDlEonYh5FioMmTdC3bg%3D&reserved=0).

```         
Packages required and available but unsuitable versions:  'stats', 'graphics'
```

I have removed the min_version requirement for above packages.

```         
    cannot create file '/home/hornik/tmp/R.check/r-devel-gcc/Work/build/Packages/geohabnet/parameters.yaml', reason 'Read-only file system'
```

Based on suggesstion from CRAN mailing list, I have replaced write destination for my configuration files to tools::R_user_dir(). The usage is following -

1.  Replace the old `parameters.yaml` in tools::R_user_dir() with new copy using `.onLoad()` callback
2.  Write YAML config into `parameters.yaml` whenever user provides with new copy using `set_parameters()`
3.  Get user config from `tools::R_user_dir()` whenever needed in the program.
4.  Unit test added for this capability which removes the file from `tools::R_user_dir()` after test check.

Addtionally, added `dontrun` tag in `\name{get_rasters}` since the example shows usage of dummy file name.

## Reviewers

In order of review,

1.  Victoria Wimmer \<[vwimmer\@wu.ac.at](mailto:vwimmer@wu.ac.at)\>
2.  Beni Altmann \<[benjamin.altmann\@wu.ac.at](mailto:benjamin.altmann@wu.ac.at)\>
3.  Victoria Wimmer \<[vwimmer\@wu.ac.at](mailto:vwimmer@wu.ac.at)\>
4.  Victoria Wimmer \<[vwimmer\@wu.ac.at](mailto:vwimmer@wu.ac.at)\>, Kurt Hornik \<[Kurt.Hornik\@wu.ac.at](mailto:Kurt.Hornik@wu.ac.at)\>
