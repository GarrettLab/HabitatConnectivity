This submission addresses the feedback from last CRAN submission.

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

I wrapped all the functions that requires downloading of files with `\dontrun` so that it doesn't slow down the build process. I might be wrong here, but what I have understood is `\donttest` is not triggered in R cmd check. I have now replaced all the occurences with `\donttest` .

## Reviewer

In order of review,

1.  Victoria Wimmer \<[vwimmer\@wu.ac.at](mailto:vwimmer@wu.ac.at)\>
2.  Beni Altmann \<[benjamin.altmann\@wu.ac.at](mailto:benjamin.altmann@wu.ac.at)\>
3.  Victoria Wimmer \<[vwimmer\@wu.ac.at](mailto:vwimmer@wu.ac.at)\>