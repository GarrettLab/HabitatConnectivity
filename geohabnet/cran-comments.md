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

One of the feedback was to avoid installation of packages in vignette. We have addressed it by setting `eval = FALSE` for the code chunks which does installation.
