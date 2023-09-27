This submission addresses the following issues from last CRAN submission -

```         
Possibly misspelled words in DESCRIPTION:
  mapspam (20:64)
  monfreda (20:51)
  sean (19:41, 24:17)
  yaml (25:22)
```

```         
Found the following (possibly) invalid URLs:
  URL: https://garrettlab.com (moved to https://www.garrettlab.com/)
    From: DESCRIPTION
          man/geohabnet-package.Rd
    Status: 301
    Message: Moved Permanently
```

```         
Found the following (possibly) invalid file URI:
  URI: GarrettLab/CroplandConnectivity
    From: README.md
```

```         
Output(s) listed in 'build/vignette.rds' but not in package:
  'inst/doc/analysis.html'
```

Following were not addressed -

```         
Package has a VignetteBuilder field but no prebuilt vignette index.
```

If I remove a `VignetteBuilder` from DESCRIPTION, I get a warning from `devtools::check()` - Package *has no Sweave vignette sources and no VignetteBuilder field.*
