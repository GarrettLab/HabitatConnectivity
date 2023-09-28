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

```         
Found the following (possibly) invalid URLs:
  URL: https://cran.r-project.org/web/packages/geodata/index.html
    From: inst/doc/analysis.html
    Status: 200
    Message: OK
    CRAN URL not in canonical form
  URL: https://dataverse.harvard.edu/dataset.xhtml
    From: inst/doc/analysis.html
    Status: 404
    Message: Not Found
  URL: https://garrettlab.com.
    From: inst/doc/analysis.html
    Status: Error
    Message: SSL: certificate subject name (garrettlab.com) does not match target host name 'garrettlab.com.'
  URL: https://github.com/GarrettLab/CroplandConnectivity,
    From: inst/doc/analysis.html
    Status: 404
    Message: Not Found
  URL: https://nam10.safelinks.protection.outlook.com/?url=http%3A%2F%2Fgeohab-site.s3-website.us-east-2.amazonaws.com%2Fdocs%2F&data=05%7C01%7Ckkeshav%40ufl.edu%7C001659722a5b4f6bd69d08dbb2ff9d1d%7C0d4da0f84a314d76ace60a62331e1b84%7C0%7C0%7C638300583136095106%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000%7C%7C%7C&sdata=GK4pGbtNQWNF60ryAB9pVtbBt4tq5K3daNHV9Un%2FhtM%3D&reserved=0
    From: inst/doc/analysis.html
    Status: 500
    Message: Internal Server Error
  The canonical URL of the CRAN page for a package is 
    https://CRAN.R-project.org/package=pkgname
```

Following were not addressed -

```         
Package has a VignetteBuilder field but no prebuilt vignette index.
```

If I remove a `VignetteBuilder` from DESCRIPTION, I get a warning from `devtools::check()` - Package *has no Sweave vignette sources and no VignetteBuilder field.*
