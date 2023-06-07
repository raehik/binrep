# generic-data-functions to-dos
  * release on Hackage, it's super useful
  * provide a different `traverse` that inspects field names, to eventually
    replicate Gabriella's optparse-generic
  * provide a mixed sum & non-sum generic derivation like my original (& Aeson)
  * provide more versions: binary, cereal, store-style

## Code quality
  * highly granular constraints nice for writing, perhaps worse for type errors?
  * use `Tagged` instead of newtype spam for via?
