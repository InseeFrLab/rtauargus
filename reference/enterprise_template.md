# Extract of SBS Eurostat template.

A dataset describing all the cells expected by Eurostat.

## Utilisation

``` r
enterprise_template
```

## Format

A data frame with 3 168 rows and 5 variables:

- TIME_PERIOD:

  Integer. The year of observation (e.g., 2022).

- INDICATOR:

  Character. The type of indicator (e.g., "SAL").

- ACTIVITY:

  Character. The sector of activity coded using a standard
  classification (e.g., "B").

- NUMBER_EMPL:

  Character. Employment size category (e.g., "E0", "E1T4", "E5T9",
  "EGE10").

- LEGAL_FORM:

  Character. Legal form of the enterprise, where "\_T" represents all
  legal forms combined.
