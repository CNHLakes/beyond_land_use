
<!-- README.md is generated from README.Rmd. Please edit that file -->

Code and data
for:

## The effect of crop type, fertilizer, and manure applications on lake nutrients in agriculture watersheds at regional scales

> Journal Target: Ecosystems

### Products

Figures: [manuscript/figures.pdf](manuscript/figures.pdf)

Tables: [manuscript/tables.pdf](manuscript/tables.pdf)

## Potential predictor variables

<details>

<summary><b>Crop Types</b> (`CDL`) </summary>

<p>

  - \[x\] IWS pasture types (forage or not)
  - \[x\] IWS specific crops (corn, wheat, etc.)
  - \[ \] ~IWS crop frequency/rotation~ -AK says this is probably not
    needed

</p>

</details>

<details>

<summary>Soil types (`gSSURGO`) </summary>

<p>

  - \[x\] IWS wetland potential (as a proxy for tile drainage)
  - \[x\] IWS soil organic carbon
  - \[ \] IWS bulk density - needed to compute soil carbon mass
  - \[ \] IWS rock content - needed to compute soil carbon mass
  - \[ \] IWS root zone depth
  - \[ \] IWS lithology (sand %, clay %, etc.)

</p>

</details>

<details>

<summary><b>Fertilizer and manure application data</b> (`USGS`)
</summary>

<p>

  - \[x\] IWS from county - manure, fertilizer, and deposition of N,P

</p>

</details>

<details>

<summary>Land use cover (`LAGOSNE`) </summary>

<p>

  - \[ \] Lake buffer LULC
  - \[x\] IWS LULC

</p>

</details>

<details>

<summary><b>Lake Characteristics and WQ</b> (`LAGOSNE`) </summary>

<p>

  - \[x\] Lake depth
  - \[x\] IWS area
  - \[x\] IWS/Lake area
  - \[x\] TP, TN, NO3
  - \[ \] IWS precip variability
      - the area of a given crop in year n-1 matters. Usually, dry years
        leave nutrients on the ground, and when followed up by a wet
        year you get the largest signal in year n.

</p>

</details>

<details>

<summary>Animal data</b> (`USDA Census`) </summary>

<p>

  - \[ \] Animal units

</p>

</details>

## Research questions

## Timeline

## Issues

  - Availability of data products through time is highly variable. How
    to align or decide to treat properties as constant?
