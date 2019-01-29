
## Milestone 4 Write-up

### Added Feature: Comparing city and state crime stats

We decided to implement one extra feature: as soon as the user picks a city, we overlay the average crime numbers from the state the city belongs to:  

(ADD PICTURE HERE WHEN THE APP IS FINISHED)

This feature is very important for our use case: it gives the user a benchmark to assess the relative performance of its city compared to the ones around it.

To implement this feature, we had to tinker with the plots a little bit:
- Removed the markers on the line plot to make it less cluttered
- Changed the data wrangling to allow for both city and state summary figures
- Wrote one single block of ggplot code that was conditional on the user's input.

