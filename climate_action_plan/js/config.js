// js/config.js

// Configuration for different datasets
export const datasets = {
  treeCoverage: {
    geojsonPath: "js/Map/lambeth-ward-map.geojson",
    csvPath: "Data/CSV Files/% tree cover by ward.csv",
    geojsonProperty: "WARD_NAME",
    csvProperty: "Ward",
    csvValueProperty: "Canopy cover %",
  },
  // Add other datasets here as needed
};

export const colorScaleConfig = {
  ranges: [
    { min: 0, max: 12, color: "#e5f5e0" }, // Very light green
    { min: 12, max: 15, color: "#c7e9c0" },
    { min: 15, max: 17, color: "#a1d99b" },
    { min: 17, max: 18, color: "#74c476" },
    { min: 18, max: 20, color: "#41ab5d" },
    { min: 20, max: 22, color: "#238b45" },
    { min: 22, max: 100, color: "#005a32" }, // Dark green
  ],
};
