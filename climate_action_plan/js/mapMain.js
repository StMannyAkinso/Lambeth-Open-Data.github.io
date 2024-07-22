import { fetchCsvData, fetchGeoJsonData } from "./fetchMapData.js";
import { mergeGeoJsonWithCsv } from "./mergeGeoJsonWithCsv.js";
import { renderChoroplethMap } from "./renderMap.js";
import { datasets } from "./config.js";
import { displayCsvData } from "./displayCsvData.js"; // Import the displayCsvData function

// Function to initialize the map
async function initMap(datasetKey, containerId) {
  const dataset = datasets[datasetKey];
  if (!dataset) {
    console.error("Dataset configuration not found.");
    return;
  }

  try {
    // Fetch and merge the data
    const [geojson, csvData] = await Promise.all([
      fetchGeoJsonData(dataset.geojsonPath),
      fetchCsvData(dataset.csvPath),
    ]);

    const mergedGeoJson = mergeGeoJsonWithCsv(
      geojson,
      csvData,
      dataset.geojsonProperty,
      dataset.csvProperty,
      dataset.csvValueProperty
    );

    // Render the choropleth map
    renderChoroplethMap(containerId, mergedGeoJson, dataset.csvValueProperty);

    // Display CSV data in the #map-data element
    await displayCsvData(); // Ensure displayCsvData is called
  } catch (error) {
    console.error("Error initializing map:", error);
  }
}

// Initialize tree coverage map when DOM is ready
document.addEventListener("DOMContentLoaded", () => {
  initMap("treeCoverage", "map");
});
