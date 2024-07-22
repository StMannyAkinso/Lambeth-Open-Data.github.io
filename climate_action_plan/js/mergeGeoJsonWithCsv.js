// Function to merge CSV data with GeoJSON
export function mergeGeoJsonWithCsv(
  geojson,
  csvData,
  geojsonProperty,
  csvProperty,
  csvValueProperty
) {
  try {
    geojson.features.forEach((feature) => {
      const geoJsonValue = feature.properties[geojsonProperty];
      const csvEntry = csvData.find((d) => d[csvProperty] === geoJsonValue);
      feature.properties[csvValueProperty] = csvEntry
        ? parseFloat(csvEntry[csvValueProperty])
        : 0;
    });
    console.log("Merged GeoJSON with CSV data:", geojson);
    return geojson;
  } catch (error) {
    console.error("Error merging GeoJSON with CSV data:", error);
    throw error;
  }
}
