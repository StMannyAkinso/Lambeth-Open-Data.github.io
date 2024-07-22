// Function to URL-encode a file path
function encodeFilePath(filePath) {
  return encodeURI(filePath);
}

// Function to fetch and parse CSV data
export async function fetchCsvData(csvFilePath) {
  try {
    const encodedCsvFilePath = encodeFilePath(csvFilePath); // Ensure URL is encoded
    const response = await fetch(encodedCsvFilePath);
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    const csvText = await response.text();
    console.log(`Fetched CSV data from ${encodedCsvFilePath}`);

    return new Promise((resolve, reject) => {
      Papa.parse(csvText, {
        header: true,
        complete: function (results) {
          console.log("Parsed CSV data:", results.data);
          resolve(results.data);
        },
        error: function (error) {
          console.error("Error parsing CSV data:", error);
          reject(error);
        },
      });
    });
  } catch (error) {
    console.error("Error fetching CSV data:", error);
    throw error;
  }
}

// Function to fetch GeoJSON data
export async function fetchGeoJsonData(geojsonFilePath) {
  try {
    const encodedGeojsonFilePath = encodeFilePath(geojsonFilePath); // Ensure URL is encoded
    const response = await fetch(encodedGeojsonFilePath);
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    const geojson = await response.json();
    console.log(`Fetched GeoJSON data from ${encodedGeojsonFilePath}`);
    return geojson;
  } catch (error) {
    console.error("Error fetching GeoJSON data:", error);
    throw error;
  }
}
