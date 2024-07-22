import { fetchCsvData } from "./fetchMapData.js";
import { datasets, colorScaleConfig } from "./config.js";

// Helper function to get color from scale
function getColorFromScale(value) {
  const { ranges } = colorScaleConfig;

  for (const range of ranges) {
    if (value >= range.min && value <= range.max) {
      return range.color;
    }
  }
  // Return a default color if value doesn't fall into any range
  return "#ffffff"; // Default to white if no range matches
}

// Function to display CSV data in the #map-data element
export async function displayCsvData() {
  const mapDataContainer = document.getElementById("map-data");

  try {
    const csvData = await fetchCsvData(datasets.treeCoverage.csvPath);

    // Log the CSV data for debugging
    console.log("CSV Data:", csvData);

    if (!csvData || csvData.length === 0) {
      mapDataContainer.innerHTML = "<p>No data available</p>";
      return;
    }

    const listItems = csvData
      .map((item) => {
        const value = parseFloat(item[datasets.treeCoverage.csvValueProperty]);
        const color = getColorFromScale(value);

        return `<li style="background-color: ${color};">${
          item[datasets.treeCoverage.csvProperty]
        }: ${value}%</li>`;
      })
      .join("");

    // Log the generated HTML
    console.log("Generated HTML:", `<ul>${listItems}</ul>`);

    mapDataContainer.innerHTML = `<ul>${listItems}</ul>`;
  } catch (error) {
    console.error("Error displaying CSV data:", error);
  }
}
