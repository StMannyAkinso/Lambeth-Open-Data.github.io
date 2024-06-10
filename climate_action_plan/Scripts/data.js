// data.js

// Function to load and parse a CSV file using PapaParse
function loadCSV(filePath) {
  return new Promise((resolve, reject) => {
    Papa.parse(filePath, {
      download: true,
      header: true,
      complete: (results) => resolve(results.data),
      error: (error) => reject(error),
    });
  });
}

// Function to map sector data into a dictionary for easy lookup
function mapSectorsData(sectorsData) {
  const sectorsMap = {};
  sectorsData.forEach((sector) => {
    sectorsMap[sector["Sector-ID"]] = {
      name: sector["Sector-Name"],
      color: sector["Sector-Colour"],
      description: sector["Sector-Description"],
    };
  });
  return sectorsMap;
}

// Function to map charts data
function mapChartsData(chartsData) {
  const chartsMap = {};
  chartsData.forEach((chart) => {
    if (!chartsMap[chart["Sector-ID"]]) {
      chartsMap[chart["Sector-ID"]] = [];
    }
    chartsMap[chart["Sector-ID"]].push(chart);
  });
  return chartsMap;
}
