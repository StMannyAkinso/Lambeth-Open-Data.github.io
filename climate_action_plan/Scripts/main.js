// main.js

// Main function to load data and initialize the dashboard
async function initializeDashboard() {
  const chartsDataPath = "Data/Content/Charts-Database.csv";
  const sectorsDataPath = "Data/Content/Sector-Database.csv";
  try {
    const chartsData = await loadCSV(chartsDataPath);
    const sectorsData = await loadCSV(sectorsDataPath);

    const sectorsMap = mapSectorsData(sectorsData);
    const chartsMap = mapChartsData(chartsData);

    populateSectorMenu(sectorsMap, chartsMap);

    const mainSection = document.getElementById("mainSection");
    const homePageContent = document.getElementById("homePageContent");
    const homeButton = document.getElementById("homeButton");
    homeButton.addEventListener("click", () => {
      mainSection.innerHTML = homePageContent.innerHTML;
    });
  } catch (error) {
    console.error("Error loading CSV files:", error);
  }
}

// Initialize the dashboard when the script loads
initializeDashboard();
