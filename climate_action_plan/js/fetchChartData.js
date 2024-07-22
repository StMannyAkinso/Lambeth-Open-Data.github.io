// js/fetchChartData.js
import { fetchCharts } from "./fetchData.js";

export async function getSectorCharts(sectorId) {
  console.log(`Fetching charts data for Sector ID: ${sectorId}`);

  try {
    // Fetch all charts data
    const charts = await fetchCharts();
    console.log("Charts data fetched:", charts);

    // Use the correct column name `Sector-ID`
    const sectorCharts = charts.filter(
      (chart) => chart["Sector-ID"] === sectorId
    );
    console.log(`Charts for Sector ID ${sectorId}:`, sectorCharts);

    return sectorCharts;
  } catch (error) {
    console.error("Error fetching charts data:", error);
    throw error; // Re-throw the error to be handled by the caller
  }
}
