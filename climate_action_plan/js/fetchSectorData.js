// js/fetchSectorData.js
import { fetchSectors } from "./fetchData.js";

export async function getSectorData(sectorId) {
  console.log(`Fetching sector data for Sector ID: ${sectorId}`);

  try {
    const sectors = await fetchSectors();
    console.log("Sectors data fetched:", sectors);

    // Use the correct column name `Sector-ID`
    const sector = sectors.find((sec) => sec["Sector-ID"] === sectorId);
    console.log(`Sector found:`, sector);

    return sector;
  } catch (error) {
    console.error("Error fetching sector data:", error);
    throw error; // Re-throw the error to be handled by the caller
  }
}
