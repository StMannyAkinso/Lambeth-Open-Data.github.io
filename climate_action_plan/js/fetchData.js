// js/fetchData.js
export async function fetchSectors() {
  console.log("Fetching sectors data from Data/Content/Sector-Database.csv");

  try {
    const response = await fetch("Data/Content/Sector-Database.csv");
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    const csvText = await response.text();
    console.log("Sectors CSV data fetched:", csvText);

    const result = Papa.parse(csvText, { header: true });
    console.log("Sectors data parsed:", result.data);

    return result.data;
  } catch (error) {
    console.error("Error fetching sectors data:", error);
    throw error; // Re-throw the error after logging
  }
}

export async function fetchCharts() {
  console.log("Fetching charts data from Data/Content/Charts-Database.csv");

  try {
    const response = await fetch("Data/Content/Charts-Database.csv");
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    const csvText = await response.text();
    console.log("Charts CSV data fetched:", csvText);

    const result = Papa.parse(csvText, { header: true });
    console.log("Charts data parsed:", result.data);

    return result.data;
  } catch (error) {
    console.error("Error fetching charts data:", error);
    throw error; // Re-throw the error after logging
  }
}
