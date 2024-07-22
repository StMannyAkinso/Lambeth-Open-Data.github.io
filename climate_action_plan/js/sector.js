// js/sector.js
import { renderHeader } from "./header.js";
import { renderMenu } from "./menu.js";
import { renderFooter } from "./footer.js";
import { getSectorData } from "./fetchSectorData.js";
import { getSectorCharts } from "./fetchChartData.js";
import { displaySectorInfo } from "./displaySectorInfo.js";
import { displayCharts } from "./renderCharts.js";

document.addEventListener("DOMContentLoaded", async () => {
  console.log("DOMContentLoaded event fired");

  try {
    console.log("Rendering header...");
    renderHeader();
    console.log("Header rendered");

    console.log("Rendering menu...");
    renderMenu();
    console.log("Menu rendered");

    console.log("Rendering footer...");
    renderFooter();
    console.log("Footer rendered");

    const urlParams = new URLSearchParams(window.location.search);
    const sectorId = urlParams.get("id");
    console.log(`Sector ID from URL parameters: ${sectorId}`);

    console.log("Fetching sector data...");
    const sector = await getSectorData(sectorId);
    console.log("Sector data fetched:", sector);

    if (sector) {
      console.log("Displaying sector information...");
      displaySectorInfo(sector);
      console.log("Sector information displayed");
    } else {
      console.warn("No sector data found");
    }

    console.log("Fetching sector charts data...");
    const sectorCharts = await getSectorCharts(sectorId);
    console.log("Sector charts data fetched:", sectorCharts);

    if (sectorCharts.length > 0) {
      console.log("Displaying sector charts...");
      displayCharts(sectorCharts);
      console.log("Sector charts displayed");
    } else {
      console.warn("No charts found for this sector");
    }
  } catch (error) {
    console.error("Error occurred in sector.js:", error);
  }
});
