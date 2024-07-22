// js/menu.js
import { fetchSectors } from "./fetchData.js";

export async function renderMenu() {
  console.log("Rendering menu...");

  const menuContainer = document.querySelector(".sideMenu");

  try {
    console.log("Fetching sectors data...");
    const sectors = await fetchSectors();
    console.log("Sectors data fetched:", sectors);

    if (!sectors || sectors.length === 0) {
      console.warn("No sectors data found.");
      menuContainer.innerHTML = `<ul><li><a href="index.html">Home</a></li></ul>`;
      return;
    }

    // Generate menu items with sector colors
    const menuList = sectors

      .map(
        (sector) =>
          `
          <a href="sector.html?id=${sector["Sector-ID"]}">
            <li class="flex align-center" style="--sector-color: ${sector["Sector-Colour"]};">
            <!--  <img
                  id="sectorImageIcon"
                  src="Images/Lambeth-CAP-Climate Goals Icons/PNGS/${sector["Sector-Name"]}-white.png"
                  alt="Sector Image"
                  style="width: 2em; height: 2em;"
              /> -->
                  <p>${sector["Sector-Name"]}</p>
            </li>
          </a>`
      )
      .join("");

    console.log("Menu list generated:", menuList);

    menuContainer.innerHTML = `
    <ul>
      <a href="kpis.html">
        <li class="flex align-center"><!--<i class="fa fa-home"></i>--><p>Home</p></li>
      </a>
      ${menuList}
      <a href="coverage.html">
        <li class="flex align-center"><!--<i class="fa fa-map-pin"></i>--><p>Coverage Map</p></li>
      </a>
    </ul>`;
    console.log("Menu rendered");
  } catch (error) {
    console.error("Error rendering menu:", error);
  }
}
