// js/displaySectorInfo.js
export function displaySectorInfo(sector) {
  console.log("Displaying sector information:", sector);

  try {
    const sectorImage = document.getElementById("sectorImage");
    const sectorName = document.getElementById("sectorName");
    const sectorDescription = document.getElementById("sectorDescription");
    const sectorGoals = document.getElementById("sectorGoals");

    // Convert sector name to lowercase
    const sectorNameLowerCase = sector["Sector-Name"];

    // Construct image URLs
    const colourImageUrl = `Images/Lambeth-CAP-Climate Goals Icons/PNGS/${sectorNameLowerCase}-colour.png`;
    const whiteImageUrl = `Images/Lambeth-CAP-Climate Goals Icons/PNGS/${sectorNameLowerCase}-white.png`;
    // Set sector image with the colour variant as default
    sectorImage.src = colourImageUrl;
    console.log(`Sector Image URL set to: ${colourImageUrl}`);

    // Setting sector name
    sectorName.textContent = sector["Sector-Name"];
    console.log(`Sector Name set to: ${sector["Sector-Name"]}`);

    // Setting sector description
    sectorDescription.textContent = sector["Sector-Description"];
    console.log(`Sector Description set to: ${sector["Sector-Description"]}`);

    // Extract color code
    const sectorColour = sector["Sector-Colour"] || "#000"; // Fallback to black if not provided

    // Set the CSS variable for the color
    document.documentElement.style.setProperty("--sectorColour", sectorColour);

    // Displaying goals
    sectorGoals.innerHTML = `
     <button class="flex space-between align-center" id="goalsButton" onclick="toggleVisibility('goals')">
     <h2>Goals</h2><i class="fa fa-plus"></i>
     </button>
<div id="goals" class="toggle-content hidden">
  <div class="goal">
    <h4>${sector["Goal-1-Name"]}</h4>
    <li>${sector["Goal-1-Description"]}</li>
  </div>
  <div class="goal">
    <h4>${sector["Goal-2-Name"]}</h4>
    <li>${sector["Goal-2-Description"]}</li>
  </div>
  <div class="goal">
    <h4>${sector["Goal-3-Name"]}</h4>
    <li>${sector["Goal-3-Description"]}</li>
  </div>
  <div class="goal">
    <h4>${sector["Goal-4-Name"]}</h4>
    <li>${sector["Goal-4-Description"]}</li>
  </div>
</div>


    `;
    console.log("Sector Goals updated");
  } catch (error) {
    console.error("Error displaying sector information:", error);
  }
}
