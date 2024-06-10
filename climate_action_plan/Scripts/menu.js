// menu.js

// Function to populate the sector menu
function populateSectorMenu(sectorsMap, chartsMap) {
  const sectorMenu = document.getElementById("sectorMenu");
  sectorMenu.innerHTML = ""; // Clear existing menu items if any

  Object.keys(sectorsMap).forEach((sectorId) => {
    const li = document.createElement("li");
    const button = createSectorButton(sectorId, sectorsMap, chartsMap);
    li.appendChild(button);
    sectorMenu.appendChild(li);
  });
}

// Function to create a sector button
function createSectorButton(sectorId, sectorsMap, chartsMap) {
  const button = document.createElement("button");
  button.textContent = sectorsMap[sectorId].name;
  button.dataset.sectorColor = sectorsMap[sectorId].color;

  button.addEventListener("mouseenter", (event) => {
    event.target.style.backgroundColor = sectorsMap[sectorId].color;
  });

  button.addEventListener("mouseleave", (event) => {
    if (!event.target.classList.contains("active")) {
      event.target.style.backgroundColor = "";
    }
  });

  button.addEventListener("click", () => {
    const buttons = document.querySelectorAll("#sectorMenu button");
    buttons.forEach((btn) => {
      btn.classList.remove("active");
      btn.style.backgroundColor = "";
    });
    button.classList.add("active");
    button.style.backgroundColor = sectorsMap[sectorId].color;
    displayChartsForSector(sectorId, sectorsMap, chartsMap);
  });

  return button;
}
