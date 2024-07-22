// js/header.js
export function renderHeader() {
  const headerContainer = document.querySelector("header");
  headerContainer.innerHTML = `
    <a href="https://uat.lambethclimatepartnership.org/" class="flex centred">
      <img src="images/lambeth-climate-partnership.png" class="lambeth-logo" alt="Lambeth Climate Logo" />
    </a>
    <div class="search-container">
      <input type="text" class="search-bar" placeholder="Search..." />
      <i class="fa fa-search search-icon"></i>
    </div>
  `;
}
