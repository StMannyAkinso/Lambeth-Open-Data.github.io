// js/footer.js
export function renderFooter() {
  const footerContainer = document.querySelector("footer");
  footerContainer.innerHTML = `
    <p>&copy; Lambeth Council 2024. All rights reserved.</p>
    <p>Designed by <a href="https://www.land-smyrna.com">Smyrna Aesthetics</a></p>
  `;
}
