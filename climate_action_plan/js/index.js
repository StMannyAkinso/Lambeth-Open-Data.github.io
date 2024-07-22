// js/index.js
import { renderHeader } from "./header.js";
import { renderMenu } from "./menu.js";
import { renderFooter } from "./footer.js";

document.addEventListener("DOMContentLoaded", () => {
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
  } catch (error) {
    console.error("Error occurred in index.js:", error);
  }
});
