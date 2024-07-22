// js/renderCharts.js
import { renderChart } from "./chartService.js";

export function displayCharts(sectorCharts) {
  console.log("Displaying charts:", sectorCharts);

  const chartsContainer = document.getElementById("chartsContainer");

  // Clear previous charts
  chartsContainer.innerHTML = "";

  // Generate HTML for each chart
  const chartsHTML = sectorCharts
    .map(
      (chart) => `
    <div class="chart ${chart["Target"]}">
      ${
        chart["Target"] === "y"
          ? "<div class='flex centred target-container'><h5>Target</h5><img id='target-img' src='Images/target.png' alt='target icon'></div>"
          : ""
      }
      <h3>${chart["Chart-Name"]}</h3>
      <div id="chart-${chart["Chart-ID"]}"></div>
      <div class="chart-meta">
        <p>${chart["Chart-Description"]}</p>
        <p><strong>Data Source:</strong> ${chart["Data-Source"]}</p>
        <p><strong>Source Website:</strong> <a href="${
          chart["Source-Website"]
        }" target="_blank">${chart["Source-Website"]}</a></p>
      </div>
    </div>
  `
    )
    .join("");

  // Insert HTML into the container
  chartsContainer.innerHTML = chartsHTML;
  console.log("Charts HTML rendered");

  // Load data for each chart and render it
  sectorCharts.forEach((chart) => {
    const csvFileName = chart["CSV-File"];
    const encodedFileName = encodeURIComponent(csvFileName);
    const csvFilePath = `Data/CSV Files/${encodedFileName}`;

    console.log(
      `Fetching data for chart ${chart["Chart-ID"]} from ${csvFilePath}`
    );

    fetch(csvFilePath)
      .then((response) => {
        if (!response.ok) {
          throw new Error(`Network response was not ok for ${csvFilePath}`);
        }
        return response.text();
      })
      .then((csvText) => {
        console.log(`CSV data for chart ${chart["Chart-ID"]}:`, csvText);
        const chartData = Papa.parse(csvText, { header: true }).data;
        console.log(`Parsed chart data for ${chart["Chart-ID"]}:`, chartData);
        renderChart(chartData, `chart-${chart["Chart-ID"]}`, chart);
      })
      .catch((error) => {
        console.error(
          `Error fetching or parsing data for chart ${chart["Chart-ID"]}:`,
          error
        );
      });
  });
}
