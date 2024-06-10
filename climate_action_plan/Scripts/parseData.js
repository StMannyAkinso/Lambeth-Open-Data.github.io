// Function to load and parse a CSV file using PapaParse
function loadCSV(filePath) {
  return new Promise((resolve, reject) => {
    Papa.parse(filePath, {
      download: true,
      header: true,
      complete: (results) => resolve(results.data),
      error: (error) => reject(error),
    });
  });
}

// Function to map sector data into a dictionary for easy lookup
function mapSectorsData(sectorsData) {
  const sectorsMap = {};
  sectorsData.forEach((sector) => {
    sectorsMap[sector["Sector-ID"]] = {
      name: sector["Sector-Name"],
      color: sector["Sector-Colour"],
    };
  });
  return sectorsMap;
}

// Function to map charts data
function mapChartsData(chartsData) {
  const chartsMap = {};
  chartsData.forEach((chart) => {
    if (!chartsMap[chart["Sector-ID"]]) {
      chartsMap[chart["Sector-ID"]] = [];
    }
    chartsMap[chart["Sector-ID"]].push(chart);
  });
  return chartsMap;
}

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

// Function to display charts for a specific sector
async function displayChartsForSector(sectorId, sectorsMap, chartsMap) {
  const mainSection = document.getElementById("mainSection");
  const chartsContainer = document.getElementById("chartsContainer");
  const sectorName = sectorsMap[sectorId].name;
  const sectorColor = sectorsMap[sectorId].color;

  mainSection.innerHTML = `<h1>${sectorName}</h1>`;
  chartsContainer.innerHTML = "";

  if (chartsMap[sectorId]) {
    for (const chart of chartsMap[sectorId]) {
      const csvFilePath = `Data/CSV Files/${encodeURIComponent(
        chart["CSV-File"]
      )}`;
      const xAxis = chart["X-Axis"];
      const yAxis = chart["Y-Axis"];
      const unit = chart["Unit"];
      const chartData = await loadCSV(csvFilePath);
      createChart(
        chart,
        chartData,
        xAxis,
        yAxis,
        unit,
        chartsContainer,
        sectorColor
      );
    }
  } else {
    chartsContainer.innerHTML = "<p>No charts available for this sector.</p>";
  }

  mainSection.appendChild(chartsContainer);
}

// Function to create and display a chart
function createChart(
  chart,
  chartData,
  xAxis,
  yAxis,
  unit,
  container,
  sectorColor
) {
  const margin = { top: 75, right: 30, bottom: 60, left: 70 };
  const width = 400 - margin.left - margin.right;
  const height = 300 - margin.top - margin.bottom;

  const chartContainer = document.createElement("div");
  chartContainer.classList.add("chart-container");

  const infoButton = createInfoButton(
    chartContainer,
    chart,
    chartData,
    xAxis,
    yAxis
  );
  chartContainer.appendChild(infoButton);

  const svg = d3
    .select(chartContainer)
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`);

  const x = d3
    .scaleBand()
    .domain(chartData.map((d) => d[xAxis]))
    .range([0, width])
    .padding(0.1);

  const y = d3
    .scaleLinear()
    .domain([0, d3.max(chartData, (d) => +d[yAxis])])
    .nice()
    .range([height, 0]);

  appendXAxis(svg, x, height, chartData, xAxis);
  appendYAxis(svg, y, unit);
  appendChartTitle(svg, chart["Chart-Name"], width);
  appendLine(svg, chartData, x, y, xAxis, yAxis, sectorColor);
  appendCircles(svg, chartData, x, y, xAxis, yAxis, unit, sectorColor);

  container.appendChild(chartContainer);
}

// Function to create an info button
function createInfoButton(chartContainer, chart, chartData, xAxis, yAxis) {
  const infoButton = document.createElement("button");
  infoButton.innerHTML = `<i class="fa fa-info"></i>`;
  infoButton.classList.add("info-button");
  infoButton.addEventListener("click", () =>
    toggleChartInfo(chartContainer, chart, chartData, xAxis, yAxis)
  );
  return infoButton;
}

// Function to append x-axis to the chart
function appendXAxis(svg, x, height, chartData, xAxis) {
  const xTickValues = chartData
    .map((d) => d[xAxis])
    .filter((_, i, arr) => i % Math.ceil(arr.length / 5) === 0);

  svg
    .append("g")
    .attr("transform", `translate(0,${height})`)
    .call(d3.axisBottom(x).tickValues(xTickValues))
    .selectAll("text")
    .attr("transform", "rotate(-45)")
    .style("text-anchor", "end");
}

// Function to append y-axis to the chart
function appendYAxis(svg, y, unit) {
  const yAxisElement = svg.append("g").call(d3.axisLeft(y).ticks(5));
  yAxisElement
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("y", -70 + 10)
    .attr("x", -150)
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("fill", "black")
    .style("font-size", "14px")
    .style("font-family", "sans-serif")
    .style("font-weight", "bold")
    .text(unit);
}

// Function to append chart title
function appendChartTitle(svg, chartName, width) {
  const wrappedTitle = wrapText(chartName, 40);
  svg
    .append("text")
    .attr("class", "chart-title")
    .attr("x", width / 2)
    .attr("y", -50)
    .attr("text-anchor", "middle")
    .style("font-size", "14px")
    .style("font-family", "sans-serif")
    .style("font-weight", "bold")
    .selectAll("tspan")
    .data(wrappedTitle.split("\n"))
    .enter()
    .append("tspan")
    .attr("x", width / 2.2)
    .attr("dy", "1.2em")
    .text((d) => d);
}

// Function to append line to the chart
function appendLine(svg, chartData, x, y, xAxis, yAxis, sectorColor) {
  const line = d3
    .line()
    .x((d) => x(d[xAxis]) + x.bandwidth() / 2)
    .y((d) => y(+d[yAxis]));

  svg
    .append("path")
    .datum(chartData)
    .attr("fill", "none")
    .attr("stroke", sectorColor)
    .attr("stroke-width", 1.5)
    .attr("d", line);
}

// Function to append circles to the chart
function appendCircles(svg, chartData, x, y, xAxis, yAxis, unit, sectorColor) {
  svg
    .selectAll("circle")
    .data(chartData)
    .enter()
    .append("circle")
    .attr("cx", (d) => x(d[xAxis]) + x.bandwidth() / 2)
    .attr("cy", (d) => y(+d[yAxis]))
    .attr("r", 4)
    .attr("fill", sectorColor)
    .attr("stroke", "black")
    .attr("stroke-width", 1)
    .style("cursor", "pointer")
    .on("mouseenter", function (event, d) {
      d3.select(this).attr("stroke-width", 2);
      const tooltipGroup = appendTooltip(svg, d, x, y, xAxis, yAxis, unit);
      d3.select(this).node().__tooltip__ = tooltipGroup;
    })
    .on("mouseleave", function () {
      d3.select(this).attr("stroke-width", 1);
      d3.select(this).node().__tooltip__.remove();
    });
}

// Function to append tooltip to the chart
function appendTooltip(svg, d, x, y, xAxis, yAxis, unit) {
  const tooltipGroup = svg
    .append("g")
    .attr("class", "data-tooltip-group")
    .attr(
      "transform",
      `translate(${x(d[xAxis]) + x.bandwidth() / 2}, ${y(+d[yAxis]) + 30})`
    )
    .style("pointer-events", "none");

  const tooltipRect = tooltipGroup
    .append("rect")
    .attr("class", "tooltip-rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("rx", 4)
    .attr("ry", 4)
    .style("fill", "white")
    .style("stroke", "black")
    .style("stroke-width", "1px");

  const tooltipText = tooltipGroup
    .append("text")
    .attr("class", "data-tooltip")
    .style("text-anchor", "start")
    .style("font-size", "12px")
    .style("fill", "black");

  tooltipText
    .append("tspan")
    .text(`Value: ${(Math.round(d[yAxis] * 10) / 10).toString()} ${unit}`)
    .attr("x", 5)
    .attr("dy", "1.2em");

  tooltipText
    .append("tspan")
    .text(`Year: ${d[xAxis]}`)
    .attr("x", 5)
    .attr("dy", "1.2em");

  const bbox = tooltipText.node().getBBox();
  tooltipRect
    .attr("width", bbox.width + 15)
    .attr("height", bbox.height + 10)
    .attr("x", -5)
    .attr("y", -bbox.height + 25);

  return tooltipGroup;
}

// Function to toggle chart and info display
function toggleChartInfo(container, chart, chartData, xAxis, yAxis) {
  const chartInfo = container.querySelector(".chart-info");
  const chartContent = container.querySelector("svg");
  const infoButton = container.querySelector(".info-button");

  if (chartInfo) {
    chartInfo.remove();
    chartContent.style.display = "block";
    infoButton.textContent = "Info";
  } else {
    const infoDiv = document.createElement("div");
    infoDiv.classList.add("chart-info");
    infoDiv.innerHTML = `
      <h3>${chart["Chart-Name"]}</h3>
      <p>${chart["Chart-Description"]}</p>
      <p><strong>Data Source:</strong> ${chart["Data-Source"]}</p>
      <p><strong>Source Website:</strong> <a href="${chart["Source-Website"]}" target="_blank">${chart["Source-Website"]}</a></p>
    `;
    chartContent.style.display = "none";
    infoButton.textContent = "X";
    container.appendChild(infoDiv);
  }
}

// Wrap text function
function wrapText(text, maxLength) {
  if (text.length > maxLength) {
    const words = text.split(" ");
    let line = "";
    let wrappedText = "";
    words.forEach((word) => {
      if ((line + word).length > maxLength) {
        wrappedText += `${line.trim()}\n`;
        line = "";
      }
      line += `${word} `;
    });
    wrappedText += line.trim();
    return wrappedText;
  }
  return text;
}

// Main function to load data and initialize the dashboard
async function initializeDashboard() {
  const chartsDataPath = "Data/Content/Charts-Database.csv";
  const sectorsDataPath = "Data/Content/Sector-Database.csv";
  try {
    const chartsData = await loadCSV(chartsDataPath);
    const sectorsData = await loadCSV(sectorsDataPath);

    const sectorsMap = mapSectorsData(sectorsData);
    const chartsMap = mapChartsData(chartsData);

    populateSectorMenu(sectorsMap, chartsMap);

    const mainSection = document.getElementById("mainSection");
    const homePageContent = document.getElementById("homePageContent");
    const homeButton = document.getElementById("homeButton");
    homeButton.addEventListener("click", () => {
      mainSection.innerHTML = homePageContent.innerHTML;
    });
  } catch (error) {
    console.error("Error loading CSV files:", error);
  }
}

// Initialize the dashboard when the script loads
initializeDashboard();
