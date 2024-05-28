// Utility function to load and parse a CSV file using PapaParse
function loadCSV(filePath) {
  return new Promise((resolve, reject) => {
    Papa.parse(filePath, {
      download: true,
      header: true,
      complete: function (results) {
        resolve(results.data);
      },
      error: function (error) {
        reject(error);
      },
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

  // Clear existing menu items if any
  sectorMenu.innerHTML = "";

  for (const sectorId in sectorsMap) {
    const li = document.createElement("li");
    const button = document.createElement("button");
    button.textContent = sectorsMap[sectorId].name;

    // Set CSS styles for the button
    button.dataset.sectorColor = sectorsMap[sectorId].color;

    // Add hover effect
    button.addEventListener("mouseenter", (event) => {
      event.target.style.backgroundColor = sectorsMap[sectorId].color;
    });

    button.addEventListener("mouseleave", (event) => {
      if (!event.target.classList.contains("active")) {
        event.target.style.backgroundColor = "";
      }
    });

    // Add click event listener to display charts and mark the button as active
    button.addEventListener("click", () => {
      // Remove active class from all buttons
      const buttons = sectorMenu.querySelectorAll("button");
      buttons.forEach((btn) => {
        btn.classList.remove("active");
        btn.style.backgroundColor = "";
      });

      // Add active class to the clicked button
      button.classList.add("active");
      button.style.backgroundColor = sectorsMap[sectorId].color;

      // Display charts for the selected sector
      displayChartsForSector(sectorId, sectorsMap, chartsMap);
    });

    li.appendChild(button);
    sectorMenu.appendChild(li);
  }
}

// Function to display charts for a specific sector
async function displayChartsForSector(sectorId, sectorsMap, chartsMap) {
  const mainSection = document.getElementById("mainSection");
  const chartsContainer = document.getElementById("chartsContainer");
  const sectorName = sectorsMap[sectorId].name;
  const sectorColor = sectorsMap[sectorId].color; // Get the sector color

  // Clear previous charts and set the active sector name as the title
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
      ); // Pass the sector color
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
  console.log("Unit:", unit); // Log the unit variable

  const {
    "Chart-Name": chartName,
    "Chart-Description": chartDescription,
    "Data-Source": dataSource,
    "Source-Website": sourceWebsite,
  } = chart;

  const margin = { top: 75, right: 30, bottom: 60, left: 70 };
  const width = 400 - margin.left - margin.right;
  const height = 300 - margin.top - margin.bottom;

  const chartContainer = document.createElement("div");
  chartContainer.classList.add("chart-container");

  const infoButton = document.createElement("button");
  infoButton.textContent = "Info";
  infoButton.classList.add("info-button");
  infoButton.addEventListener("click", () =>
    toggleChartInfo(chartContainer, chart, chartData, xAxis, yAxis)
  );

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

  // Generate x-axis ticks
  const xTickValues = chartData
    .map((d) => d[xAxis])
    .filter((_, i, arr) => i % Math.ceil(arr.length / 5) === 0);

  const xAxisElement = svg
    .append("g")
    .attr("transform", `translate(0,${height})`)
    .call(d3.axisBottom(x).tickValues(xTickValues))
    .selectAll("text")
    .attr("transform", "rotate(-45)")
    .style("text-anchor", "end");

  const yAxisElement = svg.append("g").call(d3.axisLeft(y).ticks(5));

  // Label for the y-axis
  yAxisElement
    .append("text")
    .attr("transform", "rotate(-90)")
    .attr("y", -margin.left + 10) // Adjust the y position in pixels
    .attr("x", -height / 2) // Adjust the x position in pixels
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("fill", "black") // Set text color to black
    .style("visibility", "visible") // Ensure visibility is set to visible
    .style("font-size", "14px") // Set font size
    .style("font-family", "sans-serif") // Set font family
    .style("font-weight", "bold") // Set font weight to bold
    .text(unit);

  // Ensure that no overlays are covering the y-axis label
  yAxisElement.style("position", "relative");
  yAxisElement.style("display", "block");

  // Make x and y axis lines slightly thicker
  svg.selectAll(".domain").attr("stroke-width", 2); // For axis lines
  svg.selectAll(".tick line").attr("stroke-width", 1.5); // For tick lines

  // Wrap chart title
  const wrappedTitle = wrapText(chartName, 40); // Wrap title after 40 characters

  svg
    .append("text")
    .attr("class", "chart-title")
    .attr("x", width / 2)
    .attr("y", -50) // Adjusted y position in pixels to move the title up
    .attr("text-anchor", "middle")
    .style("font-size", "14px")
    .style("font-family", "sans-serif") // Set font family
    .style("font-weight", "bold") // Set font weight to bold
    .selectAll("tspan")
    .data(wrappedTitle.split("\n"))
    .enter()
    .append("tspan")
    .attr("x", width / 2)
    .attr("dy", "1.2em")
    .text(function (d) {
      return d;
    });

  const line = d3
    .line()
    .x((d) => x(d[xAxis]) + x.bandwidth() / 2)
    .y((d) => y(+d[yAxis]));

  svg
    .append("path")
    .datum(chartData)
    .attr("fill", "none")
    .attr("stroke", sectorColor) // Use the sector color for the line
    .attr("stroke-width", 1.5)
    .attr("d", line); // Corrected syntax

  // Add circles at each data point
  const circles = svg
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

      // Append tooltip
      const tooltip = svg
        .append("text")
        .attr("class", "data-tooltip")
        .attr("x", x(d[xAxis]) + x.bandwidth() / 2)
        .attr("y", y(+d[yAxis]) + 30) // Adjusted to be below the circle
        .style("text-anchor", "middle")
        .style("font-size", "12px")
        .style("fill", "black")
        .style("pointer-events", "none");

      tooltip
        .append("tspan")
        .text(
          "Value: " + (Math.round(d[yAxis] * 10) / 10).toString() + " " + unit
        );

      tooltip
        .append("tspan")
        .text("Year: " + d[xAxis])
        .attr("x", x(d[xAxis]) + x.bandwidth() / 2)
        .attr("dy", "1.2em"); // Adjusted to be below the first tspan

      // Store tooltip reference
      d3.select(this).node().__tooltip__ = tooltip;
    })
    .on("mouseleave", function (event, d) {
      d3.select(this).attr("stroke-width", 1);

      // Remove tooltip
      d3.select(this).node().__tooltip__.remove();
    });

  chartContainer.appendChild(infoButton);
  container.appendChild(chartContainer);
}

// Function to toggle chart and info display
function toggleChartInfo(container, chart, chartData, xAxis, yAxis) {
  const chartInfo = container.querySelector(".chart-info");
  const chartContent = container.querySelector("svg");
  const infoButton = container.querySelector(".info-button");

  if (chartInfo) {
    // If info is currently displayed, hide it and show the chart
    chartInfo.remove();
    chartContent.style.display = "block";
    infoButton.textContent = "Info";
  } else {
    // If chart is currently displayed, hide it and show the info
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
    for (let i = 0; i < words.length; i++) {
      if ((line + words[i]).length > maxLength) {
        wrappedText += `${line.trim()}\n`;
        line = "";
      }
      line += `${words[i]} `;
    }
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
    // Load CSV data
    const chartsData = await loadCSV(chartsDataPath);
    const sectorsData = await loadCSV(sectorsDataPath);

    // Map data
    const sectorsMap = mapSectorsData(sectorsData);
    const chartsMap = mapChartsData(chartsData);

    // Populate the sector menu
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
