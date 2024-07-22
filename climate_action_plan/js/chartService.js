export function renderChart(data, elementId, chartInfo) {
  console.log("Rendering chart with info:", chartInfo);

  // Dimensions and margins for the chart
  const margin = { top: 20, right: 20, bottom: 30, left: 70 };
  const width = 400 - margin.left - margin.right;
  const height = 300 - margin.top - margin.bottom;

  console.log("Chart dimensions:", { width, height });

  // Create SVG container
  const svg = d3
    .select(`#${elementId}`)
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`);

  // Define X and Y scales
  const x = d3
    .scaleLinear()
    .domain(d3.extent(data, (d) => +d[chartInfo["X-Axis"]]))
    .range([0, width]);

  const y = d3
    .scaleLinear()
    .domain([0, d3.max(data, (d) => +d[chartInfo["Y-Axis"]])])
    .nice()
    .range([height, 0]);

  console.log("X scale domain:", x.domain());
  console.log("Y scale domain:", y.domain());

  // Define X-axis tick format
  const xAxisTickFormat = d3.format("d"); // Format as integer

  // Append X axis with formatted ticks
  svg
    .append("g")
    .attr("class", "x-axis")
    .attr("transform", `translate(0,${height})`)
    .call(d3.axisBottom(x).ticks(5).tickFormat(xAxisTickFormat)) // Apply custom format
    .append("text")
    .attr("class", "x-axis-label")
    .attr("x", width / 2)
    .attr("y", 40)
    .attr("text-anchor", "middle")
    .text(chartInfo["X-Axis"]);

  console.log("X axis appended");

  // Append Y axis with a maximum of 5 ticks
  svg
    .append("g")
    .attr("class", "y-axis")
    .call(d3.axisLeft(y).ticks(5)) // Limit the number of ticks to 5
    .append("text")
    .attr("class", "y-axis-label")
    .attr("transform", "rotate(-90)")
    .attr("y", -55)
    .attr("x", -height / 2)
    .attr("dy", "0.71em")
    .attr("text-anchor", "middle")
    .attr("fill", "black") // Ensure text is visible
    .style("font-size", "14px") // Adjust font size here
    .style("font-family", "sans-serif") // Set font to sans-serif
    .text(chartInfo["Unit"]);

  console.log("Y axis appended with label:", chartInfo["Unit"]);

  // Define line generator function
  const line = d3
    .line()
    .x((d) => x(+d[chartInfo["X-Axis"]]))
    .y((d) => y(+d[chartInfo["Y-Axis"]]));

  console.log("Line generator function created");

  // Append line path
  svg
    .append("path")
    .data([data])
    .attr("class", "line")
    .attr("d", line)
    .attr("fill", "none")
    .attr("stroke", "var(--sectorColour, black)") // Use CSS variable for color
    .attr("stroke-width", 2);

  console.log("Line path appended");

  // Add circles to each data point with hover effects and title elements
  svg
    .selectAll(".dot")
    .data(data)
    .enter()
    .append("circle")
    .attr("class", "dot")
    .attr("cx", (d) => x(+d[chartInfo["X-Axis"]]))
    .attr("cy", (d) => y(+d[chartInfo["Y-Axis"]]))
    .attr("r", 4) // Initial radius of the circles
    .attr("fill", "var(--sectorColour, black)") // Use CSS variable for color
    .attr("stroke", "white") // Initial stroke color
    .attr("stroke-width", 1) // Initial stroke width
    .style("cursor", "pointer") // Change cursor to pointer on hover
    .on("mouseover", function (event, d) {
      d3.select(this)
        .transition()
        .duration(200)
        .attr("r", 6) // Radius on hover
        .attr("stroke", "black") // Stroke color on hover
        .attr("stroke-width", 2); // Stroke width on hover
    })
    .on("mouseout", function (event, d) {
      d3.select(this)
        .transition()
        .duration(200)
        .attr("r", 4) // Radius on mouse out
        .attr("stroke", "white") // Stroke color on mouse out
        .attr("stroke-width", 1); // Stroke width on mouse out
    })
    .append("title") // Add title element for tooltips
    .text((d) => {
      const formatNumber = d3.format(",.0f"); // Define the format function
      return `Value: ${formatNumber(d[chartInfo["Y-Axis"]])} ${
        chartInfo["Unit"]
      } \nYear: ${d[chartInfo["X-Axis"]]}`;
    });

  console.log("Circles with hover effects and tooltips added");

  // Log final chart rendered
  console.log("Chart rendering complete");
}
