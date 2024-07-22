// js/renderMap.js

import { colorScaleConfig } from "./config.js"; // Import your color configuration

// Function to find the color based on value
function getColorForValue(value) {
  for (const range of colorScaleConfig.ranges) {
    if (value >= range.min && value <= range.max) {
      return range.color;
    }
  }
  return "#ffffff"; // Default color if no range matches
}

// Function to render a choropleth map with customizable colors
export function renderChoroplethMap(containerId, geojson, valueProperty) {
  const width = 300;
  const height = 550;

  // Create an SVG element
  const svg = d3
    .select(`#${containerId}`)
    .append("svg")
    .attr("width", width)
    .attr("height", height);

  // Create a projection and path generator
  const projection = d3.geoMercator().fitSize([width, height], geojson);
  const path = d3.geoPath().projection(projection);

  // Bind data and create one path per GeoJSON feature
  svg
    .selectAll("path")
    .data(geojson.features)
    .enter()
    .append("path")
    .attr("d", path)
    .attr("fill", (d) => {
      const value = d.properties[valueProperty];
      return getColorForValue(value);
    })
    .attr("stroke", "#000")
    .attr("stroke-width", 0.5)
    .append("title")
    .text(
      (d) =>
        `${d.properties.WARD_NAME} \nTree Canopy Cover: ${d.properties[valueProperty]}%`
    );

  console.log("Map rendering complete");
}
