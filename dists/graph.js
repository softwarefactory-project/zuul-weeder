// The config graph for the welcome page
// Based on https://bl.ocks.org/mbostock/4062045
const radius = 4,
  headerSize = 32,
  minX = radius,
  minY = headerSize + radius;

function setSize() {
  // Set and update global value on changes
  width = window.innerWidth;
  height = window.innerHeight;
  maxX = width - radius;
  maxY = height - radius;
}

const clampX = (x) => Math.max(minX, Math.min(x, maxX));
const clampY = (y) => Math.max(minY, Math.min(y, maxY));

setSize();
window.addEventListener("resize", setSize);

function renderToy(url) {
  console.log("Rendeing toy", url);
  d3.json(url, function (error, graph) {
    if (error) throw error;

    let svg = d3.select("#main").append("svg").attr("id", "d3");

    let color = d3.scaleOrdinal(d3.schemeCategory20);

    let simulation = d3
      .forceSimulation()
      .force(
        "link",
        d3.forceLink().id((d) => d.id)
      )
      .force("charge", d3.forceManyBody().strength(-5))
      .force("center", d3.forceCenter(width / 2, height / 2));

    let link = svg
      .append("g")
      .attr("class", "links")
      .selectAll("line")
      .data(graph.links)
      .enter()
      .append("line")
      .attr("stroke-width", (d) => 1);

    let node = svg
      .append("g")
      .attr("class", "nodes")
      .selectAll("circle")
      .data(graph.nodes)
      .enter()
      .append("circle")
      .attr("r", radius)
      .attr("fill", (d) => getColor(d.group))
      .call(
        d3
          .drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended)
      );

    node.append("title").text((d) => d.name);

    simulation.nodes(graph.nodes).on("tick", ticked);

    simulation.force("link").links(graph.links);

    function ticked() {
      node
        .attr("cx", (d) => {
          return (d.x = clampX(d.x));
        })
        .attr("cy", (d) => {
          return (d.y = clampY(d.y));
        });
      link
        .attr("x1", (d) => d.source.x)
        .attr("y1", (d) => d.source.y)
        .attr("x2", (d) => d.target.x)
        .attr("y2", (d) => d.target.y);
    }

    function dragstarted(d) {
      if (!d3.event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(d) {
      d.fx = d3.event.x;
      d.fy = d3.event.y;
    }

    function dragended(d) {
      if (!d3.event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }
  });
}

function addTreeHandler() {
  Array.from(document.getElementsByClassName("tree-caret")).forEach(toggler => {
    toggler.addEventListener("click", function () {
      Array.from(this.parentElement.querySelectorAll(".nested"))
         .filter(subList =>  subList.parentElement === this.parentElement)
         .forEach(subList => {
            subList.classList.toggle("active");
      });
      this.classList.toggle("tree-caret-down");
    });
  });
}
