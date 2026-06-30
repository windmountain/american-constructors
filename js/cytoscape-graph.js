cytoscape.use(cytoscapeDagre);

const dagreLayout = { name: "dagre", rankSep: 80, nodeSep: 200 };

class CytoscapeGraph extends HTMLElement {
  connectedCallback() {
    this.style.display = "block";
    this.style.width = "100%";
    this.style.height = "100vh";
    this._cy = cytoscape({
      container: this,
      elements: this._pendingElements || [],
      layout: dagreLayout,
      style: [
        { selector: "node", style: { label: "data(label)" } },
        { selector: "edge", style: { "target-arrow-shape": "triangle", "curve-style": "bezier" } },
      ],
    });
  }

  set elements(value) {
    this._pendingElements = value;
    if (this._cy) {
      this._cy.json({ elements: value });
      this._cy.layout(dagreLayout).run();
    }
  }
}

customElements.define("cytoscape-graph", CytoscapeGraph);
