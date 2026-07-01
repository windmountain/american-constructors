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
        {
          selector: "node",
          style: {
            label: "data(label)",
            shape: "round-rectangle",
            "text-wrap": "wrap",
            "text-max-width": "160px",
            "text-valign": "center",
            "text-halign": "center",
            width: "label",
            height: "label",
            padding: "12px",
            "background-color": "#e5e7eb",
            "border-width": 1,
            "border-color": "#9ca3af",
            "font-size": 12,
            color: "#111827",
          },
        },
        {
          selector: "node[kind = 'task']",
          style: { "background-color": "#3b82f6", color: "#ffffff", "border-color": "#1d4ed8" },
        },
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
