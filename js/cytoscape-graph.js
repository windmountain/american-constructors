cytoscape.use(cytoscapeDagre);

const dagreLayout = { name: "dagre", rankSep: 80, nodeSep: 200 };

function escapeAttr(value) {
  return String(value ?? "").replace(/[&"<>]/g, (c) => ({ "&": "&amp;", '"': "&quot;", "<": "&lt;", ">": "&gt;" }[c]));
}

function taskCardTpl(data) {
  return (
    `<task-card` +
    ` data-name="${escapeAttr(data.name)}"` +
    ` data-section="${escapeAttr(data.section)}"` +
    ` data-estimate="${escapeAttr(data.estimate)}"` +
    ` data-es="${escapeAttr(data.es)}"` +
    ` data-ls="${escapeAttr(data.ls)}"` +
    ` data-es-date="${escapeAttr(data.esDate)}"` +
    ` data-ls-date="${escapeAttr(data.lsDate)}"` +
    ` data-slack="${escapeAttr(data.slack)}"` +
    `></task-card>`
  );
}

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
          style: {
            label: "",
            width: 180,
            height: 70,
            shape: "rectangle",
            "background-opacity": 0,
            "border-width": 0,
          },
        },
        {
          selector: "node[slack <= 0.001][kind != 'task']",
          style: { "border-width": 4, "border-color": "#eab308" },
        },
        { selector: "edge", style: { "target-arrow-shape": "triangle", "curve-style": "bezier" } },
      ],
    });
    this._cy.nodeHtmlLabel(
      [
        {
          query: "node[kind = 'task']",
          halign: "center",
          valign: "center",
          halignBox: "center",
          valignBox: "center",
          tpl: taskCardTpl,
        },
      ],
      { enablePointerEvents: true },
    );
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
