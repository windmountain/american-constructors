cytoscape.use(cytoscapeDagre);

const dagreLayout = { name: "dagre", rankSep: 80, nodeSep: 200 };

// A cheap fingerprint of which nodes/edges are present, independent of their
// data values, so callers can tell "same elements, data changed" apart from
// "elements were added/removed" without a full set comparison. Ids never
// contain a space (they're spreadsheet codes like "T1" or edge ids like
// "T1->T2"), so joining on one is a safe way to make the list comparable.
function elementIdSignature(elements) {
  return elements
    .map((el) => el.data.id)
    .sort()
    .join(" ");
}

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
    ` data-ef="${escapeAttr(data.ef)}"` +
    ` data-lf="${escapeAttr(data.lf)}"` +
    ` data-ls="${escapeAttr(data.ls)}"` +
    ` data-s-es="${escapeAttr(data.sEs)}"` +
    ` data-s-ef="${escapeAttr(data.sEf)}"` +
    ` data-s-lf="${escapeAttr(data.sLf)}"` +
    ` data-s-ls="${escapeAttr(data.sLs)}"` +
    ` data-show-spreadsheet="${escapeAttr(data.showSpreadsheet)}"` +
    ` data-slack="${escapeAttr(data.slack)}"` +
    `></task-card>`
  );
}

class CytoscapeGraph extends HTMLElement {
  connectedCallback() {
    this.style.display = "block";
    this.style.width = "100%";
    this.style.height = "100vh";
    const initialElements = this._pendingElements || [];
    this._elementIds = elementIdSignature(initialElements);
    this._cy = cytoscape({
      container: this,
      elements: initialElements,
      layout: dagreLayout,
      minZoom: 0.2,
      maxZoom: 2,
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
            width: 200,
            height: 100,
            shape: "rectangle",
            "background-opacity": 0,
            "border-width": 0,
          },
        },
        {
          selector: "node[slack <= 0.001][kind != 'task']",
          style: { "border-width": 4, "border-color": "#dc2626" },
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
    if (!this._cy) {
      return;
    }

    const nextIds = elementIdSignature(value);
    const structureChanged = nextIds !== this._elementIds;
    this._elementIds = nextIds;

    // cy.json() diffs by id: it merges data onto existing elements in place
    // (no effect on position/pan/zoom) and only adds/removes elements whose
    // ids are new/missing. Re-running the layout is only needed when the
    // graph's shape actually changed, e.g. not on a tactic switch, which
    // only updates data (es/ls/slack/label) on the same set of nodes/edges.
    this._cy.json({ elements: value });
    if (structureChanged) {
      this._cy.layout(dagreLayout).run();
    }
  }
}

customElements.define("cytoscape-graph", CytoscapeGraph);
