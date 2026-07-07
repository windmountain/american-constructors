class TaskCard extends HTMLElement {
  connectedCallback() {
    if (!this.shadowRoot) {
      this.attachShadow({ mode: "open" });
    }
    this.render();
  }

  render() {
    const d = this.dataset;
    const critical = parseFloat(d.slack) <= 0.001;
    const root = this.shadowRoot;
    root.innerHTML = "";

    const style = document.createElement("style");
    style.textContent = `
      .card {
        width: 200px;
        box-sizing: border-box;
        border-radius: 6px;
        font: 24px/1.35 -apple-system, BlinkMacSystemFont, sans-serif;
        overflow: hidden;
      }
      .card.task {
        --divider: rgba(255, 255, 255, 0.35);
        background: #3b82f6;
        color: #ffffff;
        border: 1px solid #1d4ed8;
      }
      .card.milestone {
        --divider: rgba(17, 24, 39, 0.15);
        background: #e5e7eb;
        color: #111827;
        border: 1px solid #9ca3af;
      }
      .card.critical { outline: 10px solid #dc2626; }
      .grid-row {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr;
        border-top: 1px solid var(--divider);
      }
      .grid-row:first-child { border-top: none; }
      .cell {
        padding: 3px 2px;
        text-align: center;
        border-left: 1px solid var(--divider);
      }
      .cell:first-child { border-left: none; }
      .cell .label {
        font-size: 10px;
        text-transform: uppercase;
        letter-spacing: 0.03em;
        opacity: 0.75;
      }
      .cell .value { font-size: 24px; font-weight: 600; }
      .middle-row {
        border-top: 1px solid var(--divider);
        padding: 6px 8px;
        text-align: center;
      }
      .section {
        text-transform: uppercase;
        letter-spacing: 0.04em;
        font-size: 10px;
        opacity: 0.85;
      }
      .name { font-size: 12px; font-weight: 600; margin-top: 2px; }
      .spreadsheet-schedule {
        border-top: 1px solid var(--divider);
        padding: 4px 6px;
        text-align: center;
        font-size: 20px;
        opacity: 0.9;
      }
    `;

    const kind = d.kind === "milestone" ? "milestone" : "task";
    const card = document.createElement("div");
    card.className = `card ${kind}${critical ? " critical" : ""}`;

    const topRow = gridRow([
      ["ES", d.es],
      ["Duration", d.estimate],
      ["EF", d.ef],
    ]);

    const middleRow = document.createElement("div");
    middleRow.className = "middle-row";
    const section = document.createElement("div");
    section.className = "section";
    section.textContent = d.section || "";
    const name = document.createElement("div");
    name.className = "name";
    name.textContent = d.name || "";
    middleRow.append(section, name);

    const bottomRow = gridRow([
      ["LS", d.ls],
      ["Slack", d.slack],
      ["LF", d.lf],
    ]);

    card.append(topRow, middleRow, bottomRow);

    if (d.showSpreadsheet === "true") {
      const spreadsheetSchedule = document.createElement("div");
      spreadsheetSchedule.className = "spreadsheet-schedule";
      spreadsheetSchedule.textContent = `s_ES ${d.sEs}  ·  s_EF ${d.sEf}  ·  s_LF ${d.sLf}  ·  s_LS ${d.sLs}`;
      card.append(spreadsheetSchedule);
    }

    root.append(style, card);
  }
}

function gridRow(cells) {
  const row = document.createElement("div");
  row.className = "grid-row";
  for (const [label, value] of cells) {
    const cell = document.createElement("div");
    cell.className = "cell";

    const labelEl = document.createElement("div");
    labelEl.className = "label";
    labelEl.textContent = label;

    const valueEl = document.createElement("div");
    valueEl.className = "value";
    valueEl.textContent = value || "";

    cell.append(labelEl, valueEl);
    row.append(cell);
  }
  return row;
}

customElements.define("task-card", TaskCard);
