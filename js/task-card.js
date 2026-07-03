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
        width: 180px;
        box-sizing: border-box;
        padding: 8px 10px;
        border-radius: 6px;
        background: #3b82f6;
        color: #ffffff;
        border: 1px solid #1d4ed8;
        font: 12px/1.35 -apple-system, BlinkMacSystemFont, sans-serif;
      }
      .card.critical { border: 4px solid #dc2626; }
      .section {
        text-transform: uppercase;
        letter-spacing: 0.04em;
        font-size: 10px;
        opacity: 0.85;
      }
      .name { font-weight: 600; margin: 2px 0; }
      .estimate, .schedule, .dates { font-size: 11px; opacity: 0.9; }
    `;

    const card = document.createElement("div");
    card.className = critical ? "card critical" : "card";

    const section = document.createElement("div");
    section.className = "section";
    section.textContent = d.section || "";

    const name = document.createElement("div");
    name.className = "name";
    name.textContent = d.name || "";

    const estimate = document.createElement("div");
    estimate.className = "estimate";
    estimate.textContent = d.estimate || "";

    const schedule = document.createElement("div");
    schedule.className = "schedule";
    schedule.textContent = `ES ${d.es}  ·  LS ${d.ls}`;

    card.append(section, name, estimate, schedule);

    if (d.esDate && d.lsDate) {
      const dates = document.createElement("div");
      dates.className = "dates";
      dates.textContent = `${d.esDate} → ${d.lsDate}`;
      card.append(dates);
    }

    root.append(style, card);
  }
}

customElements.define("task-card", TaskCard);
