// hovercraft's own layer for tip_table() (experimental spike). It only writes the tip engine's public attributes onto
// the rows, cells and header cells of a DT or reactable table, and defines the templates the tips use. The attribute
// names, the values and the records all come from R (see R/table.R); nothing here is the engine's own state.
(() => {
  // `spec` is { attrs: { name: value }, record: name of the record attribute, needs: [fields the tip reads] }; `record`
  // is the row's JSON, or null. A tip whose own fields are all blank in this record (an NA cell) has no tip.
  const blank = (spec, record) => {
    if (!spec.needs?.length || typeof record !== 'string' || record === '') return false;
    const row = JSON.parse(record);
    return spec.needs.every((field) => row[field] === null || row[field] === undefined || row[field] === '');
  };

  // The derived tip id of one element, or null: R sends the base `id`, the target and the data column in `spec.id`
  // (only when the table has an id); `row` is the 1-based data row. The format is this layer's own.
  const identity = (spec, row) => {
    const id = spec.id;
    if (!id) return null;
    if (id.target === 'header') return `${id.base}/header/${id.column}`;
    if (typeof row !== 'number' || !Number.isFinite(row)) return null;
    return id.target === 'row' ? `${id.base}/row/${row}` : `${id.base}/cell/${row}/${id.column}`;
  };

  const unmark = (el, spec) => {
    for (const name of Object.keys(spec.attrs)) el.removeAttribute(name);
    el.removeAttribute(spec.record);
    if (spec.id) el.removeAttribute(spec.id.attribute);
  };

  // `row` is the 1-based data row of a row or cell, or null (a header cell has none).
  const mark = (el, spec, record, row) => {
    if (!el || !spec) return;
    if (blank(spec, record)) {
      unmark(el, spec);
      return;
    }
    for (const [name, value] of Object.entries(spec.attrs)) {
      if (el.getAttribute(name) !== value) el.setAttribute(name, value);
    }
    if (spec.id) {
      const id = identity(spec, row);
      if (id === null) el.removeAttribute(spec.id.attribute);
      else if (el.getAttribute(spec.id.attribute) !== id) el.setAttribute(spec.id.attribute, id);
    }
    if (typeof record === 'string' && record !== '') {
      if (el.getAttribute(spec.record) !== record) el.setAttribute(spec.record, record);
    } else el.removeAttribute(spec.record);
  };

  const define = (defines) => {
    for (const [name, blocks] of Object.entries(defines ?? {})) window.Hovercraft?.define(name, JSON.parse(blocks));
  };

  // DT: the row and cell callbacks R writes into the table's options call `mark` with the row's data; the header cells
  // are marked once the table exists, because DT draws them from the table's own HTML.
  const headers = (api, specs) => {
    for (const [index, spec] of Object.entries(specs)) mark(api.column(Number(index)).header(), spec, null, null);
  };

  // reactable keeps no row data in the page, so R puts a class on each row, cell and header cell (its own `rowClass`,
  // `class` and `headerClass`) that says which data row it shows; this reads the class and marks the element. React
  // reuses the elements when the table is sorted, filtered or paged, which changes the class, so a change of class
  // marks again. `cfg.records[i]` is the JSON record of data row i.
  const reactable = (el, cfg) => {
    const pick = (node, prefix) => {
      const hit = new RegExp(`(?:^|\\s)hovercraft-${prefix}-(\\d+)(?:-(\\d+))?(?:\\s|$)`).exec(
        node.getAttribute('class') ?? '',
      );
      return hit ? [Number(hit[1]), hit[2] === undefined ? null : Number(hit[2])] : null;
    };
    const apply = () => {
      for (const node of el.querySelectorAll('[class*="hovercraft-row-"]')) {
        const at = pick(node, 'row');
        if (at && cfg.rows) mark(node, cfg.rows, cfg.records[at[0]] ?? null, at[0] + 1);
      }
      for (const node of el.querySelectorAll('[class*="hovercraft-cell-"]')) {
        const at = pick(node, 'cell');
        const spec = at && cfg.cells[at[0]];
        if (spec) mark(node, spec, cfg.records[at[1]] ?? null, at[1] + 1);
      }
      for (const node of el.querySelectorAll('[class*="hovercraft-head-"]')) {
        const at = pick(node, 'head');
        const spec = at && cfg.headers[at[0]];
        if (spec) mark(node, spec, null, null);
      }
    };
    let queued = false;
    const later = () => {
      if (queued) return;
      queued = true;
      requestAnimationFrame(() => {
        queued = false;
        apply();
      });
    };
    new MutationObserver(later).observe(el, {
      childList: true,
      subtree: true,
      attributes: true,
      attributeFilter: ['class'],
    });
    apply();
  };

  window.HovercraftTable = Object.freeze({ mark, define, headers, reactable });
})();
