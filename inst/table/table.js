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

  // The elements this layer gave a `tabindex` (see `keyboard` below), so it can take it back.
  const given = new WeakSet();

  // The engine's class for the hidden description it adds inside a tagged element (and the element's
  // `aria-describedby` points at). Set by `define`.
  let descClass = null;

  // Takes away the descriptions the engine added to an element. Widgets that reuse an element for another row (React in
  // reactable) leave a description that still says the old row's tip; once gone, the engine writes a fresh one.
  const forget = (el) => {
    const ids = (el.getAttribute('aria-describedby') ?? '').split(/\s+/).filter(Boolean);
    if (!descClass || !ids.length) return;
    const keep = ids.filter((id) => {
      const node = el.ownerDocument.getElementById(id);
      if (!node?.classList.contains(descClass) || !el.contains(node)) return true;
      node.remove();
      return false;
    });
    if (keep.length) el.setAttribute('aria-describedby', keep.join(' '));
    else el.removeAttribute('aria-describedby');
  };

  const unmark = (el, spec) => {
    forget(el);
    const was = el.hasAttribute(spec.record) || el.hasAttribute(Object.keys(spec.attrs)[0]);
    for (const name of Object.keys(spec.attrs)) el.removeAttribute(name);
    el.removeAttribute(spec.record);
    if (spec.id) el.removeAttribute(spec.id.attribute);
    if (given.has(el)) {
      el.removeAttribute('tabindex');
      given.delete(el);
    }
    return was;
  };

  // Sets an attribute only when it differs, and says whether it did.
  const put = (el, name, value) => {
    if (el.getAttribute(name) === value) return false;
    el.setAttribute(name, value);
    return true;
  };

  // Tags the element for one tip and says whether anything changed. `row` is the 1-based data row of a row or cell, or
  // null (a header cell has none).
  const mark = (el, spec, record, row) => {
    if (!el || !spec) return false;
    if (blank(spec, record)) return unmark(el, spec);
    let changed = false;
    for (const [name, value] of Object.entries(spec.attrs)) changed = put(el, name, value) || changed;
    const before = el.getAttribute(spec.record);
    if (spec.id) {
      const id = identity(spec, row);
      if (id === null) el.removeAttribute(spec.id.attribute);
      else changed = put(el, spec.id.attribute, id) || changed;
    }
    if (typeof record === 'string' && record !== '') changed = put(el, spec.record, record) || changed;
    else el.removeAttribute(spec.record);
    if (changed && before !== null) forget(el);
    return changed;
  };

  // The engine describes a tagged element to a screen reader when elements are added to the page, and reactable's
  // arrive before this layer tags them. Adding and removing a node tells the engine to look again; it describes only
  // what it has not described yet. Only called when something was newly tagged, so it ends after one round.
  const describeAgain = (el) => {
    const node = el.ownerDocument.createComment('');
    el.append(node);
    node.remove();
  };

  // `park` lists the ids of Shiny outputs that the tips hold (`tip_output()`): each is taken out of the page until a tip
  // opens, as `tip_define()` does for a tag of the page's own.
  const define = (defines, desc, park) => {
    if (desc) descClass = desc;
    for (const [name, blocks] of Object.entries(defines ?? {})) window.Hovercraft?.define(name, JSON.parse(blocks));
    for (const id of park ?? []) window.HovercraftShiny?.park(id);
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
      let changed = false;
      for (const node of el.querySelectorAll('[class*="hovercraft-row-"]')) {
        const at = pick(node, 'row');
        if (at && cfg.rows) changed = mark(node, cfg.rows, cfg.records[at[0]] ?? null, at[0] + 1) || changed;
      }
      for (const node of el.querySelectorAll('[class*="hovercraft-cell-"]')) {
        const at = pick(node, 'cell');
        const spec = at && cfg.cells[at[0]];
        if (spec) changed = mark(node, spec, cfg.records[at[1]] ?? null, at[1] + 1) || changed;
      }
      for (const node of el.querySelectorAll('[class*="hovercraft-head-"]')) {
        const at = pick(node, 'head');
        const spec = at && cfg.headers[at[0]];
        if (spec) changed = mark(node, spec, null, null) || changed;
      }
      if (changed) describeAgain(el);
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

  // Keyboard reach. A row, a cell or a header cell is not focusable by itself, so each tagged one gets `tabindex="-1"`
  // and the table gets one tab stop (a roving `tabindex="0"`). Arrow keys move between the tagged elements: left and
  // right inside a row (the row itself first, then its tagged cells), up and down between rows, to the same column;
  // Home and End go to the ends of the row, with Ctrl to the ends of the table. The engine shows a tip for an element
  // that takes focus from the keyboard. An element the widget made focusable itself (DT's sortable headers) keeps
  // its own `tabindex`; it can still be reached with the arrow keys.
  const keyboard = (root, kind) => {
    const STOP = `[${kind}^="table_"]`;
    const ROW = 'tr, [role="row"]';
    let current = null;
    // DT's scrollX keeps a collapsed copy of the header (no height) that cannot be seen, so it is not a stop.
    const shown = (n) => n.getClientRects().length > 0 && n.getBoundingClientRect().height > 0;
    const stops = () => [...root.querySelectorAll(STOP)];
    const owned = (n) => !n.hasAttribute('tabindex') || given.has(n);

    const settle = () => {
      const all = stops();
      if (!current?.isConnected || !given.has(current) || !root.contains(current)) {
        current = all.find((n) => owned(n) && shown(n)) ?? null;
      }
      for (const n of all) {
        if (!owned(n)) continue;
        given.add(n);
        put(n, 'tabindex', n === current ? '0' : '-1');
      }
    };

    const rowOf = (n) => (n.matches(ROW) ? n : n.closest(ROW));
    const column = (n) => (n.matches(ROW) ? -1 : Array.prototype.indexOf.call(n.parentElement.children, n));
    const near = (list, key) =>
      list.find((n) => column(n) === key) ??
      list.reduce((best, n) => (Math.abs(column(n) - key) < Math.abs(column(best) - key) ? n : best));

    const target = (from, key, ctrl) => {
      const all = stops().filter(shown);
      const rows = [...new Set(all.map(rowOf))];
      const here = all.filter((n) => rowOf(n) === rowOf(from));
      const at = here.indexOf(from);
      const across = (list) => list[key === 'End' ? list.length - 1 : 0];
      if (key === 'ArrowRight') return here[at + 1];
      if (key === 'ArrowLeft') return here[at - 1];
      if (key === 'Home' || key === 'End') return across(ctrl ? all : here);
      const to = rows[rows.indexOf(rowOf(from)) + (key === 'ArrowDown' ? 1 : -1)];
      return to
        ? near(
            all.filter((n) => rowOf(n) === to),
            column(from),
          )
        : undefined;
    };

    const KEYS = new Set(['ArrowRight', 'ArrowLeft', 'ArrowDown', 'ArrowUp', 'Home', 'End']);
    root.addEventListener('keydown', (e) => {
      const from = e.target;
      if (!KEYS.has(e.key) || e.altKey || e.metaKey || e.shiftKey || e.isComposing || !from.matches?.(STOP)) return;
      if (e.ctrlKey && e.key !== 'Home' && e.key !== 'End') return;
      e.preventDefault();
      target(from, e.key, e.ctrlKey)?.focus();
    });
    root.addEventListener('focusin', (e) => {
      if (given.has(e.target) && e.target !== current) {
        current = e.target;
        settle();
      }
    });
    let queued = false;
    new MutationObserver(() => {
      if (queued) return;
      queued = true;
      requestAnimationFrame(() => {
        queued = false;
        settle();
      });
    }).observe(root, { childList: true, subtree: true, attributes: true, attributeFilter: [kind] });
    settle();
  };

  window.HovercraftTable = Object.freeze({ mark, define, headers, reactable, keyboard });
})();
