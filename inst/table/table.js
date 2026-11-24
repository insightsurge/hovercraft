// hovercraft's own layer for tip_table() (experimental spike). It only writes the tip engine's public attributes onto
// the rows, cells and header cells of a DT or reactable table, and defines the templates the tips use. The attribute
// names and the values come from R (see R/table.R); the record of a row is read from the row's own data in the browser
// (DT hands it to its row callbacks, reactable to its class functions), so a table that its widget updates with new
// data (`DT::replaceData()`, `reactable::updateReactable()`) keeps tips that follow the data. Nothing here is the
// engine's own state.
(() => {
  // `spec` is { attrs: { name: value }, record: name of the record attribute, needs: [fields the tip reads] }; `record`
  // is the row's JSON, or null. A tip whose own fields are all blank in this record (an NA cell) has no tip.
  const blank = (spec, record) => {
    if (!spec.needs?.length || typeof record !== 'string' || record === '') return false;
    const row = JSON.parse(record);
    return spec.needs.every((field) => row[field] === null || row[field] === undefined || row[field] === '');
  };

  // The JSON record of one row: the values of `fields` (names, in the order R gives them) taken from `values`, a
  // function of a field name. A missing or non-finite value is `null`, as in a record R writes.
  const recordFrom = (fields, read) => {
    const record = {};
    for (const field of fields) {
      const value = read(field);
      record[field] = value === undefined || (typeof value === 'number' && !Number.isFinite(value)) ? null : value;
    }
    return JSON.stringify(record);
  };

  // DT escapes the HTML of a string in the columns `escaped` says (true for all, false for none, or a list of column
  // positions) before it hands a row over, and htmltools escapes only `&`, `<` and `>`. One pass over the text undoes
  // exactly that: a value that held the text "&amp;" arrives as "&amp;amp;" and comes back as "&amp;", not "&".
  const ENTITIES = { amp: '&', lt: '<', gt: '>' };
  const decodeEntities = (text) => text.replace(/&(amp|lt|gt);/g, (_, name) => ENTITIES[name]);

  // DT: `fieldAt` maps each field to the position of its column in the row's data (an array).
  const dtRecord = (data, fieldAt, escaped) => {
    const fields = Object.keys(fieldAt ?? {});
    if (!fields.length) return null;
    return recordFrom(fields, (field) => {
      const at = fieldAt[field];
      const value = data?.[at];
      const undo = escaped === true || (Array.isArray(escaped) && escaped.includes(at));
      return undo && typeof value === 'string' ? decodeEntities(value) : value;
    });
  };

  // DT: the 1-based data row of the row DT is drawing, or null. In client mode DT's own row index is the data row. In
  // server mode DT holds one page, and the server says which data rows it sent (`DT_rows_current`).
  const dtRow = (table, index) => {
    const settings = table?.fnSettings?.();
    if (!settings?.oFeatures?.bServerSide) return typeof index === 'number' ? index + 1 : null;
    const row = settings.json?.DT_rows_current?.[index];
    return typeof row === 'number' ? row : null;
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

  // The engine's `open` attribute (name and the value of a pin), set by `define`: an element that carries it with that value
  // holds a pinned tip.
  let openAttribute = null;
  let pinnedValue = null;

  // A pinned row keeps its pin while the data row is the same, and its tip follows the row's new values. Once the widget
  // gives the element another data row (React reuses its row nodes when a table sorts, pages or filters) or takes the tip
  // away, the pin would show another row's tip under the old row's id, so it is closed first, under the id it was made for.
  const release = (el) => {
    if (openAttribute && el.getAttribute(openAttribute) === pinnedValue) window.Hovercraft?.close();
  };

  const unmark = (el, spec) => {
    release(el);
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
    if (spec.id && el.hasAttribute(spec.id.attribute) && el.getAttribute(spec.id.attribute) !== identity(spec, row)) {
      release(el);
    }
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

  // `park` lists the ids of Shiny outputs that the tips hold (`tip_output()`): each is taken out of the page until a
  // tip opens, as `tip_define()` does for a tag of the page's own.
  const define = (defines, desc, park, open) => {
    if (desc) descClass = desc;
    if (open) [openAttribute, pinnedValue] = [open.attribute, open.pinned];
    for (const [name, blocks] of Object.entries(defines ?? {})) window.Hovercraft?.define(name, JSON.parse(blocks));
    for (const id of park ?? []) window.HovercraftShiny?.park(id);
  };

  // DT: the row and cell callbacks R writes into the table's options call `mark` with the row's data; the header cells
  // are marked once the table exists, because DT draws them from the table's own HTML.
  const headers = (api, specs) => {
    for (const [index, spec] of Object.entries(specs)) mark(api.column(Number(index)).header(), spec, null, null);
  };

  // reactable: the class of a row. `rowInfo` is reactable's own: `index` is the row's place in the current data and
  // `values` its values by column id. The class says the data row and carries the record as base64url of its UTF-8
  // JSON, so it changes with the data and nothing is kept in the page.
  const toBase64Url = (text) =>
    btoa(unescape(encodeURIComponent(text)))
      .replace(/\+/g, '-')
      .replace(/\//g, '_')
      .replace(/=+$/, '');
  const fromBase64Url = (code) => {
    const padded = code.replace(/-/g, '+').replace(/_/g, '/');
    return decodeURIComponent(escape(atob(padded + '='.repeat((4 - (padded.length % 4)) % 4))));
  };
  const rowClass = (rowInfo, fields) => {
    if (!rowInfo || rowInfo.aggregated) return '';
    const own = `hovercraft-row-${rowInfo.index}`;
    if (!fields?.length) return own;
    return `${own} hovercraft-rec-${toBase64Url(recordFrom(fields, (field) => rowInfo.values?.[field]))}`;
  };

  // reactable keeps no row data in the page, so R puts a class on each row, cell and header cell (its own `rowClass`,
  // `class` and `headerClass`) that says which data row it shows, and the row's class also holds its record (see
  // `rowClass`); this reads the classes and marks the element. React reuses the elements when the table is sorted,
  // filtered, paged or given new data, which changes the class, so a change of class marks again.
  const reactable = (el, cfg) => {
    // The record a row's class holds, or null: a cell reads its row's.
    const recordOf = (node) => {
      const row = node.matches('[class*="hovercraft-rec-"]') ? node : node.closest('[class*="hovercraft-rec-"]');
      const hit = /(?:^|\s)hovercraft-rec-([A-Za-z0-9_-]+)(?:\s|$)/.exec(row?.getAttribute('class') ?? '');
      return hit ? fromBase64Url(hit[1]) : null;
    };
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
        if (at && cfg.rows) changed = mark(node, cfg.rows, recordOf(node), at[0] + 1) || changed;
      }
      for (const node of el.querySelectorAll('[class*="hovercraft-cell-"]')) {
        const at = pick(node, 'cell');
        const spec = at && cfg.cells[at[0]];
        if (spec) changed = mark(node, spec, recordOf(node), at[1] + 1) || changed;
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

  window.HovercraftTable = Object.freeze({ mark, define, headers, reactable, keyboard, dtRecord, dtRow, rowClass });
})();
