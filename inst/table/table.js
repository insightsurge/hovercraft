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

  // Where a tip is, as the JSON its Shiny inputs report (see inst/runtime/shiny.js): the table's id, the target, the 1-based
  // data row (null for a header) and the data column's name (null for a row). R asks for it (`spec.id.address` names the
  // attribute) only for the tips that send events. Null when the row is not known yet.
  const addressOf = (spec, row) => {
    const id = spec.id;
    if (id.target !== 'header' && (typeof row !== 'number' || !Number.isFinite(row))) return null;
    return JSON.stringify({
      table: id.base,
      target: id.target,
      row: id.target === 'header' ? null : row,
      column: id.target === 'row' ? null : (id.name ?? null),
    });
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
    if (openAttribute && el.getAttribute(openAttribute) === pinnedValue) window.Hovercraft?.close(el);
  };

  // What an element was given while a tip was open belongs to the row it was open for. `spec.reset` is the list of those
  // attributes, written by R: today the long-form attributes of the "More" button (inst/more/more.js) and its aria state.
  // Once the element is another row's, or has no tip, they go. Text and placement that a server call changed are not in
  // it: R must list them there for them to go too.
  const reset = (el, spec) => {
    for (const name of spec.reset ?? []) el.removeAttribute(name);
  };

  const unmark = (el, spec) => {
    release(el);
    reset(el, spec);
    forget(el);
    const was = el.hasAttribute(spec.record) || el.hasAttribute(Object.keys(spec.attrs)[0]);
    for (const name of Object.keys(spec.attrs)) el.removeAttribute(name);
    el.removeAttribute(spec.record);
    if (spec.id) el.removeAttribute(spec.id.attribute);
    if (spec.id?.address) el.removeAttribute(spec.id.address);
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
      reset(el, spec);
    }
    for (const [name, value] of Object.entries(spec.attrs)) changed = put(el, name, value) || changed;
    const before = el.getAttribute(spec.record);
    if (spec.id) {
      const id = identity(spec, row);
      if (id === null) el.removeAttribute(spec.id.attribute);
      else changed = put(el, spec.id.attribute, id) || changed;
      if (spec.id.address) {
        const address = addressOf(spec, row);
        if (address === null) el.removeAttribute(spec.id.address);
        else changed = put(el, spec.id.address, address) || changed;
      }
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

  // ---- in place: a row's pinned tip drawn as a panel under the row (`tip_spec(to = "in_place")`) -------------------------
  // A row that carries the panel mark gets its own panel node beside it, never inside it: a `<tr>` after a DT row, a `div`
  // after a reactable row in its group. The engine draws the pinned tip into that node (`pin(row, { into })`, which
  // inst/pin/pin.js asks `panelFor()` for). The node belongs to this layer: it is made when the row pins, put back beside
  // its row after every draw (DT drops foreign rows each time), and removed when the pin has really closed.
  let panelCfg = null; // the names, the same for every table on the page
  const panelTables = new WeakMap(); // a table's widget element -> the id of its table
  const panels = new Map(); // tip id -> { id, kind, root, tableId, holder, cell, box, live, hadFocus }
  const scrollers = new Map(); // a table's scrolling box -> its ResizeObserver
  let panelEvents = false;

  const rowById = (root, id) => {
    if (!panelCfg) return null;
    const sel = `[${panelCfg.idAttribute}="${CSS.escape(id)}"]`;
    return (root?.isConnected ? root : document).querySelector(sel);
  };

  // The panel is as wide as the table's visible box, not its scroll width, so it never runs off a narrow screen.
  const scrollerOf = (p) =>
    p.kind === 'dt'
      ? (p.root.querySelector('.dataTables_scrollBody') ?? p.root.querySelector('.dataTables_wrapper') ?? p.root)
      : (p.root.querySelector('.rt-table') ?? p.root);
  // A scrolling box that no open panel sits in any more, or that has left the page (a table drawn again), lets its observer go.
  const pruneScrollers = () => {
    for (const [box, watcher] of scrollers) {
      const used = [...panels.values()].some((q) => q.root.isConnected && scrollerOf(q) === box);
      if (box.isConnected && used) continue;
      watcher.disconnect();
      scrollers.delete(box);
    }
  };
  const fit = (p) => {
    const box = scrollerOf(p);
    p.box.style.width = `${box.clientWidth}px`;
    pruneScrollers();
    if (!scrollers.has(box) && typeof ResizeObserver === 'function') {
      const watcher = new ResizeObserver(() => {
        for (const q of panels.values())
          if (q.root.isConnected && scrollerOf(q) === box) q.box.style.width = `${box.clientWidth}px`;
      });
      watcher.observe(box);
      scrollers.set(box, watcher);
    }
  };

  // Puts the panel right after its row, and says whether it had to be moved there.
  const seat = (p, item) => {
    const moved = item.nextElementSibling !== p.holder;
    if (moved) item.after(p.holder);
    // The panel spans the row's visible columns: a `td colspan` in DT, `aria-colspan` on the cell of a reactable grid.
    if (p.kind === 'dt') p.cell.colSpan = Math.max(1, item.cells?.length ?? 1);
    else p.cell.setAttribute('aria-colspan', String(Math.max(1, item.querySelectorAll('[role="cell"]').length)));
    if (p.live) item.setAttribute('aria-expanded', 'true');
    fit(p);
    return moved;
  };

  const build = (item, id) => {
    const doc = item.ownerDocument;
    const dt = item.matches('tr');
    const holder = doc.createElement(dt ? 'tr' : 'div');
    const cell = doc.createElement(dt ? 'td' : 'div');
    const box = doc.createElement('div');
    holder.className = 'hovercraft-panel';
    holder.setAttribute('data-hovercraft-panel', '');
    cell.className = 'hovercraft-panel-cell';
    box.className = 'hovercraft-panel-box';
    if (!dt) {
      holder.setAttribute('role', 'row');
      cell.setAttribute('role', 'cell');
    }
    cell.append(box);
    holder.append(cell);
    const p = {
      id,
      kind: dt ? 'dt' : 'rt',
      root: item.closest('.html-widget') ?? item.parentElement,
      tableId: panelTables.get(item.closest('.html-widget')) ?? null,
      holder,
      cell,
      box,
      live: false,
      hadFocus: false,
    };
    box.addEventListener('focusin', () => {
      p.hadFocus = true;
    });
    // Focus that leaves the panel (to another element, or to nothing by a click on a part that takes none) is not lost with
    // the panel; a focused node that is removed sends no event, which is what `hadFocus` is for.
    // A DT draw takes the panel out of the page and the browser may say so with a focusout too. The panel is looked at once
    // the script that is running has finished (a microtask), by when the draw has put it back: focus that is not in a panel
    // that is in the page is focus that left it.
    box.addEventListener('focusout', () => {
      queueMicrotask(() => {
        if (p.holder.isConnected && !box.contains(document.activeElement)) p.hadFocus = false;
      });
    });
    // DT listens on its body for a click on a cell and would select a row or send `cell_clicked` for the panel's cell.
    if (dt)
      for (const type of ['click', 'mousedown', 'dblclick']) holder.addEventListener(type, (e) => e.stopPropagation());
    return p;
  };

  // The node the engine draws row `item`'s pinned tip into, or null when the row is not one that opens in place.
  const panelFor = (item) => {
    if (!panelCfg || !item?.hasAttribute?.(panelCfg.attribute)) return null;
    const id = item.getAttribute(panelCfg.idAttribute);
    if (!id) return null;
    let p = panels.get(id);
    if (!p) {
      p = build(item, id);
      panels.set(id, p);
    }
    seat(p, item);
    return p.box;
  };

  const dropPanel = (id) => {
    const p = panels.get(id);
    if (!p) return;
    const had = p.hadFocus || p.holder.contains(document.activeElement);
    p.holder.remove();
    panels.delete(id);
    const row = rowById(p.root, id);
    row?.removeAttribute('aria-expanded');
    // A reader who was in the panel when it closed goes back to the row, not to the page.
    if (had && row) row.focus?.({ preventScroll: true });
    pruneScrollers();
  };

  // A pin that has closed takes its panel with it, once it is sure to stay closed: a redraw closes the pin and pins it again
  // at once, so the panel is checked a frame later, by what the row says, not by the reason.
  const settle = (id) => {
    const p = panels.get(id);
    if (!p) return;
    const item = rowById(p.root, id);
    if (item && openAttribute && item.getAttribute(openAttribute) === pinnedValue) return;
    dropPanel(id);
  };

  const onPanelTip = (event) => {
    const detail = event.detail;
    const p = detail?.id ? panels.get(detail.id) : null;
    if (!p) return;
    if (detail.state === 'pinned') {
      p.live = true;
      const item = event.target?.nodeType === 1 ? event.target : rowById(p.root, p.id);
      item?.setAttribute('aria-expanded', 'true');
    } else if (detail.state === 'closed') {
      p.live = false;
      requestAnimationFrame(() => settle(detail.id));
    }
  };

  // Puts every open panel of one table back beside its row and tells the engine where it is now. Idempotent: a draw that
  // changed nothing changes nothing.
  const syncPanels = (root, tableId) => {
    for (const p of panels.values()) {
      if (p.tableId !== tableId || !p.live) continue;
      const item = rowById(root, p.id);
      // No row: the engine closes the pin (`gone`), and the panel goes with the close.
      if (!item) continue;
      p.root = root;
      const lost = p.hadFocus && document.activeElement === document.body;
      const moved = seat(p, item);
      // A DT draw drops the panel (`draw.dt`), so it has moved; reactable's observer calls this on every change of its rows.
      // The engine is only told, and the outputs bound again, when the panel really moved: a column that is shown or hidden
      // changes the span and nothing else.
      if (moved) {
        window.Hovercraft?.pin(item, { into: p.box });
        window.Shiny?.bindAll?.(p.box);
        // The panel's own focus target is the engine's box inside it (a region with a tab stop of -1).
        if (lost) p.box.querySelector('[role="region"]')?.focus({ preventScroll: true });
      }
    }
  };

  // The render hook of both widgets: DT puts the panels back after every draw (a sort, a page, a filter, new data, in
  // server mode too) and when a column is shown or hidden; reactable's observer (see `reactable`) does it for its rows.
  const panelsInit = (el, cfg) => {
    const jq = window.jQuery;
    if (!cfg?.panel) {
      // The table is drawn again without a panel: what it had made goes.
      panelTables.delete(el);
      jq?.(el).off('.hovercraftPanel');
      return;
    }
    panelCfg = cfg.panel;
    panelTables.set(el, cfg.panel.id);
    if (!panelEvents) {
      panelEvents = true;
      document.addEventListener(panelCfg.event, onPanelTip);
    }
    // DT's events are sent at its table and bubble, so one handler on the widget element serves the table whenever it is
    // made (the table may come after this hook) and the next render of the same element replaces it.
    if (jq) {
      jq(el)
        .off('.hovercraftPanel')
        .on('draw.dt.hovercraftPanel column-visibility.dt.hovercraftPanel', () => syncPanels(el, cfg.panel.id));
    }
  };

  // `park` lists the ids of Shiny outputs that the tips hold (`tip_output()`): each is taken out of the page until a
  // tip opens, as `tip_define()` does for a tag of the page's own.
  // The tables on the page, by id. Two tables with one id send their inputs to the same name and their tips answer for
  // each other, so the page says so once on the console. The same table drawn again (a renderDT() that runs once more)
  // keeps its element, so it is not a second table.
  const owners = new Map();
  const warned = new Set();
  const claim = (el, id) => {
    if (!id) return;
    // A table that has left the page (removeUI(), a module that went) no longer owns its id, and must not be kept alive.
    for (const [other, owner] of owners) if (!owner.isConnected && owner !== el) owners.delete(other);
    const owner = owners.get(id);
    if (owner && owner !== el && owner.isConnected && !warned.has(id)) {
      warned.add(id);
      console.warn(
        `Hovercraft: two tables have the id "${id}". Give each its own id: their inputs and tips share the name.`,
      );
    }
    owners.set(id, el);
  };

  // Shiny runs a widget's `onRender` again on the same container each time `renderDT()` or `renderReactable()` draws, so
  // what a render registers on the container (watchers, listeners) is let go of at the start of the next one: one
  // registration per container and name, never one per draw. `renew()` ends the previous one and gives the new one a
  // signal for its listeners and a `watch()` for its observers.
  const registrations = new WeakMap(); // a container -> { name -> the function that ends that registration }
  const renew = (el, name) => {
    const mine = registrations.get(el) ?? new Map();
    registrations.set(el, mine);
    mine.get(name)?.();
    const abort = new AbortController();
    const observers = [];
    mine.set(name, () => {
      abort.abort();
      for (const observer of observers) observer.disconnect();
    });
    return {
      signal: abort.signal,
      watch: (observer, target, options) => {
        observer.observe(target, options);
        observers.push(observer);
      },
    };
  };

  // A template name stands for its blocks (R builds the name from them), so a name that is already defined is left alone:
  // DT asks for the templates before it draws (see `tip_table_dt()`), and the widget's render hook asks again.
  const defined = new Map();
  const define = (defines, desc, park, open) => {
    if (desc) descClass = desc;
    if (open) [openAttribute, pinnedValue] = [open.attribute, open.pinned];
    for (const [name, blocks] of Object.entries(defines ?? {})) {
      if (defined.get(name) === blocks) continue;
      window.Hovercraft?.define(name, JSON.parse(blocks));
      defined.set(name, blocks);
    }
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
      // reactable puts `headerClassName` on the filter cell under the header too (`filterable = TRUE`); the tip belongs to
      // the header cell only, so the filter box stays free for the reader to use and nothing is described twice.
      for (const node of el.querySelectorAll('[class*="hovercraft-head-"]:not(.rt-td-filter)')) {
        const at = pick(node, 'head');
        const spec = at && cfg.headers[at[0]];
        if (spec) changed = mark(node, spec, null, null) || changed;
      }
      if (changed) describeAgain(el);
      if (panels.size && panelTables.has(el)) syncPanels(el, panelTables.get(el));
    };
    let queued = false;
    const mine = renew(el, 'reactable');
    const later = () => {
      if (queued) return;
      queued = true;
      requestAnimationFrame(() => {
        queued = false;
        // A frame asked for before the next render took over must not mark the rows with this render's words.
        if (mine.signal.aborted) return;
        apply();
      });
    };
    mine.watch(new MutationObserver(later), el, {
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
    const mine = renew(root, 'keyboard');
    root.addEventListener(
      'keydown',
      (e) => {
        const from = e.target;
        if (!KEYS.has(e.key) || e.altKey || e.metaKey || e.shiftKey || e.isComposing || !from.matches?.(STOP)) return;
        if (e.ctrlKey && e.key !== 'Home' && e.key !== 'End') return;
        e.preventDefault();
        target(from, e.key, e.ctrlKey)?.focus();
      },
      { signal: mine.signal },
    );
    root.addEventListener(
      'focusin',
      (e) => {
        if (given.has(e.target) && e.target !== current) {
          current = e.target;
          settle();
        }
      },
      { signal: mine.signal },
    );
    let queued = false;
    mine.watch(
      new MutationObserver(() => {
        if (queued) return;
        queued = true;
        requestAnimationFrame(() => {
          queued = false;
          if (mine.signal.aborted) return;
          settle();
        });
      }),
      root,
      { childList: true, subtree: true, attributes: true, attributeFilter: [kind] },
    );
    settle();
  };

  window.HovercraftTable = Object.freeze({
    mark,
    define,
    claim,
    panels: panelsInit,
    panelFor,
    dropPanel,
    // For the tests: how many observers of a table's width are alive.
    stats: () => ({ observers: scrollers.size, panels: panels.size }),
    headers,
    reactable,
    keyboard,
    dtRecord,
    dtRow,
    rowClass,
  });
})();
