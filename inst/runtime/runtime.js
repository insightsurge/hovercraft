// hovercraft's start script, on top of the tip engine; it never changes the engine. It attaches the engine to the page,
// starts the Shiny layer and runs the operations the page carries as data: defining a template, parking an output, setting
// the page's preset. Nothing here is inline: R gives the names in a <meta name="hovercraft-config">, and the operations in
// <meta name="hovercraft-ops"> or <script type="application/json" data-hovercraft-ops>, which a browser does not run, so a
// strict Content-Security-Policy (`script-src 'self'; style-src 'self'`) allows the page.
(() => {
  if (window.HovercraftRuntime) return;
  // A <meta>'s content is base64 of JSON (see tip_meta_value() in R/runtime.R).
  const decode = (content) =>
    JSON.parse(new TextDecoder().decode(Uint8Array.from(atob(content), (c) => c.charCodeAt(0))));
  const config = (() => {
    const meta = document.querySelector('meta[name="hovercraft-config"]');
    return meta ? decode(meta.content) : null;
  })();
  if (!config) return;

  const engine = window[config.global];
  const layer = () => window[`${config.global}Shiny`];

  // How many pins stay open. The engine reads one number from the page's root; the layer counts the rest. `app` is the
  // app's limit (`use_hovercraft(max_pins =)`, the first one that is given holds), `own` the tables with a count of their
  // own, by the table's id. With any table of its own the engine's limit is its cap, ten, and the layer closes the oldest
  // pin of a pool that has gone past its limit: a table's pool is its own rows, the app's is every other pin.
  const root = document.documentElement;
  const pinning = { app: 1, set: false, own: new Map() };
  const keysOff = new Set();
  let keysSeen = false;
  const syncPins = () => root.setAttribute(config.attributes.pins, String(pinning.own.size ? 10 : pinning.app));
  // A table's pool is its rows: the tip id of a row is `<table>/row/<n>`. (A table that goes away leaves its count
  // behind with no pins in it, which costs nothing.)
  const poolOf = (id) => {
    for (const table of pinning.own.keys()) if (id.startsWith(`${table}/row/`)) return table;
    return null;
  };
  // The tagged item of a tip id: the id the tip was given, else the tag's own id (the engine's own fallback).
  const itemOf = (id) => {
    for (const el of document.querySelectorAll(`[${config.attributes.id}]`))
      if (el.getAttribute(config.attributes.id) === id) return el;
    return document.getElementById(id);
  };
  // A pin that went past its pool's limit: the pool's oldest pins close, never the one that just opened.
  const onTip = (event) => {
    const detail = event.detail;
    if (!pinning.own.size || detail?.state !== 'pinned' || !Array.isArray(detail.ids)) return;
    const pool = poolOf(String(detail.id));
    const limit = pool === null ? pinning.app : pinning.own.get(pool);
    const members = detail.ids.map(String).filter((id) => poolOf(id) === pool);
    for (const id of members.slice(0, Math.max(0, members.length - limit))) {
      const el = itemOf(id);
      if (!el) continue;
      // The engine's own limit closes a pin as "replaced", by whoever pinned the new one: the Shiny layer says the same
      // for this one, though the door gives the close no reason of its own.
      layer()?.replaced(id, { by: detail.by ?? 'reader', via: detail.via ?? null });
      engine?.close(el);
    }
  };
  document.addEventListener(config.events.tip, onTip);
  // The pin key's announcement goes with the key: an item that says P pins it must not, when the key is off.
  const stripKeys = (node) => {
    if (!keysOff.has('pin')) return;
    const items = node.matches?.(`[${config.attributes.pin}][aria-keyshortcuts]`) ? [node] : [];
    for (const el of [...items, ...(node.querySelectorAll?.(`[${config.attributes.pin}][aria-keyshortcuts]`) ?? [])])
      if (/^p$/i.test(el.getAttribute('aria-keyshortcuts'))) el.removeAttribute('aria-keyshortcuts');
  };

  // The pin listener is registered before the Shiny layer's, so a pin that goes past its pool's limit has closed the
  // oldest one before the layer hears of the new pin: `_pinned` is never over the limit.
  engine?.attach(document);
  layer()?.start(config);

  // The words the page shows itself. A scope (the page's root, or a tag that `tip_on(labels = )` marked) holds its words as
  // JSON in an attribute. The engine takes its own keys for the scope, which it applies to the tag and what is inside it;
  // its keys are read from the engine, so none is typed here. The words of hovercraft's own scripts (the pin button, the
  // "Less" button) are answered by `labels(el)`, which reads the scopes from the element up, the nearest word winning.
  const labelsAttribute = config.attributes.labels;
  const readLabels = (node) => {
    const text = node.getAttribute?.(labelsAttribute);
    if (!text) return null;
    try {
      const words = JSON.parse(text);
      return words && typeof words === 'object' && !Array.isArray(words) ? words : null;
    } catch {
      return null;
    }
  };
  const engineKeys = () => Object.keys(engine?.tooltip?.(null, root)?.tooltip_labels ?? {});
  const labelsApplied = new WeakMap();
  const applyLabels = (node) => {
    const text = node.getAttribute(labelsAttribute);
    const words = readLabels(node);
    if (!words || labelsApplied.get(node) === text) return;
    labelsApplied.set(node, text);
    const mine = Object.fromEntries(
      engineKeys().flatMap((key) => (typeof words[key] === 'string' ? [[key, words[key]]] : [])),
    );
    if (Object.keys(mine).length) engine.tooltip({ tooltip_labels: mine }, node);
  };
  const labels = (el) => {
    const out = { ...config.labels };
    const scopes = [];
    for (let node = el; node; node = node.parentElement) {
      const words = readLabels(node);
      if (words) scopes.push(words);
    }
    while (scopes.length) {
      for (const [key, word] of Object.entries(scopes.pop())) if (typeof word === 'string') out[key] = word;
    }
    return out;
  };

  const cssDone = new Set();
  const addCss = (text) => {
    if (typeof CSSStyleSheet === 'function' && 'adoptedStyleSheets' in document) {
      const sheet = new CSSStyleSheet();
      sheet.replaceSync(text);
      document.adoptedStyleSheets = [...document.adoptedStyleSheets, sheet];
    } else {
      // A browser without constructed style sheets: a <style> element, which only a policy on inline style would block.
      const style = document.createElement('style');
      style.textContent = text;
      document.head.append(style);
    }
  };

  const run = {
    define: (op) => engine.define(op.name, op.blocks),
    park: (op) => layer()?.park(op.id),
    parkHtml: (op) => layer()?.parkHtml(op.html),
    // CSS that R made from a theme or a style: a constructed style sheet, which a policy that allows no inline style
    // allows, as it blocks a <style> element and a `style` attribute. Once for each text.
    css: (op) => {
      if (cssDone.has(op.id)) return;
      cssDone.add(op.id);
      addCss(op.text);
    },
    maxPins: (op) => {
      if (!pinning.set) {
        pinning.set = true;
        pinning.app = op.value;
        syncPins();
      } else if (op.value !== pinning.app) {
        console.warn(
          `Hovercraft: use_hovercraft(max_pins = ${op.value}) came after max_pins = ${pinning.app}; the first value holds.`,
        );
      }
    },
    tablePins: (op) => {
      pinning.own.set(op.id, op.max);
      syncPins();
    },
    keys: (op) => {
      const off = [...(op.off ?? [])].sort().join(' ');
      if (keysSeen && off !== [...keysOff].sort().join(' ')) {
        console.warn(`Hovercraft: use_hovercraft(keys = ) came after another keys setting; the first one holds.`);
        return;
      }
      keysSeen = true;
      for (const key of op.off ?? []) keysOff.add(key);
      root.setAttribute(config.attributes.keysOff, [...keysOff].join(' '));
      stripKeys(document);
    },
    // The page's words: merged into the root's, a later word over an earlier one.
    labels: (op) => {
      root.setAttribute(labelsAttribute, JSON.stringify({ ...readLabels(root), ...op.labels }));
      applyLabels(root);
    },
    // The page's own style: attributes on the page's root, which the engine reads before its settings.
    page: (op) => {
      for (const [name, value] of Object.entries(op.attributes ?? {}))
        document.documentElement.setAttribute(name, value);
    },
  };

  const SOURCES = 'meta[name="hovercraft-ops"], script[type="application/json"][data-hovercraft-ops]';
  const done = new WeakSet();
  // One element's operations, once. A malformed block is one the page's author wrote by hand: say so, and go on.
  const read = (element) => {
    if (done.has(element)) return;
    done.add(element);
    try {
      for (const op of element.matches('meta') ? decode(element.content) : JSON.parse(element.textContent))
        run[op.op]?.(op);
    } catch (error) {
      console.warn(`Hovercraft: an operation could not be run: ${error.message}`);
    }
  };
  const scan = (node) => {
    if (node.nodeType !== 1) return;
    stripKeys(node);
    if (node.matches(SOURCES)) read(node);
    for (const element of node.querySelectorAll(SOURCES)) read(element);
    if (node.hasAttribute(labelsAttribute)) applyLabels(node);
    for (const element of node.querySelectorAll(`[${labelsAttribute}]`)) applyLabels(element);
  };

  // What is in the page now, what the parser adds, and what Shiny adds later (a dependency's <meta> in the head, a
  // renderUI() result in the body).
  scan(document.documentElement);
  // The pin key's announcement is written on rows after they are drawn (a table's script does it on a frame), so the
  // attribute is watched too.
  new MutationObserver((records) => {
    for (const record of records) {
      if (record.type === 'attributes') stripKeys(record.target);
      for (const node of record.addedNodes) scan(node);
    }
  }).observe(document.documentElement, {
    childList: true,
    subtree: true,
    attributes: true,
    attributeFilter: ['aria-keyshortcuts'],
  });
  document.addEventListener('DOMContentLoaded', () => scan(document.documentElement));

  window.HovercraftRuntime = Object.freeze({ version: config.global, labels });
})();
