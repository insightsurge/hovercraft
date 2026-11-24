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
  engine?.attach(document);
  layer()?.start(config);

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
    if (node.matches(SOURCES)) read(node);
    for (const element of node.querySelectorAll(SOURCES)) read(element);
  };

  // What is in the page now, what the parser adds, and what Shiny adds later (a dependency's <meta> in the head, a
  // renderUI() result in the body).
  scan(document.documentElement);
  new MutationObserver((records) => {
    for (const record of records) for (const node of record.addedNodes) scan(node);
  }).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener('DOMContentLoaded', () => scan(document.documentElement));

  window.HovercraftRuntime = Object.freeze({ version: config.global });
})();
