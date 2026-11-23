// hovercraft's own layer for Shiny, on top of the tip engine; it never changes the engine. It answers the server calls
// tip_show(), tip_hide() and tip_update() (one custom message each), and sends the hint input for the tags that asked
// for it with `events =`. It uses only the engine's public doors, `Hovercraft.show()` and `Hovercraft.hide()`, its
// public attributes and its hint event. R passes the names in `start()`, so none is typed here twice.
(() => {
  if (window.HovercraftShiny) return;

  let names = null;
  const noted = new Set();
  // Says a thing once on the console: a server call that finds nothing is a mistake of the page, not an error.
  const note = (key, text) => {
    if (noted.has(key)) return;
    noted.add(key);
    console.warn(`Hovercraft: ${text}`);
  };

  // The tag with this tip id, else the tag with this HTML id (the engine's own fallback for the id of an event).
  const find = (id) => {
    for (const el of document.querySelectorAll(`[${names.attributes.id}]`)) {
      if (el.getAttribute(names.attributes.id) === id) return el;
    }
    return document.getElementById(id);
  };

  const handlers = {
    show: ({ id }) => {
      const el = find(id);
      if (!el) return note(`show|${id}`, `tip_show(): no tag with the tip id "${id}" is on the page.`);
      window.Hovercraft.show(el);
    },
    // The engine hides whichever hint is open, so only hide when this tag's tip is the one open (the engine marks it).
    hide: ({ id }) => {
      const el = find(id);
      if (!el) return note(`hide|${id}`, `tip_hide(): no tag with the tip id "${id}" is on the page.`);
      if (el.hasAttribute(names.attributes.open)) window.Hovercraft.hide();
    },
    update: ({ id, set, remove }) => {
      const el = find(id);
      if (!el) return note(`update|${id}`, `tip_update(): no tag with the tip id "${id}" is on the page.`);
      for (const [attribute, value] of Object.entries(set ?? {})) el.setAttribute(attribute, value);
      for (const attribute of [remove ?? []].flat()) el.removeAttribute(attribute);
    },
  };

  // The hint event is sent at the tag and bubbles, so one listener on the page serves every tag. A tag sends the
  // input only when its list names it, and only for its own tip: an outer tag with a list does not send for an inner one.
  const sendHint = (event) => {
    const detail = event.detail;
    const el = event.target?.closest?.(`[${names.attributes.events}]`);
    if (!el || !detail?.id || !window.Shiny?.setInputValue) return;
    if (!el.getAttribute(names.attributes.events).split(/\s+/).includes('hint')) return;
    if ((el.getAttribute(names.attributes.id) ?? el.id) !== detail.id) return;
    // "event" priority: the same state twice in a row (a hint opened, closed, opened) must still reach the server.
    window.Shiny.setInputValue(
      `${detail.id}_hint`,
      {
        state: detail.state,
        text: detail.text ?? null,
        by: detail.by ?? null,
        via: detail.via ?? null,
        reason: detail.reason ?? null,
      },
      { priority: 'event' },
    );
  };

  const register = () => {
    for (const [what, handler] of Object.entries(handlers)) {
      window.Shiny.addCustomMessageHandler(names.messages[what], handler);
    }
  };

  window.HovercraftShiny = Object.freeze({
    start: (config) => {
      if (names) return;
      names = config;
      document.addEventListener(names.events.hint, sendHint);
      if (window.Shiny?.addCustomMessageHandler) register();
      else document.addEventListener('DOMContentLoaded', () => window.Shiny?.addCustomMessageHandler && register());
    },
  });
})();
