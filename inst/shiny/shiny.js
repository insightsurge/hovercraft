// hovercraft's own layer for Shiny, on top of the tip engine; it never changes the engine. It answers the server calls
// tip_show(), tip_hide() and tip_update() (one custom message each), and sends the hint input for the tags that asked
// for it with `events =`. It uses only the engine's public doors, `show()` and `hide()`, its public attributes and its
// hint event. R passes the names in `start()`, so none is typed here twice.
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
      window[names.global].show(el);
    },
    // The engine hides whichever hint is open, so only hide when this tag's own hint is the one open: the engine marks
    // an open tag with the kind of tip it shows, and a pinned tag carries the mark too (its pin is not closed here).
    hide: ({ id }) => {
      const el = find(id);
      if (!el) return note(`hide|${id}`, `tip_hide(): no tag with the tip id "${id}" is on the page.`);
      if (el.getAttribute(names.attributes.open) === names.states.hint) window[names.global].hide();
    },
    update: ({ id, set, remove }) => {
      const el = find(id);
      if (!el) return note(`update|${id}`, `tip_update(): no tag with the tip id "${id}" is on the page.`);
      for (const [attribute, value] of Object.entries(set ?? {})) el.setAttribute(attribute, value);
      for (const attribute of [remove ?? []].flat()) el.removeAttribute(attribute);
    },
  };

  // The ids whose hint is open and was asked for: a tag that renderUI() replaces is gone when the engine says its hint
  // closed (by "data"), and the event is then sent at the page, so the tag can no longer say it asked.
  const asked = new Set();

  // The hint event is sent at the tag and bubbles, so one listener on the page serves every tag. A tag sends the
  // input only when its list names it, and only for its own tip: an outer tag with a list does not send for an inner one.
  const sendHint = (event) => {
    const detail = event.detail;
    if (!detail?.id || !window.Shiny?.setInputValue) return;
    const el = event.target?.closest?.(`[${names.attributes.events}]`);
    const wants =
      el &&
      el.getAttribute(names.attributes.events).split(/\s+/).includes('hint') &&
      (el.getAttribute(names.attributes.id) ?? el.id) === detail.id;
    if (!wants && !(detail.state === 'hidden' && asked.has(detail.id))) return;
    if (detail.state === 'shown') asked.add(detail.id);
    else asked.delete(detail.id);
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
