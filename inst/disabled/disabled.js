// hovercraft's own layer for tip_on(when_disabled = ), on top of the tip engine; it never changes the engine. R puts the
// control in a wrapper that holds the reason in its own attribute. While the control is disabled, this file copies the
// reason into the engine's reason attribute (the engine then shows "Not available: ...", and describes the wrapper) and
// gives it the control's tip id and events (so tip_show() and the hint input name the control), and makes the wrapper a
// tab stop, because a disabled control can take neither focus nor, under Bootstrap, the pointer.
// When the control is enabled it takes all of that away: the engine keeps saying "Not available", and blocks the
// control's clicks, for as long as its reason attribute stays. R passes the attribute names in `start()`.
(() => {
  if (window.HovercraftDisabled) return;

  let names = null;
  let wrappers = '';
  const CONTROLS = 'button, input, select, textarea, fieldset';

  // The wrapper is disabled-looking when the control it holds can be disabled and is (a disabled fieldset counts).
  const sync = (wrapper) => {
    const control = wrapper.querySelector(CONTROLS);
    const reason = wrapper.getAttribute(names.own);
    if (control?.matches(':disabled') && reason) {
      if (wrapper.getAttribute(names.reason) !== reason) wrapper.setAttribute(names.reason, reason);
      // The wrapper is the tip's item while the control is off, so it answers to the control's id and events.
      const id = control.getAttribute(names.id) ?? (control.id || null);
      const events = control.getAttribute(names.events);
      if (id !== null && wrapper.getAttribute(names.id) !== id) wrapper.setAttribute(names.id, id);
      if (events !== null && wrapper.getAttribute(names.events) !== events) wrapper.setAttribute(names.events, events);
      if (wrapper.getAttribute('tabindex') !== '0') wrapper.setAttribute('tabindex', '0');
      // A button that is switched off: the control inside is out of the accessibility tree and the wrapper stands in.
      if (wrapper.getAttribute('role') !== 'button') wrapper.setAttribute('role', 'button');
    } else {
      for (const attribute of [names.reason, names.id, names.events, 'tabindex', 'role'])
        wrapper.removeAttribute(attribute);
    }
  };

  // The wrappers a node is in or holds: a changed `disabled` on a fieldset reaches every control inside it.
  const syncAround = (node) => {
    if (node.nodeType !== 1) return;
    const outer = node.closest(wrappers);
    if (outer) sync(outer);
    for (const wrapper of node.querySelectorAll(wrappers)) sync(wrapper);
  };

  window.HovercraftDisabled = Object.freeze({
    start: (config) => {
      if (names) return;
      names = config;
      wrappers = `[${names.own}]`;
      const watch = new MutationObserver((records) => {
        for (const record of records) {
          if (record.type === 'attributes') syncAround(record.target);
          else for (const node of record.addedNodes) syncAround(node);
        }
      });
      watch.observe(document.documentElement, {
        subtree: true,
        childList: true,
        attributes: true,
        attributeFilter: ['disabled', names.own],
      });
      // Wrappers already on the page, and those a parser adds before the first change.
      syncAround(document.documentElement);
      document.addEventListener('DOMContentLoaded', () => syncAround(document.documentElement));
    },
  });
})();
