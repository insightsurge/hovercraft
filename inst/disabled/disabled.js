// hovercraft's own layer for tip_on(when_disabled = ), on top of the tip engine; it never changes the engine. R puts the
// control in a wrapper that holds the reason in its own attribute. While the control is disabled, this file copies the
// reason into the engine's reason attribute (the engine then shows "Not available: ...", and describes the wrapper) and
// gives it the control's tip id and events (so tip_show() and the hint input name the control), and makes the wrapper a
// tab stop (a button's role for a button, a named group for a field), because a disabled control can take neither focus
// nor, under Bootstrap, the pointer.
// When the control is enabled it takes all of that away: the engine keeps saying "Not available", and blocks the
// control's clicks, for as long as its reason attribute stays. R gives the attribute names as data in a <meta name="hovercraft-disabled-config">, which this file reads when it loads, so nothing is inline.
(() => {
  if (window.HovercraftDisabled) return;

  let names = null;
  // A <meta>'s content is base64 of JSON (see tip_meta_value() in R/runtime.R).
  const decode = (content) =>
    JSON.parse(new TextDecoder().decode(Uint8Array.from(atob(content), (c) => c.charCodeAt(0))));
  let wrappers = '';
  const CONTROLS = 'button, input, select, textarea, fieldset';

  // A control that is a button when it is read: the wrapper stands in for it with the same role. Any other control (a text
  // field, a select, a checkbox, a fieldset) keeps its meaning only if the wrapper is no button: it is a named group.
  const AS_BUTTON = 'button, input[type="button"], input[type="submit"], input[type="reset"], input[type="image"]';

  // The control's own name: its aria-label, its label, the legend of a fieldset, or the label found in the wrapper.
  const nameOf = (wrapper, control) =>
    (
      control.getAttribute('aria-label') ||
      control.labels?.[0]?.textContent ||
      control.querySelector?.('legend')?.textContent ||
      wrapper.querySelector('label, legend')?.textContent ||
      ''
    )
      .replace(/\s+/g, ' ')
      .trim();

  // While the control is disabled, the wrapper holds the reason, answers to the control's id and events, and is a tab stop.
  // When the control is enabled it holds none of that. Focus follows the control: a control that is focused when it is
  // switched off (a button that disables itself) hands the focus to the wrapper, and back when it is switched on.
  // What layout was last applied to each wrapper, so a still wrapper is not written again.
  const laid = new WeakMap();

  const sync = (wrapper) => {
    // The control's layout (a width, a flex share) arrives as data and is set from script: a policy that allows no inline
    // style allows this, and would block a `style` attribute in the markup.
    const layout = wrapper.getAttribute(names.style);
    if (layout !== null && laid.get(wrapper) !== layout) {
      wrapper.style.cssText = layout;
      laid.set(wrapper, layout);
    }
    const control = wrapper.querySelector(CONTROLS);
    const reason = wrapper.getAttribute(names.own)?.trim();
    const active = wrapper.ownerDocument.activeElement;
    if (control?.matches(':disabled') && reason) {
      if (wrapper.getAttribute(names.reason) !== reason) wrapper.setAttribute(names.reason, reason);
      // The wrapper is the tip's item while the control is off, so it answers to the id and events of the tagged element:
      // the first thing inside it, where tip_on() wrote them (a Shiny input is a `div` round the control, and it is the
      // `div` that is tagged), else the control itself.
      const tagged = wrapper.firstElementChild;
      const id = tagged?.getAttribute(names.id) ?? control.getAttribute(names.id) ?? (control.id || null);
      const events = tagged?.getAttribute(names.events) ?? control.getAttribute(names.events);
      if (id !== null && wrapper.getAttribute(names.id) !== id) wrapper.setAttribute(names.id, id);
      if (events !== null && wrapper.getAttribute(names.events) !== events) wrapper.setAttribute(names.events, events);
      const stop = wrapper.getAttribute('tabindex') === '0';
      if (!stop) wrapper.setAttribute('tabindex', '0');
      const name = control.matches(AS_BUTTON) ? '' : nameOf(wrapper, control);
      const role = control.matches(AS_BUTTON) ? 'button' : name ? 'group' : null;
      if (role && wrapper.getAttribute('role') !== role) wrapper.setAttribute('role', role);
      if (!role) wrapper.removeAttribute('role');
      if (name && wrapper.getAttribute('aria-label') !== name) wrapper.setAttribute('aria-label', name);
      if (!name) wrapper.removeAttribute('aria-label');
      if (!stop && active === control) wrapper.focus();
    } else {
      for (const attribute of [names.reason, names.id, names.events, 'tabindex', 'role', 'aria-label'])
        wrapper.removeAttribute(attribute);
      if (active === wrapper && control && !control.matches(':disabled')) control.focus();
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
        attributeFilter: ['disabled', names.own, names.style],
      });
      // Wrappers already on the page, and those a parser adds before the first change.
      syncAround(document.documentElement);
      document.addEventListener('DOMContentLoaded', () => syncAround(document.documentElement));
    },
  });
  const meta = document.querySelector('meta[name="hovercraft-disabled-config"]');
  if (meta) window.HovercraftDisabled.start(decode(meta.content));
})();
