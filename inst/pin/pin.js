// hovercraft's own layer for pins, on top of the tip engine; it never changes the engine. The engine pins an item that
// carries its pin attribute and draws the pinned tip with its own close button. A hint (the tip that shows on hover or
// focus) is read-only for a screen reader, so a pin button cannot live inside it. This file draws a small strip of
// buttons over the hint's top-right corner as a separate element, and gives the keyboard a pin key. It uses only the
// engine's public doors: the hint event, `pin(el, { by, via })` and `hide()`, the part mark (an element that carries it
// counts as part of the hint, so the pointer moving onto it does not close the hint) and the hint box's class.
// R passes the names in `start()`, so none is typed here twice.
(() => {
  if (window.HovercraftPin) return;

  let names = null;
  const STRIP = 'hovercraft-pin-strip';
  // What is open now: the strip, the hint it sits on, the item the hint is for, and the frame loop that follows the hint.
  let open = null;

  const hintBox = (doc) => doc.querySelector(`.${names.css}:not(.${names.css}-pin)`);

  // Does the reader's key press land in a field, where a letter is a letter?
  const editable = (el) =>
    !!el?.closest?.('input, textarea, select, [contenteditable]:not([contenteditable="false"]), [role="textbox"]');

  const stripOf = (doc, hint) => {
    const strip = doc.createElement('div');
    strip.className = STRIP;
    strip.setAttribute(names.attributes.part, '');
    const pin = doc.createElement('button');
    pin.type = 'button';
    pin.className = `${STRIP}-button ${STRIP}-pin`;
    pin.setAttribute('aria-label', names.labels.pin);
    pin.setAttribute('aria-keyshortcuts', names.key.toUpperCase());
    pin.title = `${names.labels.pin} (${names.key.toUpperCase()})`;
    // The hint is the only tip that has this strip; a pointer and a tap use it, the keyboard uses the pin key.
    pin.tabIndex = -1;
    strip.append(pin);
    // A hint in the top layer (a modal) needs the strip there as well, or the hint would cover it.
    if (hint.hasAttribute('popover') && typeof strip.showPopover === 'function')
      strip.setAttribute('popover', 'manual');
    return { strip, pin };
  };

  // The strip follows the hint's rectangle. The engine sends no event when a hint moves, grows or is clamped to the
  // window, so the position is read on animation frames, only while a hint with a strip is open.
  const place = () => {
    if (!open) return;
    const { strip, hint } = open;
    if (hint.hidden || !hint.isConnected) return close();
    const box = hint.getBoundingClientRect();
    const own = strip.getBoundingClientRect();
    const view = hint.ownerDocument.defaultView;
    const style = view.getComputedStyle(hint);
    const edge = parseFloat(style.borderTopWidth) || 0;
    const inset = 2;
    const rtl = style.direction === 'rtl';
    const left = rtl ? box.left + edge + inset : box.right - edge - inset - own.width;
    const top = box.top + edge + inset;
    strip.style.left = `${Math.round(left)}px`;
    strip.style.top = `${Math.round(top)}px`;
    strip.style.color = style.color;
    open.frame = view.requestAnimationFrame(place);
  };

  function close() {
    if (!open) return;
    const { strip, hint, frame } = open;
    hint.ownerDocument.defaultView.cancelAnimationFrame(frame);
    clearTimeout(open.leave);
    hint.removeAttribute('data-hovercraft-pin-room');
    strip.remove();
    open = null;
  }

  const pinItem = (item, via) => window[names.global].pin(item, { by: 'reader', via });

  const show = (item) => {
    const doc = item.ownerDocument;
    const hint = hintBox(doc);
    if (!hint || hint.hidden || item.matches(':disabled, [aria-disabled="true"]')) return;
    close();
    const { strip, pin } = stripOf(doc, hint);
    // A click on the button pins the item without a click on the item: the item's own click (a row's selection) never runs.
    pin.addEventListener('click', (e) => {
      e.stopPropagation();
      pinItem(item, e.detail === 0 ? 'key' : 'click');
    });
    // The part mark keeps the hint open while the pointer is on the strip. A pointer that leaves the strip for the
    // page is the hint's leave, which the engine does not see, since the strip is not in the hint.
    strip.addEventListener('pointerleave', (e) => {
      const to = e.relatedTarget;
      if (to && (item.contains(to) || hint.contains(to) || strip.contains(to))) return;
      clearTimeout(open?.leave);
      if (open) open.leave = setTimeout(() => open?.strip === strip && window[names.global].hide(), 160);
    });
    strip.addEventListener('pointerenter', () => clearTimeout(open?.leave));
    const host = hint.parentNode;
    host.append(strip);
    if (strip.hasAttribute('popover')) {
      try {
        strip.showPopover();
      } catch {
        strip.removeAttribute('popover');
      }
    }
    // Room for the strip on the first line, so the words never run under it.
    hint.setAttribute('data-hovercraft-pin-room', '');
    open = { strip, hint, item, frame: 0, leave: 0 };
    place();
  };

  // The hint event is sent at the item and bubbles: a hint that opens for a pinnable item gets the strip, and one that
  // closes takes it away.
  const onHint = (event) => {
    const detail = event.detail;
    const item = event.target?.closest?.(`[${names.attributes.pin}]`);
    if (detail?.state === 'shown' && item && event.target === item) show(item);
    else if (detail?.state === 'hidden') close();
  };

  // The pin key: a single letter, so only while the focus is on the tagged item itself (or inside it), never anywhere
  // else on the page (WCAG 2.1.4).
  const onKey = (event) => {
    if (event.key.toLowerCase() !== names.key || event.ctrlKey || event.metaKey || event.altKey || event.isComposing)
      return;
    if (event.defaultPrevented) return;
    const active = event.target?.ownerDocument?.activeElement ?? event.target;
    const item = active?.closest?.(`[${names.attributes.pin}]`);
    if (!item || editable(active) || item.matches(':disabled, [aria-disabled="true"]')) return;
    if (pinItem(item, 'key')) event.preventDefault();
  };

  window.HovercraftPin = Object.freeze({
    start: (config) => {
      if (names) return;
      names = config;
      document.addEventListener(names.events.hint, onHint);
      document.addEventListener('keydown', onKey);
    },
  });
})();
