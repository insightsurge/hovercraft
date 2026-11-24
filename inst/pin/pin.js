// hovercraft's own layer for pins, on top of the tip engine; it never changes the engine. The engine pins an item that
// carries its pin attribute and draws the pinned tip with its own close button. A hint (the tip that shows on hover or
// focus) is read-only for a screen reader, so a pin button cannot live inside it. This file draws a small strip of
// buttons over the hint's top-right corner as a separate element, and gives the keyboard a pin key. It uses only the
// engine's public doors: the hint event, `pin(el, { by, via })`, the part mark (an element that carries it counts as
// part of the hint, so the pointer moving onto it does not close the hint, and moving off it closes the hint the way a
// pointer leaving the hint does) and the hint box's class. R gives the names as data in a <meta name="hovercraft-pin-config">, so none is typed here twice and nothing is inline.
(() => {
  if (window.HovercraftPin) return;

  let names = null;
  // A <meta>'s content is base64 of JSON (see tip_meta_value() in R/runtime.R).
  const decode = (content) =>
    JSON.parse(new TextDecoder().decode(Uint8Array.from(atob(content), (c) => c.charCodeAt(0))));
  const STRIP = 'hovercraft-pin-strip';
  // How long a strip stays after its hint was told to close, so it fades with the hint and never outlives it.
  const LINGER = 600;
  // What is open now: the strip, the hint it sits on, the item the hint is for, the frame loop that follows the hint, and
  // what was last written to the strip.
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
    // A pointer and a tap use this button; the keyboard uses the pin key, which the tagged item announces. The button has
    // a name for a pointer user's screen reader and no `title`, which would stack a second tooltip on the tip.
    pin.setAttribute('aria-label', names.labels.pin);
    pin.tabIndex = -1;
    strip.append(pin);
    // A hint in the top layer (a modal) needs the strip there as well, or the hint would cover it.
    if (hint.hasAttribute('popover') && typeof strip.showPopover === 'function')
      strip.setAttribute('popover', 'manual');
    return { strip, pin };
  };

  // Writes a style only when it differs from what was written last, so a still hint costs no style invalidation.
  const put = (key, value) => {
    if (open.written[key] === value) return;
    open.written[key] = value;
    open.strip.style[key] = value;
  };

  // The strip follows the hint's rectangle and appearance. The engine sends no event when a hint moves, grows, fades or is
  // clamped to the window, so they are read on animation frames, only while a hint with a strip is open.
  const place = () => {
    if (!open) return;
    const { strip, hint } = open;
    const view = hint.ownerDocument.defaultView;
    const style = view.getComputedStyle(hint);
    const faded = hint.hidden || !hint.isConnected || (open.closing && parseFloat(style.opacity) < 0.02);
    if (faded) return close();
    const box = hint.getBoundingClientRect();
    const own = strip.getBoundingClientRect();
    const edge = parseFloat(style.borderTopWidth) || 0;
    const inset = 2;
    const left = style.direction === 'rtl' ? box.left + edge + inset : box.right - edge - inset - own.width;
    put('left', `${Math.round(left)}px`);
    put('top', `${Math.round(box.top + edge + inset)}px`);
    put('color', style.color);
    // The strip is the hint's corner: it fades and slides with the hint, and never shows before or after it.
    put('opacity', style.opacity);
    open.frame = view.requestAnimationFrame(place);
  };

  function close() {
    if (!open) return;
    const { strip, hint, frame } = open;
    hint.ownerDocument.defaultView.cancelAnimationFrame(frame);
    clearTimeout(open.linger);
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
    hint.parentNode.append(strip);
    if (strip.hasAttribute('popover')) {
      try {
        strip.showPopover();
      } catch {
        strip.removeAttribute('popover');
      }
    }
    // Room for the strip in the first block of the hint, so the words never run under it.
    hint.setAttribute('data-hovercraft-pin-room', '');
    open = { strip, hint, item, frame: 0, linger: 0, closing: false, written: {} };
    place();
  };

  // The hint event is sent at the item and bubbles: a hint that opens for a pinnable item gets the strip, and one that
  // closes lets the strip go once the hint has faded.
  const onHint = (event) => {
    const detail = event.detail;
    const item = event.target?.closest?.(`[${names.attributes.pin}]`);
    if (detail?.state === 'shown' && item && event.target === item) show(item);
    else if (detail?.state === 'hidden' && open) {
      open.closing = true;
      open.strip.style.pointerEvents = 'none';
      clearTimeout(open.linger);
      open.linger = setTimeout(close, LINGER);
    }
  };

  // The pin key: a single letter, so only while the focus is on the tagged item itself (or inside it), never anywhere
  // else on the page (WCAG 2.1.4). A key event with no key (the browser's autofill sends them) is none of ours.
  const onKey = (event) => {
    if (typeof event.key !== 'string' || event.key.toLowerCase() !== names.key) return;
    if (event.ctrlKey || event.metaKey || event.altKey || event.isComposing || event.defaultPrevented) return;
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
  const meta = document.querySelector('meta[name="hovercraft-pin-config"]');
  if (meta) window.HovercraftPin.start(decode(meta.content));
})();
