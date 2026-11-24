// hovercraft's own layer for the long form of a tip, on top of the tip engine; it never changes the engine. A tip made
// with `tip_more()` (the default, `on = "button"`) holds its long form in attributes of its own; this file draws a
// "More" / "Less" button over the bottom-right corner of the tip, and gives the keyboard the M key. Opening the long form
// copies it onto the attributes the engine watches (`-more`, `-more-template`, `-more-on`), and the engine draws the open
// tip again in place: it keeps the tip open, on its side and at its edge (engine 0.58.0), and sends no event for a hint.
// A pinned tip drawn again sends `updated`. Like inst/pin/pin.js the button is a separate element that carries the part
// mark, because a hint is read-only for a screen reader. R gives the names as data in a <meta name="hovercraft-more-config">.
(() => {
  if (window.HovercraftMore) return;

  let names = null;
  const decode = (content) =>
    JSON.parse(new TextDecoder().decode(Uint8Array.from(atob(content), (c) => c.charCodeAt(0))));
  const STRIP = 'hovercraft-more-strip';
  // How long the button stays after its hint was told to close, so it fades with the hint and never outlives it.
  const LINGER = 600;
  // What is open now: the strip, the button, the box it sits on, the item, the kind of tip ("hint" or "pin"), the frame
  // loop that follows the box, and what was last written to the strip.
  let open = null;
  let live = null;

  const A = () => names.attributes;
  const hintBox = (doc) => doc.querySelector(`.${names.css}:not(.${names.css}-pin)`);
  const pinBox = (doc) => doc.querySelector(`.${names.css}.${names.css}-pin:not([hidden])`);
  // Does the reader's key press land in a field, where a letter is a letter?
  const editable = (el) =>
    !!el?.closest?.('input, textarea, select, [contenteditable]:not([contenteditable="false"]), [role="textbox"]');

  // An item works this way when it holds a long form for the button.
  const isHeld = (item) => item.hasAttribute(A().held) || item.hasAttribute(A().held_template);
  const itemOf = (target) => {
    for (let n = target?.closest?.('*'); n; n = n.parentElement) if (isHeld(n)) return n;
    return null;
  };
  const expandedNow = (item) => item.getAttribute(A().state) === 'long';

  // The words of the two buttons: the item's own (`label =` in R) over the built-in ones.
  const words = (item) => {
    try {
      return { ...names.labels, ...JSON.parse(item.getAttribute(A().label) ?? '{}') };
    } catch {
      return names.labels;
    }
  };

  // The two states are two sets of attributes the engine already watches, so it draws the open tip again by itself.
  function setState(item, long) {
    const a = A();
    if (long) {
      if (item.hasAttribute(a.held)) item.setAttribute(a.more, item.getAttribute(a.held));
      if (item.hasAttribute(a.held_template)) item.setAttribute(a.more_template, item.getAttribute(a.held_template));
      item.setAttribute(a.more_on, 'hover');
      item.setAttribute(a.state, 'long');
    } else {
      item.removeAttribute(a.more);
      item.removeAttribute(a.more_template);
      item.removeAttribute(a.more_on);
      item.removeAttribute(a.state);
    }
    if (open?.item === item) label();
  }

  function label() {
    const long = expandedNow(open.item);
    const text = words(open.item);
    open.button.textContent = long ? text.less : text.more;
    open.button.setAttribute('aria-expanded', String(long));
  }

  // The long form for a screen reader: the hint is read-only, so the words go to a live region.
  function say(doc, text) {
    if (!live?.isConnected) {
      live = doc.createElement('div');
      live.setAttribute('role', 'status');
      live.setAttribute('aria-live', 'polite');
      live.className = `${STRIP}-live`;
      doc.body.append(live);
    }
    live.textContent = text;
  }

  const put = (key, value) => {
    if (open.written[key] === value) return;
    open.written[key] = value;
    open.strip.style[key] = value;
  };

  // The button follows the box's rectangle; the engine sends no event when a box moves, grows or fades.
  function place() {
    if (!open) return;
    const { strip, box } = open;
    const view = box.ownerDocument.defaultView;
    const style = view.getComputedStyle(box);
    const faded = box.hidden || !box.isConnected || (open.closing && Number.parseFloat(style.opacity) < 0.02);
    if (faded) return close();
    const r = box.getBoundingClientRect();
    const own = strip.getBoundingClientRect();
    const edge = Number.parseFloat(style.borderBottomWidth) || 0;
    const inset = 4;
    const left = style.direction === 'rtl' ? r.left + edge + inset : r.right - edge - inset - own.width;
    put('left', `${Math.round(left)}px`);
    put('top', `${Math.round(r.bottom - edge - inset - own.height)}px`);
    put('color', style.color);
    put('opacity', style.opacity);
    open.frame = view.requestAnimationFrame(place);
  }

  function close(keep = false) {
    if (!open) return;
    const { strip, box, frame, item, kind } = open;
    box.ownerDocument.defaultView.cancelAnimationFrame(frame);
    clearTimeout(open.linger);
    box.removeAttribute(A().room);
    strip.remove();
    open = null;
    // The hint is gone: the next time it opens it is short again. A pinned tip keeps what it shows.
    if (!keep && kind === 'hint' && item.getAttribute(A().open) !== 'pin') setState(item, false);
  }

  function toggle(item) {
    const long = !expandedNow(item);
    setState(item, long);
    if (long) {
      const doc = item.ownerDocument;
      setTimeout(() => {
        const box = open?.box;
        if (box && open.item === item) say(doc, box.textContent.replace(open.button.textContent, '').trim());
      }, 60);
    } else if (live) {
      live.textContent = '';
    }
  }

  function start(item, box, kind) {
    if (!box || box.hidden) return;
    const doc = item.ownerDocument;
    close();
    const strip = doc.createElement('div');
    strip.className = STRIP;
    strip.setAttribute(A().part, '');
    const button = doc.createElement('button');
    button.type = 'button';
    button.className = `${STRIP}-button`;
    // The keyboard uses the M key, which the tagged item announces; the button is for a pointer and a tap, and the pointer
    // must not move focus off the item (a text field keeps its caret).
    button.tabIndex = -1;
    button.addEventListener('mousedown', (e) => e.preventDefault());
    button.addEventListener('click', (e) => {
      e.stopPropagation();
      toggle(item);
    });
    strip.append(button);
    // A tip in the top layer (a modal) needs the button there as well, or the tip would cover it.
    if (box.hasAttribute('popover') && typeof strip.showPopover === 'function') strip.setAttribute('popover', 'manual');
    box.parentNode.append(strip);
    if (strip.hasAttribute('popover')) {
      try {
        strip.showPopover();
      } catch {
        strip.removeAttribute('popover');
      }
    }
    box.setAttribute(A().room, '');
    open = { strip, button, box, item, kind, frame: 0, linger: 0, closing: false, written: {} };
    label();
    place();
  }

  const onHint = (event) => {
    const detail = event.detail;
    const item = event.target;
    if (!isHeld(item)) return;
    if (detail?.state === 'shown') {
      // A hint that opens starts short, whatever the last one ended on.
      if (expandedNow(item)) setState(item, false);
      start(item, hintBox(item.ownerDocument), 'hint');
    } else if (detail?.state === 'hidden' && open?.item === item && open.kind === 'hint') {
      // Pinned: the pinned tip keeps what the hint showed, so the long form stays.
      if (detail.reason === 'pinned') return close(true);
      open.closing = true;
      open.strip.style.pointerEvents = 'none';
      clearTimeout(open.linger);
      open.linger = setTimeout(close, LINGER);
    }
  };

  const onTip = (event) => {
    const detail = event.detail;
    const item = event.target;
    if (!isHeld(item)) return;
    if (detail?.state === 'pinned') {
      close(true);
      start(item, pinBox(item.ownerDocument), 'pin');
    } else if (detail?.state === 'closed') {
      // The pinned tip is gone: the next one starts short.
      if (open?.item === item && open.kind === 'pin') close(true);
      if (!item.getAttribute(A().open)) setState(item, false);
    }
  };

  // Before the engine opens a hint (it waits a moment), make sure the item starts short.
  const ready = (event) => {
    const item = itemOf(event.target);
    if (item && !item.getAttribute(A().open) && expandedNow(item)) setState(item, false);
  };

  // The M key, while the focus is on the item and the tip is open: it opens and closes the long form. In a text field a
  // letter is a letter, so there the key does nothing.
  const onKey = (event) => {
    if (typeof event.key !== 'string') return;
    const active = event.target?.ownerDocument?.activeElement ?? event.target;
    // The focus is on the item, or, in a pinned tip, inside the tip: the reader pinned it and tabbed in.
    const inTip = open?.kind === 'pin' && open.box.contains(active);
    const item = inTip ? open.item : itemOf(active);
    if (!item?.getAttribute(A().open)) return;
    if (event.ctrlKey || event.metaKey || event.altKey || event.isComposing || event.defaultPrevented) return;
    if (event.key.toLowerCase() !== names.key || editable(active)) return;
    toggle(item);
    event.preventDefault();
  };

  window.HovercraftMore = Object.freeze({
    start: (config) => {
      if (names) return;
      names = config;
      document.addEventListener(names.events.hint, onHint);
      document.addEventListener(names.events.tip, onTip);
      document.addEventListener('pointerover', ready, true);
      document.addEventListener('focusin', ready, true);
      document.addEventListener('keydown', onKey);
    },
  });
  const meta = document.querySelector('meta[name="hovercraft-more-config"]');
  if (meta) window.HovercraftMore.start(decode(meta.content));
})();
