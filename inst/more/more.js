// hovercraft's own layer for the long form of a tip, on top of the tip engine; it never changes the engine. A tip made
// with `tip_more()` (the default, `on = "button"`) holds its long form in attributes of its own; this file draws a
// "More" / "Less" button over the bottom-right corner of the tip, and gives the keyboard the M key. Opening the long form
// copies it onto the attributes the engine watches (`-more`, `-more-template`, `-more-on`), and the engine draws the open
// tip again in place: it keeps the tip open, on its side and at its edge (engine 0.58.0), and sends no event for a hint.
// A pinned tip drawn again sends `updated`. Like inst/pin/pin.js the button is a separate element that carries the part
// mark, because a hint is read-only for a screen reader; in a pinned tip it is a tab stop after the tip's own. R gives
// the names as data in a <meta name="hovercraft-more-config">.
(() => {
  if (window.HovercraftMore) return;

  let names = null;
  const decode = (content) =>
    JSON.parse(new TextDecoder().decode(Uint8Array.from(atob(content), (c) => c.charCodeAt(0))));
  const STRIP = 'hovercraft-more-strip';
  // How long a button stays after its hint was told to close, so it fades with the hint and never outlives it.
  const LINGER = 600;
  // What is open now, one button for the hint and one for the pinned tip, so a hint that opens elsewhere leaves the pinned
  // tip's button alone: { strip, button, box, item, kind, frame, linger, timer, closing, written }.
  const slots = { hint: null, pin: null };
  let live = null;

  const A = () => names.attributes;
  const hintBox = (doc) => doc.querySelector(`.${names.css}:not(.${names.css}-pin)`);
  const pinBox = (doc) => doc.querySelector(`.${names.css}.${names.css}-pin:not([hidden])`);
  // Does the reader's key press land in a field where a letter is a letter? A box, a radio or a button takes no letters.
  const NOT_TEXT = 'checkbox, radio, button, submit, reset, range, color, file, image';
  const editable = (el) =>
    !!el?.closest?.(
      `textarea, select, [contenteditable]:not([contenteditable="false"]), [role="textbox"], input:not(${NOT_TEXT.split(
        ', ',
      )
        .map((t) => `[type="${t}"]`)
        .join('):not(')})`,
    );

  // An item works this way when it holds a long form for the button.
  const isHeld = (item) =>
    item?.nodeType === 1 && (item.hasAttribute(A().held) || item.hasAttribute(A().held_template));
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

  // The blocks of a box that are not its chrome (head, close, cue, reason line), by what they say, so what the long form
  // adds can be told from what the short form had.
  const signature = (node) => `${node.tagName}|${node.className}|${node.textContent}`;
  const isChrome = (node) =>
    node.matches(`.${names.css}-head, .${names.css}-close, .${names.css}-cue, .${names.css}-reason`);
  const blocksOf = (box) => [...box.children].filter((c) => !isChrome(c));
  const fresh = (box, before) => {
    const left = [...before];
    return blocksOf(box).filter((child) => {
      const at = left.indexOf(signature(child));
      if (at >= 0) left.splice(at, 1);
      return at < 0;
    });
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
    for (const o of Object.values(slots)) if (o?.item === item) label(o);
  }

  function label(o) {
    const long = expandedNow(o.item);
    const text = words(o.item);
    o.button.textContent = long ? text.less : text.more;
    o.button.setAttribute('aria-expanded', String(long));
  }

  // What a screen reader hears: the hint is read-only, so the words go to a live region. The long form only, not the short
  // form again; "Less" when it goes.
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

  const put = (o, key, value) => {
    if (o.written[key] === value) return;
    o.written[key] = value;
    o.strip.style[key] = value;
  };

  // The button follows the box's rectangle; the engine sends no event when a box moves, grows or fades.
  function place(o) {
    if (slots[o.kind] !== o) return;
    const { strip, box } = o;
    const view = box.ownerDocument.defaultView;
    const style = view.getComputedStyle(box);
    const faded = box.hidden || !box.isConnected || (o.closing && Number.parseFloat(style.opacity) < 0.02);
    if (faded || !o.item.isConnected) return close(o);
    const r = box.getBoundingClientRect();
    const own = strip.getBoundingClientRect();
    const edge = Number.parseFloat(style.borderBottomWidth) || 0;
    const inset = 4;
    const left = style.direction === 'rtl' ? r.left + edge + inset : r.right - edge - inset - own.width;
    put(o, 'left', `${Math.round(left)}px`);
    put(o, 'top', `${Math.round(r.bottom - edge - inset - own.height)}px`);
    put(o, 'color', style.color);
    // The button is opaque in the tip's own colour, so it does not show the words that scroll under it.
    if (o.written.bg !== style.backgroundColor) {
      o.written.bg = style.backgroundColor;
      strip.style.setProperty('--hovercraft-more-bg', style.backgroundColor);
    }
    put(o, 'opacity', style.opacity);
    o.frame = view.requestAnimationFrame(() => place(o));
  }

  function close(o, keep = false) {
    if (!o || slots[o.kind] !== o) return;
    const { strip, box, frame, item, kind } = o;
    box.ownerDocument.defaultView.cancelAnimationFrame(frame);
    clearTimeout(o.linger);
    clearTimeout(o.timer);
    box.removeAttribute(A().room);
    strip.remove();
    slots[kind] = null;
    if (live) live.textContent = '';
    // The hint is gone: the next time it opens it is short again. A pinned tip keeps what it shows.
    if (!keep && kind === 'hint' && item.getAttribute(A().open) !== 'pin') setState(item, false);
  }

  // A tip whose item left the page (a renderUI() drew it again) is closed by the engine with an event sent at the
  // document, not at the item: whatever is open for an item that is gone goes.
  const sweep = () => {
    for (const o of Object.values(slots)) if (o && !o.item.isConnected) close(o, true);
  };

  // The box the reader is in, hint or pinned, for the item: the pinned tip's button when it is pinned.
  const slotFor = (item) => (slots.pin?.item === item ? slots.pin : slots.hint?.item === item ? slots.hint : null);

  function toggle(item) {
    const o = slotFor(item);
    const long = !expandedNow(item);
    const before = o ? blocksOf(o.box).map(signature) : [];
    setState(item, long);
    if (!o) return;
    clearTimeout(o.timer);
    // The engine draws the tip again at once; a moment later what it added is the long form, which is what is said, and
    // the first block of it, in a pinned tip, starts with a hairline as the hover form does.
    o.timer = setTimeout(() => {
      if (slots[o.kind] !== o) return;
      const added = fresh(o.box, before);
      if (long && o.kind === 'pin' && added[0]) added[0].classList.add('hc-long-first');
      say(item.ownerDocument, long ? added.map((n) => n.textContent.trim()).join(' ') : words(item).less);
    }, 60);
  }

  function start(item, box, kind) {
    if (!box || box.hidden) return;
    const doc = item.ownerDocument;
    close(slots[kind]);
    const strip = doc.createElement('div');
    strip.className = STRIP;
    strip.setAttribute(A().part, '');
    const button = doc.createElement('button');
    button.type = 'button';
    button.className = `${STRIP}-button`;
    // A hint is for a pointer and a tap; the keyboard uses the M key, which the tagged item announces. The pointer must
    // not move focus off the item (a text field keeps its caret). A pinned tip is a place the keyboard goes into, so its
    // button is a tab stop after the tip's own.
    button.tabIndex = kind === 'pin' ? 0 : -1;
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
    const o = { strip, button, box, item, kind, frame: 0, linger: 0, timer: 0, closing: false, written: {} };
    slots[kind] = o;
    label(o);
    place(o);
  }

  const onHint = (event) => {
    sweep();
    const detail = event.detail;
    const item = event.target;
    if (!isHeld(item)) return;
    if (detail?.state === 'shown') {
      // A hint that opens starts short, whatever the last one ended on.
      if (expandedNow(item) && !item.getAttribute(A().open)) setState(item, false);
      start(item, hintBox(item.ownerDocument), 'hint');
    } else if (detail?.state === 'hidden' && slots.hint?.item === item) {
      const o = slots.hint;
      // Pinned: the pinned tip keeps what the hint showed, so the long form stays.
      if (detail.reason === 'pinned') return close(o, true);
      o.closing = true;
      o.strip.style.pointerEvents = 'none';
      clearTimeout(o.linger);
      o.linger = setTimeout(() => close(o), LINGER);
    }
  };

  const onTip = (event) => {
    sweep();
    const detail = event.detail;
    const item = event.target;
    if (!isHeld(item)) return;
    if (detail?.state === 'pinned') {
      close(slots.hint, true);
      start(item, pinBox(item.ownerDocument), 'pin');
    } else if (detail?.state === 'closed') {
      // The pinned tip is gone: the next one starts short.
      if (slots.pin?.item === item) close(slots.pin, true);
      if (!item.getAttribute(A().open)) setState(item, false);
    }
  };

  // Before the engine opens a hint (it waits a moment), make sure the item starts short.
  const ready = (event) => {
    const item = itemOf(event.target);
    if (item && !item.getAttribute(A().open) && expandedNow(item)) setState(item, false);
  };

  // The M key, while the focus is on the item or inside its pinned tip and the tip is open: it opens and closes the long
  // form. In a text field a letter is a letter, so there the key does nothing; and a key held down toggles once.
  const onKey = (event) => {
    if (typeof event.key !== 'string') return;
    const active = event.target?.ownerDocument?.activeElement ?? event.target;
    const inTip = slots.pin?.box.contains(active) || slots.pin?.strip.contains(active);
    const item = inTip ? slots.pin.item : itemOf(active);
    if (!item?.getAttribute(A().open)) return;
    if (event.ctrlKey || event.metaKey || event.altKey || event.isComposing || event.defaultPrevented) return;
    if (event.key.toLowerCase() !== names.key || editable(active)) return;
    event.preventDefault();
    if (!event.repeat) toggle(item);
  };

  // In a pinned tip the button is the last tab stop: Tab from the tip's last button or link goes to it, and Shift+Tab from
  // it goes back. The engine keeps Tab inside the pinned tip, so it is asked first.
  const stops = (box) => [...box.querySelectorAll('button, a[href]')].filter((n) => !n.hidden && !n.disabled);
  const onTab = (event) => {
    const o = slots.pin;
    if (event.key !== 'Tab' || !o || event.defaultPrevented || event.isComposing) return;
    const active = event.target?.ownerDocument?.activeElement;
    if (!event.shiftKey && o.box.contains(active)) {
      const list = stops(o.box);
      if (list.length && list[list.length - 1] === active) {
        event.preventDefault();
        event.stopPropagation();
        o.button.focus();
      }
    } else if (event.shiftKey && active === o.button) {
      const list = stops(o.box);
      if (list.length) {
        event.preventDefault();
        event.stopPropagation();
        list[list.length - 1].focus();
      }
    }
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
      document.addEventListener('keydown', onTab, true);
    },
  });
  const meta = document.querySelector('meta[name="hovercraft-more-config"]');
  if (meta) window.HovercraftMore.start(decode(meta.content));
})();
