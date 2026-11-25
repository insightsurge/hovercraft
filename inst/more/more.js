// hovercraft's own layer for the long form of a tip, on top of the tip engine; it never changes the engine. A tip made
// with `tip_more()` (the default, `on = "button"`) holds its long form in attributes of its own; this file draws a
// "More" / "Less" control over the bottom of the tip (a button in the end corner, or with `as = "link"` a text link at
// the start), and gives the keyboard the M key. Opening the long form
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
  // What is open now: one button for the hint, and one for each pinned tip (the page can keep several pins), so a hint
  // that opens elsewhere leaves the pinned tips' buttons alone: { strip, button, box, item, kind, tipId, frame, linger,
  // timer, closing, written }. The pins are kept by their item.
  const slots = { hint: null };
  const pins = new Map();
  // The pin boxes that were closed a moment ago: the engine gives a box that is leaving to the next pin at once, so a
  // box that is shown and is nobody's is told from a new pin's by this.
  const leaving = new WeakMap();
  const LEAVING = 400;
  let live = null;
  // The item whose hint is on show now, held or not, or null: the M key acts on it, since it is the tip the reader sees.
  // It is cleared the moment the hint is told to close.
  let shown = null;
  const all = () => [slots.hint, ...pins.values()].filter(Boolean);
  const current = (o) => (o.kind === 'hint' ? slots.hint === o : pins.get(o.item) === o);

  const A = () => names.attributes;
  const hintBox = (doc) => doc.querySelector(`.${names.css}:not(.${names.css}-pin)`);
  // The box a pin just opened in: the one shown that no button of ours sits on. With several pins open it is the new one;
  // a box that was closed a moment ago and is still leaving is the last resort, since the engine reuses it at once.
  const shownPins = (doc) => [...doc.querySelectorAll(`.${names.css}.${names.css}-pin:not([hidden])`)];
  // The pin boxes that were on show at the last tip event, of any tip (also one this layer has no button for): a box that
  // is on show now and was not is the one a pin just opened in, wherever it sits in the page. The boxes are known by the
  // events, not by their place in the page, which says nothing of whose they are (a panel sits in a table, a floating box
  // at the end of the body).
  let knownPins = new WeakSet();
  const pinBox = (doc) => {
    const shown = shownPins(doc);
    const taken = new Set([...pins.values()].map((o) => o.box));
    const fresh = shown.filter((box) => !taken.has(box) && !knownPins.has(box));
    if (fresh.length === 1) return fresh[0];
    const free = shown.filter((box) => !taken.has(box));
    const settled = free.filter((box) => Date.now() - (leaving.get(box) ?? 0) > LEAVING);
    return (settled.length ? settled : free).at(-1) ?? null;
  };
  // Does the reader's key press land in a field where a letter is a letter? A box, a radio or a button takes no letters.
  // FIELD is the same line in inst/pin/pin.js and must stay identical (a test compares them).
  const FIELD =
    'textarea, select, [contenteditable]:not([contenteditable="false"]), [role="textbox"], [role="combobox"], [role="searchbox"], [role="spinbutton"], input:not([type="checkbox"]):not([type="radio"]):not([type="button"]):not([type="submit"]):not([type="reset"]):not([type="range"]):not([type="color"]):not([type="file"]):not([type="image"])';
  const editable = (el) => !!el?.closest?.(FIELD);

  // An item works this way when it holds a long form for the button.
  const isHeld = (item) =>
    item?.nodeType === 1 && (item.hasAttribute(A().held) || item.hasAttribute(A().held_template));
  const itemOf = (target) => {
    for (let n = target?.closest?.('*'); n; n = n.parentElement) if (isHeld(n)) return n;
    return null;
  };
  const expandedNow = (item) => item.getAttribute(A().state) === 'long';
  // `tip_more(on = "click")`: a click on the item opens the long form and the tip has no button. `tip_more(pin = TRUE)`:
  // the button, and the M key, pin the tip as well.
  const isClick = (item) => item.hasAttribute(A().click);
  const pinsToo = (item) => item.hasAttribute(A().pin_too);
  // How the control looks and where it sits (`tip_more(as =, where =)`): a button at the end of the bottom of the tip, or
  // a text link at its start. Both are a real <button>, so a keyboard and a screen reader meet the same control.
  const looksLikeLink = (item) => item.getAttribute(A().as) === 'link';
  const sitsAtStart = (item) => (item.getAttribute(A().where) ?? (looksLikeLink(item) ? 'start' : 'end')) === 'start';

  // The words of the two buttons: the item's own (`tip_more(label =)` in R) over the page's and the tag's `labels =` (the
  // nearest scope winning) over the built-in ones.
  const words = (item) => {
    const scoped = { ...names.labels, ...window.HovercraftRuntime?.labels?.(item) };
    try {
      return { ...scoped, ...JSON.parse(item.getAttribute(A().label) ?? '{}') };
    } catch {
      return scoped;
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
    for (const o of all()) if (o.item === item) label(o);
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

  // A pinned tip outlives its item when the item is drawn again with the same tip id (a table row, a renderUI() with the
  // same id): the engine keeps the pin on the new element and says nothing. The button follows it there, and the long
  // form it had goes with the old element, so the new one starts short.
  function alive(o) {
    if (o.item.isConnected) return true;
    const next =
      o.kind === 'pin' && o.tipId ? o.item.ownerDocument.querySelector(`[${A().id}="${CSS.escape(o.tipId)}"]`) : null;
    if (!isHeld(next) || pins.has(next)) return false;
    pins.delete(o.item);
    o.item = next;
    pins.set(next, o);
    setState(next, false);
    label(o);
    return true;
  }

  // The button follows the box's rectangle; the engine sends no event when a box moves, grows or fades.
  function place(o) {
    if (!current(o)) return;
    const { strip, box } = o;
    const view = box.ownerDocument.defaultView;
    const style = view.getComputedStyle(box);
    const faded = box.hidden || !box.isConnected || (o.closing && Number.parseFloat(style.opacity) < 0.02);
    // A table row that the widget gave to another row (its tip id changed) is not the tip this button was made for: the
    // long form it had goes with the old row, and the new one starts short.
    if (faded) return close(o);
    // A pinned tip that outlives its item (a table is drawing its rows again) is the engine's to close, and when it does
    // its box goes: the button waits for that and does not guess from the page at one moment of a redraw.
    if (!alive(o)) {
      if (o.kind === 'hint') return close(o);
      o.frame = view.requestAnimationFrame(() => place(o));
      return;
    }
    if (o.tipId !== o.item.getAttribute(A().id)) {
      setState(o.item, false);
      return close(o, true);
    }
    // A button in a panel under a table row is in the flow, after the box: it has nowhere to be placed.
    if (o.inPanel) {
      o.frame = view.requestAnimationFrame(() => place(o));
      return;
    }
    const r = box.getBoundingClientRect();
    const own = strip.getBoundingClientRect();
    const edge = Number.parseFloat(style.borderBottomWidth) || 0;
    const inset = 4;
    // The end is the right in a left-to-right tip and the left in a right-to-left one; the start is the other side.
    const onRight = (style.direction === 'rtl') === sitsAtStart(o.item);
    const left = onRight ? r.right - edge - inset - own.width : r.left + edge + inset;
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
    if (!o || !current(o)) return;
    const { strip, box, frame, item, kind } = o;
    box.ownerDocument.defaultView.cancelAnimationFrame(frame);
    clearTimeout(o.linger);
    clearTimeout(o.timer);
    box.removeAttribute(A().room);
    strip.remove();
    if (kind === 'hint') slots.hint = null;
    else {
      pins.delete(item);
      leaving.set(box, Date.now());
    }
    if (live) live.textContent = '';
    // The hint is gone: the next time it opens it is short again. A pinned tip keeps what it shows.
    if (!keep && kind === 'hint' && item.getAttribute(A().open) !== 'pin') setState(item, false);
  }

  // A tip whose item left the page (a renderUI() drew it again) is closed by the engine with an event sent at the
  // document, not at the item: whatever is open for an item that is gone goes.
  const sweep = () => {
    for (const o of all()) if (o.kind === 'hint' && !alive(o)) close(o, true);
  };

  // The box the reader is in, hint or pinned, for the item: the pinned tip's button when it is pinned.
  const slotFor = (item) => pins.get(item) ?? (slots.hint?.item === item ? slots.hint : null);

  // The engine draws the tip again at once; a moment later what it added to the box is the long form, which is what is
  // said, and the first block of it, in a pinned tip, starts with a hairline as the hover form does.
  function announce(o, long, before) {
    clearTimeout(o.timer);
    o.timer = setTimeout(() => {
      if (!current(o)) return;
      const added = fresh(o.box, before);
      if (long && o.kind === 'pin' && added[0]) added[0].classList.add('hc-long-first');
      say(o.item.ownerDocument, long ? added.map((n) => n.textContent.trim()).join(' ') : words(o.item).less);
    }, 60);
  }

  function toggle(item) {
    const o = slotFor(item);
    const long = !expandedNow(item);
    const before = o ? blocksOf(o.box).map(signature) : [];
    setState(item, long);
    if (o) announce(o, long, before);
    return before;
  }

  // What the button, the M key and (with `on = "click"`) a click do: open or close the long form; with `pin = TRUE` opening
  // it from a hint pins the tip too, so the long form stays. A pinned tip only grows and shrinks.
  function activate(item, via) {
    const opening = !expandedNow(item);
    const before = toggle(item);
    if (!opening || !pinsToo(item) || item.getAttribute(A().open) === 'pin') return;
    // The hint goes when the tip is pinned, and takes its announcement with it: the pinned tip says the long form.
    if (window.HovercraftPin?.pin(item, via) && pins.has(item)) announce(pins.get(item), true, before);
  }

  function start(item, box, kind) {
    if (!box || box.hidden) return;
    const doc = item.ownerDocument;
    close(kind === 'hint' ? slots.hint : pins.get(item));
    // A pinned tip drawn in a panel under a table row (`data-side="in"`) has its button after the box, in the flow, as a
    // sibling (a redraw of the tip rebuilds what is inside the box, and a button that floats would float off a scrolling
    // table); Escape on it closes its own tip, not the newest one.
    const inPanel = box.dataset.side === 'in';
    const strip = doc.createElement('div');
    // With `on = "click"` there is no button: the strip stays, hidden, so the tip's life cycle is the same.
    const click = isClick(item);
    strip.hidden = click;
    strip.className = [
      STRIP,
      inPanel && !click && `${STRIP}-in`,
      kind === 'hint' && `${STRIP}-hint`,
      looksLikeLink(item) && `${STRIP}-link`,
      sitsAtStart(item) && `${STRIP}-start`,
    ]
      .filter(Boolean)
      .join(' ');
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
      activate(o.item, e.detail === 0 ? 'key' : 'click');
    });
    strip.append(button);
    if (inPanel) {
      strip.addEventListener('keydown', (e) => {
        if (e.key !== 'Escape' || e.defaultPrevented) return;
        e.preventDefault();
        e.stopPropagation();
        window[names.global].close(o.item);
        o.item.focus?.({ preventScroll: true });
      });
    }
    // A tip in the top layer (a modal) needs the button there as well, or the tip would cover it.
    if (!inPanel && box.hasAttribute('popover') && typeof strip.showPopover === 'function')
      strip.setAttribute('popover', 'manual');
    if (inPanel) box.after(strip);
    else box.parentNode.append(strip);
    if (strip.hasAttribute('popover')) {
      try {
        strip.showPopover();
      } catch {
        strip.removeAttribute('popover');
      }
    }
    if (!inPanel && !click) box.setAttribute(A().room, '');
    const o = {
      strip,
      button,
      box,
      item,
      kind,
      inPanel,
      tipId: item.getAttribute(A().id),
      frame: 0,
      linger: 0,
      timer: 0,
    };
    o.closing = false;
    o.written = {};
    if (kind === 'hint') slots.hint = o;
    else pins.set(item, o);
    label(o);
    place(o);
  }

  // The element that really holds the focus, also inside an open shadow root, where the page's activeElement is the host.
  const deepFocus = (el) => {
    let at = el;
    while (at?.shadowRoot?.activeElement) at = at.shadowRoot.activeElement;
    return at;
  };

  const onHint = (event) => {
    sweep();
    if (shown && !shown.isConnected) shown = null;
    const detail = event.detail;
    const item = event.target;
    if (detail?.state === 'shown') shown = item;
    else if (detail?.state === 'hidden' && shown === item) shown = null;
    if (!isHeld(item)) {
      // A hint with no long form takes the hint box over: the earlier item's button must not stay on it, as the pin
      // button's does not.
      if (detail?.state === 'shown') close(slots.hint, true);
      return;
    }
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
    if (isHeld(item)) {
      if (detail?.state === 'pinned') {
        close(slots.hint, true);
        start(item, pinBox(item.ownerDocument), 'pin');
      } else if (detail?.state === 'closed') {
        // The pinned tip is gone: the next one starts short.
        close(pins.get(item), true);
        if (!item.getAttribute(A().open)) setState(item, false);
      }
    }
    knownPins = new WeakSet(shownPins(document));
  };

  // Before the engine opens a hint (it waits a moment), make sure the item starts short.
  const ready = (event) => {
    const item = itemOf(event.target);
    if (item && !item.getAttribute(A().open) && expandedNow(item)) setState(item, false);
  };

  // The M key opens and closes the long form of the tip that is open: of the pinned tip that holds the focus (also
  // the item of a pinned tip); else of the item whose tip is shown while the pointer is on it (the tip the reader
  // sees; a tip still fading after the pointer left, or one a tap or tip_show() opened, does not count, and with the
  // focus on another item the one under the pointer wins); else of the item that holds the focus. In a text field a
  // letter is a letter, so there the key does nothing; and a key held down toggles once.
  const onKey = (event) => {
    if (typeof event.key !== 'string') return;
    // The page can switch the letter key off (`use_hovercraft(keys = c(more = FALSE))`); the button stays.
    if (document.documentElement.getAttribute(A().keysOff)?.split(/\s+/).includes('more')) return;
    if (shown && !shown.isConnected) shown = null;
    const active = event.target?.ownerDocument?.activeElement ?? event.target;
    // The pinned tip that holds the focus (its box or its button), else the tagged item that has it.
    const held = [...pins.values()].find((o) => o.box.contains(active) || o.strip.contains(active));
    const focused = held ? held.item : itemOf(active);
    // The item under the pointer whose tip is shown, itself and never an ancestor of it: the key acts on the tip that is seen.
    const hovered = shown?.matches(':hover') ? shown : null;
    const hinted = isHeld(hovered) ? hovered : null;
    const item = held || focused?.getAttribute(A().open) === 'pin' ? focused : hovered ? hinted : focused;
    if (!item?.getAttribute(A().open)) return;
    if (event.ctrlKey || event.metaKey || event.altKey || event.isComposing || event.defaultPrevented) return;
    if (event.key.toLowerCase() !== names.key || editable(deepFocus(active))) return;
    event.preventDefault();
    if (!event.repeat) activate(item, 'key');
  };

  // `tip_more(on = "click")`: a click on the item, while its tip is open, opens or closes the long form. It never pins and
  // never takes a click that something else took (R refuses the tags whose click has its own job).
  // The item whose tip was open when the pointer went down: a click that opens the tip (the first tap on a touch screen,
  // which the engine turns into a hint in the click itself) only opens it, short.
  let openAtDown = null;
  const onDown = (event) => {
    const item = itemOf(event.target);
    openAtDown = item?.getAttribute(A().open) ? item : null;
  };
  // A control inside the item (a button, a link, a field ...) keeps its click: it is not the item's own.
  const INNER =
    'a[href], button, input, select, textarea, summary, label, [role="button"], [role="link"], [role="tab"], [role="checkbox"], [role="radio"], [role="switch"], [role="menuitem"], [role="menuitemcheckbox"], [role="menuitemradio"], [role="option"], [role="treeitem"], [contenteditable]:not([contenteditable="false"]), [tabindex], [onclick]';
  const onClick = (event) => {
    const item = itemOf(event.target);
    const opened = openAtDown === item;
    openAtDown = null;
    if (event.defaultPrevented || event.button !== 0) return;
    if (!item || !isClick(item) || !opened || !item.getAttribute(A().open)) return;
    const inner = event.target.closest?.(INNER);
    if (inner && inner !== item && item.contains(inner)) return;
    toggle(item);
  };

  // In a pinned tip the button is the last tab stop: Tab from the tip's last button or link goes to it, and Shift+Tab from
  // it goes back. The engine keeps Tab inside the pinned tip, so it is asked first.
  // The controls the tip's own Tab order goes through: buttons, links and fields (the engine manages their tabindex inside a
  // pinned tip, so it is not read for them), and anything else a page made focusable. Not a disabled one, and not one that
  // is not drawn (a control inside a hidden part, such as the Retry button of tip_output() while there is no error, has no
  // layout box and no reader reaches it).
  const STOP = 'button, a[href], input, select, textarea, [tabindex]:not([tabindex="-1"])';
  const stops = (box) => [...box.querySelectorAll(STOP)].filter((n) => !n.disabled && n.getClientRects().length > 0);
  const onTab = (event) => {
    if (event.key !== 'Tab' || event.defaultPrevented || event.isComposing) return;
    const active = event.target?.ownerDocument?.activeElement;
    const o = [...pins.values()].find((q) => q.box.contains(active) || active === q.button);
    // A tip opened by a click has no button to go to.
    if (!o || o.strip.hidden) return;
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
      document.addEventListener('pointerdown', onDown, true);
      document.addEventListener('click', onClick);
      document.addEventListener('keydown', onTab, true);
    },
  });
  const meta = document.querySelector('meta[name="hovercraft-more-config"]');
  if (meta) window.HovercraftMore.start(decode(meta.content));
})();
