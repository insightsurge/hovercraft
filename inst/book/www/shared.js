// Shared helpers of the book's pages: the code note and its Copy button, the inputs log, the ticks of the test
// steps, the page colours, the page's look, and a figure drawn again. Everything from data is written with
// textContent, never as HTML. The server owns the truth (the code, the ticks); this file only draws it.
(() => {
  const $ = (s, r = document) => r.querySelector(s);
  const $$ = (s, r = document) => [...r.querySelectorAll(s)];
  const root = document.documentElement;
  const SVG = 'http://www.w3.org/2000/svg';
  // Inputs sent before Shiny has connected wait here: Shiny can take messages as soon as it loads, but inputs only
  // once it has started. The last value of each input is kept, and an event is sent as an event.
  let connected = false;
  const queued = new Map();
  const send = (name, value, event = false) => {
    if (connected) window.Shiny.setInputValue(name, value, event ? { priority: 'event' } : undefined);
    else queued.set(name, [value, event]);
  };
  // The engine's names (its global, its class prefix, its events, its id attribute), from the page's start data, which
  // the server read from the engine's page contract. Read when first needed: this script runs in the head.
  let engineNames;
  const names = () => {
    engineNames ??= JSON.parse($('#book-boot')?.textContent ?? '{}').names ?? {};
    return engineNames;
  };
  const engine = () => window[names().global];

  // A tick mark, drawn as nodes.
  const tickSvg = () => {
    const svg = document.createElementNS(SVG, 'svg');
    svg.setAttribute('viewBox', '0 0 16 16');
    svg.setAttribute('aria-hidden', 'true');
    const path = document.createElementNS(SVG, 'path');
    path.setAttribute('d', 'M3 8.6l3.1 3.1L13.2 4.4');
    path.setAttribute('fill', 'none');
    path.setAttribute('stroke', 'currentColor');
    path.setAttribute('stroke-width', '1.6');
    path.setAttribute('stroke-linecap', 'round');
    path.setAttribute('stroke-linejoin', 'round');
    svg.append(path);
    return svg;
  };

  /* ---- The code note: R code painted as spans, with a revision per plate ------------------------------------ */
  // Comments, strings and the names of called functions get a class; everything else is plain text.
  const TOKEN = /(#[^\n]*)|("(?:[^"\\]|\\.)*")|([A-Za-z_.][\w.]*(?:::[\w.$]+)?(?=\())/g;
  const paint = (pre, text) => {
    pre.replaceChildren();
    let at = 0;
    for (const m of text.matchAll(TOKEN)) {
      if (m.index > at) pre.append(text.slice(at, m.index));
      const span = document.createElement('span');
      span.className = m[1] ? 'c' : m[2] ? 's' : 'f';
      span.textContent = m[0];
      pre.append(span);
      at = m.index + m[0].length;
    }
    pre.append(text.slice(at));
    pre.dataset.raw = text;
  };
  const revs = {};
  const waiting = new Map(); // plate -> the Copy button waiting for that plate's newest code
  // A change of a plate's choices counts one more revision; the server sends code with the revision it answers.
  document.addEventListener('change', (e) => {
    // Only a choice the plate's code reads: the page colours in the Start sentence are not one.
    if (!e.target.closest?.('.plate .controls .shiny-input-radiogroup')) return;
    const plate = e.target.closest('.plate');
    revs[plate.id] = (revs[plate.id] ?? 0) + 1;
    send('book_rev', { ...revs });
  });

  const copyText = async (text) => {
    try {
      await navigator.clipboard.writeText(text);
      return true;
    } catch {
      // Over plain http a browser may refuse the clipboard: copy through a hidden text area instead.
      const area = Object.assign(document.createElement('textarea'), { value: text, readOnly: true });
      area.className = 'vh';
      document.body.append(area);
      area.select();
      let ok = false;
      try {
        ok = document.execCommand('copy');
      } catch {
        ok = false;
      }
      area.remove();
      return ok;
    }
  };
  const copyNow = async (button, pre) => {
    const ok = await copyText(pre.dataset.raw ?? pre.textContent);
    button.textContent = ok ? 'Copied' : 'Copy failed';
    button.dataset.done = '';
    button.removeAttribute('aria-busy');
    setTimeout(() => {
      button.textContent = 'Copy';
      delete button.dataset.done;
    }, 1600);
  };
  document.addEventListener(
    'click',
    (e) => {
      const button = e.target.closest?.('.js-copy');
      if (!button) return;
      const pre = button.closest('.note').querySelector('[data-code]');
      const plate = pre.dataset.code;
      // Code drawn for an older choice: wait for the server's answer to the newest one, for a few seconds at most.
      if (Number(pre.dataset.rev ?? 0) < (revs[plate] ?? 0)) {
        button.textContent = 'Updating…';
        button.setAttribute('aria-busy', 'true');
        waiting.set(plate, button);
        setTimeout(() => {
          if (waiting.get(plate) !== button) return;
          waiting.delete(plate);
          button.textContent = 'Copy failed, try again';
          button.removeAttribute('aria-busy');
          setTimeout(() => {
            button.textContent = 'Copy';
          }, 2400);
        }, 4000);
        return;
      }
      copyNow(button, pre);
      // In the capture phase: the margin stops its clicks from bubbling, so a pinned tip stays open.
    },
    true,
  );

  /* ---- The inputs log --------------------------------------------------------------------------------------- */
  let unseen = 0;
  const addLog = (msg) => {
    const list = $(`[data-log="${CSS.escape(msg.plate)}"]`);
    if (!list) return;
    $(`[data-empty="${CSS.escape(msg.plate)}"]`)?.setAttribute('hidden', '');
    const li = document.createElement('li');
    if (msg.kind === 'out') li.className = 'out';
    for (const [cls, text] of [
      ['t', msg.time],
      ['k', msg.name],
      ['v', msg.value],
    ]) {
      const span = document.createElement('span');
      span.className = cls;
      span.textContent = text;
      li.append(span);
    }
    list.prepend(li);
    requestAnimationFrame(() => li.classList.add('fresh'));
    while (list.children.length > 14) list.lastChild.remove();
    const count = $('#pull-cnt');
    if (count && !root.classList.contains('drawer-open')) {
      unseen += 1;
      count.textContent = String(unseen);
      count.hidden = false;
    }
  };
  document.addEventListener('book:drawer', (e) => {
    if (!e.detail.open) return;
    unseen = 0;
    const count = $('#pull-cnt');
    if (count) count.hidden = true;
  });

  /* ---- The ticks of the test steps: the server says which are done -------------------------------------------- */
  const drawTicks = (msg) => {
    const list = $(`[data-steps="${CSS.escape(msg.plate)}"]`);
    if (!list) return;
    const done = new Set(msg.done);
    const items = $$(':scope > li', list);
    items.forEach((li, i) => {
      const on = done.has(i);
      li.classList.toggle('g-done', on);
      const box = $('.g-tick', li);
      box.setAttribute('aria-pressed', String(on));
      box.replaceChildren(on ? tickSvg() : Object.assign(document.createElement('span'), { textContent: i + 1 }));
    });
    const count = $(`[data-count="${CSS.escape(msg.plate)}"]`);
    if (count) count.textContent = done.size ? `${done.size} of ${items.length} ticked` : '';
    const said = $('.g-said', list.closest('.note'));
    if (said && msg.said) said.textContent = msg.said;
    document.dispatchEvent(new CustomEvent('book:ticks', { detail: { plate: msg.plate, done: [...done] } }));
  };
  // A click on a step's box asks the server to tick or untick it.
  window.BookShared = {
    tickSvg,
    toggle: (plate, i, on) => send('book_tick', { plate, i, on, at: Date.now() }, true),
  };

  /* ---- Page colours: auto, light or dark, the same in every copy of the switch ------------------------------- */
  const dark = matchMedia('(prefers-color-scheme: dark)');
  let theme = 'auto';
  const setTheme = (value) => {
    theme = value;
    for (const input of $$('[data-sync="theme"] input')) input.checked = input.value === value;
    const mode = value === 'auto' ? (dark.matches ? 'dark' : 'light') : value;
    root.setAttribute('data-bs-theme', mode);
    root.dataset.bookTheme = value;
    send('page_mode', mode);
  };
  document.addEventListener('change', (e) => {
    if (e.target.closest?.('[data-sync="theme"]')) setTheme(e.target.value);
  });
  dark.addEventListener('change', () => theme === 'auto' && setTheme('auto'));

  /* ---- Which tip is pinned, and pinning it again --------------------------------------------------------------- */
  let pinned = null;
  const onPin = (e) => {
    const d = e.detail;
    if (d.state === 'pinned') pinned = d.id;
    if (d.state === 'closed' && pinned === d.id) pinned = null;
    const el = d.id ? elementOf(d.id) : null;
    el?.classList.toggle('is-pinned', d.state === 'pinned' || d.state === 'updated');
  };
  // The tag of a tip id.
  const elementOf = (id) => (names().id && $(`[${names().id}="${CSS.escape(id)}"]`)) ?? document.getElementById(id);
  // Close the pinned tip and pin it again, so it takes a new look. The engine sends both as by "host", and no test
  // step counts a host's pin.
  const repin = (id = pinned) => {
    const el = id ? elementOf(id) : null;
    if (!el || !engine()) return;
    engine().close();
    requestAnimationFrame(() => engine().pin(el));
  };

  // A figure drawn again by the server replaces its tags: the tip pinned in it is pinned again on the new tag, and the
  // focus goes back to the tag it was on, when that tag is still there.
  const before = new Map();
  document.addEventListener('shiny:outputinvalidated', (e) => {
    const out = e.target;
    if (!out?.id?.endsWith?.('_figure')) return;
    const active = document.activeElement;
    before.set(out.id, {
      pin: pinned && out.contains(elementOf(pinned)) ? pinned : null,
      focus: active && out.contains(active) ? active.id : null,
    });
  });
  document.addEventListener('shiny:value', (e) => {
    if (!e.name?.endsWith?.('_figure')) return;
    const was = before.get(e.name);
    before.delete(e.name);
    if (!was) return;
    setTimeout(() => {
      if (was.pin && elementOf(was.pin)) engine()?.pin(elementOf(was.pin));
      if (was.focus) document.getElementById(was.focus)?.focus({ preventScroll: true });
    }, 0);
  });

  // A click on a choice, in the margin or on the page's own navigation is not a click away from a pinned tip. Shiny
  // buttons never sit in these parts: Shiny listens for their clicks on the document.
  document.addEventListener('DOMContentLoaded', () => {
    for (const el of $$('.controls, .margin, .index, .bar, .toc-sheet, #pull, .cuebar')) {
      el.addEventListener('click', (e) => e.stopPropagation());
    }
    document.addEventListener(names().events.tip, onPin);
    const asked = new URLSearchParams(location.search).get('theme');
    setTheme(['auto', 'light', 'dark'].includes(asked) ? asked : 'auto');
  });

  // Messages: Shiny takes their handlers as soon as it loads.
  if (window.Shiny?.addCustomMessageHandler) {
    const Shiny = window.Shiny;
    Shiny.addCustomMessageHandler('book-code', (msg) => {
      const pre = $(`[data-code="${CSS.escape(msg.plate)}"]`);
      if (!pre) return;
      paint(pre, msg.code);
      pre.dataset.rev = String(msg.rev);
      const button = waiting.get(msg.plate);
      if (button && msg.rev >= (revs[msg.plate] ?? 0)) {
        waiting.delete(msg.plate);
        copyNow(button, pre);
      }
    });
    Shiny.addCustomMessageHandler('book-log', addLog);
    Shiny.addCustomMessageHandler('book-ticks', drawTicks);
    // The page's look: written the way the look's own operation writes it, with the names the server read from the
    // engine's page contract.
    Shiny.addCustomMessageHandler('book-look', (msg) => {
      root.setAttribute(msg.attribute, msg.preset);
      document.body.style.setProperty(msg.variable, msg.motion);
      repin();
    });
  }
  // Inputs: once Shiny has connected (an event Shiny sends through jQuery), the waiting ones go first, then a ready
  // signal that asks the server for the ticks it already has.
  window.jQuery?.(document).one('shiny:connected', () => {
    connected = true;
    for (const [name, [value, event]] of queued) send(name, value, event);
    queued.clear();
    send('book_ready', Date.now(), true);
  });
})();
