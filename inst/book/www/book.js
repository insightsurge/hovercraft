// The book's own page: the index that follows the reader, the compact bar, plate sheet and margin drawer of a
// phone, and Checklist, an opt-in layer that shows one test step at a time with a leader to its target. Which steps are
// done comes from the server (shared.js draws the ticks); this file keeps only where the checklist is.
(() => {
  const $ = (s, r = document) => r.querySelector(s);
  const $$ = (s, r = document) => [...r.querySelectorAll(s)];
  const root = document.documentElement;
  const reduce = matchMedia('(prefers-reduced-motion: reduce)');
  const compact = matchMedia('(max-width: 1099px)');

  const start = () => {
    const boot = JSON.parse($('#book-boot')?.textContent ?? '{}');
    // The engine's names, which the server read from its page contract.
    const N = boot.names ?? { css: '', events: {} };
    const TIP = `.${N.css}-tip`;
    const plates = $$('.plate');
    if (!plates.length) return;
    const behaviour = () => (reduce.matches ? 'auto' : 'smooth');

    /* ---- Navigation: the index follows the reader; scroll position is the progress ------------------------- */
    let current = null;
    const track = () => {
      const y = innerHeight * 0.38;
      let at = plates[0];
      for (const p of plates) if (p.getBoundingClientRect().top <= y) at = p;
      const max = root.scrollHeight - innerHeight;
      root.style.setProperty('--progress', `${max > 0 ? Math.min(100, (scrollY / max) * 100) : 0}%`);
      if (at === current) return;
      current?.classList.remove('is-current');
      current = at;
      at.classList.add('is-current');
      for (const a of $$('.toc a')) a.setAttribute('aria-current', String(a.dataset.plate === at.id));
      // The index opens only the part being read; the others show their titles.
      for (const part of $$('.toc .part')) {
        part.classList.toggle('is-open', !!part.querySelector(`a[data-plate="${at.id}"]`));
      }
      // The index's list scrolls on a short screen: keep the current section in its view. Its own scroll only, so
      // the page never moves.
      const list = $('#toc');
      const link = list?.querySelector('a[aria-current="true"]');
      if (link && list.scrollHeight > list.clientHeight) {
        const top = link.offsetTop - list.offsetTop;
        if (top < list.scrollTop || top + link.offsetHeight > list.scrollTop + list.clientHeight) {
          list.scrollTop = top - list.clientHeight / 2;
        }
      }
      $('#where-n').textContent = at.dataset.n;
      $('#where-t').textContent = at.dataset.title;
      $('#pull').setAttribute('aria-label', `Margin of section ${at.dataset.n}, ${at.dataset.title}`);
      if (root.classList.contains('drawer-open')) syncInert();
    };

    // Compact layout: the index drops from the bar, the margin slides in from the right edge.
    const syncInert = () => {
      for (const m of $$('.margin')) {
        m.inert = compact.matches && !(root.classList.contains('drawer-open') && m.closest('.plate') === current);
      }
      $('#toc-sheet').inert = compact.matches && !root.classList.contains('toc-open');
    };
    const setToc = (open) => {
      root.classList.toggle('toc-open', open);
      $('#where').setAttribute('aria-expanded', String(open));
      syncInert();
    };
    const setDrawer = (open) => {
      root.classList.toggle('drawer-open', open);
      $('#pull').setAttribute('aria-expanded', String(open));
      if (open) setToc(false);
      syncInert();
      document.dispatchEvent(new CustomEvent('book:drawer', { detail: { open } }));
      if (open) $('.margin-head button', current)?.focus({ preventScroll: true });
      else if (compact.matches) $('#pull').focus({ preventScroll: true });
    };
    // The drawer also closes when the focus or a click goes outside it (a tip is not outside: it may sit over it).
    const outside = (el) => el && !el.closest?.(`.drawer-open .plate.is-current .margin, #pull, ${TIP}`);
    document.addEventListener('focusin', (e) => {
      if (root.classList.contains('drawer-open') && outside(e.target)) setDrawer(false);
    });
    document.addEventListener('pointerdown', (e) => {
      if (root.classList.contains('drawer-open') && outside(e.target)) setDrawer(false);
    });
    $('#pull').addEventListener('click', () => setDrawer(true));
    for (const b of $$('.js-close-drawer')) b.addEventListener('click', () => setDrawer(false));
    $('#where').addEventListener('click', () => setToc(!root.classList.contains('toc-open')));
    $('#toc-sheet').addEventListener('click', (e) => {
      if (e.target.closest('a')) setToc(false);
    });
    document.addEventListener('keydown', (e) => {
      if (e.key !== 'Escape' || e.defaultPrevented) return;
      if (root.classList.contains('drawer-open')) setDrawer(false);
      if (root.classList.contains('toc-open')) {
        setToc(false);
        $('#where').focus({ preventScroll: true });
      }
    });
    // Focus that leaves the plate sheet closes it.
    $('#toc-sheet').addEventListener('focusout', (e) => {
      if (!e.relatedTarget || !e.currentTarget.contains(e.relatedTarget)) {
        if (e.relatedTarget !== $('#where')) setToc(false);
      }
    });
    compact.addEventListener('change', () => {
      setDrawer(false);
      setToc(false);
    });

    /* ---- Checklist: one step at a time, ticked by the server from real inputs ---------------------------------- */
    const G = { on: false, last: null, pending: null, plates: {} };
    for (const p of plates) {
      G.plates[p.id] = {
        p,
        lis: $$('.tests > li', p),
        targets: boot.targets?.[p.id] ?? [],
        done: new Set(),
        at: 0,
        step: $('.g-step', p),
        said: $('.g-said', p),
      };
    }
    const cuebar = $('#cuebar');
    const leader = $('#g-leader');
    const all = () => Object.values(G.plates);
    const finished = (g) => g.lis.length > 0 && g.done.size === g.lis.length;
    const firstOpen = (g) =>
      Math.max(
        0,
        g.lis.findIndex((_, i) => !g.done.has(i)),
      );
    const nextOpen = (g, from) => {
      for (let k = 1; k <= g.lis.length; k++) {
        const i = (from + k) % g.lis.length;
        if (!g.done.has(i)) return i;
      }
      return from;
    };
    const stepText = (li) => {
      const span = li?.querySelector(':scope > span')?.cloneNode(true);
      span?.querySelector('.bug')?.remove();
      return span?.textContent.trim() ?? '';
    };
    const targetOf = (g) => (g.targets[g.at] ? $(g.targets[g.at]) : null);
    // The section the checklist is on: the one the reader is reading, or the one Next is scrolling to.
    const act = () =>
      G.plates[(G.pending && performance.now() - G.pending.t < 1600 ? G.pending.p : (current ?? plates[0])).id];

    const box = (button, g, i) => {
      button.setAttribute('aria-pressed', String(g.done.has(i)));
      button.replaceChildren(
        g.done.has(i)
          ? window.BookShared.tickSvg()
          : Object.assign(document.createElement('span'), { textContent: i + 1 }),
      );
    };
    const render = () => {
      // The plate in view now: a scroll's own event may not have arrived yet.
      track();
      root.classList.toggle('checklist-on', G.on);
      for (const b of $$('.js-checklist')) b.setAttribute('aria-pressed', String(G.on));
      for (const g of all()) {
        g.lis.forEach((li, i) => {
          li.classList.toggle('g-current', i === g.at);
        });
        if (g.step)
          g.step.textContent = finished(g) ? `All ${g.lis.length} ticked.` : `Step ${g.at + 1} of ${g.lis.length}`;
      }
      const a = act();
      const order = plates.map((p) => p.id);
      const from = order.indexOf(G.last ?? a.p.id);
      const nextId = order.map((_, j) => order[(from + j) % order.length]).find((id) => !finished(G.plates[id]));
      for (const link of $$('.toc a[data-plate]')) {
        const g = G.plates[link.dataset.plate];
        const mark = $('.g-mark', link);
        if (finished(g)) {
          mark.replaceChildren(
            window.BookShared.tickSvg(),
            Object.assign(document.createElement('span'), { className: 'vh', textContent: ', all steps ticked' }),
          );
        } else {
          mark.textContent = g.p.id === nextId ? 'next' : '';
        }
      }
      const r = G.plates[G.last ?? a.p.id];
      $('#g-resume').textContent =
        `Resume: section ${r.p.dataset.n}, ${r.p.dataset.title}, step ${r.at + 1} of ${r.lis.length}`;
      $('.w', cuebar).textContent =
        `Section ${a.p.dataset.n}, step ${a.at + 1} of ${a.lis.length}${finished(a) ? ', all ticked' : ''}`;
      $('.x', cuebar).textContent = stepText(a.lis[a.at]);
      box($('.g-tick', cuebar), a, a.at);
      const t = G.on ? targetOf(a) : null;
      for (const el of $$('.g-target')) if (el !== t) el.classList.remove('g-target', 'g-pulse');
      t?.classList.add('g-target');
      drawLeader();
      placeBar();
    };
    const pulse = () => {
      const t = G.on && targetOf(act());
      if (!t) return;
      t.classList.remove('g-pulse');
      void t.offsetWidth;
      t.classList.add('g-pulse');
    };
    const setChecklist = (on) => {
      G.on = on;
      render();
      pulse();
    };

    // The server's ticks: a step that ticks while it is the current one moves the checklist to the next open step.
    document.addEventListener('book:ticks', (e) => {
      const g = G.plates[e.detail.plate];
      if (!g) return;
      const was = new Set(g.done);
      g.done = new Set(e.detail.done);
      if (g.done.has(g.at) && !was.has(g.at)) g.at = nextOpen(g, g.at);
      G.last = g.p.id;
      render();
    });

    const move = (dir, byButton) => {
      const g = act();
      let to = g;
      if (dir > 0 && g.at < g.lis.length - 1) g.at += 1;
      else if (dir < 0 && g.at > 0) g.at -= 1;
      else {
        // Past the plate's end: on to the next plate, or back to the previous one's last step.
        const j = plates.indexOf(g.p) + dir;
        if (j < 0 || j >= plates.length) {
          if (g.said)
            g.said.textContent = dir > 0 ? 'That was the last step of the last section.' : 'This is the first step.';
          return;
        }
        to = G.plates[plates[j].id];
        to.at = dir > 0 ? firstOpen(to) : to.lis.length - 1;
        G.pending = { p: to.p, t: performance.now() };
        to.p.scrollIntoView({ behavior: behaviour(), block: 'start' });
      }
      if (g.said) g.said.textContent = '';
      G.last = to.p.id;
      render();
      pulse();
      if (to === g) {
        // Keep the target in sight when it is out of view.
        const t = targetOf(to);
        const b = t?.getBoundingClientRect();
        const foot = compact.matches ? cuebar.getBoundingClientRect().top : innerHeight;
        if (b && (b.bottom < 60 || b.top > foot - 20)) t.scrollIntoView({ behavior: behaviour(), block: 'center' });
      } else if (byButton && !compact.matches) {
        $(`.g-btn[data-g="${dir > 0 ? 'next' : 'back'}"]`, to.p)?.focus({ preventScroll: true });
      }
    };

    // Clicks: in the capture phase, because the margin stops its clicks from bubbling (so a pin stays open).
    document.addEventListener(
      'click',
      (e) => {
        const b = e.target.closest?.('[data-g], .js-checklist, #g-resume');
        if (!b) return;
        if (b.matches('.js-checklist')) return setChecklist(!G.on);
        if (b.id === 'g-resume') {
          G.plates[G.last ?? act().p.id].p.scrollIntoView({ behavior: behaviour() });
          return;
        }
        const g = b.closest('.plate') ? G.plates[b.closest('.plate').id] : act();
        const what = b.dataset.g;
        if (what === 'next' || what === 'back') return move(what === 'next' ? 1 : -1, true);
        if (what === 'open') return $('#pull').click();
        if (what === 'tick') {
          const i = b.dataset.i !== undefined ? Number(b.dataset.i) : g.at;
          window.BookShared.toggle(g.p.id, i, !g.done.has(i));
          if (g.said) g.said.textContent = '';
        }
      },
      true,
    );
    document.addEventListener('keydown', (e) => {
      if (e.defaultPrevented || e.altKey || e.ctrlKey || e.metaKey || e.isComposing) return;
      // The letters act only while the focus is on the page itself or on the checklist's own controls, so they never
      // take a key from a control, a tip, the code or a table.
      const where = e.target;
      if (where !== document.body && where !== root && !where.closest?.('.js-checklist, .g-head, .g-fold, .cuebar'))
        return;
      const k = e.key.length === 1 ? e.key.toLowerCase() : e.key;
      if (k === 'c') {
        e.preventDefault();
        setChecklist(!G.on);
      } else if (G.on && k === 'n') {
        e.preventDefault();
        move(1);
      } else if (G.on && k === 'b') {
        e.preventDefault();
        move(-1);
      }
    });

    // The leader: from the cue in the margin to its target in the figure, redrawn as the page scrolls.
    let frame = 0;
    const drawLeader = () => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        leader.setAttribute('hidden', '');
        if (!G.on || compact.matches) return;
        const g = act();
        const li = g.lis[g.at];
        const t = targetOf(g);
        if (!li || !t) return;
        const a = li.getBoundingClientRect();
        const b = t.getBoundingClientRect();
        const seen = (r) => r.height && r.bottom > 0 && r.top < innerHeight;
        if (!seen(a) || !seen(b)) return;
        // A margin taller than its box may scroll on its own: the step must be in sight inside it.
        const box = li.closest('.margin')?.getBoundingClientRect();
        if (box && (a.bottom < box.top || a.top > box.bottom)) return;
        const clamp = (v, lo, hi) => Math.min(Math.max(v, lo), Math.max(lo, hi));
        let pts;
        if (g.p.classList.contains('wide')) {
          // A wide plate: the notes sit under the figure, so the leader runs up to the target's lower edge.
          const x = a.left - 10;
          const end = [clamp(x, b.left + 8, b.right - 8), b.bottom + 8];
          pts = [[x, a.top + 11], [x, end[1] + 22], end];
        } else {
          const y = a.top + 11;
          const gutter = $('.margin', g.p).getBoundingClientRect().left - 40;
          const end =
            getComputedStyle(t).display === 'inline'
              ? [b.right + 6, b.top - 6]
              : [b.right + 8, clamp(y, b.top + 8, b.bottom - 8)];
          pts = [[a.left - 10, y], [gutter, y], end];
        }
        $('polyline', leader).setAttribute('points', pts.map((p) => p.join(',')).join(' '));
        const end = pts.at(-1);
        const c = $('circle', leader);
        c.setAttribute('cx', end[0]);
        c.setAttribute('cy', end[1]);
        leader.removeAttribute('hidden');
      });
    };

    // Phone: the cue bar sits just above a pinned tip's bottom sheet, never over it.
    const sizes = new ResizeObserver(() => placeBar());
    const watched = new WeakSet();
    const watchSheets = () => {
      for (const b of $$(TIP)) {
        if (!watched.has(b)) {
          watched.add(b);
          sizes.observe(b);
        }
      }
      requestAnimationFrame(placeBar);
    };
    function placeBar() {
      const sheet = $$(`${TIP}.${N.css}-tip-pin`).find((b) => !b.hidden && b.dataset.side === 'sheet');
      cuebar.style.bottom = sheet ? `${sheet.offsetHeight}px` : '0px';
    }
    for (const n of ['tip', 'hint']) if (N.events[n]) document.addEventListener(N.events[n], watchSheets);

    // The page's scroll, and a column's own scroll: scroll events do not bubble, so the listener is on the document
    // in the capture phase.
    document.addEventListener(
      'scroll',
      () => {
        track();
        if (G.on) drawLeader();
      },
      { capture: true, passive: true },
    );
    addEventListener('resize', () => {
      track();
      if (G.on) render();
    });

    // The switches' boxes: a tick that shows while a switch is on.
    for (const b of $$('.g-switch .box')) b.append(window.BookShared.tickSvg());

    /* ---- Search: the index keeps the sections whose words hold every word typed ------------------------------- */
    // Each section's words (number, title, part, the functions and arguments it shows) are in its link's
    // data-search, written by the server. The index and the phone's sheet each have a box; both show the same search.
    const search = (q) => {
      // Brackets are dropped as the server drops them from each section's words: tip_on(pin) finds tip_on pin.
      const words = q.toLowerCase().replace(/[()]/g, ' ').split(/\s+/).filter(Boolean);
      let any = false;
      for (const box of $$('.js-toc-search')) if (box.value !== q) box.value = q;
      for (const part of $$('.toc .part')) {
        let shown = 0;
        for (const a of part.querySelectorAll('a[data-search]')) {
          const ok = words.every((w) => a.dataset.search.includes(w));
          a.parentElement.hidden = !ok;
          if (ok) shown++;
        }
        part.hidden = shown === 0;
        if (shown) any = true;
      }
      // A live region that stays on the page and changes its words, so a screen reader says them.
      for (const none of $$('.toc-none')) none.textContent = any ? '' : 'Nothing matches.';
      // While searching, every part with a match shows its sections, not only the part being read.
      root.classList.toggle('toc-searching', words.length > 0);
    };
    for (const box of $$('.js-toc-search')) {
      box.addEventListener('input', () => search(box.value));
      box.addEventListener('keydown', (e) => {
        if (e.key === 'Escape' && box.value) {
          e.preventDefault();
          e.stopPropagation();
          search('');
        } else if (e.key === 'Enter') {
          // Enter opens the first section left in this box's own index.
          const first = [...box.closest('nav').querySelectorAll('.toc a[data-search]')].find(
            (a) => !a.parentElement.hidden && !a.closest('.part').hidden,
          );
          if (first) {
            e.preventDefault();
            first.click();
          }
        }
      });
    }

    /* ---- Start: the section, the view and the checklist the address asks for ---------------------------------- */
    track();
    syncInert();
    if (boot.section) {
      // An instant jump: the page's smooth scroll would be cut short while the server draws the figures above. They
      // change the heights above the section, so it is aligned again once Shiny is first idle, unless the reader has
      // scrolled by then.
      const go = () => {
        document.getElementById(boot.section)?.scrollIntoView({ behavior: 'instant' });
        track();
      };
      let moved = false;
      for (const type of ['wheel', 'keydown', 'touchstart', 'pointerdown']) {
        addEventListener(type, () => (moved = true), { once: true, passive: true });
      }
      go();
      window.jQuery?.(document).one('shiny:idle', () => moved || go());
    }
    if (boot.checklist) setChecklist(true);
    else render();
  };

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
