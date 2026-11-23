// hovercraft's own layer for Shiny, on top of the tip engine; it never changes the engine. It answers the server calls
// tip_show(), tip_hide() and tip_update() (one custom message each), sends the hint input for the tags that asked for
// it with `events =`, and registers the "output" block for tip_output() through the engine's public block() door.
// R passes the names in `start()`, so none is typed here twice.
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
      el?.getAttribute(names.attributes.events).split(/\s+/).includes('hint') &&
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

  // ---- tip_output(): live Shiny outputs in tips -------------------------------------------------------------------
  const LOADER_WRAPPER =
    '.shiny-spinner-output-container, [data-spinner-id], .waiter-local-container, [data-waiter-wrapper], .bones-container, .bones-wrapper, [data-bones], [data-bones-id]';
  const LOADER_INSIDE = '.waiter-overlay, .bones-skeleton, .load-container, .shiny-spinner-placeholder';

  // Clean unrendered shell clones keyed by output id (for keep_alive = FALSE rebuilds).
  const shells = new Map();
  // Runtime state per output id: { state: 'idle'|'loading'|'ready'|'error'|'empty', message, retries, slot }.
  const outputs = new Map();
  // Output ids queued to park before DOMContentLoaded.
  const pendingPark = new Set();

  const outputRecord = (id) => {
    if (!outputs.has(id)) {
      outputs.set(id, { state: 'idle', message: '', retries: 0, slot: null });
    }
    return outputs.get(id);
  };

  const getPark = (doc) => {
    const cls = names?.parkClass ?? 'hc-output-park';
    let holder = doc.body?.querySelector(`:scope > .${cls}`);
    if (!holder && doc.body) {
      holder = doc.createElement('div');
      holder.className = cls;
      holder.hidden = true;
      holder.setAttribute('aria-hidden', 'true');
      holder.setAttribute('data-hovercraft-keep', '');
      doc.body.append(holder);
    }
    return holder;
  };

  const findOutputEl = (doc, id) => doc.getElementById(id);

  // Returns the loader wrapper around `outputEl` if it sits inside one, else `outputEl` itself.
  const wrapperOf = (outputEl) => {
    if (!outputEl) return null;
    const wrapped = outputEl.closest(LOADER_WRAPPER);
    if (wrapped && wrapped !== outputEl.ownerDocument.body && wrapped !== outputEl.ownerDocument.documentElement) {
      return wrapped;
    }
    return outputEl;
  };

  const hasExternalLoader = (wrapper, outputEl) => {
    if (!wrapper || !outputEl) return false;
    if (wrapper !== outputEl) return true;
    if (outputEl.matches(LOADER_WRAPPER) || outputEl.querySelector(LOADER_INSIDE)) return true;
    return Boolean(outputEl.parentElement?.querySelector?.('.waiter-overlay, .bones-skeleton'));
  };

  // Clones a clean unrendered copy of `wrapper` so keep_alive = FALSE can rebuild the output on the next open.
  const captureShell = (id, wrapper) => {
    if (shells.has(id) || !wrapper) return;
    const clone = wrapper.cloneNode(true);
    const inner = clone.id === id ? clone : clone.querySelector(`[id="${CSS.escape(id)}"]`);
    if (inner) {
      inner.innerHTML = '';
      inner.classList.remove(
        'shiny-bound-output',
        'recalculating',
        'shiny-output-error',
        'shiny-output-error-validation',
      );
      inner.style.removeProperty('visibility');
    }
    for (const loader of clone.querySelectorAll('.load-container, .shiny-spinner-placeholder')) {
      loader.classList.add('shiny-spinner-hidden');
    }
    shells.set(id, clone);
  };

  const domLooksEmpty = (outputEl) => {
    if (!outputEl) return true;
    if (outputEl.querySelector('img, svg, canvas, iframe, video, table, .datatables, .ReactTable, .gt_table')) {
      return false;
    }
    if (outputEl.querySelector('.shiny-output-error-validation')) return false;
    const text = (outputEl.textContent ?? '').trim();
    return text === '';
  };

  const valueLooksEmpty = (val) => {
    if (val === null || val === undefined || val === '') return true;
    if (typeof val === 'object' && 'html' in val) {
      return String(val.html ?? '').trim() === '' && !(Array.isArray(val.deps) && val.deps.length > 0);
    }
    return false;
  };

  const applySlotState = (id) => {
    const rec = outputs.get(id);
    const slot = rec?.slot;
    if (!slot?.isConnected) return;
    const loadingEl = slot.querySelector('.hc-output-loading');
    const errorEl = slot.querySelector('.hc-output-error');
    const errorMsg = slot.querySelector('.hc-output-error-msg');
    const emptyEl = slot.querySelector('.hc-output-empty');
    slot.dataset.state = rec.state;
    if (loadingEl) loadingEl.hidden = rec.state !== 'loading';
    if (errorEl) errorEl.hidden = rec.state !== 'error';
    if (errorMsg && rec.state === 'error') errorMsg.textContent = rec.message || 'Could not load output.';
    if (emptyEl) emptyEl.hidden = rec.state !== 'empty';
  };

  // Moves an output placed in the page UI into the hidden parking holder so it is out of sight until a tip opens.
  const parkId = (id, doc = document) => {
    if (!doc.body) {
      pendingPark.add(id);
      return;
    }
    const outputEl = findOutputEl(doc, id);
    if (!outputEl) return;
    const wrapper = wrapperOf(outputEl);
    captureShell(id, wrapper);
    const holder = getPark(doc);
    if (holder && !wrapper.closest('.hc-output') && wrapper.parentElement !== holder) {
      holder.append(wrapper);
    }
  };

  // Notifies Shiny and htmlwidgets that a moved output is now visible and sized inside the tip.
  const refreshOutput = (hostEl, outputEl, id) => {
    const view = hostEl.ownerDocument.defaultView;
    const shiny = view?.Shiny ?? window.Shiny;
    const jq = view?.jQuery ?? window.jQuery;
    if (shiny?.bindAll) shiny.bindAll(hostEl);
    if (jq && outputEl) {
      jq(outputEl).trigger('shown');
      jq(outputEl).trigger('resize');
    }
    // Wait a tick (timeout, not rAF: rAF stalls on background pages) so Shiny flushes visibility and widgets resize.
    setTimeout(() => {
      if (!hostEl.isConnected) return;
      if (shiny?.bindAll) shiny.bindAll(hostEl);
      if (jq && outputEl) {
        jq(outputEl).trigger('shown');
        jq(outputEl).trigger('resize');
      }
      view?.dispatchEvent?.(new Event('resize'));
      // If Shiny still thinks the output is hidden (e.g., uiOutput whose IntersectionObserver watched the old parent),
      // invoke Shiny's stored observer callback if present or re-notify via resize.
      const hiddenKey = `.clientdata_output_${id}_hidden`;
      if (shiny?.shinyapp?.$inputValues?.[hiddenKey] === true && jq && outputEl) {
        const cb =
          jq(outputEl).data('shiny-intersection-observer-callback') ??
          jq(outputEl).data('shiny-resize-observer-callback');
        if (typeof cb === 'function') cb();
      }
    }, 30);
  };

  // Re-triggers a failed Shiny output when the user clicks Retry.
  const retryOutput = (id, hostEl) => {
    const rec = outputRecord(id);
    rec.retries += 1;
    rec.state = 'loading';
    rec.message = '';
    applySlotState(id);
    const view = hostEl.ownerDocument.defaultView;
    const shiny = view?.Shiny ?? window.Shiny;
    if (shiny?.shinyapp?.$errors) delete shiny.shinyapp.$errors[id];
    if (shiny?.shinyapp?.$values) delete shiny.shinyapp.$values[id];
    if (shiny?.setInputValue) {
      shiny.setInputValue(`${id}_retry`, rec.retries, { priority: 'event' });
    }
    hostEl.dispatchEvent(
      new CustomEvent('hovercraft:retry', { bubbles: true, detail: { output: id, count: rec.retries } }),
    );
    const outputEl = findOutputEl(hostEl.ownerDocument, id);
    if (outputEl && shiny?.unbindAll && shiny?.bindAll) {
      shiny.unbindAll(hostEl);
      shiny.bindAll(hostEl);
    }
    refreshOutput(hostEl, findOutputEl(hostEl.ownerDocument, id), id);
  };

  let shinyListenersAttached = false;
  const attachOutputListeners = () => {
    if (shinyListenersAttached || !window.jQuery) return;
    shinyListenersAttached = true;
    const $doc = window.jQuery(document);
    $doc.on('shiny:outputinvalidated', (event) => {
      const id = event.target?.id;
      if (!id || !outputs.has(id)) return;
      const rec = outputRecord(id);
      rec.state = 'loading';
      rec.message = '';
      applySlotState(id);
    });
    $doc.on('shiny:value', (event) => {
      const id = event.target?.id;
      if (!id || !outputs.has(id)) return;
      const rec = outputRecord(id);
      const valEmpty = valueLooksEmpty(event.value);
      rec.state = valEmpty ? 'empty' : 'ready';
      rec.message = '';
      applySlotState(id);
      // Confirm rendered DOM after Shiny writes the value (covers htmlwidgets / renderUI returning blank tags).
      setTimeout(() => {
        if (rec.state === 'loading' || rec.state === 'error') return;
        const el = findOutputEl(document, id);
        if (!el) return;
        const next = valEmpty && domLooksEmpty(el) ? 'empty' : domLooksEmpty(el) ? 'empty' : 'ready';
        if (rec.state !== next) {
          rec.state = next;
          applySlotState(id);
        }
      }, 20);
    });
    $doc.on('shiny:error', (event) => {
      const id = event.target?.id;
      if (!id || !outputs.has(id)) return;
      const rec = outputRecord(id);
      const err = event.error ?? {};
      const types = Array.isArray(err.type) ? err.type : err.type ? [err.type] : [];
      if (types.includes('shiny.silent.error') || (err.silent && !err.message)) {
        rec.state = 'empty';
        rec.message = '';
      } else if (types.includes('validation')) {
        rec.state = 'ready';
        rec.message = '';
      } else {
        rec.state = 'error';
        rec.message = err.message || 'Could not load output.';
      }
      applySlotState(id);
    });
  };

  const registerOutputBlock = () => {
    const engine = window[names.global];
    if (!engine?.block) return;
    engine.block('output', {
      shape: {
        output: { type: 'string', required: true },
        keep_alive: { type: 'boolean', required: false },
      },
      flatten: () => '',
      render: (b, _record, ctx) => {
        const id = typeof b.output === 'string' ? b.output : '';
        const keepAlive = b.keep_alive !== false;
        const doc = ctx.doc;
        const root = doc.createElement('div');
        root.className = 'hc-output';
        root.dataset.hcOutput = id;
        root.dataset.keepAlive = keepAlive ? 'true' : 'false';

        const hostEl = doc.createElement('div');
        hostEl.className = 'hc-output-host';

        const loadingEl = doc.createElement('div');
        loadingEl.className = 'hc-output-loading';
        loadingEl.setAttribute('role', 'status');
        loadingEl.setAttribute('aria-live', 'polite');
        loadingEl.hidden = true;
        const pulse = doc.createElement('span');
        pulse.className = 'hc-output-pulse';
        pulse.setAttribute('aria-hidden', 'true');
        const loadText = doc.createElement('span');
        loadText.textContent = 'Loading…';
        loadingEl.append(pulse, loadText);

        const errorEl = doc.createElement('div');
        errorEl.className = 'hc-output-error';
        errorEl.setAttribute('role', 'alert');
        errorEl.hidden = true;
        const errorMsg = doc.createElement('span');
        errorMsg.className = 'hc-output-error-msg';
        const retryBtn = doc.createElement('button');
        retryBtn.type = 'button';
        retryBtn.className = 'hc-output-retry';
        retryBtn.textContent = 'Retry';
        retryBtn.addEventListener('click', (e) => {
          e.stopPropagation();
          retryOutput(id, hostEl);
        });
        errorEl.append(errorMsg, retryBtn);

        const emptyEl = doc.createElement('div');
        emptyEl.className = 'hc-output-empty';
        emptyEl.textContent = 'Nothing to show.';
        emptyEl.hidden = true;

        root.append(hostEl, loadingEl, errorEl, emptyEl);

        const mount = () => {
          attachOutputListeners();
          const rec = outputRecord(id);
          rec.slot = root;
          let outputEl = findOutputEl(doc, id);
          let wrapper = wrapperOf(outputEl);
          if (!wrapper && shells.has(id)) {
            wrapper = shells.get(id).cloneNode(true);
            outputEl = wrapper.id === id ? wrapper : wrapper.querySelector(`[id="${CSS.escape(id)}"]`);
          }
          if (!wrapper || !outputEl) {
            rec.state = 'empty';
            applySlotState(id);
            return {
              destroy: () => {
                if (rec.slot === root) rec.slot = null;
              },
            };
          }
          captureShell(id, wrapper);
          const hasLoader = hasExternalLoader(wrapper, outputEl);
          root.dataset.hasLoader = hasLoader ? 'true' : 'false';
          hostEl.append(wrapper);

          // Determine initial display state without flashing loading over an already-rendered keep_alive output.
          const hasRendered = !domLooksEmpty(outputEl);
          const isRecalc = outputEl.classList.contains('recalculating');
          const hasErr = Boolean(outputEl.querySelector('.shiny-output-error:not(.shiny-output-error-validation)'));
          if (hasErr) {
            rec.state = 'error';
          } else if (isRecalc) {
            rec.state = 'loading';
          } else if (rec.state === 'idle') {
            rec.state = hasRendered ? 'ready' : 'loading';
          } else if (rec.state === 'ready' && !hasRendered && !keepAlive) {
            rec.state = 'loading';
          }
          applySlotState(id);

          refreshOutput(hostEl, outputEl, id);

          return {
            resize: () => {
              const jq = doc.defaultView?.jQuery ?? window.jQuery;
              if (jq && outputEl.isConnected) jq(outputEl).trigger('resize');
            },
            mode: () => {
              const jq = doc.defaultView?.jQuery ?? window.jQuery;
              if (jq) jq(doc).trigger('shiny:themechange');
            },
            destroy: (info) => {
              if (rec.slot === root) rec.slot = null;
              if (!wrapper.isConnected) return;
              const redraw = info?.reason === 'redraw';
              if (keepAlive || redraw) {
                const holder = getPark(doc);
                if (holder) holder.append(wrapper);
              } else {
                const shiny = doc.defaultView?.Shiny ?? window.Shiny;
                if (shiny?.unbindAll) shiny.unbindAll(wrapper, true);
                wrapper.remove();
                rec.state = 'idle';
                rec.message = '';
              }
            },
          };
        };

        return { node: root, mount };
      },
    });
  };

  const registerExtraBlocks = () => {
    const engine = window[names.global];
    if (!engine?.block) return;
    // Dynamic badge status: expands {{ placeholder }} in tone from the record and applies valid Bootstrap statuses.
    engine.block('badge_data', {
      shape: {
        text: { type: 'string', required: true },
        tone: { type: 'string', required: false },
      },
      flatten: (b, _record, ctx) => ctx.words(b.text),
      render: (b, _record, ctx) => {
        const badge = ctx.doc.createElement('span');
        badge.className = `${ctx.cls}-badge`;
        badge.textContent = ctx.words(b.text);
        const tone = typeof b.tone === 'string' ? ctx.words(b.tone).trim() : '';
        if (tone && Array.isArray(names.statuses) && names.statuses.includes(tone)) {
          badge.dataset.tone = tone;
        }
        return { node: badge };
      },
    });
    // Subtle hairline divider placed before a template's `foot` slot group.
    engine.block('slot_divider', {
      shape: {},
      flatten: () => '',
      render: (_b, _record, ctx) => {
        const hr = ctx.doc.createElement('hr');
        hr.className = 'hc-slot-divider';
        hr.setAttribute('aria-hidden', 'true');
        return { node: hr };
      },
    });
  };

  // ---- Smooth short-to-long growth when pinning a tip with `more =` ----------------------------------------------
  // Captures the short hint's bounding rect before it closes for a pin, then smoothly morphs the pinned long box from
  // the short rect to its final positioned rect without scaling text, clipping the arrow, or stealing keyboard focus.
  let lastHint = null;
  let activeGrowth = [];

  const cancelGrowth = () => {
    for (const anim of activeGrowth) anim.cancel();
    activeGrowth = [];
  };

  const reducedMotion = () => Boolean(window.matchMedia?.('(prefers-reduced-motion: reduce)').matches);

  const visibleHintBox = () => {
    const cls = names?.css ?? 'hc-tip';
    return document.querySelector(`.${cls}:not(.${cls}-pin):not([hidden])`);
  };

  const snapshotHint = (el, id) => {
    const box = visibleHintBox();
    if (!box) return;
    const rect = box.getBoundingClientRect();
    if (rect.width <= 0 || rect.height <= 0) return;
    lastHint = {
      id: id ?? el?.getAttribute?.(names.attributes.id) ?? el?.id ?? null,
      el: el ?? null,
      rect: { left: rect.left, top: rect.top, width: rect.width, height: rect.height },
      side: box.dataset.side ?? null,
      children: box.children.length,
      at: Date.now(),
    };
  };

  const trackHint = (event) => {
    const detail = event.detail;
    if (!detail) return;
    if (detail.state === 'shown') {
      snapshotHint(event.target, detail.id);
    } else if (detail.state === 'hidden' && detail.reason !== 'pinned') {
      if (lastHint && (!detail.id || lastHint.id === detail.id)) lastHint = null;
    }
  };

  const refreshHintSnapshot = () => {
    if (!lastHint) return;
    const box = visibleHintBox();
    if (!box) return;
    const rect = box.getBoundingClientRect();
    if (rect.width > 0 && rect.height > 0) {
      lastHint.rect = { left: rect.left, top: rect.top, width: rect.width, height: rect.height };
      lastHint.side = box.dataset.side ?? lastHint.side;
      lastHint.at = Date.now();
    }
  };

  const animatePinGrowth = (event) => {
    const detail = event.detail;
    if (!detail) return;
    if (detail.state === 'closed') {
      cancelGrowth();
      return;
    }
    if (detail.state !== 'pinned') return;
    const prev = lastHint;
    lastHint = null;
    if (reducedMotion()) return;
    const el = event.target?.nodeType === 1 ? event.target : find(detail.id);
    if (!el) return;
    const hasMore = el.hasAttribute(names.attributes.more) || el.hasAttribute(names.attributes.moreTemplate);
    if (!hasMore || !prev || Date.now() - prev.at > 5000) return;
    if (prev.id && detail.id && prev.id !== detail.id) return;

    // Run right after the engine's synchronous pinMotion.enter() and before the first browser paint.
    Promise.resolve().then(() => {
      if (reducedMotion()) return;
      const cls = names?.css ?? 'hc-tip';
      const pinBox = document.querySelector(`.${cls}.${cls}-pin:not([hidden])`);
      if (!pinBox || typeof pinBox.animate !== 'function') return;
      cancelGrowth();
      // Cancel any opacity-0 entrance animation from the engine so the already-visible tip does not flicker.
      if (typeof pinBox.getAnimations === 'function') {
        for (const a of pinBox.getAnimations()) a.cancel();
      }
      const easing = 'cubic-bezier(0.2, 0.8, 0.2, 1)';
      if (pinBox.dataset.side === 'sheet') {
        const anim = pinBox.animate(
          [
            { transform: 'translateY(16px)', opacity: 0.25 },
            { transform: 'translateY(0)', opacity: 1 },
          ],
          { duration: 180, easing },
        );
        activeGrowth.push(anim);
        anim.finished.then(
          () => {
            activeGrowth = activeGrowth.filter((a) => a !== anim);
          },
          () => {},
        );
        return;
      }
      const to = pinBox.getBoundingClientRect();
      const from = prev.rect;
      if (to.width <= 0 || to.height <= 0) return;
      if (Math.abs(to.height - from.height) <= 2 && Math.abs(to.width - from.width) <= 2) return;
      const dx = from.left - to.left;
      const dy = from.top - to.top;
      const boxAnim = pinBox.animate(
        [
          {
            width: `${from.width}px`,
            height: `${from.height}px`,
            transform: `translate(${dx}px, ${dy}px)`,
          },
          {
            width: `${to.width}px`,
            height: `${to.height}px`,
            transform: 'translate(0px, 0px)',
          },
        ],
        { duration: 180, easing },
      );
      activeGrowth.push(boxAnim);
      boxAnim.finished.then(
        () => {
          activeGrowth = activeGrowth.filter((a) => a !== boxAnim);
        },
        () => {},
      );
      // Gently reveal the newly added long-form blocks as the box opens.
      const kids = [...pinBox.children].filter(
        (c) => !c.classList.contains(`${cls}-head`) && !c.classList.contains(`${cls}-close`),
      );
      const startAt = Math.max(1, prev.children - 1);
      for (const child of kids.slice(startAt)) {
        if (typeof child.animate !== 'function') continue;
        const cAnim = child.animate(
          [
            { opacity: 0, transform: 'translateY(-3px)' },
            { opacity: 1, transform: 'translateY(0)' },
          ],
          { duration: 140, delay: 40, fill: 'backwards', easing: 'ease-out' },
        );
        activeGrowth.push(cAnim);
        cAnim.finished.then(
          () => {
            activeGrowth = activeGrowth.filter((a) => a !== cAnim);
          },
          () => {},
        );
      }
    });
  };

  const register = () => {
    for (const [what, handler] of Object.entries(handlers)) {
      window.Shiny.addCustomMessageHandler(names.messages[what], handler);
    }
    attachOutputListeners();
  };

  const flushPendingPark = () => {
    for (const id of pendingPark) parkId(id);
    pendingPark.clear();
  };

  window.HovercraftShiny = Object.freeze({
    start: (config) => {
      if (names) return;
      names = config;
      document.addEventListener(names.events.hint, sendHint);
      document.addEventListener(names.events.hint, trackHint);
      if (names.events.tip) document.addEventListener(names.events.tip, animatePinGrowth);
      document.addEventListener('pointerdown', refreshHintSnapshot, true);
      document.addEventListener(
        'keydown',
        (e) => {
          if (e.key === 'Enter' || e.key === ' ') refreshHintSnapshot();
        },
        true,
      );
      registerOutputBlock();
      registerExtraBlocks();
      if (window.Shiny?.addCustomMessageHandler) register();
      else {
        document.addEventListener('DOMContentLoaded', () => {
          flushPendingPark();
          if (window.Shiny?.addCustomMessageHandler) register();
        });
      }
      if (document.readyState !== 'loading') flushPendingPark();
    },
    park: (id) => {
      if (typeof id === 'string' && id) parkId(id);
    },
  });
})();
