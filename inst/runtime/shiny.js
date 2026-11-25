// hovercraft's own layer for Shiny, on top of the tip engine; it never changes the engine. It answers the server calls
// tip_show(), tip_hide() and tip_update() (one custom message each), sends the hint, pin and copy inputs for the tags that
// asked for them with `events =`, and registers the "output" block for tip_output() through the engine's public block() door.
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
      // A control with `when_disabled` sits in a wrapper that answers to the control's tip id while the control is off, so
      // `find()` can give the wrapper. The new words, template and record belong to the tag inside it, which is the tip the
      // control shows once it is on; written on the wrapper they would make a tip of its own beside the reason. The
      // placement is the one value the wrapper holds too, since the reason takes its place while the control is off.
      const isWrapper = (node) => node?.hasAttribute(names.attributes.disabled);
      const wrapper = isWrapper(el) ? el : isWrapper(el.parentElement) ? el.parentElement : null;
      const tag = wrapper === el ? (el.firstElementChild ?? el) : el;
      const placement = names.attributes.placement;
      for (const [attribute, value] of Object.entries(set ?? {})) {
        tag.setAttribute(attribute, value);
        if (attribute === placement && wrapper) wrapper.setAttribute(attribute, value);
      }
      for (const attribute of [remove ?? []].flat()) {
        tag.removeAttribute(attribute);
        if (attribute === placement && wrapper) wrapper.removeAttribute(attribute);
      }
    },
  };

  // The tips whose hint or pin is open and was asked for, by tip id, with the input they send to and, for a table's tip,
  // its address: a tag that renderUI() replaces is gone when the engine says its tip closed (by "data"), and the event
  // is then sent at the page, so the tag can no longer say it asked or where it was.
  const asked = { hint: new Map(), pin: new Map() };
  // What each table has pinned now, by the table's id: the addresses, oldest first (a Map keeps the order it was set in).
  const pinnedBy = new Map();

  // Where a table's tip is, from the attribute table.js writes on the element: { table, target, row, column }, or null for
  // a tag of tip_on(). `row` is the 1-based data row (null for a header), `column` the data column's name (null for a row).
  const addressOf = (el) => {
    const raw = names.attributes.address ? el?.getAttribute(names.attributes.address) : null;
    if (!raw) return null;
    try {
      return JSON.parse(raw);
    } catch {
      return null;
    }
  };
  // What a table's input says about the tip it comes from, next to the event's own fields.
  const placeOf = (address, id) => ({
    target: address.target,
    row: address.row ?? null,
    column: address.column ?? null,
    id,
  });

  // `input$<id>_pinned`: what is pinned now. For a tag of tip_on() TRUE or FALSE; for a table a list of the addresses,
  // oldest first. Sent as a plain value, so a value equal to the last one does not fire an observer again, and in the same
  // tick as `_pin`, so Shiny sends both in one batch.
  const sendPinned = (detail, base, address) => {
    if (detail.state !== 'pinned' && detail.state !== 'closed') return;
    const opened = detail.state === 'pinned';
    if (!address) {
      window.Shiny.setInputValue(`${base}_pinned`, opened);
      return;
    }
    const list = pinnedBy.get(base) ?? new Map();
    if (opened) list.set(detail.id, placeOf(address, detail.id));
    else list.delete(detail.id);
    pinnedBy.set(base, list);
    // Shiny would flatten a plain array of objects, so the input has a type of its own (R reads it back as a list).
    window.Shiny.setInputValue(`${base}_pinned:${names.pinnedType}`, [...list.values()]);
  };

  // What each input holds, from the engine's event. The hint and the pin share the shape: what happened, to which words,
  // who did it and how, and why it closed. A copy says which field and code block, and whether the browser let it.
  const inputs = {
    hint: {
      suffix: '_hint',
      opens: 'shown',
      closes: 'hidden',
      value: (d) => ({
        state: d.state,
        text: d.text ?? null,
        by: d.by ?? null,
        via: d.via ?? null,
        reason: d.reason ?? null,
      }),
    },
    pin: {
      suffix: '_pin',
      opens: 'pinned',
      closes: 'closed',
      value: (d) => ({
        state: d.state,
        text: d.text ?? null,
        by: d.by ?? null,
        via: d.via ?? null,
        reason: d.reason ?? null,
      }),
    },
    copy: {
      suffix: '_copy',
      value: (d) => ({
        field: d.field ?? null,
        block: d.block ?? null,
        ok: d.ok === true,
        by: d.by ?? null,
        via: d.via ?? null,
      }),
    },
  };

  // The engine's events are sent at the tag and bubble, so one listener on the page serves every tag. A tag sends an
  // input only when its list names it, and only for its own tip: an outer tag with a list does not send for an inner
  // one. `kind` is "hint", "pin" or "copy", the words `events =` takes. A tag of tip_on() sends to `<its id>_<kind>`. A
  // table's row sends to `<the table's id>_<kind>`, one input per table, and the value says which tip it was.
  const sendInput = (kind) => (event) => {
    const detail = event.detail;
    const input = inputs[kind];
    if (!detail?.id || !window.Shiny?.setInputValue) return;
    const el = event.target?.closest?.(`[${names.attributes.events}]`);
    const wants =
      el?.getAttribute(names.attributes.events).split(/\s+/).includes(kind) &&
      (el.getAttribute(names.attributes.id) ?? el.id) === detail.id;
    const was = asked[kind]?.get(detail.id);
    const closing = !!input.closes && detail.state === input.closes && !!was;
    if (!wants && !closing) return;
    const address = wants ? addressOf(el) : was.address;
    const base = address ? address.table : detail.id;
    if (input.opens && detail.state === input.opens) asked[kind].set(detail.id, { base, address });
    else if (input.closes && detail.state === input.closes) asked[kind].delete(detail.id);
    if (kind === 'pin') sendPinned(detail, base, address);
    const value = address ? { ...input.value(detail), ...placeOf(address, detail.id) } : input.value(detail);
    // "event" priority: the same value twice in a row (a hint opened, closed, opened) must still reach the server.
    window.Shiny.setInputValue(`${base}${input.suffix}`, value, { priority: 'event' });
  };

  // Two tagged items with one tip id answer for each other: the events and the server calls find the first. A table checks
  // its own id when it is drawn (inst/table/table.js); this looks at the tags of tip_on(), once a page change is over.
  const checkIds = () => {
    const seen = new Set();
    const { id: idName, kind, disabled } = names.attributes;
    for (const el of document.querySelectorAll(`[${idName}]:not([${kind}^="table_"]):not([${disabled}])`)) {
      const id = el.getAttribute(idName);
      if (seen.has(id)) note(`id|${id}`, `two tagged items have the tip id "${id}". Give each its own id.`);
      seen.add(id);
    }
  };

  // ---- tip_output(): live Shiny outputs in tips -------------------------------------------------------------------
  const LOADER_WRAPPER =
    '.shiny-spinner-output-container, [data-spinner-id], .waiter-local-container, [data-waiter-wrapper], .bones-container, .bones-wrapper, [data-bones], [data-bones-id]';
  const LOADER_INSIDE = '.waiter-overlay, .bones-skeleton, .load-container, .shiny-spinner-placeholder';

  // Clean unrendered shell clones keyed by output id (for keep_alive = FALSE rebuilds).
  const shells = new Map();
  // Runtime state per output id: { state: 'idle'|'loading'|'ready'|'error'|'empty', message, retries, slot,
  // live, item, stale, wait }. `slot` is the tip box that holds the output now, `live` every open box that asked for it
  // (a tip may stay pinned while another opens), `item` the tagged element it was last shown for, `stale` whether what
  // it holds may belong to another item, and `wait` the timer for a server answer we are waiting for.
  const outputs = new Map();
  // Output ids whose tag is not in the page yet (a tag that comes later in the UI than the tip that names it).
  const pendingPark = new Set();

  // How long to wait for the server to answer a Retry, and to answer the opening of a tip for another item. An output
  // runs again only when something it reads changes, and the server may not read the Retry input or the open item.
  const RETRY_WAIT = 4000;
  const SWITCH_WAIT = 1500;
  const NO_ANSWER = 'The server did not run this output again. Try again.';

  const outputRecord = (id) => {
    if (!outputs.has(id)) {
      outputs.set(id, {
        state: 'idle',
        message: '',
        retries: 0,
        slot: null,
        live: [],
        item: null,
        stale: false,
        wait: null,
      });
    }
    return outputs.get(id);
  };

  const clearWait = (rec) => {
    if (rec.wait) clearTimeout(rec.wait.timer);
    rec.wait = null;
  };

  // Waits `ms` for the server to start (outputinvalidated) or finish (value, error) this output; once it has started,
  // it is waited for until it finishes. `onSilence` runs when nothing came.
  const awaitServer = (rec, ms, onSilence) => {
    clearWait(rec);
    const wait = { invalidated: false, timer: null };
    wait.timer = setTimeout(() => {
      if (rec.wait !== wait || wait.invalidated) return;
      rec.wait = null;
      onSilence();
    }, ms);
    rec.wait = wait;
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
    const movedEl = slot.querySelector('.hc-output-moved');
    slot.dataset.state = rec.state;
    slot.dataset.stale = rec.stale && rec.state === 'loading' ? 'true' : 'false';
    if (movedEl) movedEl.hidden = true;
    if (loadingEl) loadingEl.hidden = rec.state !== 'loading';
    if (errorEl) errorEl.hidden = rec.state !== 'error';
    if (errorMsg && rec.state === 'error') errorMsg.textContent = rec.message || 'Could not load output.';
    if (emptyEl) emptyEl.hidden = rec.state !== 'empty';
  };

  // Moves an output placed in the page UI into the hidden parking holder so it is out of sight until a tip opens.
  const parkId = (id, doc = document) => {
    const outputEl = doc.body ? findOutputEl(doc, id) : null;
    if (!outputEl) {
      // The tag is usually later in the UI than the tip that names it: keep looking until the page has it.
      pendingPark.add(id);
      watchForOutputs(doc);
      return;
    }
    pendingPark.delete(id);
    const wrapper = wrapperOf(outputEl);
    captureShell(id, wrapper);
    const holder = getPark(doc);
    if (holder && !wrapper.closest('.hc-output') && wrapper.parentElement !== holder) {
      holder.append(wrapper);
    }
  };

  let outputWatch = null;
  const watchForOutputs = (doc) => {
    if (outputWatch || typeof doc.defaultView?.MutationObserver !== 'function') return;
    outputWatch = new doc.defaultView.MutationObserver(() => {
      flushPendingPark();
      if (!pendingPark.size) {
        outputWatch.disconnect();
        outputWatch = null;
      }
    });
    outputWatch.observe(doc.documentElement, { childList: true, subtree: true });
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

  // Retry sets the input `<id>_retry`, which re-runs an output only when its render reads it (see `tip_output()`). If
  // the server neither starts nor finishes the output within RETRY_WAIT, the tip says so and offers Retry again, so it
  // never stays on "Loading…" for good.
  const retryOutput = (id, hostEl) => {
    const rec = outputRecord(id);
    rec.retries += 1;
    rec.state = 'loading';
    rec.stale = false;
    rec.message = '';
    applySlotState(id);
    const shiny = hostEl.ownerDocument.defaultView?.Shiny ?? window.Shiny;
    if (shiny?.setInputValue) {
      shiny.setInputValue(`${id}_retry`, rec.retries, { priority: 'event' });
    }
    hostEl.dispatchEvent(
      new CustomEvent('hovercraft:retry', { bubbles: true, detail: { output: id, count: rec.retries } }),
    );
    awaitServer(rec, RETRY_WAIT, () => {
      rec.state = 'error';
      rec.message = NO_ANSWER;
      applySlotState(id);
    });
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
      if (rec.wait) rec.wait.invalidated = true;
      applySlotState(id);
    });
    $doc.on('shiny:value', (event) => {
      const id = event.target?.id;
      if (!id || !outputs.has(id)) return;
      const rec = outputRecord(id);
      const valEmpty = valueLooksEmpty(event.value);
      clearWait(rec);
      rec.stale = false;
      rec.state = valEmpty ? 'empty' : 'ready';
      rec.message = '';
      applySlotState(id);
      // Confirm rendered DOM after Shiny writes the value (covers htmlwidgets / renderUI returning blank tags).
      setTimeout(() => {
        if (rec.state === 'loading' || rec.state === 'error') return;
        const el = findOutputEl(document, id);
        if (!el) return;
        const next = domLooksEmpty(el) ? 'empty' : 'ready';
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
      clearWait(rec);
      rec.stale = false;
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

        // One output has one node in the page: while another tip holds it, this one says so.
        const movedEl = doc.createElement('div');
        movedEl.className = 'hc-output-moved';
        movedEl.textContent = 'Shown in another tip.';
        movedEl.hidden = true;

        root.append(hostEl, loadingEl, errorEl, emptyEl, movedEl);

        // The tagged element whose tip holds `box` now: the engine marks it while the tip is open.
        const itemOf = (box) => {
          const kind = box?.classList.contains(`${names.css}-pin`) ? names.states.pin : names.states.hint;
          return doc.querySelector(`[${names.attributes.open}="${kind}"]`);
        };

        // Gives the node of an output that another box holds to the one that opens now: the box that had it says so.
        const takeOver = (rec) => {
          const from = rec.slot;
          if (from && from !== root && from.isConnected) {
            from.dataset.state = 'moved';
            for (const part of from.querySelectorAll('.hc-output-loading, .hc-output-error, .hc-output-empty')) {
              part.hidden = true;
            }
            const moved = from.querySelector('.hc-output-moved');
            if (moved) moved.hidden = false;
          }
          rec.slot = root;
        };

        const mount = (_node, info) => {
          attachOutputListeners();
          const rec = outputRecord(id);
          let outputEl = findOutputEl(doc, id);
          let wrapper = wrapperOf(outputEl);
          if (!wrapper && shells.has(id)) {
            wrapper = shells.get(id).cloneNode(true);
            outputEl = wrapper.id === id ? wrapper : wrapper.querySelector(`[id="${CSS.escape(id)}"]`);
          }
          if (!wrapper || !outputEl) {
            rec.slot = root;
            rec.state = 'empty';
            applySlotState(id);
            return {
              destroy: () => {
                if (rec.slot === root) rec.slot = null;
              },
            };
          }
          const mine = { root, hostEl };
          takeOver(rec);
          rec.live.push(mine);
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
          // An output drawn for another item still holds that item's content: show loading until the server answers
          // for this one. A server that never reads the open item sends nothing, so after SWITCH_WAIT the content stays.
          // A pinned tip's output says whose it is (`info.id`); with several pins open, "the item that is open" is no one's.
          const item = info?.id ? find(String(info.id)) : itemOf(info?.box);
          if (item && rec.item && item !== rec.item && rec.state === 'ready') {
            rec.state = 'loading';
            rec.stale = true;
            awaitServer(rec, SWITCH_WAIT, () => {
              rec.stale = false;
              const el = findOutputEl(doc, id);
              rec.state = domLooksEmpty(el) ? 'empty' : 'ready';
              applySlotState(id);
            });
          }
          if (item) rec.item = item;
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
              rec.live = rec.live.filter((slot) => slot !== mine);
              // Only the box that holds the node lets go of it; another box may have taken it over.
              if (rec.slot !== root) return;
              rec.slot = null;
              const redraw = info?.reason === 'redraw';
              // A box that stayed open (a pin) gets the node back; a redraw parks it, and the new box asks for it.
              const next = rec.live.at(-1);
              if (!redraw && next?.root.isConnected) {
                next.hostEl.append(wrapper);
                rec.slot = next.root;
                applySlotState(id);
                refreshOutput(next.hostEl, outputEl, id);
                return;
              }
              if (!hostEl.contains(wrapper)) return;
              if (keepAlive || redraw) {
                const holder = getPark(doc);
                if (holder) holder.append(wrapper);
              } else {
                const shiny = doc.defaultView?.Shiny ?? window.Shiny;
                if (shiny?.unbindAll) shiny.unbindAll(wrapper, true);
                wrapper.remove();
                clearWait(rec);
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

  // What a block is, for telling the short form's blocks from the long form's in a pinned box: the engine marks neither.
  const signature = (node) => `${node.tagName}|${node.className}|${node.textContent}`;
  const isChrome = (node, cls) => node.matches(`.${cls}-head, .${cls}-close, .${cls}-cue, .${cls}-reason`);
  const shortBlocks = (box) => {
    const cls = names?.css ?? 'hc-tip';
    return [...box.children].filter((c) => !isChrome(c, cls) && !c.matches(`.${cls}-more`)).map(signature);
  };

  // The blocks of a pinned box that the short form did not have, in order. Each short block is matched once, wherever it
  // sits: a pinned box moves the title into its head, so the short form's list is not a prefix of the pinned one.
  const longBlocks = (pinBox, shorts) => {
    const cls = names?.css ?? 'hc-tip';
    const left = [...shorts];
    const fresh = [];
    for (const child of pinBox.children) {
      if (isChrome(child, cls)) continue;
      const at = left.indexOf(signature(child));
      if (at >= 0) left.splice(at, 1);
      else fresh.push(child);
    }
    return fresh;
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
      blocks: shortBlocks(box),
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

  // The pin boxes that were on show at the last pin event: with several pins open, the box a pin just opened in is the one
  // that was not there before.
  let shownPins = new Set();
  const visiblePins = () => {
    const cls = names?.css ?? 'hc-tip';
    return [...document.querySelectorAll(`.${cls}.${cls}-pin:not([hidden])`)];
  };
  const animatePinGrowth = (event) => {
    const detail = event.detail;
    if (!detail) return;
    if (detail.state === 'closed') {
      cancelGrowth();
      shownPins = new Set(visiblePins());
      return;
    }
    if (detail.state !== 'pinned') return;
    const fresh = visiblePins().filter((box) => !shownPins.has(box));
    shownPins = new Set(visiblePins());
    const prev = lastHint;
    lastHint = null;
    const el = event.target?.nodeType === 1 ? event.target : find(detail.id);
    if (!el) return;
    const hasMore = el.hasAttribute(names.attributes.more) || el.hasAttribute(names.attributes.moreTemplate);
    if (!hasMore || !prev || Date.now() - prev.at > 5000) return;
    if (prev.id && detail.id && prev.id !== detail.id) return;

    // Run right after the engine's synchronous pinMotion.enter() and before the first browser paint.
    Promise.resolve().then(() => {
      const pinBox = fresh.at(-1) ?? visiblePins().at(-1);
      if (!pinBox) return;
      // The long form starts with a hairline, as in the hover form (the engine draws no wrapper for it when pinned).
      longBlocks(pinBox, prev.blocks)[0]?.classList.add('hc-long-first');
      if (reducedMotion() || typeof pinBox.animate !== 'function') return;
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
      // The box has `overflow: visible`: clip it while it grows, so the long form never draws outside its background.
      const clipped = pinBox.style.overflow;
      pinBox.style.overflow = 'clip';
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
      const unclip = () => {
        pinBox.style.overflow = clipped;
      };
      boxAnim.finished.then(() => {
        activeGrowth = activeGrowth.filter((a) => a !== boxAnim);
        unclip();
      }, unclip);
      // Gently reveal the long form's blocks as the box opens.
      for (const child of longBlocks(pinBox, prev.blocks)) {
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

  // Output tags that R hands over as HTML (a `more` template that holds a tag): put in the park holder once there is a
  // body, each only once, and bound by Shiny when it is already running.
  const pendingHtml = [];
  const parkedHtml = new Set();
  const flushParkHtml = () => {
    if (!document.body || !pendingHtml.length) return;
    const holder = getPark(document);
    for (const html of pendingHtml.splice(0)) {
      if (parkedHtml.has(html)) continue;
      parkedHtml.add(html);
      holder.insertAdjacentHTML('beforeend', html);
    }
    if (window.Shiny?.shinyapp && window.Shiny.bindAll) window.Shiny.bindAll(holder);
  };

  const flushPendingPark = () => {
    for (const id of [...pendingPark]) parkId(id);
  };

  window.HovercraftShiny = Object.freeze({
    start: (config) => {
      if (names) return;
      names = config;
      document.addEventListener(names.events.hint, sendInput('hint'));
      document.addEventListener(names.events.tip, sendInput('pin'));
      document.addEventListener(names.events.copy, sendInput('copy'));
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
      let registered = false;
      const ready = () => {
        flushParkHtml();
        flushPendingPark();
        if (!registered && window.Shiny?.addCustomMessageHandler) {
          registered = true;
          register();
        }
      };
      ready();
      if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', ready);
      let checking = 0;
      new MutationObserver(() => {
        clearTimeout(checking);
        checking = setTimeout(checkIds, 500);
      }).observe(document.documentElement, { childList: true, subtree: true });
      checkIds();
    },
    parkHtml: (html) => {
      if (typeof html !== 'string' || !html) return;
      pendingHtml.push(html);
      flushParkHtml();
    },
    park: (id) => {
      if (typeof id === 'string' && id) parkId(id);
    },
  });
})();
