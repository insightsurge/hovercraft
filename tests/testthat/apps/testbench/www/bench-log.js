// The bench's log: every tip event, the inputs the page sends, custom messages from the server and page errors, in
// window.benchLog as { n, t, kind, id, detail }, newest last. The tests read it (helper-bench.R); a person reads the
// strip at the bottom of the page. The engine's event names come from the page's boot JSON (the engine's contract).
(() => {
  const cap = 2000;
  const log = [];
  let count = 0;
  window.benchLog = log;
  window.benchLogClear = () => {
    log.length = 0;
    draw();
  };

  let pending = false;
  const draw = () => {
    if (pending) return;
    pending = true;
    requestAnimationFrame(() => {
      pending = false;
      const list = document.getElementById('bench-log-lines');
      const badge = document.getElementById('bench-log-count');
      if (badge) badge.textContent = String(log.length);
      if (!list) return;
      list.replaceChildren(
        ...log
          .slice(-60)
          .reverse()
          .map((line) => {
            const item = document.createElement('li');
            item.className = `bench-log-${line.kind}`;
            const detail = line.detail === null || line.detail === undefined ? '' : JSON.stringify(line.detail);
            item.textContent = `${line.t} ${line.kind} ${line.id ?? ''} ${detail}`;
            return item;
          }),
      );
    });
  };

  const add = (kind, id, detail) => {
    count += 1;
    log.push({ n: count, t: new Date().toISOString().slice(11, 23), kind, id: id ?? null, detail: detail ?? null });
    if (log.length > cap) log.splice(0, log.length - cap);
    draw();
  };
  window.benchLogAdd = add;

  // Page errors, for the person's strip. The tests read errors from the browser itself, not from here.
  window.addEventListener('error', (event) => add('error', null, { message: event.message }));
  window.addEventListener('unhandledrejection', (event) => add('error', null, { message: String(event.reason) }));
  for (const level of ['error', 'warn']) {
    const original = console[level].bind(console);
    console[level] = (...args) => {
      add(level, null, { message: args.map((a) => (a instanceof Error ? a.message : String(a))).join(' ') });
      original(...args);
    };
  }

  const quiet = /^\.clientdata_|^bslib|^\./;
  // Shiny triggers its events through jQuery, which a native listener does not hear: the inputs the page sends
  // (without Shiny's and bslib's own housekeeping) and the server's custom messages, by type.
  const jq = () => window.jQuery;
  const hookJquery = () => {
    if (!jq()) return;
    jq()(document).on('shiny:inputchanged', (event) => {
      if (!quiet.test(event.name)) add('input', event.name, { value: event.value });
    });
    jq()(document).on('shiny:message', (event) => {
      const custom = event.message?.custom;
      if (custom) for (const type of Object.keys(custom)) add('message', type, custom[type]);
    });
  };

  const start = () => {
    const boot = JSON.parse(document.getElementById('bench-boot')?.textContent ?? '{}');
    for (const [kind, name] of Object.entries(boot.events ?? {})) {
      document.addEventListener(
        name,
        (event) => {
          const detail = event.detail ?? {};
          add(kind, detail.id ?? null, detail);
        },
        true,
      );
    }
    hookJquery();
    document.getElementById('bench-log-clear')?.addEventListener('click', (event) => {
      event.preventDefault();
      window.benchLogClear();
    });
    draw();
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
