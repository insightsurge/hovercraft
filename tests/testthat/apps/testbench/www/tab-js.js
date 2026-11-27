// The JavaScript-driven UI tab (R/tab-js.R): the page's own scripts add, move, replace and remove tagged elements and
// send data, the way an app's own JavaScript does. Each step also writes a line to the bench's log.
(() => {
  const $ = window.jQuery;
  const note = (what, detail) => window.benchLogAdd?.('app', what, detail ?? null);
  const fromTemplate = (id) => document.getElementById(id).innerHTML;
  const on = (id, handler) => document.getElementById(id)?.addEventListener('click', handler);

  // A custom message from the server: put the HTML it carries in its slot.
  window.Shiny.addCustomMessageHandler('bench-js-insert', (message) => {
    document.getElementById(message.slot).innerHTML = message.html;
    note('inserted by a message', { slot: message.slot });
  });

  // The app's own handlers, with names a tooltip package might also like; they must not clash with the package's.
  for (const name of ['show', 'hide', 'tip-show']) {
    window.Shiny.addCustomMessageHandler(name, (message) => {
      document.getElementById('js-names-seen').textContent = `${name}: ${message.text}`;
      note(`handler ${name}`, message);
      document.dispatchEvent(new CustomEvent('app:hint', { detail: { state: 'shown', id: 'js_names_tip' } }));
    });
  }

  const start = () => {
    // Shiny.setInputValue() from a tagged button.
    let sent = 0;
    on('js-set-btn', () => {
      sent += 1;
      window.Shiny.setInputValue('js_set', `value ${sent}`, { priority: 'event' });
    });

    // innerHTML, jQuery .html() and .append(), and an input bound with Shiny.bindAll().
    on('js-html-add', () => {
      document.getElementById('js-html-slot').innerHTML = fromTemplate('js-tpl-html');
      note('innerHTML');
    });
    on('js-jq-add', () => {
      $('#js-jq-slot').html('<span class="me-2">jQuery:</span>').append(fromTemplate('js-tpl-jq'));
      note('jQuery html and append');
    });
    on('js-input-add', () => {
      const slot = document.getElementById('js-input-slot');
      slot.innerHTML = fromTemplate('js-tpl-input');
      window.Shiny.bindAll(slot);
      note('bindAll');
    });

    // Shiny.renderContent(): HTML and its dependencies, bound as Shiny binds what it renders.
    on('js-render-add', () => {
      window.Shiny.renderContent(document.getElementById('js-render-slot'), {
        html: fromTemplate('js-tpl-render'),
        deps: [],
      });
      note('renderContent');
    });

    // Unbind, then remove the bound input.
    on('js-remove', () => {
      const slot = document.getElementById('js-input-slot');
      window.Shiny.unbindAll(slot);
      slot.replaceChildren();
      note('unbindAll and removed');
    });

    // Move a tagged node to another box, and replace it with a new node.
    on('js-move', () => {
      const node = document.getElementById('js-mov-btn') ?? document.getElementById('js-rep-btn');
      const there = node.parentElement.id === 'js-box-a' ? 'js-box-b' : 'js-box-a';
      document.getElementById(there).append(node);
      note('moved', { to: there });
    });
    on('js-replace', () => {
      const node = document.getElementById('js-mov-btn');
      if (!node) return;
      const holder = document.createElement('div');
      holder.innerHTML = fromTemplate('js-tpl-replace');
      node.replaceWith(holder.firstElementChild);
      note('replaced');
    });

    // Change a class, an aria-label and the tip's own words on a tagged element.
    on('js-attr-change', () => {
      const node = document.getElementById('js-attr-btn');
      const words = JSON.parse(document.getElementById('bench-boot').textContent).attributes.tip;
      node.classList.toggle('btn-warning');
      node.setAttribute('aria-label', 'Changed by the page');
      node.setAttribute(words, 'Words after');
      note('attributes changed');
    });

    // shiny:value: tag the output the server just drew; shiny:inputchanged: count the page's input changes.
    const tipAttribute = JSON.parse(document.getElementById('bench-boot').textContent).attributes.tip;
    $(document).on('shiny:value', (event) => {
      if (event.name !== 'js_value_out') return;
      requestAnimationFrame(() => {
        document.getElementById('js_value_out')?.setAttribute(tipAttribute, `Tagged on shiny:value: ${event.value}`);
      });
    });
    let changed = 0;
    $(document).on('shiny:inputchanged', (event) => {
      if (!event.name.startsWith('js_')) return;
      changed += 1;
      document.getElementById('js-changed-count').textContent = String(changed);
    });

    // Bootstrap's own tooltip and popover, started by the page.
    const bootstrap = window.bootstrap;
    if (bootstrap) {
      new bootstrap.Tooltip(document.getElementById('js-bs-tooltip'));
      new bootstrap.Popover(document.getElementById('js-bs-popover'), { trigger: 'hover focus' });
    }
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
