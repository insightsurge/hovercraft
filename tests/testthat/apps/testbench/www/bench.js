// Small helpers for the scenario tabs: switch a control off and on from the server, and count the tip nodes the
// engine has on the page (the scale tab shows the count).
(() => {
  const start = () => {
    window.Shiny?.addCustomMessageHandler('bench-toggle-disabled', (message) => {
      const node = document.getElementById(message.id);
      if (node) node.disabled = !node.disabled;
    });
    const boot = JSON.parse(document.getElementById('bench-boot')?.textContent ?? '{}');
    const out = document.getElementById('scale-count');
    if (!out || !boot.css) return;
    setInterval(() => {
      out.textContent = String(document.querySelectorAll(`.${boot.css}`).length);
    }, 1000);
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
