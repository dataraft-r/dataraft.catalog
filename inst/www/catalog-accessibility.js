// Normalize tab semantics across supported bslib/Bootstrap versions.
(() => {
  function initialize() {
    document.querySelectorAll('#catalog-content .nav-tabs').forEach(list => {
      list.setAttribute('role', 'tablist');
      list.setAttribute('aria-label', 'Catalog sections');
      list.querySelectorAll(':scope > li').forEach(item => item.setAttribute('role', 'presentation'));
      const tabs = [...list.querySelectorAll('a[data-bs-toggle="tab"],a[data-toggle="tab"]')];
      function update() {
        tabs.forEach((tab, i) => {
          const target = tab.getAttribute('href').slice(1);
          tab.id ||= `dataraft-section-${i}`;
          tab.setAttribute('role', 'tab');
          tab.setAttribute('aria-controls', target);
          const active = tab.classList.contains('active');
          tab.setAttribute('aria-selected', String(active));
          tab.tabIndex = active ? 0 : -1;
          const panel = document.getElementById(target);
          if (panel) {
            panel.setAttribute('role', 'tabpanel');
            panel.setAttribute('aria-labelledby', tab.id);
          }
        });
      }
      update();
      list.addEventListener('click', () => setTimeout(update, 0));
      if (window.jQuery) window.jQuery(list).on('shown.bs.tab', update);
      list.addEventListener('keydown', event => {
        const i = tabs.indexOf(document.activeElement);
        if (i < 0) return;
        const target = {ArrowRight: (i + 1) % tabs.length, ArrowLeft: (i + tabs.length - 1) % tabs.length, Home: 0, End: tabs.length - 1}[event.key];
        if (target !== undefined) {
          event.preventDefault();
          tabs[target].focus();
          tabs[target].click();
          update();
        } else if (event.key === ' ') {
          event.preventDefault();
          tabs[i].click();
          update();
        }
      });
    });
    document.querySelectorAll('#catalog-content .card-body').forEach(region => {
      region.tabIndex = 0;
      region.setAttribute('role', 'region');
      region.setAttribute('aria-label', 'Selected catalog section');
    });
    function tables() {
      document.querySelectorAll('#catalog-content table th').forEach(th => th.setAttribute('scope', 'col'));
    }
    tables();
    if (window.jQuery) window.jQuery(document).on('shiny:value', () => setTimeout(tables, 0));
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initialize);
  else initialize();
})();
