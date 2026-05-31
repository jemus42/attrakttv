// Live trakt search for the show dropdown.
//
// Two-way bridge:
//   1. selectize's `type` event -> Shiny.setInputValue (debounced) so the
//      server can run a trakt search.
//   2. Server response comes back via Shiny.addCustomMessageHandler and is
//      applied with selectize's *native* addOption/removeOption APIs. We
//      deliberately avoid updateSelectizeInput here because shiny's updater
//      re-renders the input element, which clobbers whatever the user has
//      typed when results arrive mid-keystroke.
(function () {
  var DEBOUNCE_MS = 250;
  var MIN_CHARS = 2;

  function attach() {
    var el = document.getElementById("shows_cached");
    if (!el || !el.selectize || !window.Shiny) {
      window.setTimeout(attach, 50);
      return;
    }
    var sel = el.selectize;
    var lastQuery = "";
    var timer = null;

    sel.on("type", function (query) {
      window.clearTimeout(timer);
      timer = window.setTimeout(function () {
        if (query === lastQuery) return;
        lastQuery = query;
        if (query.length < MIN_CHARS) return;
        Shiny.setInputValue("shows_search_query", query, {
          priority: "event",
        });
      }, DEBOUNCE_MS);
    });

    // Apply server-returned trakt hits to the dropdown without disturbing
    // the input area. We tag added options with `__source: "trakt"` so we
    // can sweep them on the next update and leave any locally-cached
    // (cache:<id>) options alone.
    Shiny.addCustomMessageHandler("attrakttv_trakt_results", function (msg) {
      // Remove existing trakt-sourced options. Collect keys first so we
      // don't mutate the registry while iterating it. Also treat any
      // pre-existing `trakt:<id>` key as ours -- handles options left over
      // from older sessions that pre-date the __source tag.
      var toRemove = [];
      Object.keys(sel.options).forEach(function (k) {
        var opt = sel.options[k];
        if ((opt && opt.__source === "trakt") || k.indexOf("trakt:") === 0) {
          toRemove.push(k);
        }
      });
      toRemove.forEach(function (k) {
        sel.removeOption(k, true);
      });

      // Add the new ones.
      var items = msg.items || [];
      items.forEach(function (it) {
        sel.addOption({
          value: it.value,
          label: it.label,
          text: it.label,
          __source: "trakt",
        });
      });

      // Re-filter the dropdown against the current input text. `false`
      // means: don't reopen the dropdown if it isn't already open, and
      // don't touch the input element.
      sel.refreshOptions(false);

      // Highlight the server-preferred first item so Enter picks it.
      // The server orders results so exact-title matches lead.
      if (items.length > 0) {
        var $opt = sel.getOption(items[0].value);
        if ($opt && $opt.length) {
          sel.setActiveOption($opt);
        }
      }
    });
  }

  $(document).on("shiny:connected", attach);
})();
