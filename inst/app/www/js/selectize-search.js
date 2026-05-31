// Bridge selectize's `type` event to a Shiny input so the server can run
// a live trakt search as the user types. Debounced so we don't slam the
// API on every keystroke.
(function () {
  var DEBOUNCE_MS = 250;
  var MIN_CHARS = 2;

  function attach() {
    var el = document.getElementById("shows_cached");
    if (!el || !el.selectize || !window.Shiny) {
      // Selectize may not be initialised yet on first connect; retry.
      window.setTimeout(attach, 50);
      return;
    }
    var lastQuery = "";
    var timer = null;

    el.selectize.on("type", function (query) {
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
  }

  $(document).on("shiny:connected", attach);
})();
