  $(function() {
    var $els = $("[data-proxy-click]");
      $.each(
      $els,
        function(idx, el) {
          var $el = $(el);
            var $proxy = $("#" + $el.data("proxyClick"));
              $el.keydown(function (e) {
              if (e.keyCode == 13) {
            $proxy.click();
          }
        });
      }
    );
  });
