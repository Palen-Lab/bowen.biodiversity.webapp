$( document ).ready(function() {
  Shiny.addCustomMessageHandler('fun', function(_arg) {

  });

  // Map layer loading spinner
  var loaderTimer;

  function showLoader() {
    clearTimeout(loaderTimer);
    $('#map-loader').css('display', 'flex');
    // Fallback: hide after 15s in case no layeradd fires
    loaderTimer = setTimeout(hideLoader, 15000);
  }

  function hideLoader() {
    clearTimeout(loaderTimer);
    $('#map-loader').hide();
  }

  // Show spinner whenever a sidebar checkbox is toggled on
  $(document).on('change', '.sidebar input[type="checkbox"]', function() {
    if ($(this).is(':checked')) showLoader();
    else hideLoader();
  });

  // Hook into leaflet layeradd once the map widget is ready
  var mapPollInterval = setInterval(function() {
    var widget = HTMLWidgets.find('#map');
    if (!widget) return;
    clearInterval(mapPollInterval);
    widget.getMap().on('layeradd', function() {
      // Debounce: wait a tick so rapid sequential adds don't flash the loader
      clearTimeout(loaderTimer);
      loaderTimer = setTimeout(hideLoader, 300);
    });
  }, 200);
});
