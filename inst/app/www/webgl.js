function webgl_support() {
    try {
      var canvas = document.createElement('canvas');
      return !!(
        window.WebGLRenderingContext &&
        (canvas.getContext('webgl') ||
         canvas.getContext('experimental-webgl'))
      );
    } catch (e) {
      return false;
    }
  }

  $(document).on('shiny:connected', function() {
    Shiny.setInputValue('webgl_supported', webgl_support());
});
