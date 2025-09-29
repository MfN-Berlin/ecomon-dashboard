// Highlight sun and moon toggle buttons when active
$(document).on('shiny:inputchanged', function(event) {
  if(event.name === 'sun_toggle') {
    var btn = $('#sun_toggle');
    if(event.value % 2 === 1) {
      btn.addClass('active');
    } else {
      btn.removeClass('active');
    }
  }
  if(event.name === 'moonphase_toggle') {
    var btn = $('#moonphase_toggle');
    if(event.value % 2 === 1) {
      btn.addClass('active');
    } else {
      btn.removeClass('active');
    }
  }
  if(event.name === 'twilight_toggle') {
    var btn = $('#twilight_toggle');
    if(event.value % 2 === 1) {
      btn.addClass('active');
    } else {
      btn.removeClass('active');
    }
  }
});

// Set initial colormap button label and value on Shiny connection
$(document).on('shiny:connected', function() {
  $('#colormapMenuButton').html('<span class=\"bi bi-palette\" style=\"font-size: 1.1em; vertical-align: middle; color: black; opacity: 0.5;\"></span> Color Map: Plasma');
  Shiny.setInputValue('colormap', $('#colormap').val(), {priority: 'event'});

  // Simulate a click on sun_toggle if not already active
  var sunBtn = $('#sun_toggle');
  if (!sunBtn.hasClass('active')) {
    sunBtn.trigger('click');
  }

  // Simulate a click on twilight_toggle if not already active
  var twilightBtn = $('#twilight_toggle');
  if (!twilightBtn.hasClass('active')) {
    twilightBtn.trigger('click');
  }
});

// Handle colormap dropdown selection and update button label
$(document).on('click', '.colormap-option', function(e) {
  e.preventDefault();
  var val = $(this).data('value');
  $('#colormap').val(val).trigger('change');
  Shiny.setInputValue('colormap', val, {priority: 'event'});
  $('#colormapMenuButton').html('<span class=\"bi bi-palette\" style=\"font-size: 1.1em; vertical-align: middle; color: black; opacity: 0.5;\"></span> Color Map: ' + $(this).text());
});

// Disable all canvas_controls inputs on every UI render (robust for dynamic UI)
$(document).on('shiny:inputrendered shiny:value', function() {
  $('#canvas_classifier').prop('disabled', true);
  $('#canvas_threshold').prop('disabled', true);
  $('#canvas_site').prop('disabled', true);
  $('#canvas_year').prop('disabled', true);
  $('#canvas_species').prop('disabled', true);
});