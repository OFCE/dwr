
// Shiny bindings

var sliderInput2Binding = new Shiny.InputBinding();

$.extend(sliderInput2Binding, {

  find: function(scope) {
    return $(scope).find(".slider");
  },
  
  getId: function(el) {
    return el.id;
  },

  initialize: function(el){
    const value = el.parentNode.parentNode.querySelector(".slider-value");
    
    el.addEventListener("input", () => {
      sliderInput2SetValue(el, value);
    });
    sliderInput2SetValue(el, value);
  },
  
  getValue: function(el) {
    const val = Number(el.value);
    return val;
  },
  
  setValue: function(el, value) {
    $(el).val(value);
  },

  subscribe: function(el, callback) {
    $(el).on('change.input', function(event) {
      callback();
    })
  },
  
  unsubscribe: function(el) {
    $(el).off('.slider-wrapper');
  },
  
  receiveMessage: function(el, data) {
    const wrapper = el.parentNode.parentNode;
    let update_slider_value = false;
    
    if (data.hasOwnProperty("label")) {
      const label = wrapper.querySelector("label .parametres-label");
      label.innerHTML = data.label;
    }
    
    if (data.hasOwnProperty("min")) {
      el.min = data.min;
      const slider_min = wrapper.querySelector(".slider-min");
      slider_min.innerHTML = data.min;
      update_slider_value = true;
    }
    
    if (data.hasOwnProperty("max")) {
      el.max = data.max;
      const slider_max = wrapper.querySelector(".slider-max");
      slider_max.innerHTML = data.max;
      update_slider_value = true;
    }
    
    if (data.hasOwnProperty("value")) {
      this.setValue(el, data.value);
      update_slider_value = true;
      const widget_value = wrapper.querySelector(".widget-value");
      setValue(el, widget_value); // To update the value in the widget header
    }
    
    if (update_slider_value) {
      const slider_value = wrapper.querySelector(".slider-value");
      sliderInput2SetValue(el, slider_value); // To update the value close to the slider
    }
    
    if (data.hasOwnProperty("step")) {
      el.step = data.step;
    }
    
    $(el).trigger("change");
  }
  
});

Shiny.inputBindings.register(sliderInput2Binding);

function sliderInput2SetValue(slider, value) {
  const val = slider.value;
  const min = slider.min ? slider.min : 0;
  const max = slider.max ? slider.max : 100;
  const newVal = Number(((val - min) * 80) / (max - min));
  value.innerHTML = val;

  // Sorta magic numbers based on size of the native UI thumb
  value.style.left = `calc(${newVal}% + (${41 - newVal * 0.14}px))`;
}

