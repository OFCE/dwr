
const widgetWrappers = document.querySelectorAll(".widget-wrapper")

widgetWrappers.forEach(wrap => {
  var widget = wrap.querySelector("input");
  if (widget === null) {
    widget = wrap.querySelector("select");
  }
  const value = wrap.querySelector(".widget-value");
  widget.addEventListener("input", () => {
    setValue(widget, value);
  })
  setValue(widget, value);
})

function setValue(widget, value) {
  const val = widget.value;
  value.innerHTML = val;
}

