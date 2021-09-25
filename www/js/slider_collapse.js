
const allWidgetWrappers = document.querySelectorAll(".widget-wrapper");

allWidgetWrappers.forEach(wrapper => {
  var id = wrapper.id.replace(/widget-wrapper-/, "");
  var label = wrapper.querySelector(".widget-label");
  var widget = wrapper.querySelector(".input-group");
  if (widget === null) {
    widget = wrapper.querySelector("select");
  }
  label.addEventListener("click", (e) => {
    if (widget.style.display === "none") {
      $(widget).slideDown();
      label.classList.add("widget-label-selected");
    }
  });
  $(document).on("click", (e) => {
    if (!$(e.target).closest(wrapper).length) {
      $(widget).slideUp();
      label.classList.remove("widget-label-selected");
    }
  })
  $(widget).hide();
});



