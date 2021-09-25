
const allMore = document.querySelectorAll(".section-more");

allMore.forEach(more => {
  var content_id = more.id.replace(/more$/, "content");
  var content = document.getElementById(content_id);
  if (!content.className.includes("show")) {
    $(content).hide();
  }
  var icon = more.children[0];
  more.addEventListener("click", () => {
    if (content.style.display === "none") {
      $(content).slideDown(500);
      if (typeof icon !== 'undefined') {
        icon.setAttribute("aria-label", "caret-down icon");
        icon.className = "fa fa-caret-down";
      }
      if (more.text === "Lire plus") {
        more.text = "Lire moins";
      }
      if (more.text === "Read everything") {
        more.text = "Read less";
      }
    } else {
      $(content).slideUp(500);
      if (typeof icon !== 'undefined') {
        icon.setAttribute("aria-label", "caret-up icon");
        icon.className = "fa fa-caret-up";
      }
      if (more.text === "Lire moins") {
        more.text = "Lire plus";
      }
      if (more.text === "Read less") {
        more.text = "Read everything";
      }
    }
  })
})


