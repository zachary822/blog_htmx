import htmx from "htmx.org";
import Alpine from "alpinejs";
import morph from "@alpinejs/morph";

window.Alpine = Alpine;
Alpine.plugin(morph);

Alpine.start();

htmx.on("htmx:afterSettle", () => {
  document.querySelectorAll("time").forEach((el) => {
    const d = new Date(el.dateTime);

    el.innerText = d.toLocaleString();
  });
});
