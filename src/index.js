import "bulma/css/bulma.css";
import "@fortawesome/fontawesome-free/css/all.css";
import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

Elm.Main.init({
  node: document.getElementById("root")
});

registerServiceWorker();
