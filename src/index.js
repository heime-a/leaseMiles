import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';


var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.saveLocal.subscribe(function(startDate) {
  console.log(startDate);
});



registerServiceWorker();
