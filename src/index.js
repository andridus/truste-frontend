import './css/bulma.css';
import './css/uikit.min.css';
import './css/app.css';

import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
var dt = new Date().getTime();
var uuid1 = 'xxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = (dt + Math.random()*16)%16 | 0;
    dt = Math.floor(dt/16);
    return (c=='x' ? r :(r&0x3|0x8)).toString(16);
});
var stored = store.get("user");
var user = stored || { uid: uuid1 , nome: "", avatar: "/imgs/players/no-image.png", pronto: false}

var app = Main.embed(document.getElementById('root'),user);

var scrollTimer = null;
var lastScrollFireTime = 0;
var minScrollTime = 200;
window.onresize = function(){
	var body = document.querySelector("body");
	var html = document.querySelector("html");
	var Tela = {
        scrollTop: parseInt(window.pageYOffset || 0),
        pageHeight: parseInt(Math.max(body.scrollHeight, body.offsetHeight, html.clientHeight, html.scrollHeight, html.offsetHeight)),
        viewportHeight: parseInt(html.clientHeight),
        viewportWidth: parseInt(html.clientWidth),
    };
    app.ports.scrollOrResize.send(Tela);
}
app.ports.setUsuario.subscribe(function(s){
    console.log(s)
    store.set("user", s)
   
})

app.ports.play_sound.subscribe(function(s){
    switch(s){
        case "CLICK":
            var sound1 = new Howl({
              src: ["/sound/button.wav"]
            });
            sound1.play();
        break;
        case "LEAVE":
            var sound1 = new Howl({
              src: ["/sound/leave.wav"]
            });
            sound1.play();
        break;
        case "VEZ":
            var sound1 = new Howl({
              src: ["/sound/vez.wav"]
            });
            sound1.play();
        break;
        case "MONEY":
            var sound1 = new Howl({
              src: ["/sound/money.wav"]
            });
            sound1.play();
        break;
        case "NOVA_EMPRESA":
            var sound1 = new Howl({
              src: ["/sound/nova_empresa.wav"]
            });
            sound1.play();
        break;
        case "AMONEY":
            var sound1 = new Howl({
              src: ["/sound/acao_venda.wav"]
            });
            sound1.play();
        break;
        case "SWIPE":
            var sound1 = new Howl({
              src: ["/sound/swipe.wav"]
            });
            sound1.play();
        break;
        case "PIECE":
            var sound1 = new Howl({
              src: ["/sound/piece.wav"]
            });
            sound1.play();
        break;
        case "START":
            var sound1 = new Howl({
              src: ["/sound/start_game.wav"]
            });
            sound1.play();
        break;
        case "ERROR":
            var sound1 = new Howl({
              src: ["/sound/error.wav"]
            });
            sound1.play();
        break;
        case "QUESTION":
            var sound1 = new Howl({
              src: ["/sound/question.wav"]
            });
            sound1.play();
        break;
        case "JOIN":
            var sound1 = new Howl({
              src: ["/sound/join.wav"]
            });
            sound1.play();
        break;
        case "BUTTON-SUCCESS":
            var sound1 = new Howl({
              src: ["/sound/button_success.wav"]
            });
            sound1.play();
        break;
        case "BUTTON-CANCEL":
            var sound1 = new Howl({
              src: ["/sound/button_cancel.wav"]
            });
            sound1.play();
        break;
        default:
           break;
    }
    
    
   
})
app.ports.playlist_toggle_stop.subscribe(function(s){
    //var l =buzz.all().getSounds();
    //buzz.all().getSounds()[l.length-1].togglePlay()
})
app.ports.playlist_sound.subscribe(function(s){
    var sound_playlist = function(playlist, actual){
        if(actual>= playlist.length){
            actual = 0;
        }
        var sd = new buzz.sound(playlist[actual]);
        sd.play();
        sd.bind("ended", function(){
            sound_playlist(playlist, actual+1)
        })
    }
    switch(s){
        case "OUT":
            var playlist = [
                "/music/m1.mp3"
            ,   "/music/m2.mp3"
            ,   "/music/m3.mp3"
            ,   "/music/m4.mp3"
            ,   "/music/m5.mp3"
            ]   
            //sound_playlist(playlist, 0)
 
            console.log("here")
        break;
        case "IN-GAME":
            var sound = new Howl({
                  src: ['/sound/button_success.wav'],
                });
            sound.play();
        break;
        
        default:
           break;
    }
    
    
   
})

app.ports.getTela.subscribe(function(s){

	var body = document.querySelector("body");
	var html = document.querySelector("html");
	var Tela = {
        scrollTop: parseInt(window.pageYOffset || 0),
        pageHeight: parseInt(Math.max(body.scrollHeight, body.offsetHeight, html.clientHeight, html.scrollHeight, html.offsetHeight)),
        viewportHeight: parseInt(html.clientHeight),
        viewportWidth: parseInt(html.clientWidth),
    };
	app.ports.scrollOrResize.send(Tela);
})
app.ports.uuid.subscribe(function(s) {
	
	function create_UUID(){
    var dt = new Date().getTime();
    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = (dt + Math.random()*16)%16 | 0;
        dt = Math.floor(dt/16);
        return (c=='x' ? r :(r&0x3|0x8)).toString(16);
    });
    return uuid;
	}


  var s = create_UUID();
  app.ports.observe_uuid.send(s);

});

registerServiceWorker();
