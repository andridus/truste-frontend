var _heldersousa$cartel$Native_Uuid = function(){
  function gen(time){
  	function create_UUID(time){
	    var dt = time;
	    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
	        var r = (dt + Math.random()*16)%16 | 0;
	        dt = Math.floor(dt/16);
	        return (c=='x' ? r :(r&0x3|0x8)).toString(16);
	    });
	    return uuid;
		}
		return create_UUID(time);
  }

  function random(r){
  	return Math.floor(Math.random() * r)
  }

  return {
    gen: gen,
    random: random
  };
}();