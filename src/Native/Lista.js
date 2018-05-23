var _heldersousa$cartel$Native_Lista = function(){

  var M = 32;
  var E = 2;

  // An empty array.
  var empty = {
    ctor: '_Array',
    height: 0,
    table: []
  };

  function toJSArray(a)
  {
    var jsArray = new Array(length(a));
    toJSArray_(jsArray, 0, a);
    return jsArray;
  }

  function toJSArray_(jsArray, i, a)
  {
    for (var t = 0; t < a.table.length; t++)
    {
      if (a.height === 0)
      {
        jsArray[i + t] = a.table[t];
      }
      else
      {
        var inc = t === 0 ? 0 : a.lengths[t - 1];
        toJSArray_(jsArray, i + inc, a.table[t]);
      }
    }
  }

  function fromJSArray(jsArray)
  {
    if (jsArray.length === 0)
    {
      return empty;
    }
    var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
    return fromJSArray_(jsArray, h, 0, jsArray.length);
  }

  function fromJSArray_(jsArray, h, from, to)
  {
    if (h === 0)
    {
      return {
        ctor: '_Array',
        height: 0,
        table: jsArray.slice(from, to)
      };
    }

    var step = Math.pow(M, h);
    var table = new Array(Math.ceil((to - from) / step));
    var lengths = new Array(table.length);
    for (var i = 0; i < table.length; i++)
    {
      table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
      lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
    }
    return {
      ctor: '_Array',
      height: h,
      table: table,
      lengths: lengths
    };
  }
  function length(array)
  {
    if (array.height === 0)
    {
      return array.table.length;
    }
    else
    {
      return array.lengths[array.lengths.length - 1];
    }
  }



  function shuffle(array) {
    
    var lista2 =toJSArray(array);
    var counter = lista2.length;
    // While there are elements in the array
    while (counter > 0) {
        // Pick a random index
        let index = Math.floor(Math.random() * counter);

        // Decrease counter by 1
        counter--;

        // And swap the last element with it
        let temp = lista2[counter];
        lista2[counter] = lista2[index];
        lista2[index] = temp;
    }
    return fromJSArray(lista2);
  }

  function getElements(num, lista1){
  	var droped = [];
    var lista2 =toJSArray(lista1);
  	if(num>=0){
  		while(num>0){
        if(lista2.length){
          var l1 = lista2.splice(0,1);
          droped.push( l1[0] )  
        }
	  		num--;
  		}	
  	}
  	
  	return {
			ctor: '_Tuple2',
			_0: fromJSArray(droped),
			_1: fromJSArray(lista2)
		};
  }

  return {
    shuffle: shuffle,
    getElements: F2(getElements)
  };
}();