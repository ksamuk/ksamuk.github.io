// OBJECT: 'retype' controls the deletion and creation of new words
var retype = {
	// ARRAY: 'retypePhrases' contains the words that will be switched
	//		  The tool replaces the word contained within the element with the ID of 'retype'
	//		  It works cleaner if neighboring words have different first letters.
	//		  Spaces in phrases can cause a hiccup. Best practice to keep phrases as single words.
	retypePhrases: [
		'field studies.',
		'simulations.',
		'crispr/cas9.',
        'bioinformatics.',
        'genomics.'
        
	],
	index       : -1,
	elem        : document.getElementById('retype'),
	start       : function(){
		var _this = this;
		setTimeout( function(){
			_this.deleteLetter();
		}, 2000 ); // Delay the start of a new word by 3 seconds
	},// END retype.start()
	deleteRepeat: function(){
		this.deleteLetter();
	},// END retype.deleteRepeat()
	deleteLetter: function(){
		var newWord = this.elem.innerHTML;
		if( newWord.length > 0 ){
			newWord = newWord.substring(0, newWord.length - 1);
			var _this = this;
			setTimeout( function(){
				_this.elem.innerHTML = newWord;
				_this.deleteRepeat();
			}, 75 );
		}else{
			this.newLetter();
		}// END if( newWord.length > 0 )
	},// END retype.deleteLetter()
	newRepeat   : function(){
		this.newLetter();
	},// END retype.newRepeat()
	newLetter   : function(){
		var newWord = this.elem.innerHTML;
		if( newWord.length === 0 ){
			this.index++;
			if( this.index >= this.retypePhrases.length ){
				this.index = 0;
			}
		}// END if( newWord.length === 0 )
		var newLetters = this.retypePhrases[ this.index ];
		if( newLetters.length > newWord.length ){
			newLetters = newLetters.substring(0, ( newWord.length + 1 ) );
			var _this = this;
			// Add a slight random variation in retype time to make the letter typing seem more 'human'
			var time = Math.round( Math.random() * 100 ) + 100;
			setTimeout( function(){
				_this.elem.innerHTML = newLetters;
				_this.newLetter();
			}, time );
		}else{
			this.start();
			// Yep, this makes the retype an infinite loop
		}// END if( newLetters.length > newWord.length )
	}// END retype.newLetter()
};

retype.start();