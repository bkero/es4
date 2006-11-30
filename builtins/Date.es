package
{
	dynamic class Date extends Object
	{		
		// 15.9.2 The Date Constructor Called as a Function
		static intrinsic function call(...args)
		{
			// args are ignored.
			return (new Date()).toString();
		}

		// 15.9.3 The Date Constructor
		// 15.9.3.1 new Date (year, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] )
		// 15.9.3.2 new Date (value)
		// 15.9.3.3 new Date ( )
		function Date(...args)
		{
			// conceptually:
			// this.[[Value]] = @todo
		}

		// 15.9.4 Properties of the Date Constructor
		static const length = 7;
		
		// 15.9.4.1 Date.prototype
		static const prototype = { };

		// 15.9.4.2 Date.parse (string)
		dynamic static function parse(string)
		{
			return _parse(string);
		}
		ECMA4 static function parse(string:String):Date
		{
			return _parse(string);
		}
		private static function _parse(string)
		{
			return @todo;
		}

		// 15.9.4.3 Date.UTC (year, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] )
		dynamic static function UTC(year, month, date, hours, minutes, seconds, ms)
		{
			return _UTC(year, month, date, hours, minutes, seconds, ms);
		}
		UTC.length = 7;
		ECMA4 static function UTC(year:Number, month:Number, date:Number, hours:Number, minutes:Number, seconds:Number, ms:Number):Number
		{
			return _UTC(year, month, date, hours, minutes, seconds, ms);
		}
		private static function _UTC(year, month, date, hours, minutes, seconds, ms)
		{
			return @todo;
		}

		// 15.9.5 Properties of the Date Prototype Object
   		prototype.[[Prototype]] = prototype;
   		prototype.[[Class]] = "Date";
		
		// 15.9.5.1 Date.prototype.constructor
		prototype.constructor = Date;

		// 15.9.5.2 Date.prototype.toString ( )
		prototype.toString = function(this:Date) { return this.ECMA4::toString(); }
		ECMA4 native function toString():String;

		// 15.9.5.3 Date.prototype.toDateString ( )
		prototype.toDateString = function(this:Date) { return this.ECMA4::toDateString(); }
		ECMA4 native function toDateString():String;

		// 15.9.5.4 Date.prototype.toTimeString ( )
		prototype.toTimeString = function(this:Date) { return this.ECMA4::toTimeString(); }
		ECMA4 native function toTimeString():String;

		// 15.9.5.5 Date.prototype.toLocaleString ( )
		prototype.toLocaleString = function(this:Date) { return this.ECMA4::toLocaleString(); }
		ECMA4 native function toLocaleString():String;

		// 15.9.5.6 Date.prototype.toLocaleDateString ( )
		prototype.toLocaleDateString = function(this:Date) { return this.ECMA4::toLocaleDateString(); }
		ECMA4 native function toLocaleDateString():String;

		// 15.9.5.7 Date.prototype.toLocaleTimeString ( )
		prototype.toLocaleTimeString = function(this:Date) { return this.ECMA4::toLocaleTimeString(); }
		ECMA4 native function toLocaleTimeString():String;

		// 15.9.5.8 Date.prototype.valueOf ( )
		prototype.valueOf = function(this:Date) { return this.ECMA4::valueOf(); }
		ECMA4 native function valueOf():Object;

		// 15.9.5.9 Date.prototype.getTime ( )
		prototype.getTime = function(this:Date) { return this.ECMA4::getTime(); }
		ECMA4 native function getTime():Number;

		// 15.9.5.10 Date.prototype.getFullYear ( )
		prototype.getFullYear = function(this:Date) { return this.ECMA4::getFullYear(); }
		ECMA4 native function getFullYear():Number;

		// 15.9.5.11 Date.prototype.getUTCFullYear ( )
		prototype.getUTCFullYear = function(this:Date) { return this.ECMA4::getUTCFullYear(); }
		ECMA4 native function getUTCFullYear():Number;

		// 15.9.5.12 Date.prototype.getMonth ( )
		prototype.getMonth = function(this:Date) { return this.ECMA4::getMonth(); }
		ECMA4 native function getMonth():Number;
		
		// 15.9.5.13 Date.prototype.getUTCMonth ( )
		prototype.getUTCMonth = function(this:Date) { return this.ECMA4::getUTCMonth(); }
		ECMA4 native function getUTCMonth():Number;
		
		// 15.9.5.14 Date.prototype.getDate ( )
		prototype.getDate = function(this:Date) { return this.ECMA4::getDate(); }
		ECMA4 native function getDate():Number;

		// 15.9.5.15 Date.prototype.getUTCDate ( )
		prototype.getUTCDate = function(this:Date) { return this.ECMA4::getUTCDate(); }
		ECMA4 native function getUTCDate():Number;

		// 15.9.5.16 Date.prototype.getDay ( )
		prototype.getDay = function(this:Date) { return this.ECMA4::getDay(); }
		ECMA4 native function getDay():Number;

		// 15.9.5.17 Date.prototype.getUTCDay ( )
		prototype.getUTCDay = function(this:Date) { return this.ECMA4::getUTCDay(); }
		ECMA4 native function getUTCDay():Number;

		// 15.9.5.18 Date.prototype.getHours ( )
		prototype.getHours = function(this:Date) { return this.ECMA4::getHours(); }
		ECMA4 native function getHours():Number;

		// 15.9.5.19 Date.prototype.getUTCHours ( )
		prototype.getUTCHours = function(this:Date) { return this.ECMA4::getUTCHours(); }
		ECMA4 native function getUTCHours():Number;

		// 15.9.5.20 Date.prototype.getMinutes ( )
		prototype.getMinutes = function(this:Date) { return this.ECMA4::getMinutes(); }
		ECMA4 native function getMinutes():Number;
		
		// 15.9.5.21 Date.prototype.getUTCMinutes ( )
		prototype.getUTCMinutes = function(this:Date) { return this.ECMA4::getUTCMinutes(); }
		ECMA4 native function getUTCMinutes():Number;

		// 15.9.5.22 Date.prototype.getSeconds ( )
		prototype.getSeconds = function(this:Date) { return this.ECMA4::getSeconds(); }
		ECMA4 native function getSeconds():Number;

		// 15.9.5.23 Date.prototype.getUTCSeconds ( )
		prototype.getUTCSeconds = function(this:Date) { return this.ECMA4::getUTCSeconds(); }
		ECMA4 native function getUTCSeconds():Number;

		// 15.9.5.24 Date.prototype.getMilliseconds ( )
		prototype.getMilliseconds = function(this:Date) { return this.ECMA4::getMilliseconds(); }
		ECMA4 native function getMilliseconds():Number;
		
		// 15.9.5.25 Date.prototype.getUTCMilliseconds ( )
		prototype.getUTCMilliseconds = function(this:Date) { return this.ECMA4::getUTCMilliseconds(); }
		ECMA4 native function getUTCMilliseconds():Number;
		
		// 15.9.5.26 Date.prototype.getTimezoneOffset ( )
		prototype.getTimezoneOffset = function(this:Date) { return this.ECMA4::getTimezoneOffset(); }
		ECMA4 native function getTimezoneOffset():Number;

		// 15.9.5.27 Date.prototype.setTime (time)
		prototype.setTime = function(this:Date, time:*) { return this.ECMA4::setTime(Number(time)); }
		ECMA4 native function setTime(time:Number):Number;

		// 15.9.5.28 Date.prototype.setMilliseconds (ms)
		prototype.setMilliseconds = function(this:Date, ms:*) { return this.ECMA4::setMilliseconds(Number(ms)); }
		ECMA4 native function setMilliseconds(ms:Number):Number;

		// 15.9.5.29 Date.prototype.setUTCMilliseconds (ms)
		prototype.setUTCMilliseconds = function(this:Date, ms:*) { return this.ECMA4::setUTCMilliseconds(Number(ms)); }
		ECMA4 native function setUTCMilliseconds(ms:Number):Number;

		// 15.9.5.30 Date.prototype.setSeconds (sec [, ms ] )
		prototype.setSeconds = function(this:Date, sec:*, ms:*) { return this.ECMA4::setSeconds(Number(sec), Number(ms)); }
		prototype.setSeconds.length = 2;
		ECMA4 native function setSeconds(sec:Number, ms:Number = NaN):Number;

		// 15.9.5.31 Date.prototype.setUTCSeconds (sec [, ms ] )
		prototype.setUTCSeconds = function(this:Date, sec:*, ms:*) { return this.ECMA4::setUTCSeconds(Number(sec), Number(ms)); }
		prototype.setUTCSeconds.length = 2;
		ECMA4 native function setUTCSeconds(sec:Number, ms:Number = NaN):Number;

		// 15.9.5.33 Date.prototype.setMinutes (min [, sec [, ms ] ] )
		prototype.setMinutes = function(this:Date, min:*, sec:*, ms:*) { return this.ECMA4::setMinutes(Number(min), Number(sec), Number(ms)); }
		prototype.setMinutes.length = 3;
		ECMA4 native function setMinutes(min:Number, sec:Number = NaN, ms:Number = NaN):Number;

		// 15.9.5.34 Date.prototype.setUTCMinutes (min [, sec [, ms ] ] )
		prototype.setUTCMinutes = function(this:Date, min:*, sec:*, ms:*) { return this.ECMA4::setUTCMinutes(Number(min), Number(sec), Number(ms)); }
		prototype.setUTCMinutes.length = 3;
		ECMA4 native function setUTCMinutes(min:Number, sec:Number = NaN, ms:Number = NaN):Number;

		// 15.9.5.35 Date.prototype.setHours (hour [, min [, sec [, ms ] ] ] )
		prototype.setHours = function(this:Date, hour:*, min:*, sec:*, ms:*) { return this.ECMA4::setHours(Number(hour), Number(min), Number(sec), Number(ms)); }
		prototype.setHours.length = 4;
		ECMA4 native function setHours(hour:Number, min:Number = NaN, sec:Number = NaN, ms:Number = NaN):Number;

		// 15.9.5.36 Date.prototype.setUTCHours (hour [, min [, sec [, ms ] ] ] )
		prototype.setUTCHours = function(this:Date, hour:*, min:*, sec:*, ms:*) { return this.ECMA4::setUTCHours(Number(hour), Number(min), Number(sec), Number(ms)); }
		prototype.setUTCHours.length = 4;
		ECMA4 native function setUTCHours(hour:Number, min:Number = NaN, sec:Number = NaN, ms:Number = NaN):Number;

		// 15.9.5.36 Date.prototype.setDate (date)
		prototype.setDate = function(this:Date, date:*) { return this.ECMA4::setDate(Number(date)); }
		ECMA4 native function setDate(date:Number):Number;

		// 15.9.5.37 Date.prototype.setUTCDate (date)
		prototype.setUTCDate = function(this:Date, date:*) { return this.ECMA4::setUTCDate(Number(date)); }
		ECMA4 native function setUTCDate(date:Number):Number;

		// 15.9.5.38 Date.prototype.setMonth (month [, date ] )
		prototype.setMonth = function(this:Date, month:*, date:*) { return this.ECMA4::setMonth(Number(month), Number(date)); }
		prototype.setMonth.length = 2;
		ECMA4 native function setMonth(month:Number, date:Number = NaN):Number;

		// 15.9.5.39 Date.prototype.setUTCMonth (month [, date ] )
		prototype.setUTCMonth = function(this:Date, month:*, date:*) { return this.ECMA4::setUTCMonth(Number(month), Number(date)); }
		prototype.setUTCMonth.length = 2;
		ECMA4 native function setUTCMonth(month:Number, date:Number = NaN):Number;

		// 15.9.5.40 Date.prototype.setFullYear (year [, month [, date ] ] )
		prototype.setFullYear = function(this:Date, year:*, month:*, date:*) { return this.ECMA4::setFullYear(Number(year), Number(month), Number(date)); }
		prototype.setFullYear.length = 3;
		ECMA4 native function setFullYear(year:Number, month:Number = NaN, date:Number = NaN):Number;

		// 15.9.5.41 Date.prototype.setUTCFullYear (year [, month [, date ] ] )
		prototype.setUTCFullYear = function(this:Date, year:*, month:*, date:*) { return this.ECMA4::setUTCFullYear(Number(year), Number(month), Number(date)); }
		prototype.setUTCFullYear.length = 3;
		ECMA4 native function setUTCFullYear(year:Number, month:Number = NaN, date:Number = NaN):Number;

		// 15.9.5.42 Date.prototype.toUTCString ( )
		prototype.toUTCString = function(this:Date) { return this.ECMA4::toUTCString(); }
		ECMA4 native function toUTCString():String;
		
		// mark all prototype functions as {DE}
		_dontEnum(prototype);

	} // class
} // package
