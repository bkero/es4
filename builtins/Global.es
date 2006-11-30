package
{
		// 15.1.1.1 NaN
		// 15.1.1.2 Infinity
		// 15.1.1.3 undefined
		// {DE,DD,RO} -- change from ECMA-262, which has them as {DE,DD}
		const NaN = @todo;
		const Infinity = @todo;
		const undefined = void(0);
		
		// @todo: "dynamic function" is probably redundant at the top level, but is useful for clarity
		
		// 15.1.2.1 eval (x)
		dynamic function eval(x)
		{
			return @todo;
		}

		// 15.1.2.2 parseInt (string , radix)
		dynamic function parseInt(string, radix)
		{
			return @todo;
		}

		// 15.1.2.3 parseFloat (string)
		dynamic function parseFloat(string)
		{
			return @todo;
		}

		// 15.1.2.4 isNaN (number)
		dynamic function isNaN(number):Boolean
		{
			return Number(number) === NaN;
		}

		// 15.1.2.4 isFinite (number)
		dynamic function isFinite(number):Boolean
		{
			var theNumber:Number = Number(number);
			return !(theNumber === NaN && 
					theNumber === Infinity &&
					theNumber === -Infinity);
					
		}
		
		// 15.1.3.1 decodeURI (encodedURI)
		dynamic function decodeURI(encodedURI)
		{
			return @todo;
		}

		// 15.1.3.2 decodeURIComponent (encodedURIComponent)
		dynamic function decodeURIComponent(encodedURIComponent)
		{
			return @todo;
		}
		
		// 15.1.3.3 encodeURI (uri)
		dynamic function encodeURI(uri)
		{
			return @todo;
		}
		
		// 15.1.3.4 encodeURIComponent (uriComponent)
		dynamic function encodeURIComponent(uriComponent)
		{
			return @todo;
		}
	} // class
} // package
