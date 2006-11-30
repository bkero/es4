package
{
	// 15.1.1.1 NaN
	// 15.1.1.2 Infinity
	// 15.1.1.3 undefined
	// {DE,DD,RO} -- change from ECMA-262, which has them as {DE,DD}
	ECMA4 const NaN = @todo;
	ECMA4 const Infinity = @todo;
	ECMA4 const undefined = void(0);
	
	// @todo: "dynamic function" is probably redundant at the top level, but is useful for clarity
	
	// 15.1.2.1 eval (x)
	ECMA4 native function eval(x);

	// 15.1.2.2 parseInt (string , radix)
	ECMA4 native function parseInt(string:String, radix:Number);

	// 15.1.2.3 parseFloat (string)
	ECMA4 native function parseFloat(string:String);

	// 15.1.2.4 isNaN (number)
	ECMA4 native function isNaN(number:*):Boolean;

	// 15.1.2.4 isFinite (number)
	ECMA4 native function isFinite(number:*):Boolean;
	
	// 15.1.3.1 decodeURI (encodedURI)
	ECMA4 native function decodeURI(encodedURI);

	// 15.1.3.2 decodeURIComponent (encodedURIComponent)
	ECMA4 native function decodeURIComponent(encodedURIComponent);
	
	// 15.1.3.3 encodeURI (uri)
	ECMA4 native function encodeURI(uri);
	
	// 15.1.3.4 encodeURIComponent (uriComponent)
	ECMA4 native function encodeURIComponent(uriComponent);


	var NaN = ECMA4::NaN;
	var Infinity = ECMA4::Infinity;
	var undefined = ECMA4::undefined;

	var eval = ECMA4::eval;
	var parseInt = ECMA4::parseInt;
	var parseFloat = ECMA4::parseFloat;
	var isNaN = ECMA4::isNaN;
	var isFinite = ECMA4::isFinite;
	var decodeURI = ECMA4::decodeURI;
	var decodeURIComponent = ECMA4::decodeURIComponent;
	var encodeURI = ECMA4::encodeURI;
	var encodeURIComponent = ECMA4::encodeURIComponent;

} // package
