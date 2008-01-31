/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "DecimalContext" object
 *
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *
 */

package 
{
    use default namespace public;
    use namespace __ES4__;
    use strict;
    
    __ES4__ final class DecimalContext
    {
	static const ROUND_CEILING : uint = 0;
        static const ROUND_FLOOR : uint = 1;
        static const ROUND_UP : uint = 2;
        static const ROUND_DOWN : uint = 3;
	static const ROUND_HALF_UP : uint = 4;
	static const ROUND_HALF_DOWN : uint = 5;
	static const ROUND_HALF_EVEN : uint = 6;
	
	const precision : uint;
	const mode : uint;
	
        function DecimalContext(precision:uint=34, mode:uint=ROUND_HALF_EVEN) : 
	    precision = precision,
	    mode = mode 
	{}
	    
        meta static function invoke(precision:uint=34, mode:uint=ROUND_HALF_EVEN) : DecimalContext
            new DecimalContext(precision, mode);
        
        prototype function toString(this : DecimalContext)
            this.intrinsic::toString();
        
        override intrinsic function toString() : string {
	    let m : string = "DecimalContext.ROUND_";
	    switch (mode) {
	    case 0: 
		m += "CEILING";
		break;
	    case 1: 
		m += "FLOOR";
		break;
	    case 2:
		m += "UP";
		break;
	    case 3:
		m += "DOWN";
		break;
	    case 4: 
		m += "HALF_UP";
		break;
	    case 5:
		m += "HALF_DOWN";
		break;
	    case 6:
	    default:
		m += "HALF_EVEN";
		break;
	    }
            return "DecimalContext(" + precision + ", " + m + ")";
        }
        
        prototype function valueOf(this : Name)
            this.intrinsic::valueOf();
        
        override intrinsic function valueOf() : string
            intrinsic::toString();        
    }
}
