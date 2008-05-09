/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Class" object
 *
 * E262-4 proposals:meta_objects
 * E262-4 not yet documented
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

    use default namespace public;
    use namespace intrinsic;

    dynamic intrinsic class Class
    {
        /* 
           Becomes available as Object.prototype, Boolean.prototype,
           and so on. Note that this is not *really* initialized here;
           the proper initialization order is not expressible in the
           language. It's done in the underlying system. Null is just
           a placeholder so we can construct instances of Class.
        */

        const prototype = null;

        private var classname : string?;

        intrinsic function getClass() : string { return classname; }

//         intrinsic function isSubtypeOf(t/*: Type*/) /* : boolean */ {
//             for ( let s in superTypes() )
//                 if (s === t)
//                     return true;
//             return false;
//         }

//         intrinsic function superTypes() {
//             // FIXME #158: don't use "let" here, "let function" is broken.
//             var supers = []; // helper::superTypes(this);
//             var i = 0;
//             // Clunky
//             function next() {
//                 if (i == supers.length) 
//                     throw iterator::StopIteration; 
//                 return supers[i++];
//             }
//             var it = {}
//             it./*iterator::*/next = next;
//             return it;
//         }
    }
