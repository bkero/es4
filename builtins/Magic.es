/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - magic functions
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
 *
 * Magic functions are implementation traps that express aspects of
 * the language that cannot be expressed without either (a) modeling
 * the language in the language, eg, by modeling objects and their
 * property lists, or (b) breaking security.
 *
 * A good example is [[Prototype]], which exists on all objects but
 * which is presumed not to be available for reading or writing by the
 * user program.  We can model it directly using eg ___proto___, but
 * not without creating facilities that are presumed not to exist
 * (reading that field).
 *
 * Library code references magic functions as global functions in the
 * "magic" namespace, eg "magic::getPrototype(o)".
 *
 * Do note that a conforming implementation can't express the magic
 * hooks quite like shown here because the magic namespace pollutes
 * the name space for user programs.  An implementation might instead
 * supply a flag --magic to the compiler that allows the compiler to
 * magically know about "magic" when the library files are being
 * compiled.
 */
    use namespace intrinsic;

    /* --------------------------------------------------------------

       CLASS INSTANTIATION.  */

    /*
     * Given a class object, run the standard object-construction
     * protocol for it (and its base classes, initializers, settings,
     * ctors). Return the resulting instance, always an Object!
     */
    magic native function construct(cls:Class!, args:[*]) : Object!;


    /* --------------------------------------------------------------

       PROPERTY MANIPULATION.  */

    /* Retrieve the [[Class]] property of o */
    magic native function getClassName(o : Object!) : string;

    /* Retrieve the class object of o */
    magic native function getClassOfObject(o : Object!) : Class;

    /* Retrieve the base class of cls, or null. */
    magic native function getSuperClass(cls : Class!) : Class;

    /* Retrieve the kth implemented interface of cls, or null. */
    magic native function getImplementedInterface(cls: Class!, k: double) : Interface;

    /* Retrieve the kth superinterface of iface, or null. */
    magic native function getSuperInterface(iface: Interface!, k: double) : Interface;

    /* Retrieve the possibly null [[Prototype]] property of o */
    magic native function getPrototype(o : Object!) : Object;

    /* Return true iff o has a local property named by p. */
    magic native function hasOwnProperty(o : Object!, p : (Name|string)) : boolean;

    /* Return true if the property p does exists locally on o and its
       DontEnum bit is set */
    magic native function getPropertyIsDontEnum(o : Object!, p : (Name|string)) : boolean;

    /* Return true if the property p does exists locally on o and its
       DontDelete bit is set */
    magic native function getPropertyIsDontDelete(o : Object!, p : (Name|string)) : boolean;

    /* Provided that the property p exists locally on o, set its DontEnum
       flag according to f.  If the property p does not exist locally on
       o, it does nothing. */
    magic native function setPropertyIsDontEnum(o : Object!, p : (Name|string), f : boolean) : void;

    magic native function isPrimitive(v:*) : boolean;
    magic native function toPrimitive(v:*, hint:string) : *;
    magic native function defaultValue(ob:Object!, hint:string) : *;

    /* ----------------------------------------------------------------

       FUNCTION MANIPULATION.  */

    /* Given a function object, a this object, and an array of argument
       values, call the function with the this object and arguments. */
    magic native function apply(fn : Function!, t : Object!, args : Array) : *;


    magic native function fnLength(fn: Function!) : double;

    /* ----------------------------------------------------------------

       STRING MANIPULATION.  Strings contain string data in some
       unspecified way - there is no representation of string data in
       the language.  The following magic functions access and set
       those string data.  */

    /* Given a string and a position in that string, return the
       numeric value of the character at that position in the
       string.  */
    magic native function charCodeAt(s : string, pos : double) : double;

    /* Given a numeric character value, return a string of length 1
       whose element 0 is the character with that same value.  */
    magic native function fromCharCode(ch : double) : string;

    /* Given a string object, return the number of characters in the
     * string. */
    magic native function stringLength(s : string) : double;

    /* Given two string objects A and B , return a new string object
       containing the characters from A followed by the characters
       from B.  */
    magic native function stringAppend(a : string, b : string) : string;
